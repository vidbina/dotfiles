/**
 * Session runner for Claude Managed Agents.
 *
 * Lifecycle:
 * 1. Load workspace config + resolve secrets
 * 2. Discover remote skill IDs (by display_title)
 * 3. Create agent definition with skills + custom Linear tools
 * 4. Create session and stream events
 * 5. Dispatch agent.custom_tool_use → Linear handlers or HITL prompt
 */

import Anthropic from '@anthropic-ai/sdk';
import { loadWorkspace, repoPath } from '../workspace.js';
import { resolveWorkspaceSecrets } from '../tools/secrets.js';
import { createLinearToolHandlers, linearToolSchemas } from '../tools/linear.js';
import { join } from 'path';

interface RemoteSkill {
  id: string;
  display_title: string;
  latest_version: string;
}

async function discoverSkillIds(
  apiKey: string,
  skillNames: string[],
): Promise<Array<{ skill_id: string; version: string }>> {
  const res = await fetch('https://api.anthropic.com/v1/skills?source=custom&limit=100', {
    headers: {
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'anthropic-beta': 'skills-2025-10-02',
    },
  });
  const { data } = (await res.json()) as { data: RemoteSkill[] };
  const remote = new Map(data.map((s) => [s.display_title, s]));

  return skillNames
    .filter((name) => remote.has(name))
    .map((name) => ({ skill_id: remote.get(name)!.id, version: 'latest' }));
}

export interface RunSessionOptions {
  workspaceAlias: string;
  prompt: string;
  /** Optional: mount a GitHub repo resource into the session */
  githubRepo?: { url: string; branch?: string };
}

export async function runSession(options: RunSessionOptions): Promise<void> {
  const { workspaceAlias, prompt, githubRepo } = options;

  const workspace = await loadWorkspace(workspaceAlias);
  const secrets = await resolveWorkspaceSecrets(workspace.tools);

  if (!secrets.anthropicApiKey) throw new Error('No Anthropic API key resolved');
  if (!secrets.linearApiKey) throw new Error('No Linear API key resolved');

  const client = new Anthropic({ apiKey: secrets.anthropicApiKey });
  const linearHandlers = createLinearToolHandlers(secrets.linearApiKey);

  // Discover which uploaded skills are available
  const skills = await discoverSkillIds(secrets.anthropicApiKey, workspace.skills);
  if (skills.length === 0) {
    console.warn('Warning: no skills found in workspace — run deploy:skills first');
  }

  // Build custom tool definitions (Linear ops registered with MCP-compatible names)
  const customTools = linearToolSchemas.map((schema) => ({
    type: 'custom' as const,
    name: schema.name,
    description: schema.description,
    input_schema: schema.input_schema,
  }));

  // Build resources
  const resources = [];
  if (githubRepo) {
    if (!secrets.githubToken) throw new Error('GitHub repo requested but no GITHUB_TOKEN resolved');
    resources.push({
      type: 'github_repository' as const,
      url: githubRepo.url,
      authorization_token: secrets.githubToken,
      ...(githubRepo.branch ? { checkout: { type: 'branch' as const, branch_name: githubRepo.branch } } : {}),
    });
  }

  // Create agent
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const beta = (client as any).beta;
  const agent = await beta.agents.create({
    model: 'claude-opus-4-6',
    name: `managed-agent-${workspaceAlias}-${Date.now()}`,
    tools: [
      { type: 'bash_20250124' },
      { type: 'text_editor_20250429' },
      { type: 'web_search_20250305' },
      ...customTools,
    ],
    skills,
    network_access: { type: 'unrestricted' },
  });

  console.log(`Agent created: ${agent.id}`);

  // Create session
  const session = await beta.sessions.create({
    agent_id: agent.id,
    ...(resources.length > 0 ? { resources } : {}),
  });

  console.log(`Session created: ${session.id}`);

  // Send initial prompt and stream events
  const stream = await beta.sessions.events.stream(session.id, { content: prompt });

  for await (const event of stream) {
    switch (event.type) {
      case 'agent.text':
        process.stdout.write(event.content ?? '');
        break;

      case 'agent.custom_tool_use': {
        const handler = linearHandlers[event.name];
        if (!handler) {
          console.error(`\nUnknown custom tool: ${event.name}`);
          await beta.sessions.events.create(session.id, {
            type: 'user.custom_tool_result',
            tool_use_id: event.id,
            content: `Error: no handler registered for tool ${event.name}`,
            is_error: true,
          });
          break;
        }
        try {
          const result = await handler(event.input as Record<string, unknown>);
          await beta.sessions.events.create(session.id, {
            type: 'user.custom_tool_result',
            tool_use_id: event.id,
            content: JSON.stringify(result),
          });
        } catch (err) {
          await beta.sessions.events.create(session.id, {
            type: 'user.custom_tool_result',
            tool_use_id: event.id,
            content: `Error: ${(err as Error).message}`,
            is_error: true,
          });
        }
        break;
      }

      case 'agent.done':
        console.log('\n\nSession complete.');
        break;
    }
  }
}
