/**
 * Deploy skills from claude/skills/ to a workspace via the Skills API.
 *
 * Discovery-based: matches local skills to remote ones by display_title (= SKILL.md name).
 * No manifest stored — the workspace is always the source of truth for remote IDs.
 *
 * Usage: pnpm deploy:skills --workspace asabina
 */

import { readdir, readFile } from 'fs/promises';
import { join } from 'path';
import { loadWorkspace, repoPath } from '../workspace.js';
import { resolveWorkspaceSecrets } from '../tools/secrets.js';

interface RemoteSkill {
  id: string;
  display_title: string;
  latest_version: string;
}

async function listRemoteSkills(apiKey: string): Promise<Map<string, RemoteSkill>> {
  const res = await fetch('https://api.anthropic.com/v1/skills?source=custom&limit=100', {
    headers: {
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'anthropic-beta': 'skills-2025-10-02',
    },
  });
  if (!res.ok) throw new Error(`Skills API error: ${res.status} ${await res.text()}`);
  const { data } = (await res.json()) as { data: RemoteSkill[] };
  return new Map(data.map((s) => [s.display_title, s]));
}

async function uploadSkill(
  apiKey: string,
  skillName: string,
  skillPath: string,
  existingId?: string,
): Promise<void> {
  const files = await readdir(skillPath);
  const form = new FormData();
  if (!existingId) form.append('display_title', skillName);

  for (const file of files) {
    const content = await readFile(join(skillPath, file));
    form.append('files[]', new Blob([content]), `${skillName}/${file}`);
  }

  const url = existingId
    ? `https://api.anthropic.com/v1/skills/${existingId}/versions`
    : 'https://api.anthropic.com/v1/skills';

  const res = await fetch(url, {
    method: 'POST',
    headers: {
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'anthropic-beta': 'skills-2025-10-02',
    },
    body: form,
  });

  if (!res.ok) {
    throw new Error(`Skills API ${existingId ? 'version' : 'create'} error: ${await res.text()}`);
  }
}

export async function deploySkills(workspaceAlias: string): Promise<void> {
  console.log(`Deploying skills to workspace: ${workspaceAlias}`);

  const workspace = await loadWorkspace(workspaceAlias);
  const secrets = await resolveWorkspaceSecrets(workspace.tools);
  const apiKey = secrets.anthropicApiKey;
  if (!apiKey) throw new Error('No Anthropic API key resolved — set ANTHROPIC_API_KEY or configure tools.anthropic in workspace.json');

  const remote = await listRemoteSkills(apiKey);
  const skillsRoot = repoPath('claude', 'skills');

  let created = 0, updated = 0, failed = 0;

  for (const skillName of workspace.skills) {
    const skillPath = join(skillsRoot, skillName);
    const existing = remote.get(skillName);
    try {
      await uploadSkill(apiKey, skillName, skillPath, existing?.id);
      if (existing) { console.log(`  updated  ${skillName}`); updated++; }
      else           { console.log(`  created  ${skillName}`); created++; }
    } catch (err) {
      console.error(`  FAILED   ${skillName} — ${(err as Error).message}`);
      failed++;
    }
  }

  console.log(`\n${created} created, ${updated} updated, ${failed} failed`);
  if (failed > 0) process.exit(1);
}
