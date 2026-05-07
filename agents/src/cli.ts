#!/usr/bin/env node
/**
 * CLI entry point for the agents orchestrator.
 *
 * Commands:
 *   deploy:skills --workspace <alias>
 *   session --workspace <alias> --prompt <text> [--repo <url>] [--branch <name>]
 */

import { deploySkills } from './deploy/skills.js';
import { runSession } from './sessions/runner.js';

const args = process.argv.slice(2);
const command = args[0];

function flag(name: string): string | undefined {
  const i = args.indexOf(`--${name}`);
  return i !== -1 ? args[i + 1] : undefined;
}

async function main(): Promise<void> {
  switch (command) {
    case 'deploy:skills': {
      const workspace = flag('workspace');
      if (!workspace) { console.error('Usage: deploy:skills --workspace <alias>'); process.exit(1); }
      await deploySkills(workspace);
      break;
    }

    case 'session': {
      const workspace = flag('workspace');
      const prompt    = flag('prompt');
      const repo      = flag('repo');
      const branch    = flag('branch');
      if (!workspace || !prompt) {
        console.error('Usage: session --workspace <alias> --prompt <text> [--repo <url>] [--branch <name>]');
        process.exit(1);
      }
      await runSession({
        workspaceAlias: workspace,
        prompt,
        ...(repo ? { githubRepo: { url: repo, branch } } : {}),
      });
      break;
    }

    default:
      console.error(`Unknown command: ${command ?? '(none)'}`);
      console.error('Commands: deploy:skills, session');
      process.exit(1);
  }
}

main().catch((err) => {
  console.error(err instanceof Error ? err.message : err);
  process.exit(1);
});
