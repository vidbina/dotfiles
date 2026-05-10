import { readFile } from 'fs/promises';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

export type SecretRef = { op: string };
export type SecretValue = string | SecretRef;

export interface WorkspaceTools {
  linear?: { apiKey: SecretValue };
  github?: { token: SecretValue };
  anthropic?: { apiKey: SecretValue };
}

export interface WorkspaceConfig {
  workspaceId: string;
  antProfile: string;
  skills: string[];
  tools: WorkspaceTools;
}

const repoRoot = join(dirname(fileURLToPath(import.meta.url)), '..', '..');

export function repoPath(...parts: string[]): string {
  return join(repoRoot, ...parts);
}

export async function loadWorkspace(alias: string): Promise<WorkspaceConfig> {
  const configPath = repoPath('claude', 'agents', alias, 'workspace.json');
  const raw = await readFile(configPath, 'utf-8');
  return JSON.parse(raw) as WorkspaceConfig;
}
