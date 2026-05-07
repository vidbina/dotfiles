import { execFile } from 'child_process';
import { promisify } from 'util';
import type { SecretRef, SecretValue, WorkspaceTools } from '../workspace.js';

const execFileAsync = promisify(execFile);

/**
 * Resolve a secret value. Priority:
 * 1. Environment variable override (envVar)
 * 2. Plain string literal
 * 3. 1Password reference via `op read`
 */
export async function resolveSecret(value: SecretValue, envVar?: string): Promise<string> {
  if (envVar && process.env[envVar]) return process.env[envVar]!;
  if (typeof value === 'string') return value;
  const ref = value as SecretRef;
  const args = ['read', ref.op];
  const account = process.env['OP_ACCOUNT'];
  if (account) args.push('--account', account);
  const { stdout } = await execFileAsync('op', args);
  return stdout.trim();
}

export interface ResolvedSecrets {
  linearApiKey?: string;
  githubToken?: string;
  anthropicApiKey?: string;
}

export async function resolveWorkspaceSecrets(tools: WorkspaceTools): Promise<ResolvedSecrets> {
  const [linearApiKey, githubToken, anthropicApiKey] = await Promise.all([
    tools.linear   ? resolveSecret(tools.linear.apiKey,    'LINEAR_API_KEY')   : undefined,
    tools.github   ? resolveSecret(tools.github.token,     'GITHUB_TOKEN')     : undefined,
    tools.anthropic? resolveSecret(tools.anthropic.apiKey, 'ANTHROPIC_API_KEY'): undefined,
  ]);
  return { linearApiKey, githubToken, anthropicApiKey };
}
