/**
 * GitHub custom tool handlers.
 *
 * Replaces direct GITHUB_TOKEN env-var injection in the session container.
 * Using custom tool dispatch + `limited` networking eliminates the prompt-injection
 * exfiltration surface for the token.
 *
 * Each handler is async (input: Record<string, unknown>) => Promise<unknown>.
 * The session runner dispatches agent.custom_tool_use events to these by name.
 */

type ToolInput = Record<string, unknown>;
type ToolHandler = (input: ToolInput) => Promise<unknown>;

const GITHUB_API = 'https://api.github.com';

async function ghFetch(
  token: string,
  method: string,
  path: string,
  body?: unknown,
): Promise<unknown> {
  const res = await fetch(`${GITHUB_API}${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${token}`,
      Accept: 'application/vnd.github+json',
      'X-GitHub-Api-Version': '2022-11-28',
      ...(body ? { 'Content-Type': 'application/json' } : {}),
    },
    ...(body ? { body: JSON.stringify(body) } : {}),
  });
  if (!res.ok) {
    throw new Error(`GitHub HTTP ${res.status}: ${await res.text()}`);
  }
  return res.json() as Promise<unknown>;
}

export function createGithubToolHandlers(token: string): Record<string, ToolHandler> {
  const get = (path: string) => ghFetch(token, 'GET', path);
  const post = (path: string, body: unknown) => ghFetch(token, 'POST', path, body);

  return {
    github_get_repo_info: ({ owner, repo }) =>
      get(`/repos/${owner}/${repo}`),

    github_get_default_branch: async ({ owner, repo }) => {
      const info = (await get(`/repos/${owner}/${repo}`)) as { default_branch: string };
      return { default_branch: info.default_branch };
    },

    github_push_branch: async ({ owner, repo, branch, base, files }) => {
      // 1. Resolve base SHA
      const baseRef = (await get(
        `/repos/${owner}/${repo}/git/refs/heads/${base}`,
      )) as { object: { sha: string } };
      const baseSha = baseRef.object.sha;

      // 2. Build tree entries (base64 blobs)
      const fileList = files as Array<{ path: string; content: string }>;
      const tree = fileList.map((f) => ({
        path: f.path,
        mode: '100644',
        type: 'blob',
        content: f.content,
      }));

      const treeRes = (await post(`/repos/${owner}/${repo}/git/trees`, {
        base_tree: baseSha,
        tree,
      })) as { sha: string };

      // 3. Create commit
      const commitRes = (await post(`/repos/${owner}/${repo}/git/commits`, {
        message: `chore: push branch ${branch}`,
        tree: treeRes.sha,
        parents: [baseSha],
      })) as { sha: string };

      // 4. Create branch ref
      return post(`/repos/${owner}/${repo}/git/refs`, {
        ref: `refs/heads/${branch}`,
        sha: commitRes.sha,
      });
    },

    github_create_pr: ({ owner, repo, title, body, head, base }) =>
      post(`/repos/${owner}/${repo}/pulls`, { title, body, head, base }),
  };
}

/** JSON Schema definitions for registering custom tools on an agent. */
export const githubToolSchemas: Array<{
  name: string;
  description: string;
  input_schema: object;
}> = [
  {
    name: 'github_get_repo_info',
    description: 'Get repository metadata from GitHub',
    input_schema: {
      type: 'object',
      properties: {
        owner: { type: 'string', description: 'Repository owner (user or org)' },
        repo: { type: 'string', description: 'Repository name' },
      },
      required: ['owner', 'repo'],
    },
  },
  {
    name: 'github_get_default_branch',
    description: 'Get the default branch name of a GitHub repository',
    input_schema: {
      type: 'object',
      properties: {
        owner: { type: 'string', description: 'Repository owner (user or org)' },
        repo: { type: 'string', description: 'Repository name' },
      },
      required: ['owner', 'repo'],
    },
  },
  {
    name: 'github_push_branch',
    description:
      'Create a new branch on GitHub by committing a set of files on top of a base branch',
    input_schema: {
      type: 'object',
      properties: {
        owner: { type: 'string', description: 'Repository owner' },
        repo: { type: 'string', description: 'Repository name' },
        branch: { type: 'string', description: 'New branch name to create' },
        base: { type: 'string', description: 'Base branch to branch from' },
        files: {
          type: 'array',
          description: 'Files to include in the commit',
          items: {
            type: 'object',
            properties: {
              path: { type: 'string' },
              content: { type: 'string' },
            },
            required: ['path', 'content'],
          },
        },
      },
      required: ['owner', 'repo', 'branch', 'base', 'files'],
    },
  },
  {
    name: 'github_create_pr',
    description: 'Create a pull request on GitHub',
    input_schema: {
      type: 'object',
      properties: {
        owner: { type: 'string', description: 'Repository owner' },
        repo: { type: 'string', description: 'Repository name' },
        title: { type: 'string', description: 'PR title' },
        body: { type: 'string', description: 'PR description body' },
        head: { type: 'string', description: 'Branch containing the changes' },
        base: { type: 'string', description: 'Branch to merge into' },
      },
      required: ['owner', 'repo', 'title', 'head', 'base'],
    },
  },
];
