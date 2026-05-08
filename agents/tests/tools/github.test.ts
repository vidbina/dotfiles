import { describe, it, expect, vi, beforeEach } from 'vitest';
import { createGithubToolHandlers } from '../../src/tools/github.js';

const TOKEN = 'test-github-token';

function mockFetch(responseData: unknown) {
  vi.stubGlobal(
    'fetch',
    vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve(responseData),
    }),
  );
}

function mockFetchError(status: number, body: string) {
  vi.stubGlobal(
    'fetch',
    vi.fn().mockResolvedValue({
      ok: false,
      status,
      text: () => Promise.resolve(body),
    }),
  );
}

function lastFetchCall(): [string, RequestInit] {
  const calls = vi.mocked(fetch).mock.calls;
  return calls[calls.length - 1] as [string, RequestInit];
}

beforeEach(() => vi.unstubAllGlobals());

describe('createGithubToolHandlers', () => {
  const handlers = createGithubToolHandlers(TOKEN);

  it('sends Authorization header with Bearer token', async () => {
    mockFetch({ default_branch: 'main' });
    await handlers['github_get_default_branch']!({ owner: 'acme', repo: 'widget' });
    const [, init] = lastFetchCall();
    expect((init.headers as Record<string, string>)['Authorization']).toBe(`Bearer ${TOKEN}`);
  });

  it('sends Accept: application/vnd.github+json header', async () => {
    mockFetch({ default_branch: 'main' });
    await handlers['github_get_default_branch']!({ owner: 'acme', repo: 'widget' });
    const [, init] = lastFetchCall();
    expect((init.headers as Record<string, string>)['Accept']).toBe(
      'application/vnd.github+json',
    );
  });

  it('sends X-GitHub-Api-Version header', async () => {
    mockFetch({ default_branch: 'main' });
    await handlers['github_get_default_branch']!({ owner: 'acme', repo: 'widget' });
    const [, init] = lastFetchCall();
    expect((init.headers as Record<string, string>)['X-GitHub-Api-Version']).toBe('2022-11-28');
  });

  describe('github_get_repo_info', () => {
    it('calls GET /repos/{owner}/{repo}', async () => {
      const repoData = { id: 1, name: 'widget', default_branch: 'main' };
      mockFetch(repoData);
      const result = await handlers['github_get_repo_info']!({ owner: 'acme', repo: 'widget' });
      const [url, init] = lastFetchCall();
      expect(url).toBe('https://api.github.com/repos/acme/widget');
      expect(init.method).toBe('GET');
      expect(result).toEqual(repoData);
    });
  });

  describe('github_get_default_branch', () => {
    it('calls GET /repos/{owner}/{repo} and returns default_branch', async () => {
      mockFetch({ default_branch: 'trunk', id: 42 });
      const result = await handlers['github_get_default_branch']!({
        owner: 'acme',
        repo: 'widget',
      });
      const [url] = lastFetchCall();
      expect(url).toBe('https://api.github.com/repos/acme/widget');
      expect(result).toEqual({ default_branch: 'trunk' });
    });
  });

  describe('github_create_pr', () => {
    it('calls POST /repos/{owner}/{repo}/pulls with correct body', async () => {
      const prData = { number: 7, html_url: 'https://github.com/acme/widget/pull/7' };
      mockFetch(prData);
      const result = await handlers['github_create_pr']!({
        owner: 'acme',
        repo: 'widget',
        title: 'Add feature',
        body: 'Description here',
        head: 'feature-branch',
        base: 'main',
      });
      const [url, init] = lastFetchCall();
      expect(url).toBe('https://api.github.com/repos/acme/widget/pulls');
      expect(init.method).toBe('POST');
      const sentBody = JSON.parse(init.body as string);
      expect(sentBody).toEqual({
        title: 'Add feature',
        body: 'Description here',
        head: 'feature-branch',
        base: 'main',
      });
      expect(result).toEqual(prData);
    });
  });

  describe('github_push_branch', () => {
    it('calls ref, tree, commit, and ref-create endpoints in order', async () => {
      const fetchMock = vi.fn()
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ object: { sha: 'base-sha-123' } }),
        })
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ sha: 'tree-sha-456' }),
        })
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ sha: 'commit-sha-789' }),
        })
        .mockResolvedValueOnce({
          ok: true,
          json: () => Promise.resolve({ ref: 'refs/heads/new-branch', object: { sha: 'commit-sha-789' } }),
        });
      vi.stubGlobal('fetch', fetchMock);

      await handlers['github_push_branch']!({
        owner: 'acme',
        repo: 'widget',
        branch: 'new-branch',
        base: 'main',
        files: [{ path: 'src/hello.ts', content: 'export const x = 1;' }],
      });

      const calls = fetchMock.mock.calls as [string, RequestInit][];
      expect(calls).toHaveLength(4);

      // Step 1: resolve base SHA
      expect(calls[0][0]).toBe('https://api.github.com/repos/acme/widget/git/refs/heads/main');
      expect(calls[0][1].method).toBe('GET');

      // Step 2: create tree
      expect(calls[1][0]).toBe('https://api.github.com/repos/acme/widget/git/trees');
      expect(calls[1][1].method).toBe('POST');
      const treeBody = JSON.parse(calls[1][1].body as string);
      expect(treeBody.base_tree).toBe('base-sha-123');
      expect(treeBody.tree[0].path).toBe('src/hello.ts');

      // Step 3: create commit
      expect(calls[2][0]).toBe('https://api.github.com/repos/acme/widget/git/commits');
      expect(calls[2][1].method).toBe('POST');
      const commitBody = JSON.parse(calls[2][1].body as string);
      expect(commitBody.tree).toBe('tree-sha-456');
      expect(commitBody.parents).toEqual(['base-sha-123']);

      // Step 4: create branch ref
      expect(calls[3][0]).toBe('https://api.github.com/repos/acme/widget/git/refs');
      expect(calls[3][1].method).toBe('POST');
      const refBody = JSON.parse(calls[3][1].body as string);
      expect(refBody.ref).toBe('refs/heads/new-branch');
      expect(refBody.sha).toBe('commit-sha-789');
    });
  });

  describe('error handling', () => {
    it('throws with status code on HTTP error', async () => {
      mockFetchError(404, 'Not Found');
      await expect(
        handlers['github_get_repo_info']!({ owner: 'acme', repo: 'nonexistent' }),
      ).rejects.toThrow(/GitHub HTTP 404/);
    });

    it('throws with status code on 401 unauthorized', async () => {
      mockFetchError(401, 'Bad credentials');
      await expect(
        handlers['github_get_default_branch']!({ owner: 'acme', repo: 'widget' }),
      ).rejects.toThrow(/GitHub HTTP 401/);
    });
  });
});
