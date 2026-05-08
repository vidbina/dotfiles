import { describe, it, expect, vi, beforeEach } from 'vitest';
import { createLinearToolHandlers } from '../../src/tools/linear.js';

const API_KEY = 'test-key';

function mockFetch(responseData: unknown) {
  vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
    ok: true,
    json: () => Promise.resolve({ data: responseData }),
  }));
}

function lastFetchBody(): unknown {
  const calls = vi.mocked(fetch).mock.calls;
  const body = calls[calls.length - 1][1]?.body as string;
  return JSON.parse(body);
}

beforeEach(() => vi.unstubAllGlobals());

describe('createLinearToolHandlers', () => {
  const handlers = createLinearToolHandlers(API_KEY);

  it('sends Authorization header with Bearer token', async () => {
    mockFetch({ issue: { id: '1' } });
    await handlers['mcp__claude_ai_Linear__get_issue']!({ id: 'VID-1' });
    const [, init] = vi.mocked(fetch).mock.calls[0];
    expect((init?.headers as Record<string, string>)['Authorization']).toBe(`Bearer ${API_KEY}`);
  });

  describe('save_comment', () => {
    it('creates comment when no id provided', async () => {
      mockFetch({ commentCreate: { success: true, comment: { id: 'c1' } } });
      await handlers['mcp__claude_ai_Linear__save_comment']!({ issueId: 'i1', body: 'hello' });
      const body = lastFetchBody() as { query: string };
      expect(body.query).toContain('commentCreate');
      expect(body.query).not.toContain('commentUpdate');
    });

    it('updates comment when id is provided', async () => {
      mockFetch({ commentUpdate: { success: true } });
      await handlers['mcp__claude_ai_Linear__save_comment']!({ id: 'c1', body: 'updated' });
      const body = lastFetchBody() as { query: string };
      expect(body.query).toContain('commentUpdate');
      expect(body.query).not.toContain('commentCreate');
    });
  });

  describe('save_issue', () => {
    it('creates issue when no id provided', async () => {
      mockFetch({ issueCreate: { success: true, issue: { id: 'i1', identifier: 'VID-1' } } });
      await handlers['mcp__claude_ai_Linear__save_issue']!({ title: 'New issue', teamId: 't1' });
      const body = lastFetchBody() as { query: string };
      expect(body.query).toContain('issueCreate');
    });

    it('updates issue when id is provided', async () => {
      mockFetch({ issueUpdate: { success: true } });
      await handlers['mcp__claude_ai_Linear__save_issue']!({ id: 'i1', title: 'Updated' });
      const body = lastFetchBody() as { query: string };
      expect(body.query).toContain('issueUpdate');
    });
  });

  it('throws on GraphQL errors', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve({ errors: [{ message: 'Not found' }] }),
    }));
    await expect(
      handlers['mcp__claude_ai_Linear__get_issue']!({ id: 'bad' }),
    ).rejects.toThrow(/GraphQL errors/);
  });

  it('throws on HTTP error', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: false,
      status: 401,
      text: () => Promise.resolve('Unauthorized'),
    }));
    await expect(
      handlers['mcp__claude_ai_Linear__get_issue']!({ id: 'x' }),
    ).rejects.toThrow(/401/);
  });
});
