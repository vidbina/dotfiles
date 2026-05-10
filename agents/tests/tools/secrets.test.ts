import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';

// Mock child_process before importing the module under test
vi.mock('child_process', () => ({
  execFile: vi.fn(),
}));

import { execFile } from 'child_process';
import { resolveSecret } from '../../src/tools/secrets.js';

const mockExecFile = vi.mocked(execFile);

function fakeExecFile(stdout: string) {
  mockExecFile.mockImplementation((_cmd, _args, callback: any) => {
    callback(null, stdout, '');
    return {} as any;
  });
}

beforeEach(() => {
  vi.resetAllMocks();
  delete process.env['MY_SECRET'];
  delete process.env['OP_ACCOUNT'];
});

afterEach(() => {
  delete process.env['MY_SECRET'];
  delete process.env['OP_ACCOUNT'];
});

describe('resolveSecret', () => {
  it('returns env var when set, never calls op', async () => {
    process.env['MY_SECRET'] = 'from-env';
    const result = await resolveSecret({ op: 'op://vault/item/field' }, 'MY_SECRET');
    expect(result).toBe('from-env');
    expect(mockExecFile).not.toHaveBeenCalled();
  });

  it('returns plain string as-is without calling op', async () => {
    const result = await resolveSecret('literal-value');
    expect(result).toBe('literal-value');
    expect(mockExecFile).not.toHaveBeenCalled();
  });

  it('calls op read with correct ref when env var not set', async () => {
    fakeExecFile('secret-from-op\n');
    const result = await resolveSecret({ op: 'op://Employee/item/field' });
    expect(result).toBe('secret-from-op');
    expect(mockExecFile).toHaveBeenCalledWith(
      'op',
      ['read', 'op://Employee/item/field'],
      expect.any(Function),
    );
  });

  it('passes --account when OP_ACCOUNT is set', async () => {
    process.env['OP_ACCOUNT'] = 'asabina.1password.eu';
    fakeExecFile('secret\n');
    await resolveSecret({ op: 'op://Employee/item/field' });
    expect(mockExecFile).toHaveBeenCalledWith(
      'op',
      ['read', 'op://Employee/item/field', '--account', 'asabina.1password.eu'],
      expect.any(Function),
    );
  });

  it('does not pass --account when OP_ACCOUNT is not set', async () => {
    fakeExecFile('secret\n');
    await resolveSecret({ op: 'op://Employee/item/field' });
    const [, args] = mockExecFile.mock.calls[0];
    expect(args).not.toContain('--account');
  });
});
