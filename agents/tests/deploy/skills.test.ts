import { describe, it, expect } from 'vitest';
import matter from 'gray-matter';

// prepareSkillMd is not exported — re-implement inline for testing,
// or we export it. For now test via the gray-matter contract it relies on.
// We'll refactor deploy/skills.ts to export it in the next step.
import { prepareSkillMd } from '../../src/deploy/skills.js';

function makeMd(fields: Record<string, string>, body = '\n# skill\n'): string {
  return matter.stringify(body, fields);
}

describe('prepareSkillMd', () => {
  it('swaps api_description into description and removes the field', () => {
    const input = makeMd({
      name: 'test',
      description: 'x'.repeat(1025),
      api_description: 'short api description',
    });
    const result = prepareSkillMd(input, 'test');
    const { data } = matter(result);
    expect(data['description']).toBe('short api description');
    expect(data['api_description']).toBeUndefined();
  });

  it('passes through when description is within limit and no api_description', () => {
    const desc = 'short description';
    const input = makeMd({ name: 'test', description: desc });
    const result = prepareSkillMd(input, 'test');
    const { data } = matter(result);
    expect(data['description']).toBe(desc);
  });

  it('throws when description exceeds 1024 chars and no api_description', () => {
    const input = makeMd({ name: 'test', description: 'x'.repeat(1025) });
    expect(() => prepareSkillMd(input, 'test')).toThrow(/add an api_description/);
  });

  it('throws when api_description itself exceeds 1024 chars', () => {
    const input = makeMd({
      name: 'test',
      description: 'x'.repeat(1025),
      api_description: 'x'.repeat(1025),
    });
    expect(() => prepareSkillMd(input, 'test')).toThrow(/exceeds 1024 chars/);
  });

  it('throws when no description field at all', () => {
    const input = makeMd({ name: 'test' });
    expect(() => prepareSkillMd(input, 'test')).toThrow(/no description/);
  });
});
