import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    pool: 'forks',
    include: ['tests/**/*.test.ts'],
    server: {
      deps: {
        // Don't transform node_modules — only transform our own source
        external: [/node_modules/],
      },
    },
  },
});
