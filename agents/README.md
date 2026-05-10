# agents/

TypeScript orchestrator for Claude Managed Agents. Handles skill deployment, session
lifecycle, and custom tool dispatch (Linear, GitHub).

```
agents/
  src/
    deploy/        Skill deployment scripts (upload claude/skills/ to workspaces)
    tools/         Custom tool handlers (Linear API, GitHub, secret resolution)
    sessions/      Session runner — starts agents, streams events, dispatches tools
    agents/        Agent definitions (research, coding)
    index.ts       Library entry point
    cli.ts         CLI entry point
  tests/           Vitest test suite
  devenv.nix       Development shell (Node 22 + pnpm)
  package.json
  tsconfig.json
```

## Dev shell

```sh
direnv allow        # or: devenv shell
pnpm install        # auto-runs on shell entry via devenv
```

## Scripts

```sh
pnpm build                              # tsc → dist/
pnpm dev                                # tsx src/cli.ts (no build step)
pnpm typecheck                          # tsc --noEmit
pnpm test                               # vitest watch
pnpm test:run                           # vitest run (CI)
pnpm deploy:skills --workspace asabina  # upload/update skills in a workspace
```

## Custom tools

The orchestrator exposes Linear and GitHub operations as custom tools that agent
sessions call via `agent.custom_tool_use`. Secrets are resolved from 1Password at
runtime — they never enter the agent container.

Supported custom tools (Linear):
`linear_get_issue`, `linear_list_issues`, `linear_save_issue`,
`linear_list_comments`, `linear_save_comment`, `linear_list_teams`, `linear_get_team`,
`linear_list_projects`, `linear_get_project`, `linear_list_issue_statuses`,
`linear_list_issue_labels`, `linear_list_documents`, `linear_get_document`,
`linear_create_document`

GitHub auth is handled via `GITHUB_TOKEN` injected into the session environment —
the agent uses the built-in `gh` CLI directly.

## Workspace configs

Workspace configs live in `../claude/agents/{alias}/workspace.json`. The deploy script
and session runner read from there. See `../claude/agents/README.md` for the format.
