# claude/

Claude-related dotfiles. Everything here is config and definitions — no executable code.

```
claude/
  skills/        Skill definitions (SKILL.md format). Shared across all workspaces.
  agents/        Per-workspace deployment configs.
  settings.json  Claude Code settings (symlinked from ~/.claude/settings.json).
```

## Skills

Each subdirectory under `skills/` is a deployable skill. The directory name must match
the `name` field in `SKILL.md` — this is enforced by the Skills API.

Skills are workspace-agnostic. The same definitions deploy to any workspace; the
`agents/{workspace}/workspace.json` allowlist controls which subset goes where.

Skills are used in three ways:
- **Claude Code** — loaded from this directory automatically (filesystem-based, full network access)
- **Claude API** — uploaded via `/v1/skills` using the deploy script in `../agents/`
- **Claude.ai** — uploaded manually as a zip via Settings > Features

## Agents (workspace configs)

Each subdirectory under `agents/` is a workspace alias. The alias matches the `ant`
CLI profile name (`ant --profile <alias>`).

See `agents/README.md` for the workspace config format and deploy instructions.
