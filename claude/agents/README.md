# claude/agents/

Per-workspace deployment configs. One subdirectory per workspace alias.

```
agents/
  asabina/          Personal workspace (Asabina GmbH)
    workspace.json
  acme-corp/        Example client workspace
    workspace.json
```

## workspace.json format

```jsonc
{
  // Workspace ID — used to verify the correct workspace is targeted before any write.
  // Find it in ant auth status or the Anthropic console.
  "workspaceId": "wrkspc_01...",

  // ant CLI profile name (from ~/.config/anthropic/configs/).
  // Used to select credentials when running the deploy script.
  "antProfile": "default",

  // Allowlist of skill names to deploy to this workspace.
  // Only skills listed here are uploaded; everything else in claude/skills/ is ignored.
  // Omit skills that are not compliant or not relevant for this workspace.
  "skills": ["pairprog", "qsearch", "linearissue", "pr", "commitmsg"],

  // Tool secrets — resolved from 1Password at deploy/run time via `op read`.
  // Values with an "op" key are secret references; plain strings are literals.
  "tools": {
    "linear": {
      "apiKey": { "op": "op://Employee/linear.app/api-key" }
    },
    "github": {
      "token": { "op": "op://Employee/github.com/fine-grained-pat" }
    },
    "anthropic": {
      "apiKey": { "op": "op://Employee/console.anthropic.com/agents-on-mbp-m5pro" }
    }
  }
}
```

## Discovery-based deploy (no manifest)

The deploy script matches local skills to remote ones by `display_title`, which equals
the skill `name` from `SKILL.md`. The Skills API enforces uniqueness of `display_title`
per workspace, so discovery is reliable:

1. `GET /v1/skills?source=custom` → build `{display_title → id}` map
2. For each skill in the allowlist: found → new version, not found → create

No stored ID mapping is needed. Run the deploy script:

```sh
pnpm --prefix <repo-root>/agents run deploy:skills --workspace asabina
```
