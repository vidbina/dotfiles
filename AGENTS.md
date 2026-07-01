# Agent Usage Guidelines

Before making any contributions, read and follow [`CONTRIBUTING.md`](CONTRIBUTING.md).

## 🔥 CRITICAL RULE: ALWAYS PREFER DECLARATIVE SYSTEM CHANGES

This repo uses literate config — `README.org` is the system config. Editing it IS the system change, expressed declaratively and version-controlled. The user runs `darwin-rebuild switch` (or `make switch`) to apply, as part of their normal workflow.

### Tangled files

Some files in this repo are **tangled** (auto-generated) from `README.org`. These files are identified by a `# Tangled from README.org` comment on line 1 — this is a repo convention (see `CONTRIBUTING.md`).

**Workflow for tangled files:**
- For simple changes, start directly in `README.org` — edit the source block, `make tangle`, verify. This is the recommended default.
- For complex work (new tools, multi-step wiring), build the thing first, then figure out where it goes in the nix config and `README.org`. See `CONTRIBUTING.md` for the full pattern.
- Editing tangled output files directly for rapid iteration is fine — just port the change back to `README.org` once it works.
- Run `make tangle` to regenerate the output files from `README.org`.
- **ALWAYS** run `make verify-parity` before committing — parity must hold at commit time.

### ⚠️ MANDATORY for ALL package/service/config changes:
- **NEVER** run or suggest imperative installs (`brew install`, `gh extension install`, `pip install`, `npm install -g`, `cargo install`, etc.) — these bypass version control and leave ghost state
- This rule applies **regardless of commit mode** — even in yolo mode, system-scope commands are always HITL. When a tool is missing, surface it to the user with context (e.g. "gitleaks is required by the pre-commit hook — install via `nix develop` or add to `README.org`?") rather than resolving it silently
- **ALWAYS** make the change in `README.org` (via the appropriate `noweb-ref` block) or the relevant nix module — that IS the install

### Package source preference (nix-first)

When adding a new package, prefer nixpkgs over Homebrew:

1. **Check nixpkgs first:** `nix-env -qaP <pkg>` — if it exists, use a `dev-packages` noweb-ref block in `README.org`.
2. **Check freshness:** compare the nixpkgs version against the upstream release. If nixpkgs is more than one major version behind, note it and consider Homebrew as a fallback.
3. **Fall back to Homebrew** only for macOS-only GUI apps (casks) or when the nixpkgs version is too stale. Use `homebrew-brews` or `homebrew-casks` noweb-ref blocks.

### Declarative paths in this repo:
- **Nix packages (preferred):** `#+begin_src nix :noweb-ref dev-packages` or system/home-manager modules
- **Homebrew brews:** `#+begin_src nix :noweb-ref homebrew-brews` in `README.org`
- **Homebrew casks:** `#+begin_src nix :noweb-ref homebrew-casks` in `README.org`
- **Services:** use nix-darwin launchd via object-form brew entry (`start_service = true`) or home-manager service modules

### ❌ Never do imperatively:
- `brew install <pkg>` → add to `homebrew-brews` noweb-ref instead
- `brew install --cask <pkg>` → add to `homebrew-casks` noweb-ref instead
- `gh extension install <ext>` → check if a standalone cask or nix package exists; prefer that
- `pip install`, `npm install -g`, `cargo install` → use nix packages or home-manager

## 📋 Long-running commands

For searches and file inspection, use the native tools (Grep, Glob, Read) — they handle pagination natively. Only redirect to files for genuinely expensive compute where you need to slice through large output later:

```bash
# Nix build traces
nix build .#config --show-trace > build_trace.txt 2>&1

# Then inspect with Read
```

## 🧪 Dev shell (`nix develop`)

This repo uses a nix dev shell to provide a **declarative, reproducible environment**. Pre-commit hooks (gitleaks), linters, and build tools are only available inside the dev shell — they are not installed globally.

**When to use:**
- **Committing:** `nix develop --command git commit` — required because pre-commit hooks need tools from the dev shell
- **Running quality checks:** any linting or validation that depends on tools not on your global PATH
- **Building:** `make nix-darwin-switch` handles this implicitly

**Why:** The dev shell ensures every contributor (human or AI) has the same toolset. Without it, pre-commit hooks fail with "command not found" errors (e.g. `gitleaks`).

**Note:** `nix develop --command git commit` requires Touch ID for 1Password commit signing (local sessions only). For remote/walk-away sessions where Touch ID is unavailable, see `claude/rules/remote-commits.md` for the `git mcommit`/`git mpush` alias pattern.

## 🔧 Make targets

Run `make` with no arguments to see all available targets with descriptions. Key targets referenced throughout:

- `make tangle` — regenerate tangled output files from `README.org`
- `make verify-parity` — check that tangled outputs match `README.org` (must pass before committing)
- `make nix-darwin-switch` — build and apply the nix-darwin configuration (the de facto validation step)

## ✅ Validation

`make nix-darwin-switch` is the primary validation path for nix changes — it builds the configuration and applies it in one step. A separate `make nix-darwin-build` exists but is redundant since switch includes the build.

For tangled file changes, always run `make verify-parity` before committing to ensure the tangled output matches `README.org`.

## 📖 README.org orientation

`README.org` is the canonical source for most nix configuration. It uses org-babel source blocks with `noweb-ref` headers that tangle into output files.

**Key noweb-ref blocks (where to add things):**

| To add... | Use noweb-ref | Example |
|-----------|---------------|---------|
| Nix package | `dev-packages` | `pkgs.ripgrep` |
| Homebrew formula | `homebrew-brews` | `"wget"` |
| Homebrew cask | `homebrew-casks` | `"figma"` |
| Home-manager file | `home-darwin-home-file` | symlink entries |
| macOS default | `darwin-defaults` | system preferences |

**Structure:** The file is organized by concern — shell, editors, terminals, design tools, browsers, services, etc. Each section contains prose documentation around the nix source blocks. Search for the `noweb-ref` name or the tool name to find where to add things.

## 🔑 Credentials and .envrc.local

**Project-scoped credentials** (API tokens, PATs) use a memory-based pattern: the skill checks project memory on first use, prompts the user if absent, and saves for future sessions. No secrets in committed files.

**Global skill config** (worklog calendar ID, dispatch agent IDs) uses layered JSON config files: `~/.claude/worklog.json` and `~/.claude/dispatch.json` as global defaults, with project-local `.worklog-config` / `.dispatch-config` overrides.

**Managed agent configs** use `op://` references in `claude/agents/asabina/workspace.json` — these are org-specific and personal.

**`.envrc.local`** is the gitignored local override for direnv. Use it for machine-specific env vars that shouldn't be committed.

## Worktree workflow

This repo uses a **slot-based worktree model** — fixed-path slots, not branch-named directories.

```
dotfiles/     ← main checkout, always on main
dotfiles--a/  ← slot A
dotfiles--b/  ← slot B (create as needed)
```

**Why slots, not branch-named paths:** Sessions are anchored to the CWD path. A branch-named worktree (`dotfiles-VID-NNN/`) loses session continuity when the branch advances (e.g. merge train A → B). A fixed slot path keeps the session bucket stable regardless of which branch is checked out.

### Worktree setup

```bash
# Add a slot (first time)
git worktree add ../dotfiles--a <branch>

# Switch a slot to a different branch
git -C ../dotfiles--a switch <branch>

# List all worktrees
git worktree list

# Prune stale registrations
git worktree prune
```

### Session discipline

- Start each slot's Claude session from **inside** the slot directory: `cd ../dotfiles--a && claude -n <name>`
- Resume with `--continue` from inside the slot — not from `dotfiles/`
- Each slot accumulates its own session history; nothing carries across slots automatically
- Slot names are opaque: use `git branch --show-current` or your prompt to know what's checked out

### Linear tickets as handover context

Linear ticket comments are the async handover protocol between Claude sessions across worktrees.

**Role split:**
- `dotfiles/` (main) — orchestrator/dispatcher: high-level view, spawns work, posts context
- `dotfiles--a/` (slot) — focused worker: picks up one ticket, implements, posts completion

**Handover flow:**
1. Orchestrator (or end of a slot session) posts a `**Handover**` comment to the Linear ticket with: current state, what's done, what's next, any relevant commands
2. Worker session opens in the slot: `cd ../dotfiles--a && claude --continue` (or `-n <name>`)
3. Worker reads the ticket comment to orient itself, then proceeds

**Why Linear comments, not session files:** Comments are durable, visible outside the terminal, accessible from mobile, and tied to the ticket — not to a specific machine path or session ID. A cancelled session can always be resumed by any Claude instance that can read the ticket.

**Naming convention for handover comments:** Start with `**Handover**` so they're scannable in the comment thread. End with a clear "Next step" section.

## Before filing a new Linear issue

Before creating a new ticket, check whether the work fits in an existing one. Run these checks in order:

1. **Current and recent branches.** Check the current branch and the last 2 branches worked on — is this fix in scope of work that's already in progress? Cross-reference with GitHub (`gh pr list`, `gh pr view`) to confirm merge state, since local state may be stale. If the branch hasn't merged, fold the fix into that scope.
2. **Related issues.** Check the current ticket's related issues in Linear. This surfaces nearby tickets that might already cover the scope or should be extended.
3. **Project backlog (when warranted).** For complex issues or when duplicates seem likely, scan the project backlog. Look for conflicts, duplicates, or tickets that should be merged. Propose resolution rather than silently creating a new ticket.

If an existing ticket covers the scope, surface it: "This looks like it fits in VID-XYZ which is still on branch X — want me to fold it in?" Don't file a new ticket unless the work is genuinely isolated.

## GitHub MCP

This repo uses the GitHub MCP (Insiders) for GitHub operations instead of shelling out to `gh` CLI. This keeps skill permissions precise — skills declare exactly which GitHub operations they need rather than getting broad `Bash` access.

### Setup

```bash
TOKEN=$(gh auth token) && claude mcp add github https://api.githubcopilot.com/mcp/insiders \
  --transport http --scope user --header "Authorization: Bearer $TOKEN"
```

Then run `/mcp` inside Claude Code to verify the server connects.

### Available tools

GitHub MCP tools follow the naming pattern `mcp__github__<tool_name>`. Key tools used by skills:

- **PRs:** `create_pull_request`, `list_pull_requests`, `pull_request_read`, `merge_pull_request`, `update_pull_request`
- **Issues:** `issue_read`, `issue_write`, `list_issues`, `search_issues`
- **Repos:** `get_file_contents`, `list_branches`, `list_commits`, `search_repositories`
- **CI:** `pull_request_read` (includes check status)

### When to still use Bash

Some operations have no MCP equivalent and still require `Bash`:
- `git` commands (commit, branch, push, log, diff, etc.)
- `gh run view --log-failed` (workflow run log inspection)
- `make` targets (tangle, verify-parity, switch)

## CI workflows and branch protection

The repo uses **two workflow files** with native GitHub Actions path filtering:

- `.github/workflows/literate-config.yml` — runs on `*.org`, `flake.*`, `Makefile`, `.github/workflows/**` changes
- `.github/workflows/agent-scripts.yml` — runs on `agents/**`, `.github/workflows/**` changes

### ⚠️ Branch protection caveat

Native `on.paths` filtering means a workflow **does not run at all** when no matching paths change. GitHub shows no check status on those PRs. If branch protection requires these checks to pass, PRs that don't touch the relevant paths will be **blocked from merging** — not because checks failed, but because they never appeared.

**Solutions (pick one):**
- **A. Don't mark these jobs as required status checks** (current stance — personal repo, no enforcement needed)
- **B. Switch to `dorny/paths-filter`** so jobs are *skipped* (not absent) — skipped checks satisfy required-check rules

Relevant GitHub docs:
- [Required status checks](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-protected-branches/about-protected-branches#require-status-checks-before-merging)
- [Workflow trigger path filters](https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/events-that-trigger-workflows#running-your-workflow-only-when-a-push-affects-specific-files)

## Session restart for config changes

This repo contains Claude Code's own configuration (`claude/settings.json`, `claude/skills/**`). Some config — particularly hooks and permissions — is loaded at session start and **not hot-reloaded** during a session. MCP servers can be refreshed via `/mcp` and hooks can be listed via `/hooks`, but neither re-evaluates the hooks at runtime.

**When you discover a change requires a session restart:**

1. **Don't say "we're blocked."** Instead, prepare a clean handoff.
2. **Commit the config change** (or present it for commit in checkpoint mode) so the next session loads the updated config.
3. **Post a handover comment to the Linear ticket** with:
   - What was changed and why
   - Exact command to resume: e.g. `claude` or `claude --continue` from the repo directory
   - What to test on restart (e.g. "edit `flake.nix` and check that CHECKPOINT appears in chat")
   - What remains after verification (e.g. "remove debug log line, commit fix")
4. **Tell the navigator** what to type after starting the new session so they can resume without remembering context. Be specific — e.g. "Start a new session and type: `/pair VID-661` — the ticket comment has the full context."

**Why this matters:** A cancelled session with no handover comment means lost context. The ticket comment is durable — it survives across sessions, machines, and people. Always write it before suggesting a restart.

## Issue labeling conventions

This is a system configuration repo (dotfiles), not an application. The meaning of labels shifts accordingly:

- **Feature** — a new capability enabled on the system. Installing an app, enabling a service, adding a new workflow, wiring a new MCP. In an app repo this might be "Tooling" but here every new capability IS the product.
- **Improvement** — an existing capability works better. Faster devenv, colored ls, shorter shell startup. The thing already existed; now it's nicer.
- **Tooling** — meta: changes to *how we configure*, not *what we configure*. Flake refactors, switching literate config frameworks, devenv vs nix-develop, CI pipeline structure, skill infrastructure. If it's about the machinery of managing config rather than the config itself, it's Tooling.
- **Research** — investigation with a decision deliverable. Often paired with Feature (deciding whether to enable something) or Tooling (deciding how to restructure).
- **Security** — commit signing, permission models, secret management, hook guardrails.
- **Debt** — dead code removal, stale config cleanup, migration leftovers.
- **Docs** — AGENTS.md, README.org prose, CONTRIBUTING.md updates that aren't tied to a code change.
- **Engineering** — CI, testing infrastructure, validation jobs. The plumbing that keeps the repo healthy.

When in doubt between Feature and Tooling: if a user of this machine would notice the change (new app available, new command works), it's Feature. If only the maintainer of the config notices (cleaner flake, better tangle workflow), it's Tooling.

## KB skill symlink maintenance

Skills shared across repos live in the knowledge base (`./kb` symlink → the KB repo). This repo consumes them via symlinks in `claude/skills/`. Some skills are local-only (real directories, not symlinks) — those are personal or experimental and not managed by this process.

### When to run

When the user asks to "sync skills", "update KB links", or when you notice a skill reference that doesn't resolve.

### Procedure

1. **Discover what the KB offers:** list `kb/skills/*/` (directories only, skip files like README.md).
2. **Discover what's already linked:** list symlinks in `claude/skills/` whose target path contains the KB path (from `readlink kb/`). Use `[ -L "$f" ]` to detect symlinks (not `[ -d ]`, which follows through to the target). Ignore symlinks pointing elsewhere (other repos) and real directories (local skills).
3. **Check symlink health:** for each KB symlink found in step 2, test whether the target still exists using `[ -e "$f" ]` on the symlink path. A symlink where `-L` is true but `-e` is false is **broken** — the target was renamed or removed.
4. **Diff:**
   - **New in KB** — skill dir exists in KB but has no symlink here. Offer to create: `ln -s $(readlink kb)/skills/<name> claude/skills/<name>`.
   - **Broken** — symlink exists but target is gone (renamed or removed in KB). Report the broken link, show the old target path, and look for a likely rename by matching the old skill's basename against current KB skill names. Offer to remove the broken link and create a new one if a rename candidate is found.
   - **Stale** — symlink target exists but points to an outdated path (e.g. KB restructured its directory layout). Offer to re-link.
   - **Current** — symlink exists and target is valid. No action needed.
5. **Report** the diff as a markdown table and wait for confirmation before making changes. Table format:

   | Skill | Status | Old target | Rename candidate | Proposed action |
   |-------|--------|-----------|-----------------|----------------|
   | `name` | Current / Broken / New / Stale | old path or — | new KB name or — | — / Remove + relink / Create link |

   Only show rows that need action first (Broken, New, Stale), then Current rows below a separator. This keeps the actionable items scannable.
6. **Apply** confirmed changes. Broken removals, re-links, and new links are separate operations — don't batch them if the user only approves some.

### What this does NOT cover

- **Local skills** (real directories in `claude/skills/`): managed manually, not part of sync.
- **Non-KB symlinks** (e.g. `techddreport` → a different repo): left untouched. These are one-off arrangements.
- **Creating the `kb` symlink itself:** that's a human setup step (`ln -s /path/to/kb-repo kb`).

## Merge strategy

Merge via `/pr` — it reads the repo's merge convention from CONTRIBUTING.md and `.github-settings.yaml`, handles stack-aware retargeting, and checks for child PRs before deleting branches. Do not merge from the GitHub web UI for stacked PRs.

## Commit message conventions

> **Read `CONTRIBUTING.md` before drafting or making any commit.** It is the canonical source of truth for the commit format, AI co-authorship trailers, and PR title convention.

Summary:

- **Format:** `<type>(<scope>): <subject> [ai:<agent>]`
- **No ticket ID in subject:** the branch name already encodes the ticket (e.g. `vidbina/vid-616-...`); don't repeat it in the commit subject
- **AI co-authorship:** always include both trailers (see CONTRIBUTING.md for details):
  ```
  Co-authored-by: Claude Code <noreply@anthropic.com>
  Assisted-by: Claude:claude-opus-4-6
  ```
- **Commit via nix develop:** pre-commit hooks require tools only available in the dev shell — always use `nix develop --command git commit`