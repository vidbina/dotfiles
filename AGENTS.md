# Agent Usage Guidelines

Before making any contributions, read and follow [`CONTRIBUTING.md`](CONTRIBUTING.md).

## 🔥 CRITICAL RULE: ALWAYS WRITE COMMAND OUTPUT TO FILES

### ⚠️ MANDATORY for ALL commands with potentially long output:
- **NEVER** run commands directly that might produce more than a few lines
- **ALWAYS** redirect output to files: `command > output.txt 2>&1`
- **ALWAYS** use files for post-exec grepping and re-analysis

### ❌ DO NOT Use Agent Tool For:
- Long search operations that might produce extensive output
- Complex analysis that could generate large results
- Any operation where the output might be truncated or overwhelming

### ✅ Instead, Use Direct Tools:
- **Bash tool**: Write command output to files for later analysis
- **Grep tool**: For targeted searches
- **Read tool**: For examining specific files
- **Glob tool**: For finding files by pattern

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

## Best Practices

### For Long Command Output:
```bash
# Instead of running commands that produce long output directly
command_with_long_output > analysis_results.txt 2>&1

# Then read the file
# Use Read tool to examine analysis_results.txt
```

### For Comprehensive Searches:
```bash
# Write search results to files
grep -r "pattern" . > search_results.txt 2>&1
find . -name "*.nix" -exec grep -l "problematic_package" {} \; > problematic_files.txt
```

### For Build Traces:
```bash
# Capture build traces to files for analysis
nix build .#config --show-trace > build_trace.txt 2>&1
```

## Example Workflow:
1. Run command and redirect output to file
2. Use Read tool to examine the file
3. Extract specific information needed
4. Continue with targeted approach

This prevents:
- Truncated outputs
- Overwhelming the conversation
- Loss of important information
- Need to repeat expensive operations

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

## Commit message conventions

- **Format:** `<type>(<scope>): <subject> [ai:claude]` — the `[ai:claude]` tag always goes at the end of the subject line for AI-authored commits
- **No ticket ID in subject:** the branch name already encodes the ticket (e.g. `vidbina/vid-616-...`); don't repeat it in the commit subject
- **Always include:** `Co-Authored-By: Claude Code <noreply@anthropic.com>` trailer in the commit body
- **Commit via nix develop:** pre-commit hooks require tools only available in the dev shell — always use `nix develop --command git commit`