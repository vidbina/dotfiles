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
- **ALWAYS** make the change in `README.org` (via the appropriate `noweb-ref` block) or the relevant nix module — that IS the install

### Declarative paths in this repo:
- **Homebrew brews:** `#+begin_src nix :noweb-ref homebrew-brews` in `README.org`
- **Homebrew casks:** `#+begin_src nix :noweb-ref homebrew-casks` in `README.org`
- **Nix packages:** `#+begin_src nix :noweb-ref dev-packages` or system/home-manager modules
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