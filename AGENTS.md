# Agent Usage Guidelines

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

## Commit message conventions

- **Format:** `<type>(<scope>): <subject> [ai:claude]` — the `[ai:claude]` tag always goes at the end of the subject line for AI-authored commits
- **No ticket ID in subject:** the branch name already encodes the ticket (e.g. `vidbina/vid-616-...`); don't repeat it in the commit subject
- **Always include:** `Co-Authored-By: Claude Code <noreply@anthropic.com>` trailer in the commit body
- **Commit via nix develop:** pre-commit hooks require tools only available in the dev shell — always use `nix develop --command git commit`