# Contributing

This document is the canonical source of truth for commit conventions, scope discipline, and contribution workflow. All contributors — human and AI — must follow these rules.

> **For AI agents:** Read this file before drafting or making any commit. `AGENTS.md` defers to this file for commit format, the scope list, and prohibited patterns.

## Literate config and tangled files

This repo uses a literate config approach. `README.org` is the canonical source for most nix configuration files. Org-babel tangles source blocks from `README.org` into their target files using noweb references.

### Identifying tangled files

Tangled files carry a comment on **line 1** indicating their source:

```
# Tangled from README.org
```

This is a repo convention. If a file has this comment, its canonical source lives in `README.org`.

### Making changes

For straightforward changes — adding a package, tweaking a setting — start directly in `README.org`. Edit the source block, `make tangle`, verify, done. This is the recommended default.

For more complex work, it often makes sense to work outside-in: build the thing first, wire it in later. For example, when adding a new Rust tool like `gref-dedup`, the natural flow was:

1. Write the Rust crate in its own directory, get it compiling and working.
2. Only then figure out where in the nix config to reference it (which `noweb-ref`, which package list).
3. Only then add the source block to `README.org` with documentation explaining what it does.

Editing tangled output files directly is also fine for rapid iteration — test a change, see if it builds, fix it, repeat. This avoids the overhead of round-tripping through `README.org` on every attempt when you're still figuring out what works.

However you get there, the finishing steps are the same:

1. Ensure the change is in the corresponding source block in `README.org` (search for the `noweb-ref` name or the target filename), with documentation around the code block.
2. Run `make tangle` to regenerate all tangled output files.
3. Run `make verify-parity` to confirm that the tangled outputs match `README.org`.
4. Commit both `README.org` and the regenerated output files together.

The key rule: **parity must hold at commit time.** How you get there is up to you.

### Parity checks

Always run `make verify-parity` before committing changes that touch `README.org` or any tangled file. This target diffs the tangled output against the working tree — any mismatch means either:

- You edited a tangled file but haven't ported the change back to `README.org` yet, or
- You edited `README.org` but forgot to re-tangle (fix: run `make tangle`).

CI runs the same check, so commits with parity drift will fail.

## Commit Convention

### Format

```
<type>(<scope>): <subject> [ai:<agent>]

[optional body]

[optional footer(s)]
```

- **type** — what kind of change (see table below)
- **scope** — what area of the codebase is affected (see table below)
- **subject** — imperative, lowercase, no period, ≤80 characters
- **`[ai:<agent>]`** — required trailer when an AI agent authored or co-authored the commit (e.g. `[ai:claude]`, `[ai:gemini]`)
- **body** — explain *why*, not *what* (the diff shows what)
- **footer** — `Co-Authored-By:`, `BREAKING CHANGE:`, references

### AI Co-authorship

Use both trailers on every AI-assisted commit. `Co-authored-by` works with GitHub today; `Assisted-by` is the emerging standard but forges don't render it yet. Using both ensures nothing is lost as tooling catches up:

```
Co-authored-by: Claude Code <noreply@anthropic.com>
Assisted-by: Claude:claude-opus-4-6
```

**`Co-authored-by:`** is the established Git/GitHub convention. GitHub renders it as a secondary author avatar on the commit. The email is a provenance marker, not a contact address — it follows GitHub's `noreply@` convention. Use `noreply@{provider-domain}` (e.g. `noreply@anthropic.com`, `noreply@openai.com`, `noreply@google.com`).

**`Assisted-by:`** is the emerging standard from the [Linux kernel guidelines](https://github.com/torvalds/linux/blob/master/Documentation/process/coding-assistants.rst#attribution). Format: `AGENT_NAME:MODEL_VERSION`. No email field — sidesteps the fake-email problem. Not yet widely supported by forges, but forward-compatible.

Once forges render `Assisted-by` natively, `Co-authored-by` can be dropped.

Both trailers survive squash-merge in the commit body, so provenance is preserved even when individual commits are collapsed.

### Types

| Type | Use when… |
|---|---|
| `feat` | Adding a wholly new feature or capability |
| `fix` | Fixing a bug |
| `refactor` | Restructuring code without changing behavior |
| `doc` | Documentation only |
| `test` | Adding or updating tests only |
| `chore` | Build, CI, tooling, dependencies |
| `style` | Formatting, whitespace, lint fixes (no logic change) |
| `perf` | Performance improvement |

### Scopes

| Scope | Covers |
|---|---|
| `nix` | Flake, nix-darwin modules, home-manager config, `README.org` source blocks |
| `skills` | Claude Code skill definitions in `claude/skills/` |
| `ci` | GitHub Actions workflows |

> **Tip:** When no scope fits, omit it: `fix: handle null response from upstream`.
> When a change genuinely spans multiple scopes, pick the primary one — do not concatenate (`nix+skills` is never valid).

### Prohibited Scope Patterns

These patterns are **never valid** as scopes. AI agents in particular tend to invent these — reject them in review.

| Anti-pattern | Why it's wrong | Use instead |
|---|---|---|
| Ticket IDs (`VID-123`) | Scopes describe *what*, not *which task* | The correct canonical scope |
| Pluralized duplicates (`tests`, `docs`) | Creates ambiguity with singular forms | `test`, `doc` |
| Synonym drift (`flake` vs `nix`) | Fragments the log — pick one canonical name | The canonical scope from the table above |
| Concatenated scopes (`nix+skills`) | One commit, one scope — split the commit or pick the primary | The primary scope |
| Tool names (`gitleaks`, `emacs`) | Too granular — group by concern | `git`, `chore`, or `ci` |
| File names (`flake.nix`) | Too specific, doesn't generalize | The area the file belongs to |

### Subject Line Rules

1. **Imperative mood:** "add feature" not "added feature" or "adding feature"
2. **Lowercase:** no capital first letter
3. **No period** at the end
4. **≤80 characters** (enforced — see self-check below)
5. **No ticket IDs** in the subject line — the branch name already encodes the ticket
6. **Describe the change**, not the task: "add OAuth callback route" not "work on VID-123"

#### Subject Line Self-Check Protocol

This check is **not optional**. Run it before every commit:

```bash
echo -n "<type>(<scope>): <your subject> [ai:<agent>]" | wc -c
```

- If ≤80 → proceed
- If >80 → shorten the subject, re-run, repeat until ≤80

> **Why enforce this?** Long subjects wrap in `git log --oneline`, break notification previews, and make scanning history harder. The 80-char limit is a hard rule, not a suggestion.

### BAD vs GOOD Examples

```
# BAD
feat(shell): VID-123 add zsh completion for nix commands [ai:claude]   ← ticket ID in subject
Fix: Added new package support.                                         ← past tense, capital, period
feat(nix+skills): add cross-module config [ai:claude]                   ← concatenated scope
refactor(flake): rename nix modules                                     ← synonym drift (use nix)

# GOOD
feat(shell): add zsh completion for nix commands [ai:claude]
fix: handle missing flake input gracefully [ai:claude]
refactor(nix): consolidate darwin module imports [ai:claude]
doc: add literate config workflow to contributing guide [ai:claude]
```

## Pre-Commit Workflow

1. **Complete your changes**
2. **Run all quality checks** (formatting, linting, type checking, tests, build)
3. **Fix any issues** that arise from quality checks
4. **Stage specific files**: `git add <file1> <file2>` — never `git add .` or `git add -A` blindly, as this risks staging secrets, build artifacts, or unrelated changes
5. **Run the subject line self-check** (see above)
6. **Commit with proper message format**
7. **Verify commit was signed**: `git log --show-signature -1`

## Scope Discipline

When working on a branch tied to a ticket, every change should fit the branch's stated purpose. Changes that don't fit fall into two categories:

### Category A — Opportunistic Feature/Code Work

Detected when a change touches files or functionality unrelated to the current branch's purpose (e.g. fixing an unrelated bug, adding a feature that belongs on a different ticket).

**What to do:**

- **Stop and surface it** before making the change
- Options: (a) continue on the current branch if the team agrees, (b) cut a new branch, (c) file a ticket and defer
- Never silently mix unrelated work into a branch — it makes review harder and reverts dangerous

### Category B — Agent/Process Meta-Corrections

Detected when a correction needs to be made to `AGENTS.md`, `CONTRIBUTING.md`, or other process documentation based on something learned during the current work.

**What to do:**

- Do it in place on the current branch — the correction must take effect immediately
- Isolate in its own commit with a `doc:` subject so the PR diff shows the mixing explicitly

**Why the distinction matters:** Category B corrections must take effect immediately — deferring them means working with broken instructions for the rest of the session. Category A has no such urgency and should always be routed properly.

## PR Title Convention

PR titles follow the same conventional-commit format as individual commits, with a ticket ID suffix for traceability:

```
<type>(<scope>): <subject> [TICKET-ID]
```

- **type** and **scope** — same rules as commits (see tables above)
- **subject** — imperative mood, value-oriented, distinguishing phrase first
- **`[TICKET-ID]`** — the Linear ticket ID in square brackets, e.g. `[VID-31]`. Auto-injected from the branch name by the `/pr` skill. Omit only for ad-hoc branches without a ticket.

### Examples

```
feat(auth): enable Google login [VID-31]
fix: handle null response from upstream [VID-45]
doc: update contributing guide with PR title convention [VID-687]
chore(ci): enforce semantic PR titles [VID-687]
```

### Why this format?

Individual commits and merged PRs serve different audiences in `git log`:

| Layer | Format | Ticket ID? | When visible |
|---|---|---|---|
| Individual commit | `type(scope): subject [ai:agent]` | No — branch encodes it | Always |
| Merged PR (squash) | `type(scope): subject [TICKET-ID] (#N)` | Yes — for traceability | After squash-merge |
| Merged PR (merge commit) | `type(scope): subject [TICKET-ID] (#N)` | Yes — on merge commit | After merge-commit |

The ticket ID suffix is the visual signal that distinguishes a squash-merged PR from an individual commit when scanning `git log`. It also makes every merged PR traceable back to its Linear ticket without opening GitHub.

### CI enforcement

The `amannn/action-semantic-pull-request` GitHub Action validates PR titles on open/edit. See `.github/workflows/lint-pr.yaml`.
