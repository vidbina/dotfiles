---
name: pr
description: "Use this skill when the user wants to create a pull request for the current branch. Trigger for prompts like 'create a PR', 'open a PR', 'make a pull request', 'pr this', 'ship it', or when the user invokes `/pr`. Also invoked by other skills (/pairprog, /troubleshoot) at wrap-up. The skill detects the correct base branch automatically — if the current branch is stacked on another feature branch rather than main, it uses that branch as the base. It reads the Linear ticket (if any) and recent commits to draft a title and body, pitches the draft in chat for the operator to review, and creates the PR via `gh pr create` on approval. DX-first: fast, good defaults, minimal ceremony. Do NOT trigger for reviewing existing PRs, for merging, or for anything other than creating a new PR."
allowed-tools: Bash Glob Grep Read Agent AskUserQuestion mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__save_issue mcp__claude_ai_Linear__list_comments
---

# pr

Create a pull request. **Fast, good defaults, operator confirms before anything is created.**

The defining design principles:

> **DX-first.** Derive as much as possible automatically — base branch, title, body. The operator should need to type as little as possible.
>
> **Pitch before creating.** Always show the full draft in chat and get explicit approval. Never run `gh pr create` without confirmation.
>
> **Smart base for stacked PRs.** If the branch was cut from another feature branch (not main), use that branch as the base. Merging a stacked PR into main loses the stack structure.

## Phase 1 — Gather context

Run in parallel:

- **Branch:** `git branch --show-current`
- **Commits on branch:** `git log --oneline main..HEAD` (or `git log --oneline HEAD ^origin/main` if on remote)
- **Push state:** `git status -sb` — is the branch already pushed to origin?
- **Remote default branch:** `gh repo view --json defaultBranchRef -q .defaultBranchRef.name` — don't hardcode `main`

Extract a ticket ID from the branch name if present (e.g. `vidbina/vid-123-some-slug` → `VID-123`). If found, fetch the Linear issue (`get_issue`) for title and description.

## Phase 2 — Detect base branch

Find the correct base using this algorithm:

```bash
# All local branches that are ancestors of HEAD, ranked by closeness
git for-each-ref --format='%(refname:short)' refs/heads/ \
  | grep -v "$(git branch --show-current)" \
  | while read b; do
      if git merge-base --is-ancestor "$b" HEAD 2>/dev/null; then
        count=$(git log --oneline "$b"..HEAD | wc -l | tr -d ' ')
        echo "$count $b"
      fi
    done \
  | sort -n \
  | head -5
```

The branch with the **fewest commits between its tip and HEAD** is the most likely parent.

**Decision rules:**
- If the closest ancestor is the default branch (e.g. `main`) → normal PR, base = default branch.
- If the closest ancestor is another feature branch → stacked PR, base = that feature branch. Flag this clearly in the pitch: "This looks like a stacked PR — base set to `<parent-branch>`."
- If ambiguous (tie or no clear parent), default to the repo's default branch and note the ambiguity.

## Phase 3 — Draft

**Title:**
1. Use the Linear ticket title if available.
2. Otherwise, derive from the branch name slug (strip the owner prefix and ticket ID, humanize the remainder).
3. Keep it under 72 characters.

**Body:**
```markdown
## Summary
<2-4 bullet points — what changed and why, derived from commits and ticket description>

## Test plan
<bullet checklist of how to verify the change>

<Linear ticket link if available>

🤖 Generated with [Claude Code](https://claude.com/claude-code)
```

Derive the summary from commit messages and the Linear ticket description. Derive the test plan from the nature of the changes (e.g. "run tests", "manually verify X flow", "check CI passes").

## Phase 4 — Pitch in chat

Print the full draft before doing anything:

```
PR draft

Title:  <title>
Base:   <base-branch>  [stacked ⚠] or [default branch]
Branch: <current-branch>

Body:
---
<body>
---

Create this PR? yes / edit / cancel
```

Use `AskUserQuestion` to get the response. Options:
- **yes** → proceed to Phase 5
- **edit** → the operator pastes corrections; apply them and re-pitch
- **cancel** → stop, no PR created

## Phase 5 — Create

1. **Push if needed:** if the branch isn't on origin yet, run `git push -u origin <branch>`. Print the push result.
2. **Create:** run `gh pr create --base <base> --title "<title>" --body "<body>"` via `Bash`.
3. **Print the PR URL** returned by `gh`.
4. **Transition the ticket:** if a ticket ID was extracted from the branch name in Phase 1, call `save_issue` with `id: {ticket-id}` and `state: "In Review"`. This is unconditional — a PR being open means the work is ready for review.

## Phase 6 — Monitor CI

After the PR is created, launch a **background subagent** to monitor CI status. Do not suggest merging or offer to merge until CI results are in.

The subagent should:

1. **Poll CI checks** using `gh pr checks <pr-number> --watch` or periodic `gh pr checks <pr-number>` until all checks complete or a timeout (10 minutes) is reached.
2. **On all checks green:**
   - Report to the user: "CI passed on PR #N. Ready to merge."
   - Check the repo's merge strategy: `gh api repos/{owner}/{repo} --jq '.allow_merge_commit, .allow_squash_merge, .allow_rebase_merge'`
   - Offer to merge using the repo's preferred strategy, or let the user choose if multiple are enabled. Wait for explicit approval before running `gh pr merge`.
3. **On any check red:**
   - Report the failure: which check failed, link to the logs.
   - Fetch the failure details: `gh pr checks <pr-number>` and `gh run view <run-id> --log-failed` for actionable output.
   - Summarize what went wrong and suggest next steps (e.g. "lint failure in X — fixable here" or "test timeout — needs investigation").
   - Ask the user how to proceed: fix it now (resume pairprog), investigate further, or leave it for later.
4. **On timeout (no checks appear within 2 minutes):**
   - Report: "No CI checks have started on PR #N. This repo may not have CI configured for this path, or checks are queued."
   - Do not suggest merging.

**Key rules:**
- **Never suggest merging before CI results are known.** A freshly created PR has no check data — don't offer to merge it.
- The subagent runs in the background so the user can continue other work.
- If the user explicitly asks to merge before CI completes, warn them that checks haven't finished and confirm they want to proceed.

## Anti-patterns

- **Don't hardcode `main` as base.** Always detect the default branch and check for stacking.
- **Don't create the PR without pitching.** The operator must see and approve the draft.
- **Don't fabricate the test plan.** Derive it from the diff or say "verify manually" — don't invent test steps that don't exist.
- **Don't push without noting it.** If a push is needed, say so before running it.
- **Don't ask more than one round of questions.** All clarification happens in the pitch → edit loop, not via separate `AskUserQuestion` calls before Phase 4.
- **Don't suggest merging before CI is green.** A freshly created PR has no check data. Wait for the CI monitor subagent to report results before offering merge options.
