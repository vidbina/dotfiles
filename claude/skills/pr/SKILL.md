---
name: pr
description: "Use this skill when the user wants to create a pull request for the current branch. Trigger for prompts like 'create a PR', 'open a PR', 'make a pull request', 'pr this', 'ship it', or when the user invokes `/pr`. Also invoked by other skills (/pairprog, /troubleshoot) at wrap-up. The skill detects the correct base branch automatically — if the current branch is stacked on another feature branch rather than main, it uses that branch as the base. It reads the Linear ticket (if any) and recent commits to draft a title and body, pitches the draft in chat for the operator to review, and creates the PR via the GitHub MCP on approval. DX-first: fast, good defaults, minimal ceremony. Do NOT trigger for reviewing existing PRs, for merging, or for anything other than creating a new PR."
allowed-tools: Bash Glob Grep Read Agent AskUserQuestion mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__save_issue mcp__claude_ai_Linear__list_comments mcp__github__create_pull_request mcp__github__list_pull_requests mcp__github__pull_request_read mcp__github__merge_pull_request mcp__github__update_pull_request mcp__github__search_repositories mcp__github__get_file_contents mcp__github__list_branches
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
- **Remote default branch:** use `mcp__github__get_file_contents` or `mcp__github__list_branches` to determine the default branch — don't hardcode `main`

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
3. Run the title through the **title quality gate** below.

### Title quality gate

Evaluate the drafted title against these criteria. The gate is **advisory** — warn and suggest, never block.

**Principles:**
1. **Value over mechanism** — frame what the PR delivers, not how it's implemented. "Stable room link for live AV" not "Implement re-entrancy for Daily transport".
2. **Distinguishing phrase first** — the most identifiable words lead, so truncated branch names (`~25 chars` after the ticket prefix) stay meaningful.
3. **Brevity** — keep titles under 60 characters. Warn above 60.
4. **No metadata in the title** — priority, work type (spike, research), and category belong in labels, not bracket tags or prefixes.

**Anti-patterns to detect:**
- **Mechanism verbs leading** — "Wire up", "Implement", "Add", "Decouple", "Compose", "Inject", "Conduct", "Redesign" as the first word describe implementation, not value. Exception: action verbs that describe user-facing behavior are fine ("Allow bot to interrupt").
- **Bracket tags** — `[Research]`, `[Spike]`, `[WIP]` waste leading characters. Use labels instead.
- **"Explore/Research/Spike:" prefixes** — front-load metadata that belongs in labels.
- **"Draft design note:" prefixes** — the work type is obvious from context.
- **"Fix broken..." with mechanism** — "Fix broken X — replace Y with Z" buries the value. Prefer "X works again" or name the symptom.
- **Parenthetical lists** — "(Meet, Zoom, WhatsApp)" or "(Cartesia disconnect, room leave)" add precision but kill branch readability.
- **Question-format titles** — "Does X expose Y?" belongs in the description.
- **Kitchen-sink titles** — "Improve X — reduce Y and explore Z" tries to do too much in one title.

**When the gate fires:**

If any issues are detected, draft a suggested rewrite alongside the original. Present both in the Phase 4 pitch so the operator can choose:

```
Title:     <original title>
Suggested: <rewritten title>  ← value-oriented, distinguishing phrase first
Warning:   <what triggered the gate — e.g. "mechanism verb 'Implement' leading", "67 chars (>60)">
```

**Examples (before → after):**
- "Wire up Logfire for APM and distributed tracing" → "Logfire APM and tracing"
- "Implement e2e tests: HTTP contract and Daily room verification" → "E2e tests for /connect and Daily"
- "Explore service-level hooks for production frame-level observability" → "Production frame-level o11y hooks"
- "Decouple FilterSink frame-type routing from pipecat string class names" → "Decouple frame routing from pipecat"
- "Architectural spike: bot-as-participant in external video calls (Meet, Zoom, WhatsApp)" → "Bot joins Meet/Zoom/WhatsApp calls"
- "Graceful LLM provider fallback when primary flakes" → "LLM fallback on provider outage"

**Titles that pass (no rewrite needed):**
- "Select auth provider" — short, clear, distinguishing
- "Optimise for showtime" — punchy, value obvious
- "Allow bot to interrupt" — user-facing behavior, 4 words
- "Uncensor STT input" — brief, clear what it delivers

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
Suggested: <rewritten title>              ← only if quality gate fired
Warning:   <what triggered>               ← only if quality gate fired
Base:   <base-branch>  [stacked ⚠] or [default branch]
Branch: <current-branch>

Body:
---
<body>
---

Create this PR? yes / edit / cancel
```

If the title quality gate fired, the `Suggested:` and `Warning:` lines appear. The operator can accept the original, use the suggestion, or type their own via the `edit` flow. If no issues were detected, omit those lines.

Use `AskUserQuestion` to get the response. Options:
- **yes** → proceed to Phase 5
- **edit** → the operator pastes corrections; apply them and re-pitch
- **cancel** → stop, no PR created

## Phase 5 — Create

1. **Push if needed:** if the branch isn't on origin yet, run `git push -u origin <branch>`. Print the push result.
2. **Create:** use `mcp__github__create_pull_request` with the owner, repo, title, body, base, and head branch.
3. **Print the PR URL** from the response.
4. **Transition the ticket:** if a ticket ID was extracted from the branch name in Phase 1, call `save_issue` with `id: {ticket-id}` and `state: "In Review"`. This is unconditional — a PR being open means the work is ready for review.

## Phase 6 — Monitor CI

After the PR is created, launch a **background subagent** to monitor CI status. Do not suggest merging or offer to merge until CI results are in.

The subagent should:

1. **Poll CI checks** using `mcp__github__pull_request_read` periodically or `Bash` with `gh pr checks <pr-number> --watch` until all checks complete or a timeout (10 minutes) is reached.
2. **On all checks green:**
   - Report to the user: "CI passed on PR #N. Ready to merge."
   - Check the repo's merge strategy via `mcp__github__search_repositories` or `mcp__github__get_file_contents`.
   - Offer to merge using `mcp__github__merge_pull_request`. Wait for explicit approval.
   - **After merge:** run `git checkout main && git pull --ff-only` (i.e. `git ready`) to return to a clean, up-to-date main. This is the ready position for starting new work. Transition linked tickets to Done.
3. **On any check red:**
   - Report the failure: which check failed, link to the logs.
   - Fetch the failure details via `mcp__github__pull_request_read` and `Bash` with `gh run view <run-id> --log-failed` for actionable output.
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
