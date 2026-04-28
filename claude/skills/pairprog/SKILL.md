---
name: pairprog
description: "Use this skill when the user wants to pair-program on a ticket — working through a Linear issue collaboratively with the AI driving and the human navigating. Trigger for prompts like 'let's work on this ticket', 'pair on LIN-123', 'pairprog', 'let's tackle this branch', 'work on the current branch', 'pick up LIN-123', 'start working on this', 'let's pair on this'. Also trigger when the user invokes `/pairprog`. The skill reads the current branch (or a specified ticket/branch), looks up the Linear ticket, explores the codebase for context, identifies unknowns and feasibility risks, asks the human navigator for direction on key decisions, then executes the work in atomic steps with frequent HITL checkpoints. It uses concurrent subagents to parallelize independent exploration and implementation work. It comments assessments, spike findings, and plans back to the Linear ticket so context survives across sessions. In default mode, the human commits after reviewing each atomic change. In yolo mode, the skill auto-commits atomically and gates at PR creation. This is NOT a walk-away skill. It is an interactive pairing session where the human stays engaged as navigator. Do NOT trigger for autonomous background work (use designnote or direct implementation), for ticket creation (use linearissue), for research without implementation (use qsearch), or for commit message drafting (use commitmsg)."
allowed-tools: Bash Glob Grep Read Write Edit Task AskUserQuestion WebFetch mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__list_issues mcp__claude_ai_Linear__list_comments mcp__claude_ai_Linear__save_comment mcp__claude_ai_Linear__list_teams mcp__claude_ai_Linear__get_team mcp__claude_ai_Linear__list_projects mcp__claude_ai_Linear__get_project mcp__claude_ai_Linear__list_issue_statuses mcp__claude_ai_Linear__list_issue_labels
---

# pairprog

Pair-program on a ticket. **You drive, the human navigates.**

This skill is the opposite of walk-away. The human is at the keyboard, engaged, steering. You write the code, explain your thinking, surface decisions, and check in frequently. The rhythm is: explore → plan → check in → implement a step → check in → repeat.

The defining design principles are:

> **Driver/navigator dynamic.** You drive (write code, run exploration, propose changes). The human navigates (steers direction, makes judgment calls, approves changes, commits). Neither works alone.
>
> **Derisk first.** Before writing production code, identify what's uncertain and spike on it. Feasibility unknowns resolved early prevent wasted work late.
>
> **Atomic steps with checkpoints.** Each change is small enough to review in one pass. The human sees the change, gives feedback, and commits when satisfied. No large PRs assembled silently.
>
> **Parallel where possible.** Use subagents for independent exploration and implementation tasks. Don't serialize work that can run concurrently. But always reconvene with the navigator before acting on findings.
>
> **Comment everything back to the ticket.** Assessments, spike findings, plans, and step completions are posted as Linear comments. This means a cancelled session can be picked up later — the ticket carries the full context.

## Commit modes

The skill supports two commit modes. Default is **checkpoint mode**. The navigator can switch to **yolo mode** at any time during the session.

- **Checkpoint mode (default):** The skill presents each atomic change, the navigator reviews it, and the navigator commits manually. The skill never runs git write commands.
- **Yolo mode:** The skill auto-commits each atomic step using the `commitmsg` skill for message generation, and gates at PR creation. The navigator can switch back to checkpoint mode at any time.

At the start of the session, if the skill detects this is a fresh ticket with no prior work, ask:

> "Checkpoint mode (you review and commit each step) or yolo mode (I auto-commit, you review at PR time)?"

If prior work exists on the branch, default to checkpoint without asking (the navigator is resuming deliberate work).

## Phase 0 — Pick up the ticket

### Identify the work

Determine what ticket to work on, in this order:

1. **Explicit argument** — if the user passed a ticket ID (`LIN-123`, `Z-123`) or branch name, use that.
2. **Current branch** — run `git branch --show-current`. If the branch name contains a ticket identifier (e.g., `vidbina/z-123-some-description`), extract it.
3. **Ask** — if neither yields a ticket, ask the user what to work on.

### Check out the branch

If the ticket has a git branch name (from the Linear issue's `branchName` field or inferred from the ticket ID), and that branch is not currently checked out:

- Run `git fetch` to ensure the branch is available locally.
- Run `git checkout {branch}` to switch to it.
- If the branch doesn't exist yet, ask the navigator: "No branch for this ticket yet. Create `{suggested-branch-name}` from `main`?" On approval, create and check out.

If the user passed a branch name directly rather than a ticket ID, check it out if not already on it.

### Load context (parallel)

Once the ticket ID is known and the branch is checked out, fetch all of these in parallel:

- **Linear ticket:** `get_issue` with the ticket ID. Capture title, description, state, priority, labels, project, parent issue.
- **Linear comments:** `list_comments` — read existing comments. If a previous pairprog session left assessment/plan/spike comments, note them. This is the resumption path.
- **Branch state:** `git log --oneline -10`, `git status`, `git diff main...HEAD` (or appropriate base branch) to understand what's already been done on this branch.
- **Codebase orientation:** Use `Task` with `subagent_type: Explore` to understand the areas of the codebase most likely relevant to the ticket (based on ticket title/description keywords).
- **Repo conventions:** Read `CLAUDE.md` / `AGENTS.md` / `CONTRIBUTING.md` at the repo root for development workflow, testing, and quality requirements.

### Present the ticket

Print a compact summary:

```
Ticket: Z-123 — Add OAuth2 support for Google login
Status: In Progress | Priority: High | Project: Auth
Branch: vidbina/z-123-add-oauth2-google-login (3 commits ahead of main)

Description:
> [Quoted ticket description, trimmed to essentials]

Prior session context (from comments):
> [If previous pairprog comments exist, summarize what was assessed/planned/completed]

Branch work so far:
> [Summary of existing commits on this branch, if any]
```

If prior pairprog comments indicate the session was interrupted, note what was completed and what remains.

## Phase 1 — Assess and derisk

### Identify unknowns

Read the ticket and existing branch work. Categorize what remains:

- **Clear and ready** — requirements are unambiguous, implementation path is known, can start immediately.
- **Unclear requirements** — the ticket is vague or contradictory on what exactly to build. Needs navigator input.
- **Feasibility unknowns** — "can we even do X?" questions. These need a spike before committing to an approach.
- **Dependency unknowns** — blocked by something external (API availability, another PR, a library feature). Needs investigation.

### Comment the assessment to Linear

Post a single comment to the ticket with the full assessment using `save_comment`:

```markdown
**Assessment** (pairprog)

**Clear and ready:**
- Add Google OAuth callback endpoint
- Store refresh tokens in existing session table

**Unclear — need navigator input:**
- Token expiry handling — ticket says "handle gracefully" but doesn't specify
- Scope of Google permissions — email only, or also profile?

**Feasibility unknowns — spiking:**
- Google OAuth library compatibility with our Node version
- Session table schema — can we store tokens without a migration?

**Blocked:**
- (none)
```

### Spike on feasibility (parallel)

For each feasibility unknown, spawn a `Task` subagent to investigate:

- Read relevant source code, docs, or external references
- Try small proof-of-concept explorations (read-only or in scratch space)
- Return a structured answer: feasible / not feasible / feasible with caveats

Subagents are read-only investigators. They don't write production code.

**Wait for all spikes to complete before commenting.** Don't post "starting spike" comments — post one consolidated findings comment per spike after the subagent returns:

```markdown
**Spike: Google OAuth library compatibility** (pairprog)

**Verdict:** Feasible ✓

`@googleapis/google-auth-library` v9.x supports Node 18+. Tested import resolution
against our `tsconfig.json` paths — no conflicts. The `OAuth2Client` class exposes
`getToken()` and `refreshToken()` which map directly to our needs.

Relevant code: `src/config/auth.ts` already has a provider pattern we can extend.
```

### Check in with the navigator

Present findings to the human:

```
Ready to implement:
 1. Add Google OAuth callback endpoint
 2. Store refresh tokens in existing session table

Need your input:
 3. Token expiry handling — ticket says "handle gracefully" but doesn't specify.
    Options: (a) silent refresh, (b) redirect to login, (c) both with fallback
 4. Scope of Google permissions — email only, or also profile?

Spiked and resolved:
 5. Google OAuth library — compatible ✓ (see ticket comment)
 6. Session table schema — can use metadata JSONB column ✓ (see ticket comment)

Blocked:
 (none)
```

Use `AskUserQuestion` for the items that need input. In pair programming, asking questions as they arise is expected. Batch questions that are ready at the same time rather than asking them one by one.

## Phase 2 — Plan the work

### Break into atomic steps

Each step should be:

- **Reviewable in isolation** — a human can read the diff and understand the intent
- **Testable** — either via existing tests, a new test, or manual verification
- **Committable** — it doesn't leave the codebase in a broken state

### Identify parallelism

Mark which steps are independent and can be worked on by concurrent subagents. Typical parallel opportunities:

- Tests for different modules
- Implementation of independent functions/components
- Documentation updates alongside code changes

### Present the plan

```
Plan for Z-123 — Add OAuth2 support for Google login

Step 1: Add Google OAuth callback route
  Files: src/routes/auth.ts, src/config/oauth.ts (new)
  Test: src/routes/__tests__/auth.test.ts

Step 2: Token storage in session metadata    ← parallel with Step 3
  Files: src/services/session.ts
  Test: src/services/__tests__/session.test.ts

Step 3: Silent token refresh middleware       ← parallel with Step 2
  Files: src/middleware/auth.ts
  Test: src/middleware/__tests__/auth.test.ts

Step 4: Update login UI with Google button   ← after Steps 1-3
  Files: src/components/LoginForm.tsx
  Test: manual verification

Ready to start?
```

Wait for the navigator's approval or adjustment before proceeding.

### Comment the plan to Linear

After the navigator approves (or adjusts) the plan, post it as a comment:

```markdown
**Plan** (pairprog)

- [ ] Step 1: Add Google OAuth callback route
- [ ] Step 2: Token storage in session metadata (parallel with 3)
- [ ] Step 3: Silent token refresh middleware (parallel with 2)
- [ ] Step 4: Update login UI with Google button (after 1-3)
```

This checklist serves as the resumption point if the session is interrupted.

## Phase 3 — Execute

### Work through the plan

For each step:

1. **Announce** what you're about to do. "Starting Step 1 — adding the OAuth callback route."
2. **Implement** the change. Use `Edit` / `Write` for code changes.
3. **Run quality checks** if the repo has them (lint, typecheck, test). Use `Bash` for this. If a check fails, fix it before presenting.
4. **Present the change.** Print a concise summary of what changed and why. Don't dump the full diff — describe the intent and highlight non-obvious decisions.
5. **Checkpoint.**
   - **Checkpoint mode:** Pause for the navigator's feedback. If the navigator says "looks good," they commit. If they want changes, iterate.
   - **Yolo mode:** Invoke the `commitmsg` skill's methodology (read repo convention, draft message) and auto-commit. Print the commit hash and message. Continue to the next step.

### Comment step completions to Linear

After each step is committed (by the navigator in checkpoint mode, or auto-committed in yolo mode), post a brief comment:

```markdown
**Step 1 complete** (pairprog)

Added Google OAuth callback route in `src/routes/auth.ts` and `src/config/oauth.ts`.
Test added in `src/routes/__tests__/auth.test.ts` — passing.

Commit: `a1b2c3d`
```

### Parallel execution

When the plan has independent steps, use `Task` subagents to work on them concurrently:

- Each subagent receives the full plan context and its assigned step
- Each subagent writes its changes and runs its tests
- When both complete, present the combined results to the navigator
- Each step is committed separately (atomic commits) — in checkpoint mode the navigator reviews each; in yolo mode both auto-commit

After parallel subagents complete, post one combined comment to keep the thread focused rather than interleaving.

### Adapting mid-session

Pair programming is fluid. The plan from Phase 2 is a starting point, not a contract. If the navigator wants to:

- Reprioritize steps → adjust the order
- Add a step → insert it
- Skip a step → skip it
- Change approach entirely → re-plan from Phase 2

Accommodate without friction. The navigator steers.

When mid-session adaptation happens, comment the change to Linear with a recognizable marker:

```markdown
🔀 **Plan adjusted** (pairprog)

Step 3 (Silent token refresh middleware) replaced with:
Step 3a: Explicit re-login flow on token expiry

Reason: Navigator decided silent refresh adds complexity without clear UX benefit.
```

Use these emojis for mid-session events:
- 🔀 — plan change (step reordered, replaced, or approach changed)
- ⏭️ — step skipped
- ➕ — step added
- 🚧 — blocker discovered mid-implementation
- ⚠️ — conflict between plan and reality (code doesn't behave as expected, API doesn't work as documented, etc.)

## Phase 4 — Wrap up

When all steps are implemented (or the session ends naturally):

### Summary

Print what was accomplished:

- What was implemented, what tests were added/updated
- What's left (if anything) and whether it needs a follow-up session or more design
- Any blockers or open questions that surfaced during implementation

### Commit threshold

If there are uncommitted changes (checkpoint mode only):

- Use the `commitmsg` skill to generate messages for any uncommitted work
- If multiple logical changes are uncommitted, suggest breaking them into separate commits and provide a message for each
- The navigator decides what to stage and commit

### PR creation

If the branch looks complete relative to the ticket scope:

1. **Draft the PR pitch in chat.** Show the proposed title, body (summary of changes, test plan), and target branch. Format it so the navigator can review before anything is created.
2. **Ask:** "Ready to create this PR via `gh`?" with options: **Yes, create it** / **Edit first** / **Not yet — more work needed**
3. On approval, create the PR using `gh pr create` via `Bash`.
4. On "Edit first," let the navigator adjust the pitch, then create.
5. On "Not yet," skip. The navigator will create it when ready.

### Comment wrap-up to Linear

Post a final session summary comment:

```markdown
**Session complete** (pairprog)

**Completed:**
- [x] Step 1: OAuth callback route
- [x] Step 2: Token storage
- [x] Step 3a: Explicit re-login flow
- [x] Step 4: Login UI with Google button

**Remaining:**
- (none — PR created: #47)

**Commits:** a1b2c3d, d4e5f6a, b7c8d9e, f0a1b2c
```

## Anti-patterns

- **Don't work silently for long stretches.** This is pairing, not autonomous mode. Check in after each atomic step. If exploration takes a while, print progress notes.
- **Don't commit in checkpoint mode.** All git write operations are HITL unless the navigator explicitly switched to yolo mode.
- **Don't skip the derisk phase.** Feasibility unknowns caught early save time. Don't jump to implementation because "it's probably fine."
- **Don't serialize parallelizable work.** If two steps are independent, use subagents. The navigator's time is valuable.
- **Don't make large changes without checkpoints.** If a step turns out bigger than expected, break it down further and checkpoint mid-step.
- **Don't assume the plan is fixed.** The navigator can redirect at any checkpoint. Accommodate without pushback.
- **Don't ask trivial questions.** The navigator is engaged but shouldn't be pestered. Ask about direction and judgment calls, not about syntax or obvious implementation details.
- **Don't ignore the repo's quality gates.** If the repo has lint/test/typecheck commands in CLAUDE.md or README, run them before presenting a change.
- **Don't touch unrelated code.** Stay within the scope of the ticket. If you notice unrelated issues, mention them verbally — don't fix them unless the navigator asks.
- **Don't post "starting spike" comments.** Wait for the spike to complete, then post findings. One comment per spike, not a running log.
- **Don't interleave parallel step comments.** When subagents work concurrently, post one combined comment after both complete.
- **Don't create PRs without pitching first.** Always show the proposed PR content in chat and get explicit approval before running `gh pr create`.
