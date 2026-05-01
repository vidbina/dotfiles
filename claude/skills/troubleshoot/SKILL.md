---
name: troubleshoot
description: "Use this skill when the user wants to diagnose an error, investigate a failure, or root-cause a bug — without necessarily fixing it right away. Trigger for prompts like 'troubleshoot X', 'why is Y failing', 'debug this error', 'investigate LIN-123', 'figure out what's wrong with Z', 'look into this CI failure', 'root-cause this crash', or when the user invokes `/troubleshoot`. The skill is a walk-away investigator: it runs read-only commands, tees output to files, investigates in parallel via subagents, formulates a root-cause hypothesis, and confirms it with a minimal reproducible test — all with minimal interrupts so the operator can step away and return to a finished report. Findings are posted as Linear comments or design note updates depending on context. If a fix is needed after the diagnosis, the skill hands off to /pairprog. Do NOT trigger when the user wants to immediately implement a fix (use /pairprog), when pure web research suffices (use /qsearch), or when the issue is a design decision rather than a failure (use /designnote)."
allowed-tools: Bash Glob Grep Read Task AskUserQuestion WebSearch WebFetch mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__list_issues mcp__claude_ai_Linear__list_comments mcp__claude_ai_Linear__save_comment mcp__claude_ai_Linear__get_document mcp__claude_ai_Linear__list_documents mcp__claude_ai_Linear__list_teams mcp__claude_ai_Linear__get_team mcp__claude_ai_Linear__list_projects mcp__claude_ai_Linear__get_project
---

# troubleshoot

Diagnose a failure. **You investigate autonomously; the human reviews findings.**

This is the opposite of /pairprog. The operator may be away. You cover as much ground as possible uninterrupted, tee every command's output to a file, and return a clear root-cause report with a reproducible test. The human returns to findings, not questions.

The defining design principles are:

> **Walk-away investigation.** Minimize interrupts. Ask at most one up-front clarification batch, then run to completion. The operator should be able to kick this off, answer a handful of questions if needed, and come back to a finished diagnosis.
>
> **Tee everything to files.** Every command that produces non-trivial output is redirected to a file (`command > output/cmd-name.txt 2>&1`). This prevents context-window flooding, makes re-inspection cheap, and means the investigation survives a restart without re-running expensive commands.
>
> **Root cause, not symptoms.** A good diagnosis names the specific line of code, configuration value, dependency version, or environmental condition that caused the failure — not just "the test failed." Support the hypothesis with evidence.
>
> **Reproduce to confirm.** A hypothesis without a reproduction is a guess. Before reporting, write a minimal repro (a command, script, or test invocation) that demonstrates the failure. If you cannot reproduce it, say so explicitly and explain why.
>
> **Parallel investigation.** Use `Task` subagents for independent investigation threads. Don't serialize work that can run concurrently. Reconvene with consolidated findings before writing the report.
>
> **Comment findings back to the ticket.** Every investigation — even a partial one — gets commented to the associated Linear ticket or design note. A cancelled session carries forward.

## Phase 0 — Identify the problem

Determine what to investigate, in this order:

1. **Explicit argument** — if the user passed a ticket ID, error message, file path, or description, use that as the starting point.
2. **Current branch** — extract a ticket identifier from `git branch --show-current` (e.g. `vidbina/lin-123-...` → `LIN-123`).
3. **Ask** — if neither yields a clear target, ask the user what to investigate.

### Load context (parallel)

Once the target is known, load in parallel:

- **Linear ticket** (if applicable): `get_issue` — title, description, comments, labels, project.
- **Recent comments**: `list_comments` — check for prior investigation notes or known constraints.
- **Branch state**: `git log --oneline -10`, `git status`, `git diff main...HEAD --stat`.
- **Error artifacts**: any log files, CI output files, or stack traces already present in the repo — glob for `*.log`, `*.txt` in common output directories, or paths the user mentioned.
- **Repo orientation**: `Task` subagent to identify the areas of the codebase most relevant to the failure (based on the error message or ticket keywords).

### Clarify once, up front

Identify any inputs that would materially change the investigation path (environment, reproduction steps, constraints). Batch them into a **single** `AskUserQuestion` call. Keep it short. If the ticket or error is clear enough, skip this entirely.

After answering (or if no questions are needed), the user can walk away.

## Phase 1 — Investigate

### Create an output directory

```bash
mkdir -p troubleshoot-output
```

All command output goes here. Never run a long command and discard its output — always tee:

```bash
some-command > troubleshoot-output/some-command.txt 2>&1
```

### Identify investigation threads

Read the ticket / error and list the independent investigation threads. For example:
- Thread A: Inspect the failing code path
- Thread B: Check dependency versions and compatibility
- Thread C: Review CI logs for environment differences
- Thread D: Search for related issues or prior occurrences

### Run threads in parallel

Spawn one `Task` subagent per independent thread. Each subagent:
- Runs read-only commands and tees output to `troubleshoot-output/`
- Reads source code, config files, logs relevant to its thread
- Returns a structured mini-report: **what was found**, **what was ruled out**, **remaining unknowns**

Subagents are read-only investigators. They do not edit production code.

### Permitted read-only operations

- **Filesystem**: `ls`, `find`, `cat`, `head`, `tail`, `stat` — across project tree and system paths (for nix/system dep investigation)
- **Git**: all read operations — `git log`, `git diff`, `git show`, `git blame`, `git stash list`, etc.
- **Nix**: `nix eval`, `nix path-info`, `nix store ls`, `nix why-depends`, `nix flake show`, `nix flake metadata` — introspect the store and derivations
- **Search**: `grep`, `ripgrep` — across project tree and system paths
- **Web**: `WebSearch`, `WebFetch` — for error messages, library issues, known bugs, release notes
- **GitHub / CI**: `gh run list`, `gh run view`, `gh run download` — read CI logs and artifact output

## Phase 2 — Formulate the hypothesis

Synthesize the subagent findings into a single root-cause hypothesis:

```
Hypothesis: <one sentence naming the specific cause>

Evidence:
- <finding 1 from Thread A>
- <finding 2 from Thread B>
- ...

Ruled out:
- <alternative explanation X> — because <evidence>
- <alternative explanation Y> — because <evidence>

Confidence: high / medium / low
Reason for confidence level: <explanation>
```

If multiple hypotheses remain viable, rank them by likelihood and note what evidence would distinguish between them.

## Phase 3 — Reproduce

Write a minimal reproduction that confirms the hypothesis. Aim for the smallest command, script, or test invocation that demonstrates the failure.

```bash
# Minimal repro — tee the output
<repro-command> > troubleshoot-output/repro.txt 2>&1
```

Read the output and verify it shows the expected failure.

**If reproduction succeeds:** note the exact repro command and its output in the report.

**If reproduction fails (can't reproduce):** state this explicitly. Explain what you tried and why you think the failure may be environment-specific, timing-dependent, or already fixed. Do not guess at a root cause you cannot confirm.

**If reproduction is destructive or would cause side effects:** describe the repro steps without running them. Flag clearly that human verification is needed.

## Phase 4 — Report

### TL;DR in chat first

Always print the report in chat before touching any persistence target. The operator may be reading on a phone, in a hurry, or just not want a ticket comment for a quick investigation.

```
TL;DR

Root cause: <one sentence>
Confidence: high / medium / low — <reason>

Repro: <minimal repro command>  (output → troubleshoot-output/repro.txt)

Next step: <fix suggestion, or what input is needed>

Ruled out: <alternative X> (reason), <alternative Y> (reason)

Artifacts: troubleshoot-output/
```

### Ask about persistence

After printing the TL;DR, ask explicitly — do not assume:

> "Save these findings somewhere?
> - **a)** Linear comment on [TICKET-ID]
> - **b)** Design note update
> - **c)** Local markdown file (`troubleshoot-output/report.md`)
> - **d)** None — chat only is fine"

Wait for the operator's answer before writing anything to disk, posting any comment, or updating any document. Multiple options can be selected (e.g. "a and c").

### Persist on confirmation

If the operator selects Linear comment, post:

```markdown
**Troubleshoot findings** (troubleshoot)

**Root cause:**
<one-sentence hypothesis>

**Evidence:**
- <finding 1>
- <finding 2>

**Reproduction:**
```bash
<minimal repro command>
```
Output: `troubleshoot-output/repro.txt`

**Ruled out:**
- <alternative X> — <reason>

**Confidence:** high / medium / low — <reason>

**Recommended next step:**
<fix suggestion, or "needs human input on X before proceeding">

**Investigation artifacts:** `troubleshoot-output/` (in working directory)
```

If the operator selects design note update or local file, write the same content to the appropriate target.

## Phase 5 — Optional: hand off to /pairprog

If the root cause is confirmed and a fix is clearly scoped, offer to hand off:

> "Root cause confirmed. Ready to fix? I can invoke `/pairprog` to implement the fix atomically. The fix scope would be: [brief description]. Shall I proceed?"

On approval, invoke the `pairprog` skill. Pass the diagnosis as context so pairprog can skip its own Phase 1 spike on this issue.

On "not yet" or "no," leave the findings in the ticket comment. The operator decides when and how to fix.

## Anti-patterns

- **Don't fix code during investigation.** Diagnosis and repair are separate phases. Troubleshoot diagnoses; pairprog repairs. Mixing them produces unreviewed changes.
- **Don't discard command output.** Every non-trivial command goes to `troubleshoot-output/`. Never run and discard.
- **Don't report a hypothesis without a repro.** If you can't reproduce, say so — don't present an unconfirmed hypothesis as fact.
- **Don't ask multiple rounds of questions.** One up-front batch at most. The operator walks away after that.
- **Don't summarize investigations serially when they're independent.** Use parallel subagents. Time is the operator's scarcest resource.
- **Don't ignore prior comments on the ticket.** Someone may have already investigated this. Read all comments before spawning subagents.
- **Don't auto-persist.** Always ask before posting a Linear comment, writing a file, or updating a doc. Not every investigation warrants a paper trail.
- **Don't post "starting investigation" comments.** Post findings, not progress pings. One consolidated comment per investigation session — and only after the operator says to.
- **Don't leave troubleshoot-output/ unmentioned.** Always tell the operator where the artifacts are so they can inspect raw output if needed.
- **Don't make destructive repro attempts.** If reproducing the failure would cause side effects (drop a DB, send emails, bill a customer), describe the steps and flag for human verification instead of running them.
