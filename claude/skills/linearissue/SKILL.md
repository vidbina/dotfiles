---
name: linearissue
description: "Use this skill when the user asks you to turn a design note's action items into Linear issues, OR when the user wants to iterate on an existing Linear issue, OR when the user wants to create a new issue freeform (without a design note). Filing mode triggers: 'file tickets from this note', 'linearize X', 'create Linear issues for the action items in X', 'turn this note into tickets', 'ship this to Linear', 'file these TODOs from the design note', 'make tickets from X', 'linearissue this note', 'create issues from X.md'. Iteration mode triggers: user passes a Linear issue URL (e.g. https://linear.app/...) or issue ID (e.g. VID-123) with a follow-on prompt like 'help me refine this', 'roast this scope', 'add context about X', 'what questions should we answer first', 'sharpen the description', or any request to think about, improve, or comment on an existing issue. Freeform mode triggers: 'create an issue for X', 'file a ticket about Y', 'new issue: Z', 'ticket this', or any request to create a Linear issue without referencing a design note or existing issue. Also trigger when the user invokes `/linearissue`. Do NOT trigger for creating Linear documents (that's the designnote skill's strategy-routing path), for filing tickets from `docs/decisions/` ADRs (decisions are made; they don't spawn tickets), or for any operation that would also commit to git."
api_description: "File action items from a design note as Linear issues with bidirectional cross-references and idempotent re-runs, iterate on an existing Linear issue by posting analysis, context, or refinements as comments, or create new issues freeform with title quality enforcement. Three modes: filing (from a note), iteration (from a ticket ID or URL), and freeform (from a prompt)."
allowed-tools: Bash Glob Grep Read Edit WebFetch AskUserQuestion Skill mcp__claude_ai_Linear__list_issues mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__save_issue mcp__claude_ai_Linear__list_comments mcp__claude_ai_Linear__list_teams mcp__claude_ai_Linear__get_team mcp__claude_ai_Linear__list_projects mcp__claude_ai_Linear__get_project mcp__claude_ai_Linear__list_issue_statuses mcp__claude_ai_Linear__list_issue_labels mcp__github__list_pull_requests mcp__github__pull_request_read mcp__github__search_pull_requests
---

# linearissue

Create Linear issues — from a design note's action items, from an existing ticket (iteration), or freeform from a prompt. All modes enforce value-oriented titles via a quality gate, use an in-chat confirmation gate, and support idempotent re-runs.

The defining design principles of this skill are:

> **The design note is the single source of truth. The skill reads it, derives tickets, and writes Linear ticket IDs back into it. No parallel work-plan file. No re-modeling.**
>
> **Confirm in chat, not via files.** The skill prints proposed tickets in the conversation, confirms with one `AskUserQuestion`, and creates on approval. One invocation, one session.
>
> **Idempotent re-runs.** A note that's already been linearized has inline `Linear: LIN-NNN` annotations. The skill detects them and updates instead of duplicating.
>
> **Issues are minimal anchors; comments carry the thinking.** A ticket's title and description are a stable, concise anchor — what the work is, nothing more. Scope crystallization (additions, reductions, pushback, context, reasoning) happens through comments, which are immutable and preserve the history of thinking. Keep descriptions short and pragmatic. Never use them as a context dump from the design note — the backlink to the note is enough. This principle applies in both filing mode and iteration mode.

## How this fits the broader workflow

```
prompt
  │
  ▼
designnote skill  →  design note (with action items in canonical sections)
  │
  ▼
[HITL gate: human reviews, edits, greenlights the note]
  │
  ▼
linearissue       →  prints proposed tickets in chat
  │
  ▼
[HITL gate: AskUserQuestion confirmation]
  │
  ▼
creates tickets + writes Linear: LIN-NNN annotations back into the note
```

The human gate on the design note itself is where the real review happens. By the time `linearissue` runs, the note is greenlit — ticket derivation is mechanical. The in-chat confirmation is a final sanity check, not a deep review.

## Tools declared in allowed-tools

- `Glob`, `Grep`, `Read` — discover and parse the design note
- `Edit` — append cross-references and Status Log rows back to the note
- `WebFetch` — follow URLs referenced in the note when enriching ticket descriptions
- `AskUserQuestion` — the confirmation gate (and batched clarification if needed)
- Linear MCP read tools — search for existing tickets, resolve teams/projects, look up statuses and labels
- `Skill` — invoke the `commenting` skill for posting comments to Linear issues
- Linear MCP write tools — `save_issue`

The skill does not declare `Task` — there is no parallel research phase. Ticket derivation is deterministic and serial.

The skill does not declare `Write` — it never creates new files. It only edits the existing design note (appending cross-references and a Status Log row).

## Phase 0 — Preflight

### Parse invocation and detect mode

Examine the first argument (or the full prompt if no explicit argument):

- **Iteration mode** — if the input contains a Linear issue URL (`https://linear.app/...`) or a bare issue identifier matching the pattern `[A-Z]+-\d+` (e.g. `VID-123`, `Z-419`), enter **iteration mode**. Capture the issue ID and the rest of the prompt as the user's intent. Skip to the [Iteration mode](#iteration-mode) section below; do not run the filing-mode phases.
- **Filing mode** — if the input references a design note path (explicitly or inferable from `docs/design-notes/`), enter **filing mode**. Continue with the filing-mode phases below.
- **Freeform mode** — if the input is neither a Linear URL/ID nor a design note path — e.g. "create an issue for X", "file a ticket about Y", "new issue: Z", a bare description of work to be ticketed, or a batch of items to file — enter **freeform mode**. Skip to the [Freeform mode](#freeform-mode) section below.

### Rename the session

After detecting the mode, rename the session to reflect the scope:

Rename the session so the user can identify it at a glance (e.g. in a terminal tab). A good name reflects the mode and scope — e.g. "card layout abstraction (linearissue)" for filing, "VID-123 OAuth2 Google login (linearissue:iterate)" for iteration, or "auth token storage (linearissue:freeform)" for freeform.

If a tool or command is available to rename the session programmatically, use it. Otherwise, suggest the user rename it — but don't block on this. Move on to the next phase either way.

### Triage: check for duplicates and scope overlap

Before creating any new issue — whether from a design note or freeform — run a tiered check to avoid duplicates, scope overlap, and contradictions in the backlog. This applies in both filing mode and when other skills (pairprog, freeform conversation) route through `save_issue` to create a ticket.

**Tier 1 — Current work (always run, lightweight):**
- Check the current branch and the last 2 branches worked on (`git branch --sort=-committerdate | head -3`).
- Cross-reference with GitHub (`mcp__github__list_pull_requests`, `mcp__github__pull_request_read`) to confirm merge state — local state may be stale.
- If an in-progress branch covers this scope, surface it: "This looks like it fits in VID-XYZ which is still on branch X — want me to fold it in?"

**Tier 2 — Related issues (always run):**
- If a ticket ID is known (from the branch name, the design note, or the user's prompt), fetch it and check its related issues (`get_issue` with `includeRelations: true`).
- Scan the related issues for scope overlap or contradictions with the proposed new ticket.

**Tier 3 — Backlog scan (run when the issue is complex or duplicates seem likely):**
- Search the project backlog (`list_issues` filtered by project and team) for tickets with similar titles or keywords.
- If matches are found, present them: "These existing tickets look related — should we extend one of them, or is this genuinely new?"
- Propose resolution: merge scopes, re-prioritize an existing ticket, mark as duplicate, or confirm isolation.

If the triage surfaces a match, present options via `AskUserQuestion`:
- **Fold into VID-XYZ** — add scope to the existing ticket (comment the new context)
- **Stack on VID-XYZ** — create as a sub-issue or related issue
- **Create new** — confirmed isolated, proceed with filing
- **Cancel** — the user decides to handle it differently

Only proceed to filing after the user confirms the triage result.

### Filing mode: parse invocation

- **Note path** (positional or inferred). If not specified, default to *the most recently modified* file in `docs/design-notes/` whose filename state token is `TODO`, `WIP`, or `REVIEW`. If multiple match, ask in Phase 2.
- Optional flags: `--team`, `--project` (override note-derived routing).

### Permission preflight

Read the Claude Code settings files that may exist (in override order):

1. `~/.claude/settings.json`
2. `.claude/settings.json` (workspace)
3. `.claude/settings.local.json` (local override)

Check `permissions.allow` for entries matching `Edit(docs/design-notes/**)`. If the grant is missing, warn the user:

> The `Edit(docs/design-notes/**)` permission is not pre-approved in your Claude Code settings. Each write-back to the note will trigger a permission prompt.
>
> To fix, add to your settings:
> ```jsonc
> { "permissions": { "allow": ["Edit(docs/design-notes/**)"] } }
> ```
>
> Proceed anyway, or bail and configure first?

Offer two options via `AskUserQuestion`: **Proceed with prompts** / **Bail — I'll configure first**. If the grant is present, skip this entirely.

### Verify the note

- The path exists and is readable.
- The filename state token is one of `TODO`, `WIP`, `REVIEW`. If it's `DONE`, `SUPERSEDED`, `CANCELED`, or `DEPRECATED`, **refuse** with: "This note is in state `{STATE}`. Historical notes are read-only. Did you mean a different one?"
- The path does not contain `ARCHIVE/`. If it does, refuse for the same reason.
- The path is *not* in `docs/decisions/`. If it is, refuse: "Decisions are records of choices already made; they don't spawn tickets. Use a design note to track follow-up work."

Read the note in full. Read the repo's `CLAUDE.md` / `AGENTS.md` for any project-specific HITL or commit policies.

## Phase 1 — Extract candidate tickets

Parse the note for action items. Canonical sources, in priority order:

1. **`## Action items`** / **`## Action Items`** / **`## Action items when revisiting`** — every checklist item is a candidate
2. **`## Next Action`** / **`## Next Actions`** — every checklist item or bullet is a candidate
3. **`## Trigger Points`** / **`## Implementation Timing`** — only items the note explicitly marks as ready to execute (look for "ready", "now", or a checked precondition)
4. **`## Open Questions`** — only items explicitly tagged for ticketing (e.g., `→ ticket`, `[ticket]`, `[file]`)
5. **Top-level checklist items `- [ ] ...`** anywhere in the note that are NOT inside a "rejected", "considered", or "Never" section, AND that don't already carry an inline `Linear: LIN-NNN` annotation

For each candidate, capture:

- **Title:** the first line of the item, cleaned up (strip checklist markers, leading "TODO:", trailing punctuation). Run through the **title quality gate** — if the gate fires, store both the original and suggested rewrite for presentation in Phase 3.
- **Description:** the item's body content + the parent section heading + up to ~10 lines of preceding contextual prose from the section. Quote rather than paraphrase.
- **Source anchor:** `{relative-note-path}#{slugified-section-heading}` so the Linear ticket can deep-link back.
- **Tags from the note's front matter** — these may inform Linear labels.
- **Existing Linear cross-ref:** if the item already has `Linear: LIN-NNN` inline, mark it as "existing — will update, not create".

Skip items that:

- Are already marked complete (`- [x]`)
- Live inside a `Never`, `Rejected`, `Discarded`, `Out of scope`, or `Superseded` subsection
- Are inside a fenced code block, table cell, or HTML comment
- Are in a `<details>` block summarizing rejected approaches

## Phase 2 — Classify, sequence, route

For each candidate:

- **Dependency hints.** Detect "blocked by", "after", "depends on", "requires", "once X is done", or explicit cross-links to other action items. Build a dependency graph. Flag cycles.
- **Parallel-safety hints.** Only set if the note explicitly mentions file/module scope or parallelism. Otherwise leave unset — don't predict merge-conflict risk you can't actually predict.
- **Priority hints.** Look for `URGENT`, `P0`/`P1`, `Now/Next/Later/Never` framing, "blocker", "asap". Translate to Linear priority (1=Urgent, 2=High, 3=Normal, 4=Low). Default Normal.
- **Estimate hints.** Look for `(Effort: S/M/L)` markers from the kb's TODO.md convention. Translate to a numeric estimate if your team uses one. Otherwise leave unset.

**Routing — team and project:**

- If the note's Status Log "Related Tickets" column already references Linear tickets, use the same team/project as those (most recent wins).
- If the note's front matter or body contains a Linear project link or `team: X` / `project: Y` marker, use that.
- If `--team` or `--project` flags were passed, use those.
- Otherwise: include team/project as a question in the confirmation gate (Phase 3).

## Phase 3 — Confirm and create

### Print the proposal

Print a structured summary in chat. This is the reasoning trace — not a file, just chat output:

```
Proposed tickets from: docs/design-notes/{filename}
Target: {team-key} / {project-name}

 1. [NEW]    {Title}                          Priority: Normal
 2. [NEW]    {Title}                          Priority: High
             ↳ depends on #1
 3. [NEW]    {Title}                          Priority: Normal
             Suggested: {Rewritten title}  ← title gate fired
             Warning: mechanism verb 'Implement' leading
 4. [UPDATE] {Title} (LIN-123)               Priority: Normal
             ↳ description changed

Skipped:
 - "already done thing" — marked complete
 - "explored option C" — inside Discarded options section

{N} new · {K} updates · {S} skipped
```

### Confirm

Issue one `AskUserQuestion` call. Combine the confirmation with any unresolved routing questions from Phase 2 into a single batch:

- **Always ask:** "Create these {N} tickets?" with options: **Yes, create all** / **Exclude some — let me pick** / **Bail**
- **Only if team/project is unresolved:** include a question for team and/or project selection
- **Only if candidate count is large (>10):** include a question about narrowing scope

If the user chooses "Exclude some", issue one follow-up `AskUserQuestion` with multiSelect listing the candidates so the user can deselect. This is the only case where a second question is permitted.

If the user bails, stop. Print nothing further.

### Create tickets

For each confirmed ticket:

1. **Existing detection.** If the note's source-anchor area already has an inline `Linear: LIN-NNN` annotation, or if a Linear search by source anchor URL in the description footer finds an existing ticket, treat as **update**. Otherwise **create**.
2. **Create:** call `save_issue` with team, project, title, description (including the source-anchor footer), priority, parent (if dependency-hinted as a sub-task — but default to flat unless the note structure clearly implies sub-tasking). Keep the description to 2–5 sentences maximum: one sentence on what the work is, one sentence on why it matters (if not obvious), a **DoD** sentence ("Done when X is true/observable/verifiable"), and the source-anchor backlink. The DoD should be concrete enough that someone can check it without reading the full context — e.g. "Done when `/linearissue` activates freeform mode on bare prompts and all titles pass the quality gate." Prepend `*[ai:claude-code]*` as the first line of the description so authorship is visible before the content. Do not copy prose, context, or reasoning from the design note into the description — the backlink is the bridge. If there is important context that should accompany the ticket, post it as a comment after creation instead.
3. **Update:** call `save_issue` with `id` set to the existing `LIN-NNN`, updating only fields that have meaningfully changed (description content, priority). Don't touch state, assignee, or labels unless explicitly indicated.
4. **Capture** the returned ticket ID and URL.

### Write back to the note

After all `save_issue` calls succeed:

5. **Re-read the note** (in case of any concurrent edits, though unlikely).
6. **Append `Linear: LIN-NNN` cross-references.** For each newly-created ticket, find the source action item line in the note and append ` (Linear: [LIN-NNN]({url}))` immediately after the item text. For multi-line items, append on a new line indented to the item's level.
7. **Append a Status Log row** to the note: today's date, `claude` as author, the comma-separated list of created/updated ticket IDs in the Related Tickets column, and a Notes column summary like "Filed N tickets via linearissue (M new, K updated)".
8. **Save the note** via `Edit`.

### Partial failure

If any `save_issue` call fails partway through:

- Stop the loop. Do not roll back successful tickets.
- Print which tickets succeeded and which failed.
- Write back annotations for the items that succeeded, so a re-run picks up where this one left off.
- Re-running is safe: idempotency on existing IDs prevents duplicates.

### Summary

Print:

- List of created/updated tickets with their URLs
- Any failures
- **Suggested commit message** following the repo's convention from `CLAUDE.md` (e.g., `chore(notes): file tickets from {slug} [ai:claude]`). **Do not commit** — git is HITL.

## Iteration mode

Entered when Phase 0 detects a Linear URL or issue ID. The filing-mode phases (1–3) are skipped entirely.

### Load the issue

Fetch in parallel:
- `get_issue` — title, description, status, labels, priority
- `list_comments` — full comment thread, ordered by `createdAt`

Print a compact header so the user can confirm you're looking at the right thing:

```
Iterating on: VID-123 — {title}
Status: {status} | Priority: {priority}
{N} comments
```

### Understand the intent

Read the user's prompt. Classify what they're asking for:

- **Analytical** — "roast this", "what's missing", "does this make sense", "what questions should we answer first" → produce analysis in chat, then offer to post it as a comment
- **Additive** — "add context about X", "note that we've decided Y" → draft a comment with the new context
- **Refinement** — "sharpen the description", "the title is off" → propose a new title and/or description (keep it minimal — anchor principle applies)
- **Mixed** — any combination of the above

For mixed intents, address all of them in a single pass rather than asking for clarification upfront.

### Draft the response

Produce one or both of:

1. **Comment draft** — the primary output in almost all cases. Write it as you would a thoughtful comment from a collaborator: direct, specific, actionable. Scope crystallization, analysis, questions, and context all belong here. (Provenance and formatting are handled by the commenting skill — do not add `*[ai:claude-code]*` manually.)
2. **Anchor update** — only if the intent is explicitly to fix the title or description. Draft the new title and/or description. Keep to 2–4 sentences. Do not migrate comment-appropriate content into the description.

Print both drafts in chat before asking for confirmation. Make it easy to scan.

### Confirm and write

Issue one `AskUserQuestion` with:
- **Always:** "Post this comment?" — **Yes** / **Edit first** / **Skip**
- **Only if an anchor update was drafted:** "Apply the title/description update?" — **Yes** / **Edit first** / **Skip**

Both questions can be batched into a single `AskUserQuestion` call.

On "Edit first": ask what to change, revise, re-present, confirm again.

On approval, write in this order:
1. `save_issue` for any title/description changes (only if confirmed)
2. `Skill("commenting", ...)` to delegate the comment to the commenting skill (only if confirmed) — describe the issue ID, comment title (e.g. "Assessment", "Context", "Scope refinement"), the originating skill name ("linearissue"), and the comment body. The commenting skill handles formatting, provenance, and threading.

### Summary

Print:
- What was posted/updated and the issue URL
- If nothing was written (user skipped both): note that and stop cleanly

## Freeform mode

Entered when Phase 0 detects neither a Linear URL/ID nor a design note path. This mode creates one or more issues directly from the user's prompt, with full title quality enforcement.

### Parse the request

Read the user's prompt and extract:

- **One or more items to ticket.** The user may describe a single issue ("create a ticket for X") or a batch ("file issues for: A, B, C" or a bulleted list). Each distinct item becomes one ticket.
- **Context clues** for routing: project mentions, team references, related tickets, priority signals.

For each item, draft:

- **Title** — derive from the user's description, then run through the **title quality gate**. If the gate fires, present the original and suggested rewrite.
- **Description** — 2–5 sentences maximum: what the work is, why it matters (if not obvious), and a **DoD** sentence ("Done when X is true/observable/verifiable"). Prepend `*[ai:claude-code]*` as the first line. Keep it minimal — the anchor principle applies just as in filing mode.
- **Priority** — infer from the user's language (urgent, blocker, nice-to-have, etc.). Default Normal.
- **Labels** — infer from context (e.g. "bug" → Bug label, "research" → Research label). Only apply if confident.

### Triage

Run the same triage check as filing mode (Tier 1–3 from Phase 0) to catch duplicates and scope overlap. If matches are found, present them before proceeding.

### Confirm and create

Print a structured proposal, similar to filing mode:

```
Proposed issues (freeform)
Target: {team-key} / {project-name}

 1. [NEW]  {Title}                          Priority: Normal
           {2-line description preview}
 2. [NEW]  {Title}                          Priority: High
           {2-line description preview}

{N} new issues
```

If any title triggered the quality gate, show both versions:

```
 1. [NEW]  {Original title}                 Priority: Normal
           Suggested: {Rewritten title}  ← value-oriented
           Warning: mechanism verb 'Implement' leading
```

Issue one `AskUserQuestion`: "Create these {N} issues?" with options: **Yes, create all** / **Exclude some** / **Edit titles** / **Bail**

- **Edit titles** — the user provides corrections; revise and re-present.
- **Exclude some** — one follow-up `AskUserQuestion` with multiSelect.

### Create tickets

For each confirmed ticket, call `save_issue` with team, project, title, description, priority, and labels. The same rules as filing mode apply:

- Descriptions are minimal anchors (2–4 sentences max)
- Prepend `*[ai:claude-code]*` to the description
- If extra context is needed beyond the anchor, post it as a comment after creation via the commenting skill

### Summary

Print:

- List of created tickets with their URLs
- Any failures
- No commit suggestion — freeform mode doesn't touch files

### Differences from filing mode

| | Filing mode | Freeform mode |
|---|---|---|
| Source | Design note file | User's prompt |
| Write-back | Inline `Linear: LIN-NNN` annotations in the note | None — no file to annotate |
| Status Log | Appended to the note | None |
| Commit suggestion | Yes (note was modified) | No (no files touched) |
| Title derivation | From action item text | From user's description |
| Title quality gate | Applied | Applied |
| Triage | Applied | Applied |

## Title quality gate

Every title — whether derived from a design note action item, drafted in freeform mode, or proposed as an iteration refinement — must pass through this gate. The gate is **advisory**: warn and suggest, never block.

### Principles

1. **Value over mechanism** — frame what the issue delivers, not how it's implemented. "Stable room link for live AV" not "Implement re-entrancy for Daily transport". The title should answer "what changes for the user/system?" not "what code do we write?".
2. **Distinguishing phrase first** — the most identifiable words lead. Linear branch names truncate after ~25 chars past the ticket prefix, so front-loaded meaning survives truncation.
3. **Brevity** — keep titles under 72 characters. Warn above 72. Shorter is almost always better.
4. **No metadata in the title** — priority, work type (spike, research), and category belong in labels, not bracket tags or prefixes.
5. **Branch-name-friendly** — avoid special characters, deep nesting, or parenthetical lists that produce ugly slugs.

### Anti-patterns to detect

- **Mechanism verbs leading** — "Wire up", "Implement", "Add", "Decouple", "Compose", "Inject", "Conduct", "Redesign", "Create", "Build", "Set up" as the first word. Exception: action verbs describing user-facing behavior are fine ("Allow bot to interrupt", "Select auth provider").
- **Bracket tags** — `[Research]`, `[Spike]`, `[WIP]`, `[Bug]` waste leading characters. Use Linear labels.
- **"Explore/Research/Spike:" prefixes** — metadata that belongs in labels.
- **Parenthetical lists** — "(Meet, Zoom, WhatsApp)" or "(bioguide_id, thomas_id)" add precision but kill readability. Use slash-separated inline: "Bot joins Meet/Zoom/WhatsApp calls".
- **Question-format titles** — "Does X expose Y?" belongs in the description.
- **Kitchen-sink titles** — "Improve X — reduce Y and explore Z" tries to do too much in one title.
- **Technical-identifier-first** — "bioguide_id mapping for member identity" buries the value. Prefer "Member identity resolver" or "Resolve member identity from bioguide".
- **Redundant context** — "linearissue skill: value-oriented titles + freeform creation mode" — the project/component prefix is redundant when the ticket already has a project and labels.

### When the gate fires

If any issues are detected, draft a suggested rewrite alongside the original. Present both in the confirmation step so the user can choose:

```
Title:     <original title>
Suggested: <rewritten title>  ← value-oriented, distinguishing phrase first
Warning:   <what triggered — e.g. "mechanism verb 'Implement' leading", "78 chars (>72)">
```

### Examples (before → after)

- "Implement member identity resolver (bioguide_id mapping)" → "Member identity resolver"
- "Create ticker identity resolver (security master)" → "Ticker identity from security master"
- "Add CI job for agents test suite" → "CI for agents test suite"
- "Set up authentication token storage mechanism" → "Auth token storage"
- "Wire up Logfire for APM and distributed tracing" → "Logfire APM and tracing"
- "Explore service-level hooks for production frame-level observability" → "Production frame-level o11y hooks"
- "Architectural spike: bot-as-participant in external video calls (Meet, Zoom, WhatsApp)" → "Bot joins Meet/Zoom/WhatsApp calls"

### Titles that pass (no rewrite needed)

- "Select auth provider" — short, clear, distinguishing
- "Optimise for showtime" — punchy, value obvious
- "Allow bot to interrupt" — user-facing behavior, 4 words
- "Uncensor STT input" — brief, clear what it delivers
- "Graceful LLM fallback on provider outage" — value-first, concise

## Anti-patterns

- **Don't run on `DONE`/`SUPERSEDED`/`CANCELED`/`DEPRECATED` notes** or anything in `ARCHIVE/`. Historical notes are read-only.
- **Don't run on `docs/decisions/` ADRs.** Decisions are records, not pending work.
- **Don't run on Linear documents.** This skill operates on repo-local `docs/design-notes/**` files. Linear-routed design notes (from `designnote`'s strategy path) are not in scope.
- **Don't transition the note's filename state.** No `git mv`, no rename. State transitions are HITL.
- **Don't commit or stage.** All git operations are HITL per the repo's `CLAUDE.md`.
- **Don't overwrite existing `Linear: LIN-NNN` annotations.** If present, the item is already ticketed; treat as update or skip, never re-create.
- **Don't expand the note's substantive content.** The only writes the skill makes to the source note are: (a) inline `Linear: LIN-NNN` cross-references on action items, and (b) one new Status Log row. Do not edit any other text in the note.
- **Don't create new files.** The note itself is the only file touched.
- **Don't interpret non-canonical sections creatively.** Only known sections and explicit `→ ticket` markers count. If a note has work in a non-canonical section, ask in Phase 3.
- **Don't infer parallel-safety from nothing.** If the note doesn't mention file scope or parallelism, leave parallel-safety unset. Don't fabricate predictions.
- **Don't bundle unrelated action items into a single ticket.** One action item = one ticket. If items are tightly coupled, the note should reflect that and the skill can model them as parent/sub-tasks — but only if the note is structured that way.
- **Don't over-fill descriptions.** A description is a minimal anchor — 2–5 sentences max (what, why, DoD, backlink). Do not copy prose, reasoning, or context from the design note into it. The backlink to the note is the bridge; if extra context is needed, post it as a comment after creation. Stuffing descriptions creates maintenance debt and obscures the signal.
- **Don't skip the DoD.** Every ticket needs a "Done when..." sentence in the description. It should be concrete and verifiable — not "done when implemented" but "done when X is observable/testable." If the source material doesn't imply a clear DoD, draft one from the action item's intent and surface it in the confirmation gate for the user to refine.
- **Don't default to description updates in iteration mode.** The user's intent is almost always to add a comment. Only propose a title/description change when the prompt explicitly targets the anchor (e.g. "the title is wrong", "rewrite the description"). When in doubt, put it in a comment.
- **Don't run filing-mode phases in iteration mode.** If a Linear URL or issue ID was detected, skip straight to the iteration phase. Do not look for design notes, parse action items, or propose ticket creation.
- **Don't silently rewrite comment history.** The skill can only add new comments via `save_comment`. It cannot edit or delete existing comments. If a prior comment is wrong, post a follow-up — don't attempt to alter the record.
- **Don't ask more than one round of clarification** (the "Exclude some" follow-up is the only permitted second question). If something surfaces during creation that should have been asked, note it in the summary and stop.
- **Don't follow URLs unbounded.** Use `WebFetch` only to enrich descriptions for URLs *already referenced in the note*, not to search the web. This skill is deterministic; research belongs in `designnote`.

## Required grants

The skill's `allowed-tools` declaration pre-approves the tool categories. For scoped edits to design notes, configure Claude Code settings:

```jsonc
{
  "permissions": {
    "allow": [
      "Edit(docs/design-notes/**)"
    ]
  }
}
```

The skill checks for this grant in Phase 0 and warns if it's missing.

Linear MCP write tool (`save_issue`) is declared in `allowed-tools` and should not require per-call approval. If it does, the walk-away UX breaks — configure pre-approval before first use. Comments are posted via `Skill("commenting", ...)`, which requires the `Skill` tool to be allowed so the commenting skill can be invoked.
