---
name: linearissue
description: "Use this skill when the user asks you to turn a design note's action items into Linear issues — filing tickets from a note, linearizing the todos in a note, creating Linear issues from pending work in a design note, or shipping a note's action items to Linear for execution. Trigger for prompts like 'file tickets from this note', 'linearize X', 'create Linear issues for the action items in X', 'turn this note into tickets', 'ship this to Linear', 'file these TODOs from the design note', 'make tickets from X', 'linearissue this note', 'create issues from X.md'. Also trigger when the user invokes `/linearissue`. The skill reads a specific design note (or the most recent TODO/WIP note if not specified), parses its action items from canonical sections (## Action items, ## Next Action, ## Trigger Points, and inline checklists), prints a proposed-tickets summary in chat, confirms via AskUserQuestion, then creates the Linear issues and appends bidirectional cross-references back into the note. It refuses to run on DONE, SUPERSEDED, CANCELED, DEPRECATED notes, or anything in `ARCHIVE/`. Do NOT trigger for ad-hoc ticket creation not derived from a note (use the Linear MCP directly), for updating existing tickets in arbitrary ways (use the Linear MCP directly), for creating Linear documents (that's the designnote skill's strategy-routing path), for filing tickets from `docs/decisions/` ADRs (decisions are made; they don't spawn tickets), or for any operation that would also commit to git."
allowed-tools: Glob Grep Read Edit WebFetch AskUserQuestion mcp__claude_ai_Linear__list_issues mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__save_issue mcp__claude_ai_Linear__list_comments mcp__claude_ai_Linear__save_comment mcp__claude_ai_Linear__list_teams mcp__claude_ai_Linear__get_team mcp__claude_ai_Linear__list_projects mcp__claude_ai_Linear__get_project mcp__claude_ai_Linear__list_issue_statuses mcp__claude_ai_Linear__list_issue_labels
---

# linearissue

Take a design note's action items and **file them as Linear issues** — with bidirectional cross-references, idempotent re-runs, and an in-chat confirmation gate.

The defining design principles of this skill are:

> **The design note is the single source of truth. The skill reads it, derives tickets, and writes Linear ticket IDs back into it. No parallel work-plan file. No re-modeling.**
>
> **Confirm in chat, not via files.** The skill prints proposed tickets in the conversation, confirms with one `AskUserQuestion`, and creates on approval. One invocation, one session.
>
> **Idempotent re-runs.** A note that's already been linearized has inline `Linear: LIN-NNN` annotations. The skill detects them and updates instead of duplicating.

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
- Linear MCP write tools — `save_issue`, `save_comment`

The skill does not declare `Task` — there is no parallel research phase. Ticket derivation is deterministic and serial.

The skill does not declare `Write` — it never creates new files. It only edits the existing design note (appending cross-references and a Status Log row).

## Phase 0 — Preflight

### Parse invocation

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

- **Title:** the first line of the item, cleaned up (strip checklist markers, leading "TODO:", trailing punctuation).
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
 3. [UPDATE] {Title} (LIN-123)               Priority: Normal
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
2. **Create:** call `save_issue` with team, project, title, description (including the source-anchor footer), priority, parent (if dependency-hinted as a sub-task — but default to flat unless the note structure clearly implies sub-tasking).
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

Linear MCP write tools (`save_issue`, `save_comment`) are declared in `allowed-tools` and should not require per-call approval. If they do, the walk-away UX breaks — configure pre-approval before first use.
