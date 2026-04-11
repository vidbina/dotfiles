---
name: linearissue
description: "Use this skill when the user asks you to turn a design note's action items into Linear issues — filing tickets from a note, linearizing the todos in a note, creating Linear issues from pending work in a design note, or shipping a note's action items to Linear for execution. Trigger for prompts like 'file tickets from this note', 'linearize X', 'create Linear issues for the action items in X', 'turn this note into tickets', 'ship this to Linear', 'file these TODOs from the design note', 'make tickets from X', 'linearissue this note', 'create issues from X.md'. Also trigger when the user invokes `/linearissue`. The skill reads a specific design note (or the most recent TODO/WIP note if not specified), parses its action items from canonical sections (## Action items, ## Next Action, ## Trigger Points, and inline checklists), produces a dry-run preview file alongside the note, and on `--apply` creates the Linear issues and appends bidirectional cross-references back into the note. It refuses to run on DONE, SUPERSEDED, CANCELED, DEPRECATED notes, or anything in `ARCHIVE/`. Do NOT trigger for ad-hoc ticket creation not derived from a note (use the Linear MCP directly), for updating existing tickets in arbitrary ways (use the Linear MCP directly), for creating Linear documents (that's the designnote skill's strategy-routing path), for filing tickets from `docs/decisions/` ADRs (decisions are made; they don't spawn tickets), or for any operation that would also commit to git."
allowed-tools: Glob Grep Read Write Edit WebFetch AskUserQuestion mcp__claude_ai_Linear__list_issues mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__save_issue mcp__claude_ai_Linear__list_comments mcp__claude_ai_Linear__save_comment mcp__claude_ai_Linear__list_teams mcp__claude_ai_Linear__get_team mcp__claude_ai_Linear__list_projects mcp__claude_ai_Linear__get_project mcp__claude_ai_Linear__list_issue_statuses mcp__claude_ai_Linear__list_issue_labels
---

# linearissue

Take a design note's action items and **file them as Linear issues** — with bidirectional cross-references, idempotent re-runs, and a mandatory dry-run preview gate between ideation and execution.

The defining design principles of this skill are:

> **The design note is the single source of truth. The skill reads it, derives tickets, and writes Linear ticket IDs back into it. No parallel work-plan file. No re-modeling.**
>
> **Dry run by default. The HITL gate is a mandatory preview file that the human reviews before any Linear ticket is created. `--apply` is a separate, deliberate step.**
>
> **Idempotent re-runs.** A note that's already been linearized has inline `Linear: LIN-NNN` annotations. The skill detects them and updates instead of duplicating.

The user should be able to run this on a reviewed design note, get a preview file, walk away, come back to review the preview, and then run `--apply` to materialize the tickets in one short step.

## How this fits the broader workflow

```
prompt
  │
  ▼
designnote skill  →  design note (with action items in canonical sections)
  │
  ▼
[HITL gate: human reviews, edits, greenlights]
  │
  ▼
linearissue       →  preview file (dry run)
  │
  ▼
[HITL gate: human reviews preview]
  │
  ▼
linearissue --apply  →  Linear tickets + Linear: LIN-NNN annotations in the note
```

The skill never closes its own loop. There are exactly two gates between ideation and execution, and the human owns both.

## Tools declared in allowed-tools

- `Glob`, `Grep`, `Read` — discover and parse the design note
- `Write`, `Edit` — write the preview file and append cross-references back to the note
- `WebFetch` — follow URLs referenced in the note when enriching ticket descriptions
- `AskUserQuestion` — one-shot batched clarification (only when truly needed)
- Linear MCP read tools — search for existing tickets, resolve teams/projects, look up statuses and labels
- Linear MCP write tools — `save_issue`, `save_comment` for the apply phase

The skill does not declare `Task` — there is no parallel research phase. Ticket derivation is deterministic and serial.

## Phase 0 — Preflight

Parse invocation:

- **Note path** (positional or inferred). If not specified, default to *the most recently modified* file in `docs/design-notes/` whose filename state token is `TODO`, `WIP`, or `REVIEW`. If multiple match, ask in Phase 3.
- **`--apply` flag** (default: dry-run).
- Other flags: `--team`, `--project` (override note-derived routing).

Verify the note:

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
- Otherwise: ask in Phase 3.

## Phase 3 — Batched clarification (only if needed)

Issue **one** `AskUserQuestion` call only when:

- Multiple notes match the inferred default and the skill can't pick deterministically
- Target team or project is genuinely ambiguous (multiple plausible signals)
- The candidate count is large (>10) and the user might want to narrow scope before proceeding
- The note has non-canonical sections that *might* contain action items (the skill defaults to skip — ask only if it would materially change the output)

Otherwise skip the tool call entirely. The walk-away contract is the same as `designnote`: zero or one round of questions, never more.

## Phase 4 — Generate preview file

Write the preview to: `{note-path-without-extension}.tickets-preview.md`

Example: for `docs/design-notes/2026-04-11.TODO.foo-bar.md`, the preview is `docs/design-notes/2026-04-11.TODO.foo-bar.tickets-preview.md`.

Preview structure:

```markdown
# Ticket Preview — {Note Title}

**Generated:** YYYY-MM-DD HH:MM
**Source note:** `docs/design-notes/{filename}`
**Mode:** DRY RUN — no Linear tickets created
**Target team:** {team-key}
**Target project:** {project-name}

> Review the proposed tickets below. To create them in Linear, run:
>
> ```
> /linearissue {note-path} --apply
> ```
>
> The skill will regenerate the preview first to catch any drift since this file was written, then create or update tickets idempotently. Any candidate that already has a `Linear: LIN-NNN` annotation in the source note will be updated rather than re-created.

---

## Ticket 1 — {Title}

- **Source anchor:** `{note-path}#{anchor}`
- **Status:** new — will create
- **Priority:** Normal
- **Dependencies:** (none)
- **Existing Linear ID:** —

### Description

> Quoted context from the note, including parent section heading and surrounding prose.

**Source:** `{note-path}#{anchor}`

---

## Ticket 2 — {Title}

- **Source anchor:** `{note-path}#{anchor}`
- **Status:** existing — will update LIN-123
- **Priority:** High
- **Dependencies:** depends on Ticket 1
- **Existing Linear ID:** [LIN-123](https://linear.app/...)

### Description

...

---

## Sequencing

Linear order (dependencies):

1. Ticket 1
2. Ticket 2 (after Ticket 1)
3. Ticket 3 (after Ticket 1)

Ticket 2 and Ticket 3 are dependency-independent of each other, but parallel safety has not been analyzed.

## Skipped candidates

- `- [x] already done thing` — already marked complete
- `- [ ] explored option C` — inside `## Discarded options` section
```

Stop after writing the preview. Print summary:

- Path to the preview file
- Count of new vs existing-update tickets
- Any cycles, ambiguities, or skipped sections to be aware of
- Reminder: re-run with `--apply` to materialize

## Phase 5 — Apply (only if `--apply` is set)

**Precondition:** Always regenerate the preview first. If the note has been edited since the last preview, the new preview reflects the current state. Compare counts and warn if they differ significantly.

For each ticket in the regenerated preview:

1. **Existing detection.** If the note's source-anchor area already has an inline `Linear: LIN-NNN` annotation, or if a Linear search by source anchor URL in the description footer finds an existing ticket, treat as **update**. Otherwise **create**.
2. **Create:** call `save_issue` with team, project, title, description (including the source-anchor footer), priority, parent (if dependency-hinted as a sub-task — but default to flat unless the note structure clearly implies sub-tasking).
3. **Update:** call `save_issue` with `id` set to the existing `LIN-NNN`, updating only fields that have meaningfully changed (description content, priority). Don't touch state, assignee, or labels unless explicitly indicated.
4. **Capture** the returned ticket ID and URL.

After all `save_issue` calls succeed:

5. **Re-read the note.**
6. **Append `Linear: LIN-NNN` cross-references.** For each newly-created ticket, find the source action item line in the note and append ` (Linear: [LIN-NNN]({url}))` immediately after the item text. For multi-line items, append on a new line indented to the item's level.
7. **Append a Status Log row** to the note: today's date, `claude` as author, the comma-separated list of created/updated ticket IDs in the Related Tickets column, and a Notes column summary like "Filed N tickets via linearissue (M new, K updated)".
8. **Save the note.**

After the note is updated:

9. **Rename the preview file** from `{slug}.tickets-preview.md` to `{slug}.tickets-applied.md`. Do not delete it. The applied preview is auditable via git.
10. **Print summary:** list of created/updated tickets with their URLs, and any failures.
11. **Suggest a commit message** following the repo's convention from `CLAUDE.md` (e.g., `chore(notes): file tickets from {slug} [ai:claude]`). **Do not commit** — git is HITL.

If any `save_issue` call fails partway through:

- Stop the loop. Do not roll back successful tickets.
- Print which tickets succeeded and which failed.
- The successful tickets' IDs should still be written back into the note (for the items that succeeded), so a re-run picks up where this one left off.
- Re-running is safe: idempotency on existing IDs prevents duplicates.

## Anti-patterns

- **Don't create tickets without writing the preview first.** Even with `--apply`, regenerate the preview to catch drift.
- **Don't run on `DONE`/`SUPERSEDED`/`CANCELED`/`DEPRECATED` notes** or anything in `ARCHIVE/`. Historical notes are read-only.
- **Don't run on `docs/decisions/` ADRs.** Decisions are records, not pending work.
- **Don't run on Linear documents.** This skill operates on repo-local `docs/design-notes/**` files. Linear-routed design notes (from `designnote`'s strategy path) are not in scope.
- **Don't transition the note's filename state.** No `git mv`, no rename. State transitions are HITL.
- **Don't commit or stage.** All git operations are HITL per the repo's `CLAUDE.md`.
- **Don't overwrite existing `Linear: LIN-NNN` annotations.** If present, the item is already ticketed; treat as update or skip, never re-create.
- **Don't expand the note's substantive content.** The only writes the skill makes to the source note are: (a) inline `Linear: LIN-NNN` cross-references on action items, and (b) one new Status Log row. Do not edit any other text in the note.
- **Don't delete the preview file.** Rename to `.tickets-applied.md` so the applied state is auditable.
- **Don't interpret non-canonical sections creatively.** Only known sections and explicit `→ ticket` markers count. If a note has work in a non-canonical section, ask in Phase 3.
- **Don't infer parallel-safety from nothing.** If the note doesn't mention file scope or parallelism, leave parallel-safety unset. Don't fabricate predictions.
- **Don't bundle unrelated action items into a single ticket.** One action item = one ticket. If items are tightly coupled, the note should reflect that and the skill can model them as parent/sub-tasks — but only if the note is structured that way.
- **Don't ask more than one round of clarification.** Phase 3 is the only interruption. If something surfaces during apply that should have been asked, surface it in the post-apply summary and stop.
- **Don't pre-mint synthetic IDs.** Linear ticket IDs are the cross-reference. They don't exist until `--apply`. Dry-run previews refer to candidates by sequence number ("Ticket 1", "Ticket 2"), not by fake IDs.
- **Don't follow URLs unbounded.** Use `WebFetch` only to enrich descriptions for URLs *already referenced in the note*, not to search the web. This skill is deterministic; research belongs in `designnote`.

## Required grants

The skill's `allowed-tools` declaration pre-approves the tool categories. For scoped writes to design notes, the user should configure Claude Code settings:

```jsonc
{
  "permissions": {
    "allow": [
      "Write(docs/design-notes/**)",
      "Edit(docs/design-notes/**)"
    ]
  }
}
```

Linear MCP write tools (`save_issue`, `save_comment`) are declared in `allowed-tools` and should not require per-call approval. If they do, the walk-away UX breaks — configure pre-approval before first use.
