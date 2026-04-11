---
name: designnote
description: "Use this skill when the user asks you to start, extend, or record a design note — ideation around a design problem, architectural choice, trade-off analysis, open question, or a decision that needs long-term awareness. Trigger for prompts like 'take a note on X', 'design note for X', 'let's think through X', 'capture the options for X', 'document this decision', 'explore trade-offs for X', 'start a dnote for X', 'write up the problem of X', 'we should think about X and park it', 'note the constraints around X', 'ideate on X'. Also trigger when the user invokes `/designnote`. The skill reads existing notes, related Linear tickets, and the repo's TODO.md, researches open questions in parallel via subagents, and writes (or extends) a design note in `docs/design-notes/` — OR, for strategy/process/team-facing content, a Linear document. It batches clarifying questions into ONE up-front round so the user can walk away and return to a finished artifact. Do NOT trigger for quick factual questions (use qsearch or answer directly), for code changes (just do the work), for small one-liners that belong in TODO.md (add the line directly without invoking the skill), for operational status updates on existing Linear tickets, for creating Linear issues from an existing design note's action items (that's the linearissue skill), or for writing ADRs in `docs/decisions/` (the skill intentionally does not touch that directory)."
allowed-tools: Task WebSearch WebFetch AskUserQuestion Glob Grep Read Write Edit mcp__claude_ai_Linear__list_issues mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__list_comments mcp__claude_ai_Linear__list_documents mcp__claude_ai_Linear__get_document mcp__claude_ai_Linear__save_document mcp__claude_ai_Linear__list_projects mcp__claude_ai_Linear__get_project mcp__claude_ai_Linear__list_teams mcp__claude_ai_Linear__get_team mcp__claude_ai_Linear__search_documentation
---

# designnote

Draft or extend a **design note** — a living, evolving document that records a problem, the options considered, the reasoning, the chosen approach, and any open questions that may be revisited. Design notes are the canonical surface for long-lived architectural thinking in this workflow.

The defining design principles of this skill are:

> **Batch every clarifying question into one up-front round, then run to completion uninterrupted.**
>
> **Persist findings to the design note as the single source of truth. No parallel work-plan files. No duplicate task representations.**
>
> **Prefer extending an existing note to creating a new one. Duplication erodes the knowledge base.**

The user should be able to kick this off, answer a small batch of up-front questions, walk away, and return to a design note they can review, edit, and then hand off to the `linearissue` skill for execution.

## Context on the artifact

A design note is a markdown file in `docs/design-notes/` following the convention:

- **Filename:** `YYYY-MM-DD.STATE.kebab-slug.md` where `STATE ∈ {TODO, WIP, REVIEW, DONE, SUPERSEDED, CANCELED, DEPRECATED}`. New notes start as `TODO`.
- **Front matter:** YAML block with `title` and `tags` (lowercase, kebab-case where multi-word).
- **Status Log:** table tracking lifecycle transitions with `Status | Date | Author | Related Tickets | Notes`.
- **Template sections:** `## Context`, `## Exploration`, `## Recommendation` at minimum. Notes may add custom sections like `## Problem Statement`, `## Open Questions`, `## Trigger Points`, `## Action items`, `## Upstream References`, `## Related Design Notes`.

A design note can record a decision inside itself (problem → options → choice → revisit history). This skill **never writes to `docs/decisions/`** — that directory is intentionally reserved for a separate (and in this workflow rarely used) immutable-ADR artifact and is HITL.

## Tools declared in allowed-tools

- `Task` — spawn parallel subagents for independent web research
- `WebSearch`, `WebFetch` — direct research from the main agent when needed
- `AskUserQuestion` — the one-shot batched-clarification tool
- `Glob`, `Grep`, `Read` — scope discovery across `docs/design-notes/**`, `docs/TODO.md`, `CLAUDE.md`
- `Write`, `Edit` — creating new notes and extending existing ones
- Linear MCP read tools — discovering related tickets and existing Linear docs
- `mcp__claude_ai_Linear__save_document` — for the strategy/process routing path

The `allowed-tools` declaration pre-approves tool *categories*. For scoped file writes, the user should configure Claude Code settings to pre-approve paths (see **Required grants** at the bottom).

## Phase 0 — Preflight

Before anything else, verify the repo follows the convention:

- `docs/design-notes/` exists and is writable. If not, stop and ask: "This repo doesn't have `docs/design-notes/`. Bootstrap it from the kb convention, or bail?" Do not silently create the directory.
- Read the repo's `CLAUDE.md` / `AGENTS.md` at the root for project-specific HITL policies (especially around git and external dependencies).
- Note whether `docs/TODO.md` exists (it may not — the skill degrades gracefully).
- Note whether `docs/decisions/` exists (read-only awareness; the skill never writes there).

If `WebSearch`, `WebFetch`, `Task`, or the Linear MCP tools are unavailable for policy or environment reasons, degrade gracefully: produce a note based on existing context only, and flag in the Status Log that web/Linear/subagent research was skipped.

## Phase 1 — Interpret and route

Restate the prompt in one or two sentences. Be explicit about the kind of design question it is.

Then apply the **routing test** from the asabina kb framework:

- **Code-adjacent** — architecture, implementation, tech stack, APIs, deployment, testing, tooling, developer experience, data model, security patterns → **repo** (`docs/design-notes/`)
- **Strategy / process / team** — business strategy, positioning, marketing/GTM, team process, product prioritization rationale, cross-project org decisions → **Linear document** (via `save_document`)
- **Ambiguous** → flag explicitly; ask in Phase 2 rather than silently defaulting

The kb's key test: *"Does this decision affect how code is written, deployed, or maintained?"* → repo. *"Is this about what to build or how to operate as a team?"* → Linear.

Write out the routing decision explicitly in your Phase 4 "locked decisions" commitment so the user can see and override.

## Phase 2 — Scope discovery (parallel, read-only)

Fan out read-only queries to discover what already exists. These can run in parallel:

1. **Tag-based note match.** Grep front matter `tags:` lines across `docs/design-notes/**/*.md`. Derive candidate tags from the prompt's semantic core. Find notes whose tags overlap. Respect the **too-general guard** (below).
2. **Content match.** Grep note titles and bodies for load-bearing keywords from the prompt.
3. **Linear tickets.** `list_issues` filtered by keyword(s). Capture ticket IDs, titles, state, and project.
4. **Linear documents** (only if Phase 1 leaned Linear-routed). `list_documents` filtered by keyword(s).
5. **TODO.md stubs.** Read `docs/TODO.md` and grep for related lines (look for existing stubs that link to design notes).
6. **Related notes cross-graph.** For each candidate match, check its `## Related Design Notes` section for transitive matches.

### Tag vocabulary handling

- Collect *all* existing tags from front matter across `docs/design-notes/**` (and `docs/decisions/**` if present — read only).
- Compute frequency per tag.
- **Too-general guard:** Any tag appearing on more than ~30% of notes is disqualified as a *sole* match. It can still be used, but only when paired with a narrower sibling tag. This keeps discovery actually discoverable.
- Prefer 2–4 specific tags over 1 general one.
- Only mint a new tag when no existing tag fits. If minted, log the reason in the Status Log `Notes` column.

## Phase 3 — Batched clarification

Walk the rubric and identify interpretive choices the prompt left open:

- **Match or create** — which existing note (if any) to extend
- **Routing** — only if Phase 1 was ambiguous
- **Research depth** — quick (no subagents, current-context only) / medium (2–4 parallel subagent queries) / deep (5+ queries)
- **Focus** — specific angles to prioritize, or free range
- **Deliverable shape** — new note from scratch, new section in existing note, extend existing sections, or just a TODO.md line

For each choice: **ask** or **assume**?

- **Ask** only when (a) the answer would materially change the research or output, AND (b) a reasonable default has a real chance of being wrong.
- **Assume** everything else.

Issue **all questions in a single `AskUserQuestion` call**. Prefer 0–3 questions; **4 is the hard cap**.

If you have zero questions, skip the tool call and write `**No clarification needed — proceeding directly.**` This is the walk-away contract: one interruption max.

### Typical Phase 2 question shapes

- "Which note should I extend (or create new)?" with concrete candidates
- "Route this to repo or Linear?" with the kb framework's default pre-selected
- "How deep should the research go?" with quick / medium / deep options

## Phase 4 — Commit to locked decisions

Write out the locked decisions explicitly. Format:

> - **D1 (match):** Extend `2025-09-04.TODO.card-layout-abstraction-layer.md` — *you chose*
> - **D2 (routing):** Repo design note — *assumed (code-adjacent)*
> - **D3 (depth):** Medium — 3 parallel research queries — *assumed*
> - **D4 (tags):** `layouts`, `abstraction`, `flutter` — *reused from vocabulary*
> - **D5 (shape):** Add new `## Proposed Solutions` subsections; extend `## Open Questions` — *assumed*

Aim for 4–8 decisions total. **After Phase 4 closes, no more questions for the rest of the run.** Anything that surfaces mid-research goes into the note's `## Open Questions` section, not into a new `AskUserQuestion` call.

## Phase 5 — Research execution

If depth is `quick`, skip to Phase 6.

Otherwise, decompose the research into N independent questions. Spawn N subagents via `Task` with `subagent_type: general-purpose` (or `Explore` for codebase-facing questions). Each subagent receives:

- The locked decisions
- The relevant Linear/note context the main agent has already gathered, passed as prompt input
- **One specific research question**
- Explicit instruction: *return structured findings as a string. Do not write to the filesystem. Do not read or modify any design notes.*

Main agent waits for all subagents, then integrates findings. **Subagents never hold the write lock — the main agent serializes all writes to the note.**

Guidelines:

- **Don't give subagents Linear read tools by default.** Main agent pulls Linear context once and passes it in as prompt input. This avoids each subagent re-querying the same data.
- **Cross-check load-bearing claims.** Any number, date, attribution, or architectural claim that would drive a recommendation should appear in at least two sources before being stated as fact.
- **Respect stopping criteria.** Don't over-research. Match depth to Phase 4.

## Phase 6 — Write

Two modes: **create new** or **extend existing**.

### Create new note

1. **Filename:** `YYYY-MM-DD.TODO.{slug}.md` where slug is kebab-case, terse, derived from the prompt, not duplicated in the directory.
2. **Template structure:**

   ```markdown
   ---
   title: "Human-Readable Title"
   tags: [tag1, tag2, tag3]
   ---

   # Design Note: Human-Readable Title

   ## Status Log

   | Status | Date       | Author | Related Tickets | Notes                         |
   | :----- | :--------- | :----- | :-------------- | :---------------------------- |
   | TODO   | YYYY-MM-DD | claude | [LIN-NNN](...)  | Initial creation via designnote |

   ## Context

   _What is the business or technical context? Why is this worth exploring? What is the scope?_

   ## Exploration

   _What alternatives were considered? What are the trade-offs (pros/cons) of each?_

   ## Recommendation

   _What is the recommended path forward? Why is it the best option?_
   ```

3. **Populate** with findings from the research phase. Add custom sections as needed: `## Open Questions`, `## Trigger Points`, `## Upstream References`, `## Related Design Notes`. Match the richer style of existing notes in the repo if they follow one.
4. **Action items.** If the research surfaces concrete follow-up work, add a `## Action items` section with checklist items. These are the hooks the `linearissue` skill will find later.

### Extend existing note

1. **Read the entire existing note** before touching it.
2. **Preserve all existing sections verbatim.** Do not rewrite. Do not normalize. Do not reformat.
3. **Append rather than replace.** New options go into `## Exploration` as new subsections. New questions go into `## Open Questions`. New action items go into `## Action items`. New references go into `## Upstream References`. If a section doesn't exist, add it at the natural location.
4. **Status Log row.** Append a new row: today's date, `claude` as author, any Linear ticket IDs discovered during research, and a concise note describing the nature of the extension (e.g., "Added Option 4 with performance analysis; raised 2 new open questions").
5. **Never transition the filename state.** `TODO → WIP` and similar are HITL git operations.

### Linear doc mode (routing test sent this to Linear)

1. Use `save_document` to create or update a Linear document in the workspace.
2. Content structure mirrors the repo template adapted for Linear's markdown:
   - `# Title`
   - Inline metadata block (tags, date, author) — Linear docs don't support front matter
   - `## Status Log` table (Linear renders markdown tables)
   - `## Context` / `## Exploration` / `## Recommendation`
3. If an existing Linear doc matches (from Phase 2 discovery), update it rather than creating a new one.
4. Do not write a corresponding file to the repo. This note lives in Linear.

## Phase 7 — Wrap up

Print a concise summary:

- **What was written:** path (or Linear doc URL) and the sections touched
- **Tags used:** with reuse-or-mint justification for each
- **Related discovered:** Linear tickets, related notes, TODO.md stubs that informed the work
- **Open questions captured:** count and pointer to the section
- **Suggested next step:** e.g., "Review the note, edit as needed, then run `/linearissue` on this path to file tickets from the action items."
- **Suggested commit message** following the repo's convention (if the repo's `CLAUDE.md` documents one). Include an `[ai:claude]` tag if that's the repo convention. **Do not commit** — git is HITL.

If any research step failed or was skipped (e.g., web unavailable, Linear MCP absent), flag it clearly in both the wrap-up and the note's Status Log.

## Anti-patterns

- **Don't touch `docs/decisions/`.** Decisions are recorded inside design notes in this workflow. The `decisions/` directory is reserved for a separate immutable-ADR pattern that is intentionally HITL.
- **Don't rename files.** Filename state transitions (TODO → WIP → DONE → SUPERSEDED) are git operations and HITL. The skill only creates new files or edits existing ones in place.
- **Don't commit or stage.** All git operations are HITL per the repo's `CLAUDE.md`.
- **Don't skip scope discovery.** Prefer extending an existing note. Duplication erodes the knowledge base and fragments context.
- **Don't write from subagents.** Subagents return findings as strings. The main agent holds the write lock on the design note and serializes all writes.
- **Don't silently mint new tags.** Prefer reuse from the existing vocabulary. Disqualify the too-general tags. If a new tag is necessary, log the reason in the Status Log `Notes` column.
- **Don't overwrite custom sections when extending.** Preserve exactly what's there. Only append.
- **Don't write to `ARCHIVE/` directories.** They are historical. Read-only from the skill's perspective.
- **Don't ask more than one round of clarification.** Phase 3 is the only interruption. Anything that surfaces mid-research goes into the note's `## Open Questions` section and the run continues.
- **Don't silently fall back to training data.** If web research fails and the prompt requires fresh info, flag it in the Status Log notes column and in the wrap-up.
- **Don't assume repo conventions.** If `docs/design-notes/` doesn't exist, ask the user whether to bootstrap or bail. Don't create the directory silently.
- **Don't replicate action items elsewhere.** The design note is the single source of truth. No parallel work-plan files, no task list extractions, no duplicate representations. The `linearissue` skill will consume the note directly.
- **Don't pre-mint ticket IDs.** The note carries action items in plain markdown. Linear IDs are written back by `linearissue` after tickets exist, not before.
- **Don't interpret "create a design note" as license to create churn.** If an existing note covers the scope, extend it. If a TODO.md one-liner would suffice, just add the line. The warrants-a-note test is: (a) involves a choice with trade-offs, (b) has open questions worth parking, or (c) couples to other architectural concerns. If fewer than one applies, it's probably a TODO.md line.

## Required grants

The skill's `allowed-tools` declaration pre-approves the tool categories it needs. For file writes to be scoped to the design-notes directory and TODO.md, configure Claude Code settings (user or workspace) to pre-approve the paths:

```jsonc
{
  "permissions": {
    "allow": [
      "Write(docs/design-notes/**)",
      "Write(docs/TODO.md)",
      "Edit(docs/design-notes/**)",
      "Edit(docs/TODO.md)"
    ]
  }
}
```

Linear MCP tools are declared in `allowed-tools` and should not require per-call approval. The strategy-routing path uses `save_document` which needs Linear write access — same grant.

If these paths aren't pre-approved, the skill will still work but will prompt on each write, breaking the walk-away UX. Configure them before running the skill for the first time.
