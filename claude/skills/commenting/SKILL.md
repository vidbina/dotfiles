---
name: commenting
description: "Use this skill when another skill or the user needs to post a structured comment to a Linear issue. This skill owns the commenting protocol: thread lifecycle (anchor → replies → resolution), provenance markers, formatting, and threading via parentId. Trigger when a skill wants to post an assessment, finding, plan, status update, or session summary to a Linear issue. Also trigger when the user invokes `/commenting`. Do NOT use this skill for reading comments (use list_comments directly) or for creating/updating issues (use save_issue directly). This skill only handles comment creation with proper formatting and threading."
api_description: "Post structured comments to Linear issues following a uniform protocol: thread anchors, threaded replies, provenance markers, and resolution semantics. Handles formatting and threading so calling skills don't have to."
allowed-tools: mcp__claude_ai_Linear__save_comment mcp__claude_ai_Linear__list_comments mcp__claude_ai_Linear__get_issue
---

# commenting

Post structured comments to Linear issues. This skill owns the **how** of commenting — formatting, provenance, threading. Calling skills own the **what** — when to comment and what content to include.

## Protocol

Every comment follows this structure:

```
Issue comments:
├── Thread anchor (top-level): one per unit of work
│   ├── Reply: interim update
│   ├── Reply: interim update
│   └── Reply: completion/resolution
├── Thread anchor (top-level): another unit of work
│   └── Reply: completion/resolution
```

### Comment anatomy

Every comment has three parts:

1. **Heading line:** `**{Title}** ({caller-skill})`
2. **Provenance line:** `*[ai:claude-code]*`
3. **Body:** the content, in markdown

Example:

```markdown
**Assessment** (pairprog)

*[ai:claude-code]*

**Clear and ready:**
- Add Google OAuth callback endpoint
- Store refresh tokens in existing session table
```

### Thread lifecycle

- **Anchor:** A top-level comment that starts a thread. One anchor per logical unit of work (one assessment, one spike, one plan, one session summary). Post with `save_comment(issueId, body)`.
- **Reply:** A threaded response under an anchor. Used for updates, completions, and corrections to the anchor's unit of work. Post with `save_comment(issueId, body, parentId)` where `parentId` is the anchor's comment ID.
- **Resolution:** The final reply in a thread. Prefix the heading with ✅ to signal completion: `**✅ Step 1 complete** (pairprog)`. (Linear MCP doesn't expose `resolvedAt`, so the emoji is the visual signal.)

### When to anchor vs reply

| Scenario | Action |
|---|---|
| New unit of work (assessment, plan, spike, investigation) | **Anchor** — new top-level comment |
| Update to an existing unit (step progress, plan adjustment, interim finding) | **Reply** — thread under the anchor |
| Completion of a unit (step done, session wrap-up, investigation concluded) | **Reply** with ✅ heading — thread under the anchor |
| Unrelated new unit during same session | **Anchor** — new top-level comment |

### Provenance

The provenance marker identifies the authoring surface:

- `*[ai:claude-code]*` — comment authored by Claude Code (interactive session)
- `*[ai:managed-agent]*` — comment authored by a managed agent (walk-away)

The marker always appears on line 3 (after heading + blank line), never inline or at the end.

### Mid-thread events

When something changes mid-thread (plan adjusted, step skipped, blocker found), use these emoji prefixes on the heading:

- 🚨 — plan change
- ⏭️ — step skipped
- 🌟 — step added
- 🚧 — blocker discovered
- ⚠️ — conflict between plan and reality

## Interface

Calling skills invoke this skill via `Skill("commenting", args)` where `args` is a natural-language instruction. The commenting skill parses the intent and calls `save_comment` with proper formatting.

### Anchor examples

```
Post an anchor comment on VID-123 titled "Assessment" from skill "pairprog" with body:

**Clear and ready:**
- Item 1
- Item 2

**Unclear:**
- Question 1
```

```
Post an anchor comment on VID-456 titled "Troubleshoot findings" from skill "troubleshoot" with body:

**Root cause:**
The config file is missing the API key entry.

**Evidence:**
- `src/config.ts:42` — reads `process.env.API_KEY` but `.env.example` doesn't list it
```

### Reply examples

```
Reply to anchor {comment-id} on VID-123 titled "Step 1 complete" from skill "pairprog" with body:

Added OAuth callback route in `src/routes/auth.ts`.
Test added — passing.
Commit: `a1b2c3d`
```

```
Reply to anchor {comment-id} on VID-123 titled "🚨 Plan adjusted" from skill "pairprog" with body:

Step 3 replaced with Step 3a: explicit re-login flow.
Reason: navigator decided silent refresh adds complexity.
```

### Resolution example

```
Reply to anchor {comment-id} on VID-123 titled "✅ Session complete" from skill "pairprog" with body:

**Completed:**
- [x] Step 1: OAuth callback route
- [x] Step 2: Token storage

**Remaining:**
- (none — PR created: #47)
```

## Return value

After posting, the skill returns the comment ID in chat so the calling skill can use it for threading:

```
Posted anchor comment {comment-id} on VID-123.
```

or

```
Posted reply to {parent-id} on VID-123.
```

The calling skill should store anchor IDs for the duration of its session to thread subsequent replies correctly.

## Anti-patterns

- **Don't post without a heading.** Every comment needs `**{Title}** ({skill})` on line 1.
- **Don't skip provenance.** Every comment needs `*[ai:claude-code]*` on line 3.
- **Don't post "starting X" comments.** Wait until X is done, then post findings. One comment per completed unit, not a running log.
- **Don't interleave parallel results.** When multiple subagents complete, post one combined anchor rather than separate anchors that fragment the timeline.
- **Don't use top-level comments for updates.** If an anchor exists for this unit of work, reply to it. Only create a new anchor for a genuinely new unit.
- **Don't post empty resolution replies.** If there's nothing to say beyond "done", fold it into the last substantive reply with the ✅ prefix.
