---
name: worklog
description: "Auto-log working time to the Work Log Google Calendar. Fires on a 31-min cron interval during active sessions. Creates/extends calendar entries per-project with 10-min granularity. On first use per project, backpopulates 30 days of history from git log (HITL — presents entries for approval). Do NOT invoke this skill manually — it is set up automatically via SessionStart and runs via CronCreate. The skill only writes to the dedicated Work Log calendar, never to any other calendar."
api_description: "Automatic work time logging to Google Calendar. Creates and extends calendar entries per-project based on session activity. Manages staleness detection, entry deduplication, and git-based backpopulation."
allowed-tools: Bash Grep Read mcp__claude_ai_Google_Calendar__list_events mcp__claude_ai_Google_Calendar__create_event mcp__claude_ai_Google_Calendar__update_event mcp__claude_ai_Google_Calendar__list_calendars
---

# worklog

Auto-log working time to Google Calendar. **Runs automatically — not user-invoked.**

This skill is set up by a `SessionStart` hook and maintained by a `CronCreate` interval. It writes exclusively to the **Work Log** calendar.

## Design principles

> **Invisible when working.** The cron fires every 31 min. If you're active, it silently extends the calendar entry. If you're idle, it closes the entry and goes quiet. No prompts, no noise.
>
> **10-minute granularity.** All timestamps round to the nearest 10-min boundary. Good enough for time allocation; not a timeclock.
>
> **One calendar, hardcoded.** All writes go to the Work Log calendar. No other calendar is ever touched. This is a scope constraint, not a suggestion.
>
> **Git history as ground truth for backpopulation.** On first use per project, reconstruct 30 days of work blocks from commit timestamps. Backpopulation is HITL — present the entries for approval before creating them.
>
> **31-min interval.** Guarantees crossing at least one 30-min threshold between fires. ~16 fires for an 8h workday. Cheaper than 15-min and still well within tolerance.

## Calendar

| Field | Value |
|---|---|
| Name | Work log |
| ID | `c_1e9d038a334d5c71f17ceb90e0b4c1563f2ef37c38ae0ae926f9c5434bc2c53a@group.calendar.google.com` |
| Timezone | Europe/Berlin |

**This is the only calendar the skill writes to.** The ID is hardcoded. Do not parameterize it.

## Entry format

```
{project}: {short description}
```

Two elements only. No categories, no tags, no brackets.

**Examples:**
- `KB: skill migration`
- `Dotfiles: devenv upgrade`
- `I+D #2: junction tooling`
- `Yo Convo Bot: frame routing`
- `Philipps-Byrne: techdd report`
- `Flash Decks: ci pipeline`

### Project detection

Derive the project name from the working directory's git remote or directory name. Map to the Linear project name:

| Directory / remote pattern | Project name |
|---|---|
| `*/kb` or `*/kb.git` | KB |
| `*/dotfiles` or `*/dotfiles.git` | Dotfiles |
| `*/yo-convo-bot` or `*/yo-convo-bot.git` | Yo Convo Bot |
| `*/ivos-trades` or `*/ivos-trades.git` | I+D #2 |
| `*/flash-decks` or `*/flash-decks.git` | Flash Decks |
| `*/philipps-byrne` or `*/philipps-byrne.git` | Philipps-Byrne |

If the directory doesn't match any known project, use the directory basename as the project name.

### Short description

Derive from (in priority order):
1. **Session name** — if the session has been renamed, use that
2. **Branch name** — strip the owner prefix and ticket ID, humanize the slug (e.g. `vidbina/vid-676-auto-log-work-sessions` → `auto-log work sessions`)
3. **Last commit subject** — if no session name or meaningful branch

Keep to ~3-6 words. This is for calendar glanceability, not precision.

## Cron behavior (every 31 min)

On each cron fire:

### 1. Check staleness

Determine when the last meaningful activity occurred in this session. Meaningful activity = last user prompt or last tool use (whichever is more recent).

| Last activity | Action |
|---|---|
| **Within 20 min** | Create or extend the calendar entry (active work) |
| **20-30 min ago** | Extend entry to the last activity timestamp, then stop updating (close off the block) |
| **31+ min ago** | Do nothing. Don't touch the calendar. Exit immediately. |

### 2. Find or create the entry

Search for today's entries on the Work Log calendar matching this project:

```
list_events(
  calendarId: WORKLOG_CALENDAR_ID,
  startTime: today 00:00,
  endTime: today 23:59,
  fullText: "{project}:"
)
```

**If a matching entry exists and its `endTime` is within 30 min of now:** extend it by updating `endTime` to now (rounded to 10-min).

**If a matching entry exists but its `endTime` is >30 min ago:** this is a previous work block from earlier today. Create a new entry (don't extend a stale block from this morning).

**If no matching entry exists:** create a new entry:
- `summary`: `{project}: {short description}`
- `startTime`: now (rounded down to 10-min)
- `endTime`: now + 15 min (rounded to 10-min)
- `calendarId`: the Work Log calendar ID

### 3. Round to 10-min boundaries

All times round to the nearest 10 minutes:
- 14:03 → 14:00
- 14:07 → 14:10
- 14:25 → 14:30

## Backpopulation (first use per project)

On the first cron fire for a project (determined by zero matching entries in the Work Log calendar for that project in the last 30 days), reconstruct historical work blocks from git history.

### Algorithm

```bash
# Get all commits by the user in the last 30 days, with timestamps
git log --format="%aI" --author="David" --since="30 days ago" --all
```

**Clustering:** Group commits into work blocks. Two commits are in the same block if they're within 30 min of each other. Each block's:
- `startTime` = first commit in the cluster (rounded down to 10-min)
- `endTime` = last commit in the cluster + 15 min (rounded up to 10-min)
- `summary` = `{project}: backfill` (or derive from the most common commit scope in the cluster)

**Guard:** Only backpopulate if the Work Log calendar has zero entries for this project in the 30-day window. If any entries exist, skip backpopulation entirely — the user may have manually logged some blocks.

**HITL gate:** Backpopulation is a significant calendar mutation. Before creating any entries:

1. Present the proposed entries as a table:

```
Backpopulation for {project} — {N} work blocks from git history (last 30 days)

| # | Date | Start | End | Duration | Summary |
|---|------|-------|-----|----------|---------|
| 1 | Jun 03 | 10:20 | 12:40 | 2h20m | {project}: {derived from commits} |
| 2 | Jun 03 | 14:30 | 16:10 | 1h40m | {project}: {derived from commits} |
...

Create all {N} entries? yes / edit / cancel
```

2. Wait for explicit approval via `AskUserQuestion`. The operator can approve all, edit individual entries, or cancel.

**Cap at 100 entries.** If git history produces more than 100 work blocks, present only the 100 most recent and note how many were truncated.

**No bulk API.** Google Calendar MCP only supports one `create_event` call per entry. Backpopulation of 100 entries = 100 sequential API calls. This is slow but runs once per project.

After backpopulation, post a one-line summary in the session: "Backpopulated {N} work blocks for {project} from git history (last 30 days)."

## SessionStart setup

The `SessionStart` hook should output text that tells the agent to:

1. Detect the project from the working directory
2. Set up a `CronCreate` with `recurring: true`, cron expression `*/31 * * * *` (every 31 min), and a prompt that executes the worklog cron behavior described above
3. On the first fire, check for backpopulation eligibility

The hook output is injected into the session context, so the agent picks it up naturally.

## Anti-patterns

- **Don't write to any calendar other than Work Log.** The calendar ID is hardcoded. Period.
- **Don't fire when idle.** If last activity was 31+ min ago, exit immediately. Don't burn tokens.
- **Don't create duplicate entries.** Always search before creating. Extend existing entries within the 30-min window.
- **Don't backpopulate twice.** Check for existing entries before backpopulating. If any exist for the project, skip.
- **Don't over-describe.** Entry summaries are 3-6 words. Calendar glanceability matters more than precision.
- **Don't prompt the user.** This skill runs silently. No `AskUserQuestion`, no chat output except the backpopulation summary and errors.
- **Don't round-trip on errors.** If a Calendar MCP call fails, log it in chat once and move on. Don't retry in a loop.
