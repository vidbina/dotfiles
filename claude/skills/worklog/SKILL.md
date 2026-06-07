---
name: worklog
description: "Auto-log working time to the Work Log Google Calendar. Two modes: (1) auto-sync via 31-min cron creates/extends calendar entries silently, (2) /worklog review mode shows all unreviewed entries across all projects for approval. Heartbeats are never purged until the user explicitly approves them. On first use per project, backpopulates 30 days of history from git log (HITL). The skill only writes to the dedicated Work Log calendar, never to any other calendar."
api_description: "Work time logging to Google Calendar with two modes: auto-sync (cron, silent) and review (/worklog, cross-project approval). Three-state heartbeat lifecycle: pending → synced → approved."
allowed-tools: Bash Grep Read AskUserQuestion mcp__claude_ai_Google_Calendar__list_events mcp__claude_ai_Google_Calendar__create_event mcp__claude_ai_Google_Calendar__update_event mcp__claude_ai_Google_Calendar__list_calendars
---

# worklog

Auto-log working time to Google Calendar. **Two modes: auto-sync (cron) and review (user-invoked).**

Auto-sync runs via `SessionStart` hook + `CronCreate` interval — silently writes to the **Work Log** calendar. Review mode (`/worklog`) is user-invoked and shows all unreviewed entries across all projects for approval.

## Design principles

> **Invisible when working.** The cron fires every 31 min. If you're active, it silently extends the calendar entry. If you're idle, it closes the entry and goes quiet. No prompts, no noise.
>
> **Review when ready.** `/worklog` is the user's single review surface across all projects. It shows everything auto-synced since the last review. The user approves, corrects, or flags entries — on their schedule, not the cron's.
>
> **Heartbeats persist until approved.** Auto-sync marks heartbeats as `synced` (written to calendar) but never purges them. Only after the user explicitly approves via `/worklog` are they marked `approved` and eligible for purge. This preserves traceability.
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

The project code must be a single non-whitespace token before the colon — no spaces, no special characters. This makes entries easy to visually scan and grep.

**Examples:**
- `kb: skill migration`
- `dotfiles: devenv upgrade`
- `ivos: junction tooling`
- `yo: frame routing`
- `pb: techdd report`
- `flashy: ci pipeline`

### Project detection

Derive the project code from the repo directory name. The heuristic: take the first path component of the repo folder name (before any hyphens that aren't part of the code), lowercased.

For repos under an org folder (e.g. `Code/asabina-de/yo-convo-bot`), the first segment of the repo name is the code: `yo`.

| Repo directory | Project code |
|---|---|
| `kb` | kb |
| `yo-convo-bot` | yo |
| `ivos-trades` | ivos |
| `flashy-flutter-app` | flashy |
| `dotfiles` | dotfiles |
| `philipps-byrne` | pb |

**Fallback:** If the directory doesn't match any known pattern, use the full directory basename lowercased as the project code.

**Override:** If the repo has a `.worklog-project` file at the root, its contents (trimmed) override the heuristic. This handles edge cases like `philipps-byrne` → `pb` where the first segment doesn't match the desired code.

**First-use confirmation:** On `SessionStart`, if `.worklog-project` does not exist, the hook outputs a prompt telling the agent to confirm the heuristic-derived code with the user before bootstrapping the cron. On confirmation (or correction), the agent writes the code to `.worklog-project` so the user is never asked again. The `UserPromptSubmit` hook does not prompt — it falls back to the heuristic silently when the file is missing (it can't interact with the user from a shell hook).

### Short description

Derive from (in priority order):
1. **Session name** — if the session has been renamed, use that
2. **Branch name** — strip the owner prefix and ticket ID, humanize the slug (e.g. `vidbina/vid-676-auto-log-work-sessions` → `auto-log work sessions`)
3. **Last commit subject** — if no session name or meaningful branch

Keep to ~3-6 words. This is for calendar glanceability, not precision.

## Heartbeat database

Activity is tracked via a SQLite database at `~/.claude/worklog.db`. A `UserPromptSubmit` hook appends a heartbeat on every user message — this captures thinking/reviewing/discussing time that git history misses.

### Schema

```sql
CREATE TABLE IF NOT EXISTS heartbeats (
  ts TEXT NOT NULL,
  project TEXT NOT NULL,
  cwd TEXT NOT NULL,
  flushed INTEGER DEFAULT 0  -- 0 = pending, 1 = synced, 2 = approved
);
CREATE INDEX IF NOT EXISTS idx_heartbeats_project_flushed ON heartbeats(project, flushed);
```

**Three states:**

| Value | State | Meaning |
|---|---|---|
| 0 | pending | Heartbeat recorded, not yet written to calendar |
| 1 | synced | Auto-flushed to calendar by cron, awaiting user review |
| 2 | approved | User signed off via `/worklog`, safe to purge |

### UserPromptSubmit hook

The hook appends one row per user message:

```bash
INPUT=$(cat)
DIR=$(echo "$INPUT" | jq -r '.cwd // empty')
PROJECT=$(cd "$DIR" 2>/dev/null && basename "$(git remote get-url origin 2>/dev/null || echo "$DIR")" .git | cut -d- -f1 | tr '[:upper:]' '[:lower:]')
[ -n "$PROJECT" ] && sqlite3 ~/.claude/worklog.db "
  CREATE TABLE IF NOT EXISTS heartbeats (ts TEXT NOT NULL, project TEXT NOT NULL, cwd TEXT NOT NULL, flushed INTEGER DEFAULT 0);
  INSERT INTO heartbeats(ts, project, cwd) VALUES(
    strftime('%Y-%m-%dT%H:%M:%S', 'now', 'localtime'),
    '$PROJECT',
    '$DIR'
  );
"
```

**Note:** Claude Code passes session context (including `cwd`) via JSON on stdin, not as environment variables. The hook must read stdin first.

The `CREATE TABLE IF NOT EXISTS` makes the hook idempotent — first run creates the table, subsequent runs just insert.

### Lifecycle

1. **Hook writes heartbeats** — one per user prompt, with project code and timestamp (`flushed=0`)
2. **Cron reads pending heartbeats** — clusters `flushed=0` rows into work blocks
3. **Cron writes to calendar** — creates/extends entries
4. **Cron marks heartbeats as synced** — `UPDATE heartbeats SET flushed = 1 WHERE ...`
5. **User runs `/worklog`** — reviews all `flushed=1` rows across all projects
6. **User approves** — `UPDATE heartbeats SET flushed = 2 WHERE ...`
7. **Periodic purge** — `DELETE FROM heartbeats WHERE flushed = 2 AND ts < datetime('now', '-30 days')`

## Cron behavior (every 31 min)

On each cron fire:

### 1. Read unflushed heartbeats

```sql
SELECT ts, project FROM heartbeats
WHERE project = '{project}' AND flushed = 0
ORDER BY ts;
```

If no unflushed heartbeats exist, exit immediately. Don't touch the calendar.

### 2. Check staleness

Determine when the most recent heartbeat occurred for this project.

| Last heartbeat | Action |
|---|---|
| **Within 20 min of now** | Create or extend the calendar entry (active work) |
| **20-30 min ago** | Extend entry to the last heartbeat timestamp, then flush (close off the block) |
| **31+ min ago** | Flush heartbeats up to that point, close the entry. Exit. |

### 3. Find or create the calendar entry

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
- `startTime`: earliest unflushed heartbeat (rounded down to 10-min)
- `endTime`: now + 15 min (rounded to 10-min)
- `calendarId`: the Work Log calendar ID

### 4. Mark heartbeats as synced

After successfully writing to the calendar, mark as `synced` (not purged — user must approve first):

```sql
UPDATE heartbeats SET flushed = 1
WHERE project = '{project}' AND flushed = 0 AND ts <= '{latest_ts}';
```

### 5. Purge approved data

On each cron fire, clean up **approved** heartbeats older than 30 days:

```sql
DELETE FROM heartbeats WHERE flushed = 2 AND ts < datetime('now', '-30 days');
```

Only `flushed=2` (user-approved) rows are purged. Synced (`flushed=1`) rows persist until the user reviews them via `/worklog`.

### 6. Round to 10-min boundaries

All times round to the nearest 10 minutes:
- 14:03 → 14:00
- 14:07 → 14:10
- 14:25 → 14:30

## Backpopulation (first use per project)

On the first cron fire for a project (determined by zero matching entries in the Work Log calendar for that project in the last 30 days), reconstruct historical work blocks from git history.

Backpopulated entries use git commit timestamps only — they miss thinking/reviewing/discussing time between commits. The `[backfill]` postfix makes this quality distinction visible so backfilled entries are never confused with heartbeat-sourced entries.

### Algorithm

```bash
# Get all commits by the user in the last 30 days, with timestamps and subjects
git log --format="%aI|%s" --author="David" --since="30 days ago" --all
```

**Clustering:** Group commits into work blocks. Two commits are in the same block if they're within 30 min of each other. Each block's:
- `startTime` = first commit in the cluster (rounded down to 10-min)
- `endTime` = last commit in the cluster + 15 min (rounded up to 10-min)
- `summary` = `{project}: {scope} [backfill]` — scope derived from the most common conventional commit scope in the cluster (e.g. `skills`, `hooks`, `git tooling`, `nix config`). Always postfixed with `[backfill]`.

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

## Hook and cron setup

Two hooks work together:

### UserPromptSubmit hook (heartbeat writer)

Configured in `claude/settings.json`. Fires on every user message. Writes a heartbeat to the SQLite database. This is the activity signal the cron reads.

The hook is a shell command — it does NOT call MCP tools. It's purely a local file write.

### SessionStart hook (cron bootstrapper)

The `SessionStart` hook should output text that tells the agent to:

1. Detect the project from the working directory
2. Set up a `CronCreate` with `recurring: true`, firing every 31 min, and a prompt that executes the worklog cron behavior described above
3. On the first fire, check for backpopulation eligibility

The hook output is injected into the session context, so the agent picks it up naturally.

### Why two hooks

The `UserPromptSubmit` hook runs on every message — it must be fast and side-effect-free (just a SQLite insert). The cron fires every 31 min and does the expensive work (Calendar MCP reads/writes). Separating collection from processing keeps the prompt-response loop fast.

## Review mode (`/worklog`)

User-invoked. Shows all synced-but-unreviewed heartbeats across **all projects** — not just the current one. This is the single review surface.

### 1. Read synced heartbeats

```sql
SELECT project, MIN(ts) as first, MAX(ts) as last, COUNT(*) as beats
FROM heartbeats
WHERE flushed = 1
GROUP BY project
ORDER BY MAX(ts) DESC;
```

If no synced heartbeats exist, report "Nothing to review — all entries approved." and exit.

### 2. Present the review

```
Worklog review — {N} projects with unreviewed entries

| # | Project  | First activity | Last activity | Heartbeats | Calendar entries |
|---|----------|----------------|---------------|------------|------------------|
| 1 | dotfiles | Jun 07 05:43   | Jun 07 06:10  | 12         | 1 (extended)     |
| 2 | ivos     | Jun 07 05:50   | Jun 07 05:55  | 2          | 1 (new)          |
| 3 | yo       | Jun 06 14:20   | Jun 06 16:30  | 8          | 2 (new)          |

Approve all? yes / project {name} / cancel
```

Cross-reference with the Work Log calendar to show whether entries were created or extended.

### 3. Process approval

- **yes** — approve all: `UPDATE heartbeats SET flushed = 2 WHERE flushed = 1;`
- **project {name}** — approve one project: `UPDATE heartbeats SET flushed = 2 WHERE flushed = 1 AND project = '{name}';`
- **cancel** — exit without approving. Entries stay as `flushed=1`.

### 4. Handle pending heartbeats during review

If there are also `flushed=0` (pending, not yet synced to calendar) heartbeats, flush them to the calendar first, then include them in the review. This handles the case where the user runs `/worklog` before the cron has fired.

## Anti-patterns

- **Don't write to any calendar other than Work Log.** The calendar ID is hardcoded. Period.
- **Don't fire when idle.** If last activity was 31+ min ago, exit immediately. Don't burn tokens.
- **Don't create duplicate entries.** Always search before creating. Extend existing entries within the 30-min window.
- **Don't backpopulate twice.** Check for existing entries before backpopulating. If any exist for the project, skip.
- **Don't over-describe.** Entry summaries are 3-6 words. Calendar glanceability matters more than precision.
- **Don't prompt the user during cron.** The cron path runs silently. No `AskUserQuestion`, no chat output except errors. Prompting is only for `/worklog` review mode and backpopulation.
- **Don't purge synced heartbeats.** Only `flushed=2` (approved) rows older than 30 days are purged. Never delete `flushed=1` rows — the user hasn't reviewed them yet.
- **Don't round-trip on errors.** If a Calendar MCP call fails, log it in chat once and move on. Don't retry in a loop.
