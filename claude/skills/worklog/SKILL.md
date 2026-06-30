---
name: worklog
description: "Auto-log working time to the Work Log Google Calendar. Three modes: (1) collect — silent heartbeat recording via UserPromptSubmit hook, (2) sync — flush pending heartbeats across all projects to Calendar (run in a dedicated session), (3) review — show all unreviewed entries for approval. Heartbeats are never purged until the user explicitly approves them. On first use per project, backpopulates 30 days of history from git log (HITL). The skill only writes to the dedicated Work Log calendar, never to any other calendar."
api_description: "Work time logging to Google Calendar with three modes: collect (hook, silent), sync (dedicated session, cross-project flush), and review (/worklog, cross-project approval). Three-state heartbeat lifecycle: pending → synced → approved."
allowed-tools: Bash Grep Read AskUserQuestion mcp__claude_ai_Google_Calendar__list_events mcp__claude_ai_Google_Calendar__create_event mcp__claude_ai_Google_Calendar__update_event mcp__claude_ai_Google_Calendar__list_calendars
---

# worklog

Auto-log working time to Google Calendar. **Three modes: collect (automatic), sync (dedicated session), and review (user-invoked).**

## Design principles

> **Collection is invisible.** The `UserPromptSubmit` hook writes heartbeats to SQLite on every user message. It's a local file write — no MCP calls, no chat output, no interruption. Work sessions never know it's happening.
>
> **Sync is explicit.** Calendar writes happen only when the user explicitly runs `/worklog sync` — typically in a dedicated session whose sole purpose is time-tracking. This session is allowed to be noisy because that's what it's for. Sync operates across ALL projects, not just the current one.
>
> **Review when ready.** `/worklog` (or `/worklog review`) is the user's single review surface across all projects. It shows everything synced since the last review. The user approves, corrects, or flags entries — on their schedule.
>
> **Heartbeats persist until approved.** Sync marks heartbeats as `synced` (written to calendar) but never purges them. Only after the user explicitly approves via review are they marked `approved` and eligible for purge.
>
> **10-minute granularity.** All timestamps round to the nearest 10-min boundary. Good enough for time allocation; not a timeclock.
>
> **One calendar, hardcoded.** All writes go to the Work Log calendar. No other calendar is ever touched.
>
> **Git history as ground truth for backpopulation.** On first use per project, reconstruct 30 days of work blocks from commit timestamps. Backpopulation is HITL.

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

The project code must be a single non-whitespace token before the colon — no spaces, no special characters.

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

**Override:** If the repo has a `.worklog-project` file at the root, its contents (trimmed) override the heuristic.

**First-use confirmation:** On `SessionStart`, if `.worklog-project` does not exist, the hook outputs a prompt telling the agent to confirm the heuristic-derived code with the user. On confirmation (or correction), the agent writes the code to `.worklog-project` so the user is never asked again.

### Short description

Derive from (in priority order):
1. **Branch name** — strip the owner prefix and ticket ID, humanize the slug (e.g. `vidbina/vid-676-auto-log-work-sessions` → `auto-log work sessions`)
2. **Last commit subject** — if no meaningful branch
3. **Project code alone** — as a last resort (e.g. `dotfiles: misc`)

Keep to ~3-6 words. This is for calendar glanceability, not precision.

## Heartbeat database

Activity is tracked via a SQLite database at `~/.claude/worklog.db`. A `UserPromptSubmit` hook appends a heartbeat on every user message — this captures thinking/reviewing/discussing time that git history misses.

### Schema

```sql
CREATE TABLE IF NOT EXISTS heartbeats (
  ts      TEXT    NOT NULL,
  project TEXT    NOT NULL,
  cwd     TEXT    NOT NULL,
  source  TEXT    DEFAULT 'claude-code',  -- which tool recorded this heartbeat
  flushed INTEGER DEFAULT 0              -- 0 = pending, 1 = synced, 2 = approved
);
CREATE INDEX IF NOT EXISTS idx_heartbeats_project_flushed ON heartbeats(project, flushed);
```

**Migration for existing databases:** The hook's `CREATE TABLE IF NOT EXISTS` handles new installs. For existing DBs, run once:

```sql
ALTER TABLE heartbeats ADD COLUMN source TEXT DEFAULT 'claude-code';
```

Existing rows get `source='claude-code'` (the default). This is backwards-compatible — queries that don't select `source` continue to work.

**Source values:**

| Value | Tool |
|---|---|
| `claude-code` | Claude Code (via `UserPromptSubmit` hook) |
| `opencode` | OpenCode (via `chat.message` plugin) |

**Three flushed states:**

| Value | State | Meaning |
|---|---|---|
| 0 | pending | Heartbeat recorded, not yet written to calendar |
| 1 | synced | Flushed to calendar by sync, awaiting user review |
| 2 | approved | User signed off via review, safe to purge |

### UserPromptSubmit hook (collect mode)

The hook appends one row per user message:

```bash
INPUT=$(cat)
DIR=$(echo "$INPUT" | jq -r '.cwd // empty')
PROJECT=$(cd "$DIR" 2>/dev/null && basename "$(git remote get-url origin 2>/dev/null || echo "$DIR")" .git | cut -d- -f1 | tr '[:upper:]' '[:lower:]')
[ -n "$PROJECT" ] && sqlite3 ~/.claude/worklog.db "
  CREATE TABLE IF NOT EXISTS heartbeats (ts TEXT NOT NULL, project TEXT NOT NULL, cwd TEXT NOT NULL, source TEXT DEFAULT 'claude-code', flushed INTEGER DEFAULT 0);
  INSERT INTO heartbeats(ts, project, cwd, source) VALUES(
    strftime('%Y-%m-%dT%H:%M:%S', 'now', 'localtime'),
    '$PROJECT',
    '$DIR',
    'claude-code'
  );
"
```

**Note:** Claude Code passes session context (including `cwd`) via JSON on stdin. The hook reads stdin first.

The `CREATE TABLE IF NOT EXISTS` makes the hook idempotent — first run creates the table, subsequent runs just insert.

### Lifecycle

1. **Hook writes heartbeats** — one per user prompt, with project code and timestamp (`flushed=0`)
2. **User runs `/worklog sync`** — reads pending heartbeats, clusters into work blocks, writes to calendar
3. **Sync marks heartbeats as synced** — `UPDATE heartbeats SET flushed = 1 WHERE ...`
4. **User runs `/worklog review`** — reviews all `flushed=1` rows across all projects
5. **User approves** — `UPDATE heartbeats SET flushed = 2 WHERE ...`
6. **Periodic purge** — `DELETE FROM heartbeats WHERE flushed = 2 AND ts < datetime('now', '-30 days')`

## Sync mode (`/worklog sync`)

Flushes pending heartbeats across **all projects** to the Work Log calendar. This is the only mode that writes to Google Calendar.

**Intended usage:** Run in a dedicated Claude session whose sole purpose is time-tracking. The user spins up a "worklog" session, runs `/worklog sync` (or sets up a `/loop` to repeat it), and leaves it running. The sync session is allowed to produce visible output — tool calls, status messages — because the user has opted into that by opening the session.

### 1. Read all unflushed heartbeats

```sql
SELECT project, ts, cwd FROM heartbeats
WHERE flushed = 0
ORDER BY project, ts;
```

If no unflushed heartbeats exist, report "Nothing to sync — no pending heartbeats." and exit.

### 2. Group by project

Cluster heartbeats by project. For each project:

### 3. Cluster into work blocks

Group heartbeats into continuous work blocks using **adaptive merging**. Instead of a fixed merge window, the gap threshold grows with the size of the current cluster:

1. Sort heartbeats chronologically.
2. Walk the sorted list. For each consecutive pair, compute the gap.
3. Compute the **merge threshold** from the current cluster's heartbeat count (`N`): `min(20, 8 + N)` minutes. More heartbeats in the cluster = more confidence that work is ongoing = wider tolerance for the next gap.
   - N = 1 → 9 min threshold (new cluster, moderate window)
   - N = 2–3 → 10–11 min (establishing activity)
   - N = 5–8 → 13–16 min (focused seated work)
   - N ≥ 12 → capped at 20 min
4. If the gap exceeds the threshold, start a new block. Reset N to 1.

**Worked example** (real data from a mixed seated/road session):

```
Heartbeats: 14:34, 14:38, 14:45, 14:50, 14:58, 15:06, 15:14,
            [29 min gap],
            15:43, 15:55, 16:07,
            [29 min gap],
            16:36, 16:43,
            [87 min gap],
            18:10, 18:15, 18:22, 18:30, 18:42, 18:52

Walk-through:
  14:34        → N=1, start block A
  14:38 (+4m)  → threshold=min(20,8+1)=9, 4<9   → merge. N=2
  14:45 (+7m)  → threshold=min(20,8+2)=10, 7<10  → merge. N=3
  14:50 (+5m)  → threshold=min(20,8+3)=11, 5<11  → merge. N=4
  14:58 (+8m)  → threshold=min(20,8+4)=12, 8<12  → merge. N=5
  15:06 (+8m)  → threshold=min(20,8+5)=13, 8<13  → merge. N=6
  15:14 (+8m)  → threshold=min(20,8+6)=14, 8<14  → merge. N=7
  15:43 (+29m) → threshold=min(20,8+7)=15, 29>15 → SPLIT. Block A = 14:34–15:14
  15:43        → N=1, start block B
  15:55 (+12m) → threshold=9, 12>9               → SPLIT. Block B = 15:43–15:43
  15:55        → N=1, start block C
  16:07 (+12m) → threshold=9, 12>9               → SPLIT. Block C = 15:55–15:55
  16:07        → N=1, start block D
  16:36 (+29m) → threshold=9, 29>9               → SPLIT. Block D = 16:07–16:07
  16:36        → N=1, start block E
  16:43 (+7m)  → threshold=9, 7<9                → merge. N=2
  18:10 (+87m) → threshold=10, 87>10             → SPLIT. Block E = 16:36–16:43
  18:10        → N=1, start block F
  18:15 (+5m)  → threshold=9, 5<9                → merge. N=2
  18:22 (+7m)  → threshold=10, 7<10              → merge. N=3
  18:30 (+8m)  → threshold=11, 8<11              → merge. N=4
  18:42 (+12m) → threshold=12, 12≤12             → merge. N=5
  18:52 (+10m) → threshold=13, 10<13             → merge. N=6

Result: 6 blocks
  A: 14:30–15:20 (dense seated work — 7 heartbeats)
  B: 15:40–15:50 (single road check-in)
  C: 15:50–16:10 (single road check-in)
  D: 16:00–16:20 (single road check-in)
  E: 16:30–16:50 (brief check — 2 heartbeats)
  F: 18:10–19:00 (evening session — 6 heartbeats)
```

The dense seated block (A) stays merged because the threshold grows with each heartbeat. The sparse road check-ins (B, C, D) split correctly because the 12-min gaps exceed the threshold for small clusters. The evening session (F) merges correctly as heartbeats accumulate.

**Key insight:** the algorithm is self-correcting. Each merged heartbeat grows N, raising the threshold for the next gap. Each split resets N to 1, tightening the threshold. This means dense work earns progressively more tolerance, while sporadic check-ins stay conservative.

Each block's:
- `startTime` = first heartbeat in the cluster (rounded down to 10-min)
- `endTime` = last heartbeat in the cluster + 10 min (rounded up to 10-min)

### 4. Find or create calendar entries

For each work block, search for today's entries on the Work Log calendar matching this project:

```
list_events(
  calendarId: WORKLOG_CALENDAR_ID,
  startTime: block date 00:00,
  endTime: block date 23:59,
  fullText: "{project}:"
)
```

**If a matching entry exists and its `endTime` is within 20 min of the block's start:** extend it by updating `endTime` to the block's `endTime`.

**If a matching entry exists but its `endTime` is >20 min before the block's start:** this is a previous work block. Create a new entry.

**If no matching entry exists:** create a new entry:
- `summary`: `{project}: {short description}`
- `startTime`: block start
- `endTime`: block end
- `calendarId`: the Work Log calendar ID

For the short description, derive it from the `cwd` field of the heartbeats — look up the branch name via `git -C {cwd} branch --show-current` for each unique cwd, then apply the short description heuristic.

### 5. Mark heartbeats as synced

After successfully writing to the calendar:

```sql
UPDATE heartbeats SET flushed = 1
WHERE project = '{project}' AND flushed = 0 AND ts <= '{latest_ts}';
```

### 6. Purge approved data

Clean up approved heartbeats older than 30 days:

```sql
DELETE FROM heartbeats WHERE flushed = 2 AND ts < datetime('now', '-30 days');
```

### 7. Report

After processing all projects, print a summary. When heartbeats come from multiple sources, show the breakdown:

```
Synced {N} projects:
  dotfiles: 1 entry extended (07:30–08:20) [claude-code: 8, opencode: 4]
  yo: 1 new entry (09:10–10:40) [opencode: 6]
  kb: nothing to sync

Next sync: run `/worklog sync` again or set up `/loop 31m /worklog sync`
```

### Running sync on a loop

In a dedicated worklog session, the user can set up continuous syncing:

```
/loop 31m /worklog sync
```

This fires every 31 minutes, flushing all projects. The loop runs in the dedicated session — work sessions are never interrupted. The 31-min interval guarantees crossing at least one 30-min threshold between fires.

### Round to 10-min boundaries

All times round to the nearest 10 minutes:
- 14:03 → 14:00
- 14:07 → 14:10
- 14:25 → 14:30

## Review mode (`/worklog` or `/worklog review`)

User-invoked. Shows all synced-but-unreviewed heartbeats across **all projects**.

### 1. Read synced heartbeats

```sql
SELECT project, source, MIN(ts) as first, MAX(ts) as last, COUNT(*) as beats
FROM heartbeats
WHERE flushed = 1
GROUP BY project, source
ORDER BY MAX(ts) DESC;
```

If no synced heartbeats exist, report "Nothing to review — all entries approved." and exit.

### 2. Present the review

```
Worklog review — {N} projects with unreviewed entries

| # | Project  | Source      | First activity | Last activity | Heartbeats | Calendar entries |
|---|----------|-------------|----------------|---------------|------------|------------------|
| 1 | dotfiles | claude-code | Jun 07 05:43   | Jun 07 06:10  | 12         | 1 (extended)     |
| 2 | dotfiles | opencode    | Jun 07 06:15   | Jun 07 06:30  | 4          | 1 (extended)     |
| 3 | ivos     | claude-code | Jun 07 05:50   | Jun 07 05:55  | 2          | 1 (new)          |
| 4 | yo       | opencode    | Jun 06 14:20   | Jun 06 16:30  | 8          | 2 (new)          |

Approve all? yes / project {name} / cancel
```

Cross-reference with the Work Log calendar to show whether entries were created or extended.

### 3. Process approval

- **yes** — approve all: `UPDATE heartbeats SET flushed = 2 WHERE flushed = 1;`
- **project {name}** — approve one project: `UPDATE heartbeats SET flushed = 2 WHERE flushed = 1 AND project = '{name}';`
- **cancel** — exit without approving. Entries stay as `flushed=1`.

### 4. Handle pending heartbeats during review

If there are also `flushed=0` (pending, not yet synced to calendar) heartbeats, flush them to the calendar first, then include them in the review. This handles the case where the user runs `/worklog` before syncing.

## Backpopulation (first use per project)

On the first sync for a project (determined by zero matching entries in the Work Log calendar for that project in the last 30 days), reconstruct historical work blocks from git history.

Backpopulated entries use git commit timestamps only — they miss thinking/reviewing/discussing time between commits. The `[backfill]` postfix makes this quality distinction visible.

### Algorithm

```bash
# Get all commits by the user in the last 30 days, with timestamps and subjects
git log --format="%aI|%s" --author="David" --since="30 days ago" --all
```

**Clustering:** Group commits into work blocks using the same adaptive merging algorithm as sync mode (see Section 3). Commits are sparser than heartbeats, so the threshold naturally stays tighter — a few scattered commits won't inflate into a mega-block. Each block's:
- `startTime` = first commit in the cluster (rounded down to 10-min)
- `endTime` = last commit in the cluster + 10 min (rounded up to 10-min)
- `summary` = `{project}: {scope} [backfill]` — scope derived from the most common conventional commit scope in the cluster. Always postfixed with `[backfill]`.

**Guard:** Only backpopulate if the Work Log calendar has zero entries for this project in the 30-day window.

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

2. Wait for explicit approval via `AskUserQuestion`.

**Cap at 100 entries.** If git history produces more than 100 work blocks, present only the 100 most recent and note how many were truncated.

After backpopulation, post a one-line summary: "Backpopulated {N} work blocks for {project} from git history (last 30 days)."

## SessionStart hook

The `SessionStart` hook handles project detection only. It does NOT bootstrap any cron or sync — that's the dedicated session's job.

If `.worklog-project` exists: output a one-line acknowledgment ("Worklog: project '{project}' — heartbeats collecting.").

If `.worklog-project` does not exist: output a prompt telling the agent to confirm the heuristic-derived code with the user and write it to `.worklog-project`.

## Anti-patterns

- **Don't write to any calendar other than Work Log.** The calendar ID is hardcoded. Period.
- **Don't sync in work sessions.** Sync belongs in a dedicated session. Work sessions only collect heartbeats.
- **Don't create duplicate entries.** Always search before creating. Extend existing entries within the 30-min window.
- **Don't backpopulate twice.** Check for existing entries before backpopulating.
- **Don't over-describe.** Entry summaries are 3-6 words. Calendar glanceability matters more than precision.
- **Don't purge synced heartbeats.** Only `flushed=2` (approved) rows older than 30 days are purged. Never delete `flushed=1` rows.
- **Don't round-trip on errors.** If a Calendar MCP call fails, log it once and move on. Don't retry in a loop.
