---
name: dispatch
description: "Use this skill to dispatch N parallel managed agent sessions (fire-and-forget), then gather results after they complete. For local dev workstations only — laptops where sessions are interruptible (lid close, Wi-Fi drop). Not useful for cloud runners or CI which have persistent infrastructure. The skill owns infrastructure only — session creation, ID persistence, polling, result extraction, and reporting. The caller owns prompt content, output shape, and merge logic. Trigger for prompts like 'dispatch research on X', 'run these topics in parallel', 'fire off N sessions', 'gather results', 'check on my dispatch run', 'dispatch these to the cloud', or when the user invokes `/dispatch`. Also trigger when another skill needs to fan out work to managed agents."
api_description: "Dispatch N parallel managed agent sessions via the ant CLI, persist session IDs to disk, poll for completion, extract results, and report with runtime and token metrics. For local dev workstations — fire-and-forget pattern that survives laptop sleep and connectivity loss."
allowed-tools: Bash Glob Grep Read Write AskUserQuestion Skill mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__save_comment mcp__claude_ai_Linear__list_comments
---

# dispatch

Dispatch N parallel managed agent sessions and gather results. **Fire-and-forget — the laptop can close between dispatch and gather.**

## When to use this skill

This skill exists for **local dev workstations** — specifically laptops running Claude Code interactively, where the session is inherently interruptible (lid close, Wi-Fi drop, context switch to another task). The dispatch pattern offloads long-running parallel work to server-side managed agent sessions that keep running regardless of client state.

**Not useful for:** cloud-based agent runners, CI pipelines, Linear AI, or any environment with persistent infrastructure. Those systems don't sleep, don't lose connectivity, and don't need a fire-and-forget recovery mechanism. Including this skill in a cloud runner's tool context adds noise without benefit — the runner can simply spawn subagents directly and wait.

The defining design principles:

> **Infrastructure only.** The skill handles session lifecycle (create, persist, poll, extract, report). It does NOT own prompt content, output shape, or what to do with results. The caller decides all of that.
>
> **Survive laptop close.** Session IDs are persisted to disk before the skill reports "dispatched." Server-side sessions keep running regardless of client state. A new Claude Code session can pick up where the old one left off.
>
> **Auto-detect phase.** If no run file exists for the given context, dispatch. If a run file exists with incomplete sessions, gather. If all sessions are complete, report. No explicit `--phase` flag needed.
>
> **Observable.** Every gather reports runtime, token usage, and session links in a table. This data helps decide whether workloads can run locally instead.

## Prerequisites

- `ant` CLI installed and configured (`/opt/homebrew/bin/ant`)
- `jq` installed (for NDJSON extraction)
- Anthropic API key accessible (used by `ant` under the hood)

### Resource config

Dispatch needs three Anthropic resource IDs: agent, environment, and workspace. These are resolved in this order (first match wins):

1. **Caller override:** The caller passes `--agent`, `--environment`, or `--workspace` flags explicitly.
2. **Project-local config:** `.dispatch-config` in the git root — a JSON file. Gitignored. Use when a project has a dedicated agent/environment.
3. **Global default:** `~/.claude/dispatch.json` — same schema. Set once, applies everywhere without a local override.
4. **First-use prompt:** If no config exists, prompt the user for agent ID, environment ID, and workspace ID. Write to `~/.claude/dispatch.json`.

```json
{
  "agent_id": "agent_01EXAMPLE",
  "environment_id": "env_01EXAMPLE",
  "workspace_id": "wrkspc_01EXAMPLE"
}
```

## Run file format

All state is persisted to a JSON file in the `.dispatch-runs/` directory at the repo root:

```json
{
  "id": "20260605-085200",
  "created_at": "2026-06-05T08:52:00Z",
  "agent_id": "agent_01EXAMPLE",
  "environment_id": "env_01EXAMPLE",
  "workspace_id": "wrkspc_01EXAMPLE",
  "context": "PROJ-42 market landscape research",
  "sessions": [
    {
      "id": "sesn_01EXAMPLE",
      "topic": "Enterprise search landscape",
      "url": "https://console.anthropic.com/workspaces/wrkspc_01EXAMPLE/sessions/sesn_01EXAMPLE",
      "status": null,
      "created_at": null,
      "updated_at": null,
      "output_tokens": null,
      "result": null,
      "output_file": null
    }
  ]
}
```

The `.dispatch-runs/` directory should be gitignored (add to `.gitignore` if not present). Run files are local state, not version-controlled.

## Phase 0 — Detect mode

### Parse invocation

Examine the input to determine what phase to enter:

1. **Explicit run file** — if the user passes a path to an existing run file (`.dispatch-runs/20260605-085200.json`), load it and enter **gather** phase.
2. **Existing run in context** — if `.dispatch-runs/` contains a run file with sessions still in `running` or `null` status, ask: "Found an in-progress run ({id}, {N} sessions). Gather results, or start a new dispatch?"
3. **New dispatch** — if neither applies, enter **dispatch** phase. The user's prompt contains the topics/prompts to dispatch.

### Rename the session

Rename the session to reflect the scope — e.g. "VID-669 dispatch 8 topics (dispatch)" or "gather PROJ-42 results (dispatch)". If no programmatic rename is available, suggest it to the user and move on.

## Phase 1 — Dispatch

### Collect prompts

The caller provides N prompts. These can come from:

- **Inline in the user's message** — a bulleted list, numbered list, or prose describing N topics
- **A Linear ticket** — if a ticket ID is provided, read the description for a structured list of topics
- **A file** — if a file path is provided, read topics from it

For each item, the skill needs:
- **Topic label** — short identifier for the table (e.g. "Enterprise search")
- **Full prompt** — the complete text to send to the managed agent session

If the user provides only topic labels without full prompts, draft prompts for each and present them for approval before dispatching.

### Confirm

Print the dispatch plan:

```
Dispatch plan: {N} sessions

Agent:       agent_01EXAMPLE (Coding Assistant, opus-4-7)
Environment: env_01EXAMPLE (research-env)

| # | Topic | Prompt preview (first 80 chars) |
|---|-------|----------------------------------|
| 1 | Enterprise search | "Survey the enterprise search landscape..." |
| 2 | Graph databases | "Compare graph database options for..." |
...

Dispatch all {N}? yes / edit / cancel
```

Use `AskUserQuestion` for confirmation.

### Create sessions and send prompts

For each confirmed topic:

1. **Create session:**
   ```bash
   ant beta:sessions create \
     --agent "$AGENT_ID" \
     --environment-id "$ENV_ID"
   ```
   Parse the session ID from the JSON output.

2. **Send prompt:**
   Write the prompt to a temp file to avoid shell escaping issues, then:
   ```bash
   ant beta:sessions:events send \
     --session-id "$SID" \
     --event "{\"type\": \"user.message\", \"content\": [{\"type\": \"text\", \"text\": $(jq -Rs . < /tmp/prompt-N.txt)}]}"
   ```

3. **Build session URL:** Construct `https://console.anthropic.com/workspaces/{workspace_id}/sessions/{session_id}` and store it in the session's `url` field. This happens at creation time so the URL is persisted from the start — not deferred to gather.

4. **Record** the session ID, URL, and topic in the run file.

Dispatch sessions in parallel where possible (background with `&`). But always write the run file to disk **before** reporting success — if the session dies mid-dispatch, the persisted IDs let us recover.

### Report

After all sessions are dispatched:

```
Dispatched {N} sessions. Run file: .dispatch-runs/{id}.json

You can close the laptop now. Sessions run server-side.
To check progress later: /dispatch .dispatch-runs/{id}.json
```

## Phase 2 — Gather

### Poll sessions

For each session in the run file:

```bash
ant beta:sessions retrieve --session-id "$SID" 2>/dev/null | jq -r '{status, created_at, updated_at, usage}'
```

Statuses:
- `idle` — done, results available
- `running` — still working
- `rescheduling` — transient error, auto-retrying
- `terminated` — failed permanently

If any sessions are still `running` or `rescheduling`, report progress and offer to poll again:

```
Progress: {done}/{total} sessions complete ({running} still running)

| # | Topic | Status | Runtime |
|---|-------|--------|---------|
| 1 | Enterprise search | idle | 3m14s |
| 2 | Graph databases | running | 2m31s... |
...

Poll again in 60s? yes / cancel
```

If the user says yes, sleep 60s and re-poll. If all sessions are complete, proceed to extraction.

### Extract results

For each completed (`idle`) session:

```bash
ant beta:sessions:events list --session-id "$SID" 2>/dev/null | \
  jq -s '[.[] | select(.type == "agent.message") | .content[] | select(.type == "text") | .text] | join("\n")' -r
```

Store the extracted text in the run file's `result` field for each session.

For `terminated` sessions, log the failure and extract any partial output if available.

### Build session URLs

**Mandatory step — do not skip.** For each session, construct the console URL and store it in the session's `url` field in the run file:

```
URL = https://console.anthropic.com/workspaces/{workspace_id}/sessions/{session_id}
```

where `workspace_id` comes from the run file's top-level `workspace_id` field and `session_id` is the session's `id` field. Construct this URL immediately after polling each session — before writing any output (table, file, comment, or run file update). The `url` field is the single source of truth; every output surface reads from it. Never reconstruct the URL ad-hoc and never display a bare session ID.

**Every rendered session reference must be a markdown hyperlink:** `[sesn_01Aa...](url)` — truncate the display ID to the first 12 characters for readability. This applies to:

- The in-chat completion table
- Local markdown output files
- Linear comments (completion table, threaded results, synthesis)
- Any follow-up action that references a session

If the `workspace_id` is missing from the run file, **stop and ask the user** rather than falling back to a bare ID.

### Write output files

**Mandatory step — do not skip.** For each session with a non-null `result`, write the full output to a local markdown file so it survives provider session retention windows.

**Directory:** `.dispatch-runs/{run-id}/` (create if it doesn't exist).

**Filename:** `{N}-{topic-slug}.md` where `{N}` is the 1-based session index (zero-padded to 2 digits) and `{topic-slug}` is the topic label lowercased with spaces replaced by hyphens and non-alphanumeric characters (except hyphens) removed. Example: `01-enterprise-search.md`.

**File format:**

```markdown
---
session_id: sesn_01AaEXAMPLE
session_url: https://console.anthropic.com/workspaces/wrkspc_.../sessions/sesn_01Aa...
topic: Enterprise search landscape
status: idle
runtime: 3m14s
output_tokens: 3803
extracted_at: 2026-06-05T09:15:00Z
---

{full extracted result text}
```

Store the relative file path (`.dispatch-runs/{run-id}/01-enterprise-search.md`) in the session's `output_file` field in the run file. This creates a cross-reference: the run file points to the output files, and each output file's frontmatter points back to the session.

For `terminated` sessions with no result, skip file creation and leave `output_file` as `null`.

### Update run file

Write back all gathered data (status, timestamps, usage, URLs, results, output file paths) to the run file.

### Report

Print the completion table:

```
Dispatch run {id} — {done}/{total} complete

| # | Topic | Status | Runtime | Tokens (out) | Session |
|---|-------|--------|---------|--------------|---------|
| 1 | Enterprise search | idle | 3m14s | 3,803 | [sesn_01Aa...](https://console.anthropic.com/workspaces/wrkspc_01EXAMPLE/sessions/sesn_01AaEXAMPLE) |
| 2 | Graph databases | idle | 2m58s | 3,540 | [sesn_01Bb...](https://console.anthropic.com/workspaces/wrkspc_01EXAMPLE/sessions/sesn_01BbEXAMPLE) |
| 3 | Auth patterns | terminated | 1m02s | 0 | [sesn_01Cc...](https://console.anthropic.com/workspaces/wrkspc_01EXAMPLE/sessions/sesn_01CcEXAMPLE) |

Output files: .dispatch-runs/{id}/

Totals: 3m14s avg runtime | 11,146 output tokens | $X.XX estimated cost
```

**The Session column must always contain a markdown hyperlink.** Use the `url` field from the run file — never a bare session ID. Truncate the display label to `sesn_01Aa...` (first 12 characters) for readability.

Runtime is computed as `updated_at - created_at` from the session metadata.

### Handoff to caller

After reporting, the skill's job is done. The results are in the run file and printed in chat. The caller decides what to do next:

- Read specific results: "show me the result for topic 3"
- Synthesize: "summarize the findings across all topics"
- Post to Linear: "post these as threaded comments on VID-XXX"
- Retry failures: "re-dispatch the terminated sessions"
- Start a new wave: "dispatch 5 more topics based on these findings"

The skill responds to these follow-ups using the loaded run file context.

## Phase 3 — Follow-up actions

These are optional actions the caller can request after gather completes.

### Show result

Print the full extracted text for a specific session by number or topic label.

### Retry terminated sessions

For each `terminated` session, create a new session with the same prompt. Update the run file with the new session ID (keep the old one as `previous_id` for audit trail).

### Post to Linear

If the caller asks to post results to a Linear ticket:

1. Post an anchor comment with the completion table (via comment skill)
2. Post one threaded reply per completed session with the full result text
3. Optionally post a synthesis reply if the caller provides synthesis instructions

## Anti-patterns

- **Don't own prompt content.** The skill dispatches whatever the caller provides. It doesn't draft, improve, or constrain prompts (unless the caller asks for help drafting).
- **Don't own merge logic.** Synthesis, aggregation, and posting are the caller's responsibility. The skill extracts raw results and presents them.
- **Don't dispatch without confirmation.** Always show the plan and get explicit approval. Managed agent sessions cost money.
- **Don't lose session IDs.** The run file MUST be written to disk before reporting "dispatched." This is the crash-recovery mechanism.
- **Don't block on polling.** If sessions are still running, report progress and offer to poll again. Don't loop silently.
- **Don't hardcode agent/environment/workspace IDs in the skill file.** Read from config (project-local → global → first-use prompt). Accept caller overrides.
- **Don't assume the computer stays open.** The entire design assumes the laptop may close between dispatch and gather. Never rely on in-memory state surviving between phases.
- **Don't skip the report table.** Every gather must print the completion table with runtime, token counts, and session links. This data informs future decisions about local vs cloud execution.
- **Don't output bare session IDs.** Every session reference in every output surface (chat, file, comment) must be a markdown hyperlink built from the `url` field. A bare `sesn_01X3...` without a link is a bug — the reader cannot navigate to the session without manually constructing the URL, and the ID alone is useless once the session expires.
- **Don't skip output file extraction.** Every gather must write output files to `.dispatch-runs/{id}/`. The run file JSON alone is not archival — session results must exist as standalone markdown files that survive independent of the run file and the provider's retention window.
