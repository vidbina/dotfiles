---
name: dispatch
description: "Use this skill to dispatch N parallel managed agent sessions (fire-and-forget), then gather results after they complete. The skill owns infrastructure only — session creation, ID persistence, polling, result extraction, and reporting. The caller owns prompt content, output shape, and merge logic. Trigger for prompts like 'dispatch research on X', 'run these topics in parallel', 'fire off N sessions', 'gather results', 'check on my dispatch run', 'dispatch these to the cloud', or when the user invokes `/dispatch`. Also trigger when another skill needs to fan out work to managed agents."
api_description: "Dispatch N parallel managed agent sessions via the ant CLI, persist session IDs to disk, poll for completion, extract results, and report with runtime and token metrics. Fire-and-forget pattern — laptop can close between dispatch and gather."
allowed-tools: Bash Glob Grep Read Write AskUserQuestion Skill mcp__claude_ai_Linear__get_issue mcp__claude_ai_Linear__save_comment mcp__claude_ai_Linear__list_comments
---

# dispatch

Dispatch N parallel managed agent sessions and gather results. **Fire-and-forget — the laptop can close between dispatch and gather.**

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

### Validated resources

| Resource | ID | Notes |
|---|---|---|
| Agent | `agent_01QfZL1GJS9KnSiG2RDDMsMH` | "Coding Assistant", claude-opus-4-7, full sandbox |
| Environment | `env_01C7wJ2yBNzNuZG4dmVDKPnm` | "research-env", org-scoped, unrestricted networking |
| Workspace | `wrkspc_01NWo31ZpJLwkJeAcTeyaQxm` | For constructing console URLs |

These are defaults. The caller can override agent and environment IDs.

## Run file format

All state is persisted to a JSON file in the `dispatch-runs/` directory at the repo root:

```json
{
  "id": "20260605-085200",
  "created_at": "2026-06-05T08:52:00Z",
  "agent_id": "agent_01QfZL1GJS9KnSiG2RDDMsMH",
  "environment_id": "env_01C7wJ2yBNzNuZG4dmVDKPnm",
  "workspace_id": "wrkspc_01NWo31ZpJLwkJeAcTeyaQxm",
  "context": "IA-6 junction tooling research",
  "sessions": [
    {
      "id": "sesn_01Mk1X7c9WeTiPbUNwPf7MQk",
      "topic": "Enterprise search landscape",
      "status": null,
      "created_at": null,
      "updated_at": null,
      "output_tokens": null,
      "result": null
    }
  ]
}
```

The `dispatch-runs/` directory should be gitignored (add to `.gitignore` if not present). Run files are local state, not version-controlled.

## Phase 0 — Detect mode

### Parse invocation

Examine the input to determine what phase to enter:

1. **Explicit run file** — if the user passes a path to an existing run file (`dispatch-runs/20260605-085200.json`), load it and enter **gather** phase.
2. **Existing run in context** — if `dispatch-runs/` contains a run file with sessions still in `running` or `null` status, ask: "Found an in-progress run ({id}, {N} sessions). Gather results, or start a new dispatch?"
3. **New dispatch** — if neither applies, enter **dispatch** phase. The user's prompt contains the topics/prompts to dispatch.

### Rename the session

Rename the session to reflect the scope — e.g. "VID-669 dispatch 8 topics (dispatch)" or "gather IA-6 results (dispatch)". If no programmatic rename is available, suggest it to the user and move on.

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

Agent:       agent_01QfZL1GJS9KnSiG2RDDMsMH (Coding Assistant, opus-4-7)
Environment: env_01C7wJ2yBNzNuZG4dmVDKPnm (research-env)

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

3. **Record** the session ID and topic in the run file.

Dispatch sessions in parallel where possible (background with `&`). But always write the run file to disk **before** reporting success — if the session dies mid-dispatch, the persisted IDs let us recover.

### Report

After all sessions are dispatched:

```
Dispatched {N} sessions. Run file: dispatch-runs/{id}.json

You can close the laptop now. Sessions run server-side.
To check progress later: /dispatch dispatch-runs/{id}.json
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

### Update run file

Write back all gathered data (status, timestamps, usage, results) to the run file.

### Report

Print the completion table:

```
Dispatch run {id} — {done}/{total} complete

| # | Topic | Status | Runtime | Tokens (out) | Session |
|---|-------|--------|---------|--------------|---------|
| 1 | Enterprise search | idle | 3m14s | 3,803 | [sesn_01Mk...](https://console.anthropic.com/workspaces/wrkspc_01NWo31ZpJLwkJeAcTeyaQxm/sessions/sesn_01Mk1X7c9WeTiPbUNwPf7MQk) |
| 2 | Graph databases | idle | 2m58s | 3,540 | [sesn_01Mo...](https://console.anthropic.com/workspaces/wrkspc_01NWo31ZpJLwkJeAcTeyaQxm/sessions/sesn_01MoL1w7MzsfCNPD8NV2Yw3t) |
| 3 | Auth patterns | terminated | 1m02s | 0 | [sesn_01Bh...](https://console.anthropic.com/workspaces/wrkspc_01NWo31ZpJLwkJeAcTeyaQxm/sessions/sesn_01Bhmus6WTcoDEfn8Z2ma8ej) |

Totals: 3m14s avg runtime | 11,146 output tokens | $X.XX estimated cost
```

The console URL pattern is: `https://console.anthropic.com/workspaces/{workspace_id}/sessions/{session_id}`

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

1. Post an anchor comment with the completion table (via commenting skill)
2. Post one threaded reply per completed session with the full result text
3. Optionally post a synthesis reply if the caller provides synthesis instructions

## Anti-patterns

- **Don't own prompt content.** The skill dispatches whatever the caller provides. It doesn't draft, improve, or constrain prompts (unless the caller asks for help drafting).
- **Don't own merge logic.** Synthesis, aggregation, and posting are the caller's responsibility. The skill extracts raw results and presents them.
- **Don't dispatch without confirmation.** Always show the plan and get explicit approval. Managed agent sessions cost money.
- **Don't lose session IDs.** The run file MUST be written to disk before reporting "dispatched." This is the crash-recovery mechanism.
- **Don't block on polling.** If sessions are still running, report progress and offer to poll again. Don't loop silently.
- **Don't hardcode agent/environment.** Use the validated defaults but accept overrides from the caller.
- **Don't assume the computer stays open.** The entire design assumes the laptop may close between dispatch and gather. Never rely on in-memory state surviving between phases.
- **Don't skip the report table.** Every gather must print the completion table with runtime, token counts, and session links. This data informs future decisions about local vs cloud execution.
