# Dispatch skill — context log

Errors and friction encountered during dispatch runs. Use this log to identify
patterns worth extracting into reusable shell scripts.

## 2026-06-30 — ivos-trades broker model research (10 sessions)

### Error 1: zsh array indexing off-by-one

**What happened:** Built `SESSION_IDS` array in a bash heredoc-style loop using
`for i in $(seq 1 10)` (appending with `+=`), then iterated with
`for i in $(seq 0 9)` to build the run file JSON. In zsh, arrays are
1-indexed, so `SESSION_IDS[0]` and `TOPICS[0]` resolved to empty strings. The
first JSON entry had empty `topic` and `id` fields, and the 10th session
(Pepperstone) was dropped entirely.

**Fix applied:** `jq` post-hoc to remove the empty entry and append the missing
session.

**Script opportunity:** A `dispatch-create-sessions.sh` script that takes a
count N, creates N sessions via `ant beta:sessions create`, and outputs a clean
JSON array of `{id, index}` pairs. No shell array indexing needed — the script
owns the loop and guarantees correct output. Something like:

```bash
#!/usr/bin/env bash
# dispatch-create-sessions.sh <agent_id> <env_id> <count>
# Outputs: JSON array of session IDs
AGENT_ID="$1"; ENV_ID="$2"; COUNT="$3"
echo "["
for i in $(seq 1 "$COUNT"); do
  SID=$(ant beta:sessions create --agent "$AGENT_ID" --environment-id "$ENV_ID" 2>/dev/null | jq -r '.id')
  [ "$i" -lt "$COUNT" ] && COMMA="," || COMMA=""
  echo "  \"$SID\"$COMMA"
done
echo "]"
```

### Error 2: workspace_id not discoverable from `ant` CLI

**What happened:** `ant beta:sessions create` output does not include
`workspace_id`. Tried `ant config`, `ant whoami`, `ant beta:workspaces list` —
none returned it. Had to find an existing run file in another repo
(`kb/dispatch-runs/`) to extract the workspace ID
(`wrkspc_01NWo31ZpJLwkJeAcTeyaQxm`).

**Fix applied:** Manual lookup from prior run file.

**Script opportunity:** A `dispatch-config.sh` that reads/writes a
`~/.claude/dispatch-config.json` with validated resource IDs:

```json
{
  "agent_id": "agent_01EXAMPLE",
  "environment_id": "env_01EXAMPLE",
  "workspace_id": "wrkspc_01EXAMPLE"
}
```

The skill reads this config instead of rediscovering IDs every run. First-run
wizard populates it interactively.

### Error 3: prompt file creation with shell quoting

**What happened:** First attempt used a bash `for` loop with `IFS='|'` parsing
and heredocs containing single quotes (apostrophes in "eToro's", "NAGA's",
etc.). zsh `bad substitution` error on array expansion `${platforms[$i]}`.

**Fix applied:** Rewrote as 10 sequential `cat + printf` commands with manual
quote escaping (`'"'"'`).

**Script opportunity:** A `dispatch-send-prompt.sh` that reads a prompt from a
file and sends it to a session, handling all JSON escaping via `jq`:

```bash
#!/usr/bin/env bash
# dispatch-send-prompt.sh <session_id> <prompt_file>
SID="$1"; PROMPT_FILE="$2"
PROMPT_JSON=$(jq -Rs . < "$PROMPT_FILE")
EVENT="{\"type\": \"user.message\", \"content\": [{\"type\": \"text\", \"text\": ${PROMPT_JSON}}]}"
ant beta:sessions:events send --session-id "$SID" --event "$EVENT"
```

### Error 4: multiple CLI probes to find resource IDs

**What happened:** Spent 5 tool calls probing `ant beta:agents list`,
`ant beta:environments list`, `ant config`, `ant whoami`, and inspecting
existing run files — all to assemble the three IDs (agent, environment,
workspace) needed before any session can be created.

**Script opportunity:** Consolidate into `dispatch-preflight.sh` that:
1. Reads `~/.claude/dispatch-config.json` if it exists
2. Falls back to `ant beta:agents list | jq` + `ant beta:environments list | jq`
3. Prompts for workspace_id if not cached
4. Writes the config for next time
5. Outputs the three IDs as JSON

### Summary of proposed scripts

| Script | Purpose | Eliminates |
|---|---|---|
| `dispatch-preflight.sh` | Resolve + cache agent/env/workspace IDs | Errors 2, 4 |
| `dispatch-create-sessions.sh` | Create N sessions, output clean JSON array | Error 1 |
| `dispatch-send-prompt.sh` | Send a prompt file to a session with proper escaping | Error 3 |
| `dispatch-gather.sh` | Poll session statuses, extract results | (not yet encountered, but predictable) |
| `dispatch-run-file.sh` | Build/update run file JSON from session array + topics | Error 1 |
