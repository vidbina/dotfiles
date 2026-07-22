#!/usr/bin/env python3
"""Create dispatch managed agent sessions and build the run file.

Owns the fragile parts of session creation that shell arrays got wrong: pairing
created session IDs with their topics by index, and assembling a clean run-file
JSON. zsh arrays are 1-indexed, so a `for i in $(seq 0 …)` loop over a
`+=`-built array silently blanked the first entry and dropped the last (see
DISPATCH.md Error 1). This script owns the loop in Python, guaranteeing a
one-to-one, order-preserving mapping with an explicit length guard.

The assembly functions here are pure and CLI-free so they can be unit-tested
without the `ant` CLI. The subprocess wrapper and the `create` command (with the
credential preflight) build on top of them.

Designed for agentic use: structured JSON on stdout, diagnostics on stderr,
meaningful exit codes.
"""

# Exit codes
EXIT_OK = 0
EXIT_BAD_CREDENTIAL = 1  # preflight create failed — credential likely expired
EXIT_BAD_INPUT = 2       # invalid input (length mismatch, missing config)

CONSOLE_URL_BASE = "https://console.anthropic.com"


def build_session_url(workspace_id, session_id):
    """Construct the console URL for a session.

    Mirrors the URL the gather phase reads from the run file's `url` field, so
    every output surface points at the same place.
    """
    return (
        f"{CONSOLE_URL_BASE}/workspaces/{workspace_id}/sessions/{session_id}"
    )


def build_session(topic, session_id, index, workspace_id):
    """Build a single run-file session entry.

    The schema matches what the gather phase expects to read and update:
    creation-time fields are populated; the rest are null placeholders filled in
    later. `index` is 1-based (the display position), kept for callers that want
    it but not stored — the position is implied by list order.
    """
    return {
        "id": session_id,
        "topic": topic,
        "url": build_session_url(workspace_id, session_id),
        "status": None,
        "created_at": None,
        "updated_at": None,
        "output_tokens": None,
        "result": None,
        "output_file": None,
    }


def assemble_sessions(created_ids, topics, workspace_id):
    """Pair created session IDs with their topics, in order.

    This is the drop-proof core: `created_ids[i]` corresponds to `topics[i]`
    for every i, with no off-by-one and no silent truncation. A length mismatch
    is a hard error rather than a silently blanked or dropped entry — the exact
    failure mode DISPATCH.md Error 1 documents.
    """
    if len(created_ids) != len(topics):
        raise ValueError(
            f"session/topic count mismatch: {len(created_ids)} sessions "
            f"but {len(topics)} topics — refusing to build a run file with "
            f"blank or dropped entries"
        )
    return [
        build_session(topic, session_id, index, workspace_id)
        for index, (session_id, topic) in enumerate(
            zip(created_ids, topics), start=1
        )
    ]


def build_run_file(
    run_id,
    agent_id,
    environment_id,
    workspace_id,
    context,
    sessions,
    created_at,
):
    """Assemble the top-level run-file dict.

    `created_at` and `run_id` are passed in (not stamped here) so the function
    stays pure and deterministic for tests; the `create` command generates them
    from the wall clock.
    """
    return {
        "id": run_id,
        "created_at": created_at,
        "agent_id": agent_id,
        "environment_id": environment_id,
        "workspace_id": workspace_id,
        "context": context,
        "sessions": sessions,
    }
