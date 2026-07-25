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

import argparse
import json
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

# Exit codes
EXIT_OK = 0
EXIT_BAD_CREDENTIAL = 1  # preflight create failed — credential likely expired
EXIT_BAD_INPUT = 2       # invalid input (length mismatch, missing config)

CONSOLE_URL_BASE = "https://console.anthropic.com"

CREATE_TIMEOUT_SECONDS = 60


class SessionCreateError(Exception):
    """A single `ant beta:sessions create` call failed."""


class CredentialError(Exception):
    """The preflight (first) create failed — the credential is likely bad."""


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


def create_one_session(agent_id, environment_id):
    """Create a single session via the `ant` CLI and return its ID.

    Raises SessionCreateError if the CLI fails, times out, is missing, or emits
    output we can't parse an `id` out of.
    """
    try:
        proc = subprocess.run(
            [
                "ant", "beta:sessions", "create",
                "--agent", agent_id,
                "--environment-id", environment_id,
            ],
            capture_output=True,
            text=True,
            timeout=CREATE_TIMEOUT_SECONDS,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError) as exc:
        raise SessionCreateError(f"ant create failed: {exc}") from exc

    if proc.returncode != 0:
        raise SessionCreateError(
            f"ant create exited {proc.returncode}: "
            f"{proc.stderr.strip() or proc.stdout.strip()}"
        )

    try:
        session_id = json.loads(proc.stdout).get("id")
    except json.JSONDecodeError as exc:
        raise SessionCreateError(
            f"could not parse ant create output as JSON: {exc}"
        ) from exc

    if not session_id:
        raise SessionCreateError("ant create output had no `id` field")

    return session_id


def create_sessions(
    topics,
    agent_id,
    environment_id,
    workspace_id,
    creator=create_one_session,
):
    """Create one session per topic and assemble the run-file session list.

    The **first** create doubles as a credential preflight: if it fails, we
    raise CredentialError immediately and never attempt the rest of the batch —
    a bad credential is discovered after one call, not N. That first session is
    kept as session #1 (not thrown away). `creator` is injectable so the batch
    logic is testable without the `ant` CLI.
    """
    if not topics:
        return []

    created_ids = []

    # Preflight: the first create is the credential probe, reused as session #1.
    try:
        created_ids.append(creator(agent_id, environment_id))
    except SessionCreateError as exc:
        raise CredentialError(str(exc)) from exc

    # Batch: the credential is proven, create the rest.
    for _ in topics[1:]:
        created_ids.append(creator(agent_id, environment_id))

    return assemble_sessions(created_ids, topics, workspace_id)


def read_topics(topics_file):
    """Read topics from a file — one topic per line, blank lines ignored."""
    path = Path(topics_file)
    if not path.exists():
        print(f"Error: topics file not found: {topics_file}", file=sys.stderr)
        sys.exit(EXIT_BAD_INPUT)
    return [
        line.strip()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]


def cmd_create(args):
    """Create sessions for a batch of topics and write the run file."""
    topics = read_topics(args.topics_file)
    if not topics:
        print("Error: no topics to dispatch.", file=sys.stderr)
        sys.exit(EXIT_BAD_INPUT)

    now = datetime.now(timezone.utc)
    run_id = args.run_id or now.strftime("%Y%m%d-%H%M%S")
    created_at = now.strftime("%Y-%m-%dT%H:%M:%SZ")

    try:
        sessions = create_sessions(
            topics,
            args.agent,
            args.environment_id,
            args.workspace_id,
        )
    except CredentialError as exc:
        print(
            f"Error: preflight session create failed — the credential is "
            f"likely expired. Re-authenticate with `ant auth login`, then "
            f"retry. ({exc})",
            file=sys.stderr,
        )
        sys.exit(EXIT_BAD_CREDENTIAL)

    run = build_run_file(
        run_id,
        args.agent,
        args.environment_id,
        args.workspace_id,
        args.context,
        sessions,
        created_at,
    )

    run_file = args.run_file or f".dispatch-runs/{run_id}.json"
    run_path = Path(run_file)
    run_path.parent.mkdir(parents=True, exist_ok=True)
    run_path.write_text(
        json.dumps(run, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )
    print(f"Wrote run file: {run_file}", file=sys.stderr)

    json.dump(
        {"run_id": run_id, "run_file": run_file, "created": len(sessions)},
        sys.stdout,
        ensure_ascii=False,
    )
    sys.stdout.write("\n")
    return EXIT_OK


def build_parser():
    parser = argparse.ArgumentParser(
        prog="dispatch_create",
        description="Create dispatch sessions and build the run file.",
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p_create = sub.add_parser(
        "create",
        help="Create one session per topic, with a credential preflight.",
    )
    p_create.add_argument("topics_file", help="File with one topic per line.")
    p_create.add_argument("--agent", required=True, help="Agent ID.")
    p_create.add_argument(
        "--environment-id", required=True, dest="environment_id",
        help="Environment ID.",
    )
    p_create.add_argument(
        "--workspace-id", required=True, dest="workspace_id",
        help="Workspace ID (for session URLs).",
    )
    p_create.add_argument("--context", default="", help="Run context label.")
    p_create.add_argument(
        "--run-id", dest="run_id",
        help="Run ID (default: generated from the current timestamp).",
    )
    p_create.add_argument(
        "--run-file", dest="run_file",
        help="Run file path (default: .dispatch-runs/{run_id}.json).",
    )
    p_create.set_defaults(func=cmd_create)

    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()
    sys.exit(args.func(args))


if __name__ == "__main__":
    main()
