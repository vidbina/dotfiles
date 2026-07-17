#!/usr/bin/env python3
"""Extract results from dispatch managed agent sessions.

Reads NDJSON event streams from `ant beta:sessions:events list`, extracts
agent.message text, validates byte counts, writes markdown output files with
YAML frontmatter, and atomically updates the run file.

Designed for agentic use: structured JSON output on stdout, diagnostics on
stderr, meaningful exit codes.
"""

import argparse
import json
import os
import re
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path


# Exit codes
EXIT_OK = 0
EXIT_PARTIAL = 1        # some sessions had low byte counts
EXIT_BAD_INPUT = 2      # invalid input (missing file, bad JSON)
EXIT_NO_EVENTS = 3      # no events file provided or found

MIN_BYTES_THRESHOLD = 500


def parse_ndjson(source):
    """Parse NDJSON (or pretty-printed concatenated JSON) from a file path or
    stdin. Returns a list of event dicts.

    Handles two formats:
    - True NDJSON: one JSON object per line
    - Pretty-printed concatenated JSON: as output by `ant` CLI (multi-line
      objects separated by newlines)
    """
    if source == "-":
        content = sys.stdin.read()
    else:
        path = Path(source)
        if not path.exists():
            print(f"Error: events file not found: {source}", file=sys.stderr)
            sys.exit(EXIT_BAD_INPUT)
        content = path.read_text(encoding="utf-8")

    if not content.strip():
        return []

    # Try NDJSON first (one object per line)
    lines = content.strip().splitlines()
    first_line = lines[0].strip()
    if first_line.startswith("{") and first_line.endswith("}"):
        try:
            events = [json.loads(line) for line in lines if line.strip()]
            return events
        except json.JSONDecodeError:
            pass

    # Fall back to incremental parsing for pretty-printed JSON
    events = []
    decoder = json.JSONDecoder()
    pos = 0
    while pos < len(content):
        remaining = content[pos:].lstrip()
        if not remaining:
            break
        pos = len(content) - len(remaining)
        try:
            obj, end = decoder.raw_decode(content, pos)
            events.append(obj)
            pos += end
        except json.JSONDecodeError as exc:
            print(
                f"Warning: JSON parse error at position {pos}: {exc}",
                file=sys.stderr,
            )
            break

    return events


def extract_text(events):
    """Extract all text content from agent.message events.

    Returns the concatenated text from all text content blocks across all
    agent.message events, joined by newlines.
    """
    parts = []
    for event in events:
        if event.get("type") != "agent.message":
            continue
        for block in event.get("content", []):
            if block.get("type") == "text":
                parts.append(block["text"])
    return "\n".join(parts) if parts else ""


def compute_runtime(created_at, updated_at):
    """Compute human-readable runtime from ISO timestamps."""
    if not created_at or not updated_at:
        return "unknown"
    try:
        start = datetime.fromisoformat(created_at.replace("Z", "+00:00"))
        end = datetime.fromisoformat(updated_at.replace("Z", "+00:00"))
        delta = end - start
        total_seconds = int(delta.total_seconds())
        if total_seconds < 0:
            return "unknown"
        minutes, seconds = divmod(total_seconds, 60)
        if minutes > 0:
            return f"{minutes}m{seconds:02d}s"
        return f"{seconds}s"
    except (ValueError, TypeError):
        return "unknown"


def slugify(text):
    """Convert a topic label to a filename-safe slug."""
    slug = text.lower().strip()
    slug = re.sub(r"[^a-z0-9\s-]", "", slug)
    slug = re.sub(r"[\s]+", "-", slug)
    slug = re.sub(r"-+", "-", slug)
    slug = slug.strip("-")
    return slug


def write_output_file(output_dir, index, session, text, extracted_at):
    """Write a markdown output file with YAML frontmatter.

    Returns the relative path to the output file.
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    slug = slugify(session["topic"])
    filename = f"{index:02d}-{slug}.md"
    filepath = output_dir / filename

    runtime = compute_runtime(
        session.get("created_at"), session.get("updated_at")
    )

    frontmatter = (
        f"---\n"
        f"session_id: {session['id']}\n"
        f"session_url: {session.get('url', '')}\n"
        f"topic: {session['topic']}\n"
        f"status: {session.get('status', 'unknown')}\n"
        f"runtime: {runtime}\n"
        f"output_tokens: {session.get('output_tokens', 0)}\n"
        f"extracted_at: {extracted_at}\n"
        f"---\n\n"
    )

    filepath.write_text(frontmatter + text, encoding="utf-8")
    return str(filepath)


def update_run_file(run_file_path, updates):
    """Atomically update the run file with extraction results.

    `updates` is a dict mapping session ID to a dict with keys:
    result, output_file, and optionally status.

    Writes to a temp file in the same directory, then renames (atomic on
    POSIX).
    """
    run_path = Path(run_file_path)
    if not run_path.exists():
        print(f"Error: run file not found: {run_file_path}", file=sys.stderr)
        sys.exit(EXIT_BAD_INPUT)

    run_data = json.loads(run_path.read_text(encoding="utf-8"))

    for session in run_data.get("sessions", []):
        sid = session["id"]
        if sid in updates:
            session.update(updates[sid])

    run_data["gathered_at"] = datetime.now(timezone.utc).strftime(
        "%Y-%m-%dT%H:%M:%SZ"
    )

    # Atomic write: temp file + rename
    fd, tmp_path = tempfile.mkstemp(
        dir=run_path.parent, suffix=".tmp", prefix=".run-"
    )
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            json.dump(run_data, f, indent=2, ensure_ascii=False)
            f.write("\n")
        os.replace(tmp_path, run_path)
    except Exception:
        # Clean up temp file on failure
        try:
            os.unlink(tmp_path)
        except OSError:
            pass
        raise

    return run_data


def cmd_extract(args):
    """Extract text from an NDJSON events file (or stdin)."""
    source = args.events_file or "-"
    events = parse_ndjson(source)

    if not events:
        print("Error: no events found in input.", file=sys.stderr)
        sys.exit(EXIT_NO_EVENTS)

    text = extract_text(events)
    byte_count = len(text.encode("utf-8"))

    result = {
        "text": text,
        "byte_count": byte_count,
        "event_count": len(events),
        "agent_message_count": sum(
            1 for e in events if e.get("type") == "agent.message"
        ),
        "flagged": byte_count < MIN_BYTES_THRESHOLD,
    }

    if result["flagged"]:
        print(
            f"Warning: extracted text is only {byte_count} bytes "
            f"(threshold: {MIN_BYTES_THRESHOLD}). "
            f"Agent likely wrote to files instead of messages.",
            file=sys.stderr,
        )

    if args.output == "json":
        json.dump(result, sys.stdout, ensure_ascii=False)
        sys.stdout.write("\n")
    else:
        sys.stdout.write(text)

    return EXIT_PARTIAL if result["flagged"] else EXIT_OK


def cmd_gather(args):
    """Gather results for all sessions in a run file.

    For each session, reads NDJSON from the events directory (or calls
    `ant` CLI if --live is set), extracts text, writes output files, and
    updates the run file atomically.
    """
    run_path = Path(args.run_file)
    if not run_path.exists():
        print(f"Error: run file not found: {args.run_file}", file=sys.stderr)
        sys.exit(EXIT_BAD_INPUT)

    run_data = json.loads(run_path.read_text(encoding="utf-8"))
    run_id = run_data["id"]
    sessions = run_data.get("sessions", [])

    # Output directory: sibling to run file, named after run ID
    output_dir = run_path.parent / run_id

    events_dir = Path(args.events_dir) if args.events_dir else None

    extracted_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    updates = {}
    results = []
    has_flagged = False

    for i, session in enumerate(sessions, start=1):
        sid = session["id"]
        status = session.get("status", "")

        if status == "terminated":
            results.append({
                "index": i,
                "session_id": sid,
                "topic": session["topic"],
                "status": "terminated",
                "byte_count": 0,
                "flagged": False,
                "output_file": None,
            })
            continue

        # Find events for this session
        events = []
        if events_dir:
            # Look for a file named after the session ID
            events_file = events_dir / f"{sid}.ndjson"
            if events_file.exists():
                events = parse_ndjson(str(events_file))
            else:
                print(
                    f"Warning: no events file for session {sid} "
                    f"at {events_file}",
                    file=sys.stderr,
                )
        elif args.live:
            # Call ant CLI to fetch events
            import subprocess

            try:
                proc = subprocess.run(
                    [
                        "ant", "beta:sessions:events", "list",
                        "--session-id", sid,
                        "--type", "agent.message",
                        "--max-items", "-1",
                    ],
                    capture_output=True,
                    text=True,
                    timeout=120,
                )
                if proc.returncode == 0 and proc.stdout.strip():
                    events = parse_ndjson_string(proc.stdout)
                else:
                    print(
                        f"Warning: ant CLI returned no data for {sid}",
                        file=sys.stderr,
                    )
            except (subprocess.TimeoutExpired, FileNotFoundError) as exc:
                print(
                    f"Warning: ant CLI failed for {sid}: {exc}",
                    file=sys.stderr,
                )
        else:
            print(
                f"Error: no events source specified. "
                f"Use --events-dir or --live.",
                file=sys.stderr,
            )
            sys.exit(EXIT_BAD_INPUT)

        text = extract_text(events)
        byte_count = len(text.encode("utf-8"))
        flagged = byte_count < MIN_BYTES_THRESHOLD

        if flagged:
            has_flagged = True
            print(
                f"Warning: session {sid} ({session['topic']}): "
                f"only {byte_count} bytes extracted. "
                f"Agent likely wrote to files.",
                file=sys.stderr,
            )

        # Write output file if we have text
        output_file = None
        if text:
            output_file = write_output_file(
                output_dir, i, session, text, extracted_at
            )

        updates[sid] = {
            "result": text if text else None,
            "output_file": output_file,
        }

        results.append({
            "index": i,
            "session_id": sid,
            "topic": session["topic"],
            "status": status,
            "byte_count": byte_count,
            "flagged": flagged,
            "output_file": output_file,
        })

    # Update run file atomically
    if not args.dry_run:
        update_run_file(args.run_file, updates)
        print(f"Updated run file: {args.run_file}", file=sys.stderr)
    else:
        print("Dry run: run file not updated.", file=sys.stderr)

    # Output results
    output = {
        "run_id": run_id,
        "sessions": results,
        "total": len(sessions),
        "extracted": sum(1 for r in results if r["byte_count"] > 0),
        "flagged": sum(1 for r in results if r["flagged"]),
        "terminated": sum(
            1 for r in results if r["status"] == "terminated"
        ),
        "output_dir": str(output_dir),
    }

    json.dump(output, sys.stdout, indent=2, ensure_ascii=False)
    sys.stdout.write("\n")

    return EXIT_PARTIAL if has_flagged else EXIT_OK


def parse_ndjson_string(content):
    """Parse NDJSON from a string (used for subprocess output)."""
    if not content.strip():
        return []
    lines = content.strip().splitlines()
    first_line = lines[0].strip()
    if first_line.startswith("{") and first_line.endswith("}"):
        try:
            return [json.loads(line) for line in lines if line.strip()]
        except json.JSONDecodeError:
            pass
    events = []
    decoder = json.JSONDecoder()
    pos = 0
    while pos < len(content):
        remaining = content[pos:].lstrip()
        if not remaining:
            break
        pos = len(content) - len(remaining)
        try:
            obj, end = decoder.raw_decode(content, pos)
            events.append(obj)
            pos += end
        except json.JSONDecodeError:
            break
    return events


def build_parser():
    parser = argparse.ArgumentParser(
        prog="dispatch_gather",
        description="Extract results from dispatch managed agent sessions.",
        epilog=(
            "Exit codes:\n"
            "  0  All sessions extracted successfully\n"
            "  1  Some sessions had low byte counts (< 500 bytes)\n"
            "  2  Invalid input (missing file, bad JSON)\n"
            "  3  No events found\n"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    sub = parser.add_subparsers(dest="command", required=True)

    # extract: single-session extraction
    p_extract = sub.add_parser(
        "extract",
        help="Extract text from a single session's NDJSON events.",
    )
    p_extract.add_argument(
        "events_file",
        nargs="?",
        help="Path to NDJSON events file. Reads stdin if omitted.",
    )
    p_extract.add_argument(
        "--output",
        choices=["json", "text"],
        default="json",
        help="Output format (default: json).",
    )
    p_extract.set_defaults(func=cmd_extract)

    # gather: multi-session extraction from a run file
    p_gather = sub.add_parser(
        "gather",
        help="Gather results for all sessions in a run file.",
    )
    p_gather.add_argument(
        "run_file",
        help="Path to the dispatch run file (.json).",
    )
    p_gather.add_argument(
        "--events-dir",
        help=(
            "Directory containing per-session NDJSON files "
            "(named {session_id}.ndjson)."
        ),
    )
    p_gather.add_argument(
        "--live",
        action="store_true",
        help="Fetch events from ant CLI instead of local files.",
    )
    p_gather.add_argument(
        "--dry-run",
        action="store_true",
        help="Extract and report without updating the run file.",
    )
    p_gather.set_defaults(func=cmd_gather)

    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()
    exit_code = args.func(args)
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
