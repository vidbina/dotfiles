"""Tests for dispatch_gather.py."""

import json
import subprocess
import sys
from pathlib import Path

import pytest

# Add scripts dir to path so we can import the module
SCRIPTS_DIR = Path(__file__).parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS_DIR))

import dispatch_gather as dg


class TestParseNdjson:
    def test_parses_ndjson_file(self, complete_events_path):
        events = dg.parse_ndjson(str(complete_events_path))
        assert len(events) == 13
        types = {e["type"] for e in events}
        assert "agent.message" in types
        assert "user.message" in types
        assert "session.status_running" in types

    def test_parses_empty_file(self, tmp_path):
        empty = tmp_path / "empty.ndjson"
        empty.write_text("")
        events = dg.parse_ndjson(str(empty))
        assert events == []

    def test_exits_on_missing_file(self):
        with pytest.raises(SystemExit) as exc_info:
            dg.parse_ndjson("/nonexistent/file.ndjson")
        assert exc_info.value.code == dg.EXIT_BAD_INPUT


class TestExtractText:
    def test_extracts_agent_message_text(self, complete_events_path):
        events = dg.parse_ndjson(str(complete_events_path))
        text = dg.extract_text(events)
        assert "Enterprise Search Landscape 2026" in text
        assert "Typesense" in text
        assert "I'll compile the findings now." in text

    def test_ignores_non_agent_message_events(self, complete_events_path):
        events = dg.parse_ndjson(str(complete_events_path))
        text = dg.extract_text(events)
        # user.message content should not appear
        assert "Research the enterprise search landscape" not in text

    def test_empty_on_no_agent_messages(self):
        events = [
            {"type": "session.status_running", "id": "x"},
            {"type": "user.message", "id": "y", "content": [
                {"type": "text", "text": "hello"}
            ]},
        ]
        text = dg.extract_text(events)
        assert text == ""

    def test_joins_multiple_text_blocks(self):
        events = [
            {"type": "agent.message", "id": "a", "content": [
                {"type": "text", "text": "Part 1."},
                {"type": "text", "text": "Part 2."},
            ]},
        ]
        text = dg.extract_text(events)
        assert "Part 1." in text
        assert "Part 2." in text

    def test_low_byte_count_flagged(self, empty_events_path):
        events = dg.parse_ndjson(str(empty_events_path))
        text = dg.extract_text(events)
        byte_count = len(text.encode("utf-8"))
        assert byte_count < dg.MIN_BYTES_THRESHOLD
        assert text == "Done."


class TestComputeRuntime:
    def test_computes_minutes_and_seconds(self):
        result = dg.compute_runtime(
            "2026-07-08T12:32:57.000000Z",
            "2026-07-08T12:35:33.000000Z",
        )
        assert result == "2m36s"

    def test_computes_seconds_only(self):
        result = dg.compute_runtime(
            "2026-07-08T12:32:57.000000Z",
            "2026-07-08T12:33:02.000000Z",
        )
        assert result == "5s"

    def test_handles_none(self):
        assert dg.compute_runtime(None, None) == "unknown"
        assert dg.compute_runtime("2026-07-08T12:32:57Z", None) == "unknown"


class TestSlugify:
    def test_basic_slugify(self):
        assert dg.slugify("Enterprise search landscape") == "enterprise-search-landscape"

    def test_strips_special_chars(self):
        assert dg.slugify("Dark-pool / options-flow") == "dark-pool-options-flow"

    def test_handles_unicode(self):
        slug = dg.slugify("Politician/Congressional trades")
        assert "/" not in slug


class TestWriteOutputFile:
    def test_writes_frontmatter_and_content(self, tmp_path):
        session = {
            "id": "sesn_01TEST",
            "topic": "Test topic",
            "url": "https://example.com/session",
            "status": "idle",
            "created_at": "2026-07-08T12:32:57.000000Z",
            "updated_at": "2026-07-08T12:35:33.000000Z",
            "output_tokens": 1234,
        }
        text = "# Test Output\n\nSome content here."
        path = dg.write_output_file(
            tmp_path / "output", 1, session, text, "2026-07-08T13:00:00Z"
        )

        content = Path(path).read_text()
        assert "---" in content
        assert "session_id: sesn_01TEST" in content
        assert "topic: Test topic" in content
        assert "runtime: 2m36s" in content
        assert "output_tokens: 1234" in content
        assert "# Test Output" in content

    def test_filename_format(self, tmp_path):
        session = {
            "id": "sesn_01TEST",
            "topic": "Enterprise search landscape",
            "url": "",
            "status": "idle",
            "output_tokens": 0,
        }
        path = dg.write_output_file(
            tmp_path / "output", 3, session, "text", "2026-07-08T13:00:00Z"
        )
        assert Path(path).name == "03-enterprise-search-landscape.md"

    def test_creates_output_dir(self, tmp_path):
        output_dir = tmp_path / "nested" / "output"
        session = {
            "id": "sesn_01TEST",
            "topic": "Test",
            "url": "",
            "status": "idle",
            "output_tokens": 0,
        }
        dg.write_output_file(output_dir, 1, session, "text", "now")
        assert output_dir.exists()


class TestUpdateRunFile:
    def test_updates_sessions_atomically(self, tmp_run_file):
        updates = {
            "sesn_01COMPLETE": {
                "result": "extracted text here",
                "output_file": "output/01-enterprise-search.md",
            },
        }
        run_data = dg.update_run_file(str(tmp_run_file), updates)

        # Verify the file was updated
        saved = json.loads(tmp_run_file.read_text())
        session = next(
            s for s in saved["sessions"] if s["id"] == "sesn_01COMPLETE"
        )
        assert session["result"] == "extracted text here"
        assert session["output_file"] == "output/01-enterprise-search.md"
        assert "gathered_at" in saved

    def test_preserves_unmodified_sessions(self, tmp_run_file):
        updates = {"sesn_01COMPLETE": {"result": "text"}}
        dg.update_run_file(str(tmp_run_file), updates)

        saved = json.loads(tmp_run_file.read_text())
        terminated = next(
            s for s in saved["sessions"] if s["id"] == "sesn_01TERMINATED"
        )
        assert terminated["status"] == "terminated"
        assert terminated["result"] is None

    def test_exits_on_missing_run_file(self):
        with pytest.raises(SystemExit) as exc_info:
            dg.update_run_file("/nonexistent/run.json", {})
        assert exc_info.value.code == dg.EXIT_BAD_INPUT


class TestCLIExtract:
    def test_extract_json_output(self, complete_events_path):
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPTS_DIR / "dispatch_gather.py"),
                "extract",
                str(complete_events_path),
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        data = json.loads(result.stdout)
        assert data["byte_count"] > dg.MIN_BYTES_THRESHOLD
        assert data["flagged"] is False
        assert data["agent_message_count"] == 2

    def test_extract_text_output(self, complete_events_path):
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPTS_DIR / "dispatch_gather.py"),
                "extract",
                "--output", "text",
                str(complete_events_path),
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        assert "Enterprise Search Landscape" in result.stdout
        # JSON should not be in text output
        assert '"byte_count"' not in result.stdout

    def test_extract_flagged_exit_code(self, empty_events_path):
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPTS_DIR / "dispatch_gather.py"),
                "extract",
                str(empty_events_path),
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == dg.EXIT_PARTIAL
        data = json.loads(result.stdout)
        assert data["flagged"] is True
        assert "wrote to files" in result.stderr


class TestCLIGather:
    def test_gather_dry_run(
        self, tmp_run_file, tmp_events_dir
    ):
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPTS_DIR / "dispatch_gather.py"),
                "gather",
                str(tmp_run_file),
                "--events-dir", str(tmp_events_dir),
                "--dry-run",
            ],
            capture_output=True,
            text=True,
        )
        # Exit 1 because one session is flagged
        assert result.returncode == dg.EXIT_PARTIAL
        data = json.loads(result.stdout)
        assert data["total"] == 3
        assert data["extracted"] == 2
        assert data["flagged"] == 1
        assert data["terminated"] == 1

        # Dry run should not modify the run file
        original = json.loads(
            (Path(__file__).parent / "fixtures" / "run-file.json").read_text()
        )
        current = json.loads(tmp_run_file.read_text())
        assert current == original

    def test_gather_updates_run_file(
        self, tmp_run_file, tmp_events_dir
    ):
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPTS_DIR / "dispatch_gather.py"),
                "gather",
                str(tmp_run_file),
                "--events-dir", str(tmp_events_dir),
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == dg.EXIT_PARTIAL

        saved = json.loads(tmp_run_file.read_text())
        assert "gathered_at" in saved

        complete = next(
            s for s in saved["sessions"] if s["id"] == "sesn_01COMPLETE"
        )
        assert complete["result"] is not None
        assert "Enterprise Search" in complete["result"]
        assert complete["output_file"] is not None

        terminated = next(
            s for s in saved["sessions"] if s["id"] == "sesn_01TERMINATED"
        )
        assert terminated["result"] is None
        assert terminated["output_file"] is None
