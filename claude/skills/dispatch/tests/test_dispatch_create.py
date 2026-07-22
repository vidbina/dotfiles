"""Tests for dispatch_create.py — the drop-proof assembly core."""

import sys
from pathlib import Path

import pytest

# Add scripts dir to path so we can import the module
SCRIPTS_DIR = Path(__file__).parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS_DIR))

import dispatch_create as dc


WORKSPACE = "wrkspc_01EXAMPLE"


class TestBuildSessionUrl:
    def test_constructs_console_url(self):
        url = dc.build_session_url(WORKSPACE, "sesn_01AA")
        assert url == (
            "https://console.anthropic.com/workspaces/"
            "wrkspc_01EXAMPLE/sessions/sesn_01AA"
        )


class TestBuildSession:
    def test_populates_creation_fields(self):
        session = dc.build_session("Enterprise search", "sesn_01AA", 1, WORKSPACE)
        assert session["id"] == "sesn_01AA"
        assert session["topic"] == "Enterprise search"
        assert session["url"] == dc.build_session_url(WORKSPACE, "sesn_01AA")

    def test_leaves_gather_fields_null(self):
        session = dc.build_session("Topic", "sesn_01AA", 1, WORKSPACE)
        for field in (
            "status",
            "created_at",
            "updated_at",
            "output_tokens",
            "result",
            "output_file",
        ):
            assert session[field] is None


class TestAssembleSessions:
    def test_pairs_ids_and_topics_in_order(self):
        ids = ["sesn_01A", "sesn_01B", "sesn_01C"]
        topics = ["Alpha", "Beta", "Gamma"]
        sessions = dc.assemble_sessions(ids, topics, WORKSPACE)

        assert [s["id"] for s in sessions] == ids
        assert [s["topic"] for s in sessions] == topics

    def test_preserves_count_for_ten_sessions(self):
        # The regression guard: DISPATCH.md Error 1 dropped the 10th session and
        # blanked the 1st. Ten in, ten out, none blank.
        ids = [f"sesn_{i:02d}" for i in range(1, 11)]
        topics = [f"Topic {i}" for i in range(1, 11)]
        sessions = dc.assemble_sessions(ids, topics, WORKSPACE)

        assert len(sessions) == 10
        assert all(s["id"] and s["topic"] for s in sessions)
        assert sessions[0]["topic"] == "Topic 1"
        assert sessions[-1]["topic"] == "Topic 10"

    def test_empty_inputs_yield_empty_list(self):
        assert dc.assemble_sessions([], [], WORKSPACE) == []

    def test_length_mismatch_raises(self):
        with pytest.raises(ValueError, match="count mismatch"):
            dc.assemble_sessions(["sesn_01A"], ["Alpha", "Beta"], WORKSPACE)

    def test_more_ids_than_topics_raises(self):
        with pytest.raises(ValueError, match="count mismatch"):
            dc.assemble_sessions(
                ["sesn_01A", "sesn_01B"], ["Alpha"], WORKSPACE
            )


class TestBuildRunFile:
    def test_assembles_top_level_structure(self):
        sessions = dc.assemble_sessions(
            ["sesn_01A"], ["Alpha"], WORKSPACE
        )
        run = dc.build_run_file(
            run_id="20260605-085200",
            agent_id="agent_01EXAMPLE",
            environment_id="env_01EXAMPLE",
            workspace_id=WORKSPACE,
            context="PROJ-42 research",
            sessions=sessions,
            created_at="2026-06-05T08:52:00Z",
        )

        assert run["id"] == "20260605-085200"
        assert run["created_at"] == "2026-06-05T08:52:00Z"
        assert run["agent_id"] == "agent_01EXAMPLE"
        assert run["environment_id"] == "env_01EXAMPLE"
        assert run["workspace_id"] == WORKSPACE
        assert run["context"] == "PROJ-42 research"
        assert run["sessions"] == sessions
