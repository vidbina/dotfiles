"""Tests for dispatch_create.py — the drop-proof assembly core."""

import sys
import types
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


class FakeCreator:
    """A stand-in for create_one_session that records calls.

    Returns sequential fake IDs, or raises SessionCreateError on the Nth call
    (1-based) when `fail_on` is set — used to simulate a preflight failure.
    """

    def __init__(self, fail_on=None):
        self.calls = 0
        self.fail_on = fail_on

    def __call__(self, agent_id, environment_id):
        self.calls += 1
        if self.fail_on is not None and self.calls == self.fail_on:
            raise dc.SessionCreateError("simulated create failure")
        return f"sesn_{self.calls:02d}"


class TestCreateSessions:
    def test_happy_path_creates_one_per_topic(self):
        creator = FakeCreator()
        topics = ["Alpha", "Beta", "Gamma"]
        sessions = dc.create_sessions(
            topics, "agent_01", "env_01", WORKSPACE, creator=creator
        )

        assert creator.calls == 3
        assert [s["id"] for s in sessions] == ["sesn_01", "sesn_02", "sesn_03"]
        assert [s["topic"] for s in sessions] == topics

    def test_empty_topics_creates_nothing(self):
        creator = FakeCreator()
        assert dc.create_sessions(
            [], "agent_01", "env_01", WORKSPACE, creator=creator
        ) == []
        assert creator.calls == 0

    def test_preflight_failure_stops_after_one_call(self):
        # A bad credential must be discovered after ONE create, not N.
        creator = FakeCreator(fail_on=1)
        with pytest.raises(dc.CredentialError):
            dc.create_sessions(
                ["Alpha", "Beta", "Gamma"],
                "agent_01", "env_01", WORKSPACE, creator=creator,
            )
        assert creator.calls == 1  # never attempted the rest of the batch


class TestCreateOneSession:
    def test_parses_id_from_json(self, monkeypatch):
        def fake_run(*args, **kwargs):
            return types.SimpleNamespace(
                returncode=0, stdout='{"id": "sesn_01ABC"}', stderr=""
            )

        monkeypatch.setattr(dc.subprocess, "run", fake_run)
        assert dc.create_one_session("agent_01", "env_01") == "sesn_01ABC"

    def test_nonzero_exit_raises(self, monkeypatch):
        def fake_run(*args, **kwargs):
            return types.SimpleNamespace(
                returncode=1, stdout="", stderr="unauthorized"
            )

        monkeypatch.setattr(dc.subprocess, "run", fake_run)
        with pytest.raises(dc.SessionCreateError, match="unauthorized"):
            dc.create_one_session("agent_01", "env_01")

    def test_missing_id_raises(self, monkeypatch):
        def fake_run(*args, **kwargs):
            return types.SimpleNamespace(
                returncode=0, stdout='{"foo": "bar"}', stderr=""
            )

        monkeypatch.setattr(dc.subprocess, "run", fake_run)
        with pytest.raises(dc.SessionCreateError, match="no `id`"):
            dc.create_one_session("agent_01", "env_01")


class TestReadTopics:
    def test_reads_one_per_line_ignoring_blanks(self, tmp_path):
        f = tmp_path / "topics.txt"
        f.write_text("Alpha\n\nBeta\n  \nGamma\n")
        assert dc.read_topics(str(f)) == ["Alpha", "Beta", "Gamma"]
