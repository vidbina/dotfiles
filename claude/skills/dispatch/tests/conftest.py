"""Shared fixtures for dispatch gather tests."""

from pathlib import Path

import pytest

FIXTURES_DIR = Path(__file__).parent / "fixtures"


@pytest.fixture
def fixtures_dir():
    return FIXTURES_DIR


@pytest.fixture
def complete_events_path(fixtures_dir):
    return fixtures_dir / "session-complete.ndjson"


@pytest.fixture
def empty_events_path(fixtures_dir):
    return fixtures_dir / "session-empty.ndjson"


@pytest.fixture
def run_file_path(fixtures_dir):
    return fixtures_dir / "run-file.json"


@pytest.fixture
def tmp_run_file(tmp_path, run_file_path):
    """Copy the run file fixture to a temp dir so tests can mutate it."""
    import shutil

    dest = tmp_path / "run-file.json"
    shutil.copy2(run_file_path, dest)
    return dest


@pytest.fixture
def tmp_events_dir(tmp_path, complete_events_path, empty_events_path):
    """Set up a temp events directory with per-session NDJSON files."""
    import shutil

    events_dir = tmp_path / "events"
    events_dir.mkdir()
    shutil.copy2(
        complete_events_path, events_dir / "sesn_01COMPLETE.ndjson"
    )
    shutil.copy2(
        empty_events_path, events_dir / "sesn_01EMPTY.ndjson"
    )
    return events_dir
