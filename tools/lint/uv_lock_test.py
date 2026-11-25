"""Hermetic Bazel test verifying uv.lock is in sync with pyproject.toml."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest


def _uv_bin() -> Path:
    """Return the Bazel-managed uv binary path from UV_BIN."""
    raw = os.environ.get("UV_BIN")
    assert raw, "UV_BIN environment variable must be set by Bazel"
    path = Path(raw).resolve()
    assert path.is_file(), f"uv binary not found at {path}"
    return path


def _copy_project(dest: Path) -> Path:
    """Copy pyproject.toml and uv.lock into an isolated directory."""
    dest.mkdir(parents=True, exist_ok=True)
    shutil.copy2("pyproject.toml", dest / "pyproject.toml")
    shutil.copy2("uv.lock", dest / "uv.lock")
    return dest


def _run_uv_lock_check(
    workdir: Path, tmp_path: Path
) -> subprocess.CompletedProcess[str]:
    """Run uv lock --check --offline with no host PATH or cache."""
    home_dir = tmp_path / "home"
    cache_dir = tmp_path / "cache"
    home_dir.mkdir(exist_ok=True)
    cache_dir.mkdir(exist_ok=True)

    env = {
        "HOME": str(home_dir),
        "PATH": "",
        "UV_CACHE_DIR": str(cache_dir),
    }
    cmd = [
        str(_uv_bin()),
        "lock",
        "--check",
        "--offline",
        "--no-cache",
        "--no-python-downloads",
        "--python",
        sys.executable,
    ]
    return subprocess.run(
        cmd,
        cwd=workdir,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def test_repo_uv_lock_in_sync(tmp_path: Path) -> None:
    """Repository uv.lock matches pyproject.toml offline."""
    workdir = _copy_project(tmp_path / "repo")
    result = _run_uv_lock_check(workdir, tmp_path)
    assert result.returncode == 0, (
        f"uv lock --check failed:\n{result.stdout}{result.stderr}"
    )


def test_uv_lock_fails_when_dependency_added(tmp_path: Path) -> None:
    """Adding a dependency to pyproject.toml fails uv lock --check."""
    workdir = _copy_project(tmp_path / "added")
    pyproject = workdir / "pyproject.toml"
    content = pyproject.read_text(encoding="utf-8")
    updated = content.replace(
        '"protovalidate",',
        '"protovalidate",\n    "requests",',
        1,
    )
    assert updated != content
    pyproject.write_text(updated, encoding="utf-8")

    result = _run_uv_lock_check(workdir, tmp_path)
    assert result.returncode != 0, (
        "expected uv lock --check to fail after adding a dependency"
    )


def test_uv_lock_fails_when_dependency_removed(tmp_path: Path) -> None:
    """Removing a dependency from pyproject.toml fails uv lock --check."""
    workdir = _copy_project(tmp_path / "removed")
    pyproject = workdir / "pyproject.toml"
    content = pyproject.read_text(encoding="utf-8")
    updated = content.replace('    "protovalidate",\n', "", 1)
    assert updated != content
    pyproject.write_text(updated, encoding="utf-8")

    result = _run_uv_lock_check(workdir, tmp_path)
    assert result.returncode != 0, (
        "expected uv lock --check to fail after removing a dependency"
    )


def test_uv_lock_fails_when_dev_dependency_added(tmp_path: Path) -> None:
    """Adding a dev dependency to pyproject.toml fails uv lock --check."""
    workdir = _copy_project(tmp_path / "dev_added")
    pyproject = workdir / "pyproject.toml"
    content = pyproject.read_text(encoding="utf-8")
    updated = content.replace(
        "dev = [",
        'dev = [\n    "ruff",',
        1,
    )
    assert updated != content
    pyproject.write_text(updated, encoding="utf-8")

    result = _run_uv_lock_check(workdir, tmp_path)
    assert result.returncode != 0, (
        "expected uv lock --check to fail after adding a dev dependency"
    )


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
