"""Bazel test running Bazel-managed gitleaks across git history."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import pytest


def _gitleaks_bin() -> Path:
    """Return the Bazel-managed gitleaks binary path from GITLEAKS_BIN."""
    raw = os.environ.get("GITLEAKS_BIN")
    assert raw, "GITLEAKS_BIN environment variable must be set by Bazel"
    path = Path(raw).resolve()
    assert path.is_file(), f"gitleaks binary not found at {path}"
    return path


def _repo_root() -> Path:
    """Resolve the real checkout root from the go.mod data file symlink."""
    cur = Path.cwd()
    while True:
        candidate = cur / "go.mod"
        if candidate.exists():
            return candidate.resolve().parent
        if cur.parent == cur:
            raise RuntimeError("go.mod not found")
        cur = cur.parent


def _run_gitleaks(repo_dir: Path) -> subprocess.CompletedProcess[str]:
    """Run Bazel-managed gitleaks git scan in repo_dir."""
    cmd = [
        str(_gitleaks_bin()),
        "git",
        "--redact",
        "--no-banner",
        ".",
    ]
    return subprocess.run(
        cmd,
        cwd=repo_dir,
        capture_output=True,
        text=True,
        check=False,
    )


def _git_env(tmp_path: Path) -> dict[str, str]:
    """Return an isolated git environment unaffected by host git config."""
    home_dir = tmp_path / "home"
    home_dir.mkdir(exist_ok=True)
    env = os.environ.copy()
    env["HOME"] = str(home_dir)
    env["GIT_CONFIG_NOSYSTEM"] = "1"
    return env


def _init_repo_with_secret(tmp_path: Path, name: str) -> Path:
    """Create a temporary git repo with one committed synthetic secret."""
    repo = tmp_path / name
    repo.mkdir()
    env = _git_env(tmp_path)

    def git(*args: str) -> None:
        subprocess.run(
            ["git", *args],
            cwd=repo,
            env=env,
            check=True,
            capture_output=True,
            text=True,
        )

    git("init", "-b", "main")
    git("config", "user.name", "Test User")
    git("config", "user.email", "test@example.com")
    git("config", "commit.gpgsign", "false")

    # Build synthetic GitHub PAT dynamically so gitleaks does not flag source.
    prefix = "ghp_"
    body = "aB1cD2eF3gH4iJ5kL6mN7oP8qR9sT0uV1wX2"
    fake_secret = f"{prefix}{body}"
    (repo / "leak.txt").write_text(
        f"token={fake_secret}\n",
        encoding="utf-8",
    )
    git("add", "leak.txt")
    git("commit", "-m", "add synthetic secret")
    return repo


def test_repo_git_history_has_no_leaks() -> None:
    """Repository git history passes gitleaks scan with .gitleaks.toml."""
    root = _repo_root()
    assert (root / ".gitleaks.toml").is_file()
    result = _run_gitleaks(root)
    assert result.returncode == 0, (
        f"gitleaks found leaks:\n{result.stdout}{result.stderr}"
    )


def test_detects_secret_in_git_history(tmp_path: Path) -> None:
    """Gitleaks fails when git history contains a secret."""
    repo = _init_repo_with_secret(tmp_path, "with_leak")
    result = _run_gitleaks(repo)
    assert result.returncode != 0, (
        "expected gitleaks to fail on synthetic secret"
    )


def test_respects_gitleaks_toml_allowlist(tmp_path: Path) -> None:
    """Gitleaks respects .gitleaks.toml allowlists in the scanned repo."""
    repo = _init_repo_with_secret(tmp_path, "with_allowlist")
    (repo / ".gitleaks.toml").write_text(
        "[extend]\n"
        "useDefault = true\n"
        "\n"
        "[allowlist]\n"
        "paths = ['''leak\\.txt''']\n",
        encoding="utf-8",
    )
    result = _run_gitleaks(repo)
    assert result.returncode == 0, (
        f"expected .gitleaks.toml allowlist to suppress finding:\n"
        f"{result.stdout}{result.stderr}"
    )


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
