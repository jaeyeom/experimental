"""Hermetic Bazel test running mypy with the repository's pyproject.toml."""

from __future__ import annotations

import sys
from pathlib import Path

import pytest
from mypy import api as mypy_api


def _run_mypy(cache_dir: Path, *extra_args: str) -> tuple[str, str, int]:
    """Run mypy using the repository's pyproject.toml and a writable cache."""
    args = [
        "--config-file",
        "pyproject.toml",
        "--cache-dir",
        str(cache_dir),
        *extra_args,
    ]
    return mypy_api.run(args)


def test_repo_mypy(tmp_path: Path) -> None:
    """Repository Python files pass mypy with //:pyproject.toml."""
    stdout, stderr, exit_status = _run_mypy(tmp_path / ".mypy_cache")
    assert exit_status == 0, f"mypy failed:\n{stdout}{stderr}"
    assert "Success: no issues found in " in stdout, stdout


def test_mypy_fails_on_type_error(tmp_path: Path) -> None:
    """Mypy fails and reports return-value on a type mismatch."""
    bad = tmp_path / "bad_types.py"
    bad.write_text(
        "def bad(x: int) -> str:\n    return x\n",
        encoding="utf-8",
    )
    stdout, stderr, exit_status = _run_mypy(tmp_path / ".mypy_cache", str(bad))
    output = stdout + stderr
    assert exit_status != 0, "expected mypy to fail on type error"
    assert "return-value" in output, output


def test_mypy_strict_catches_untyped_def(tmp_path: Path) -> None:
    """Strict mode from pyproject.toml rejects untyped function defs."""
    untyped = tmp_path / "untyped.py"
    untyped.write_text(
        "def untyped(x):\n    return x\n",
        encoding="utf-8",
    )
    stdout, stderr, exit_status = _run_mypy(
        tmp_path / ".mypy_cache", str(untyped)
    )
    output = stdout + stderr
    assert exit_status != 0, "expected strict mypy to fail on untyped def"
    assert "no-untyped-def" in output, output


def test_mypy_rejects_unknown_import(tmp_path: Path) -> None:
    """Tightened config rejects missing third-party imports by default."""
    missing = tmp_path / "missing_import.py"
    missing.write_text(
        "import definitely_not_a_real_package_xyz\n",
        encoding="utf-8",
    )
    stdout, stderr, exit_status = _run_mypy(
        tmp_path / ".mypy_cache", str(missing)
    )
    output = stdout + stderr
    assert exit_status != 0, "expected mypy to fail on unknown import"
    assert "import-not-found" in output, output


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
