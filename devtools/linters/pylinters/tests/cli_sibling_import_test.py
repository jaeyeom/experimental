"""End-to-end tests for the py-sibling-import CLI."""

import io
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from pylinters.cli import sibling_import as cli

pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git not on PATH"
)

VIOLATION = (
    "p/b.py:1: import a: bare import of sibling module; use `from p import a`"
)


def make_repo(root: Path, tree: dict[str, str]) -> Path:
    """Create a git repo at ``root`` holding ``tree``, all files added."""
    for rel, text in tree.items():
        path = root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
    subprocess.run(["git", "init", "-q"], cwd=root, check=True)
    subprocess.run(["git", "add", "-A"], cwd=root, check=True)
    return root


def call(argv: list[str], stdin: bytes = b"") -> tuple[int, str, str]:
    """Run the CLI in-process and capture exit code, stdout, stderr."""
    out, err = io.StringIO(), io.StringIO()
    code = cli.main(argv, stdin=io.BytesIO(stdin), stdout=out, stderr=err)
    return code, out.getvalue(), err.getvalue()


BAD = {"p/__init__.py": "", "p/a.py": "", "p/b.py": "import a\n"}


def test_pass_exit_0_prints_counts(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A clean tree exits 0 and prints the scan counts."""
    make_repo(tmp_path, {"p/__init__.py": "", "p/a.py": ""})
    monkeypatch.chdir(tmp_path)
    code, out, _ = call([])
    assert code == 0
    assert out == (
        "PASSED: no bare sibling import (2 file(s) scanned, "
        "1 package director(ies) detected).\n"
    )


def test_violation_exit_1_prints_line_and_hint(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Violations exit 1 with the violation line and the directive hint."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    code, out, _ = call([])
    assert code == 1
    assert VIOLATION in out
    assert "py-sibling-import: allow" in out


def test_paths_skip_deleted_untracked_non_py(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Paths git diff may list but that cannot be checked are skipped."""
    make_repo(tmp_path, {**BAD, "README.md": ""})
    (tmp_path / "new.py").write_text("import (\n")
    monkeypatch.chdir(tmp_path)
    code, out, err = call(["README.md", "gone.py", "new.py"])
    assert (code, err) == (0, "")
    assert "0 file(s) scanned" in out


def test_path_outside_repo_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A path outside the repo is a usage error."""
    repo = make_repo(tmp_path / "r", BAD)
    monkeypatch.chdir(repo)
    code, _, err = call([str(tmp_path / "elsewhere.py")])
    assert code == 2
    assert "outside" in err


def test_stdin0_paths(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """--stdin0 reads NUL-separated paths."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    code, out, _ = call(["--stdin0"], stdin=b"p/b.py\0p/a.py\0")
    assert code == 1
    assert VIOLATION in out
    code, out, _ = call(["--stdin0"], stdin=b"p/a.py\0")
    assert code == 0


def test_runs_from_subdirectory(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Paths are relative to the cwd; output is repo-relative."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path / "p")
    code, out, _ = call(["b.py"])
    assert code == 1
    assert VIOLATION in out


def test_stdin0_paths_are_toplevel_relative(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Git diff --name-only prints top-level-relative paths."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path / "p")
    code, out, _ = call(["--stdin0"], stdin=b"p/b.py\0")
    assert code == 1
    assert VIOLATION in out


def test_root_flag(tmp_path: Path) -> None:
    """--root selects the repo without changing directory."""
    make_repo(tmp_path, BAD)
    code, out, _ = call(["--root", str(tmp_path)])
    assert code == 1
    assert VIOLATION in out


def test_not_a_git_repo_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Outside a git work tree the CLI exits 2."""
    monkeypatch.setenv("GIT_CEILING_DIRECTORIES", str(tmp_path.parent))
    monkeypatch.chdir(tmp_path)
    code, _, err = call([])
    assert code == 2
    assert err.startswith("py-sibling-import: error:")


def test_unknown_config_key_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """An unknown pyproject key exits 2."""
    make_repo(tmp_path, {**BAD, "pyproject.toml": "[tool.pylinters]\nx=1\n"})
    monkeypatch.chdir(tmp_path)
    code, _, err = call([])
    assert code == 2
    assert "'x'" in err


def test_parse_error_exit_2_names_file(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A scanned file that does not parse exits 2 naming the file."""
    make_repo(tmp_path, {"bad.py": "import (\n"})
    monkeypatch.chdir(tmp_path)
    code, _, err = call([])
    assert code == 2
    assert "bad.py" in err


def test_flag_overrides(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--generated, --exclude and --allow-directive override config."""
    make_repo(
        tmp_path,
        {
            "g/__init__.py": "",
            "g/a_pb2.py": "",
            "g/b_pb2.py": "import a_pb2\n",
            "p/__init__.py": "",
            "p/a.py": "",
            "p/b.py": "import a  # X\n",
            "pyproject.toml": '[tool.pylinters.sibling-import]\n'
            'exclude = ["p/**"]\n',
        },
    )
    monkeypatch.chdir(tmp_path)
    assert call([])[0] == 0
    code, out, _ = call(["--generated", "none/**"])
    assert code == 1
    assert "g/b_pb2.py:1" in out
    code, out, _ = call(["--exclude", "none/**"])
    assert code == 1
    assert "p/b.py:1" in out
    assert call(["--exclude", "none/**", "--allow-directive", "X"])[0] == 0


def test_version_flag(capsys: pytest.CaptureFixture[str]) -> None:
    """--version prints a version and exits 0."""
    with pytest.raises(SystemExit) as exc:
        call(["--version"])
    assert exc.value.code == 0
    assert capsys.readouterr().out.strip()


def test_usage_error_exit_2() -> None:
    """An unknown flag is a usage error."""
    with pytest.raises(SystemExit) as exc:
        call(["--nope"])
    assert exc.value.code == 2


def test_directory_path_expands(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A directory argument checks the tracked .py files below it."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    for arg in ("p", "."):
        code, out, _ = call([arg])
        assert code == 1
        assert VIOLATION in out


def test_exclude_help_describes_merge(
    capsys: pytest.CaptureFixture[str],
) -> None:
    """--exclude help says the shared excludes still apply."""
    with pytest.raises(SystemExit):
        call(["--help"])
    help_text = " ".join(capsys.readouterr().out.split())
    assert "replaces the checker excludes; shared excludes still apply" in (
        help_text
    )


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
