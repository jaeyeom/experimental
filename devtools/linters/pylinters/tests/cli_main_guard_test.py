"""End-to-end tests for the py-main-guard CLI."""

import io
import json
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from pylinters.cli import main_guard as cli

pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git not on PATH"
)

SHEBANG = "#!/usr/bin/env python3\n"
GOOD = {
    "tool.py": SHEBANG
    + "def main(): ...\nif __name__ == '__main__':\n    main()\n",
    "lib.py": "x = 1\n",
}
BAD = {**GOOD, "tool.py": SHEBANG + "def main(): ...\n"}
VIOLATION = (
    "tool.py:1: missing-guard: run directly (executable) but has no "
    "`if __name__ == '__main__':` guard"
)
BASELINE = ".baselines/py-main-guard.json"
NO_BAZEL = ["--entry-source", "executable"]


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


def test_pass_exit_0(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """A clean tree exits 0 and prints per-source entry counts."""
    make_repo(tmp_path, GOOD)
    monkeypatch.chdir(tmp_path)
    assert call(NO_BAZEL) == (
        0,
        "PASSED: py-main-guard (1 entries: executable 1; 2 file(s) scanned).\n",
        "",
    )


def test_bazel_skipped_note(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Default sources without a Bazel workspace print the skip note."""
    make_repo(tmp_path, GOOD)
    monkeypatch.chdir(tmp_path)
    code, out, _ = call([])
    assert code == 0
    assert out.splitlines()[1] == "bazel: skipped (no workspace)"


def test_missing_guard_exit_1(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A violation exits 1 with the finding and the hints."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    code, out, err = call(NO_BAZEL)
    assert (code, err) == (1, "")
    assert out.splitlines()[0] == VIOLATION
    assert "py-main-guard: allow" in out
    assert "--update-baseline" in out


def test_entry_list(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """--entry-list adds entries; paths are top-level relative."""
    make_repo(
        tmp_path, {"sub/run.py": "x = 1\n", "list.txt": "script sub/run.py\n"}
    )
    monkeypatch.chdir(tmp_path / "sub")
    code, out, _ = call(
        ["--entry-source", "executable", "--entry-list", "../list.txt"]
    )
    assert code == 1
    assert out.startswith("sub/run.py:1: missing-guard: run directly (list)")


def test_entry_list_bad_path_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A list line naming an untracked file is exit 2."""
    make_repo(tmp_path, {**GOOD, "list.txt": "script nope.py\n"})
    monkeypatch.chdir(tmp_path)
    code, _, err = call([*NO_BAZEL, "--entry-list", "list.txt"])
    assert code == 2
    assert "list.txt:1:" in err


def test_bazel_failure_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A failing bazel is exit 2, never a silent pass."""
    make_repo(tmp_path, {**GOOD, "MODULE.bazel": ""})
    monkeypatch.chdir(tmp_path)
    code, _, err = call(
        ["--entry-source", "bazel", "--bazel", "/nonexistent/bazel"]
    )
    assert code == 2
    assert "py-main-guard: error:" in err


def test_require_test_runner_flag(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--require-test-runner turns on rule 3 for test entries."""
    make_repo(
        tmp_path,
        {"t_test.py": "if __name__ == '__main__':\n    print('x')\n"},
    )
    monkeypatch.chdir(tmp_path)
    argv = ["--entry-source", "globs", "--test-glob", "*_test.py"]
    assert call(argv)[0] == 0
    code, out, _ = call([*argv, "--require-test-runner"])
    assert code == 1
    assert out.startswith("t_test.py:1: missing-test-runner:")


def test_no_require_test_runner_overrides_config(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--no-require-test-runner turns off rule 3 set in pyproject.toml."""
    make_repo(
        tmp_path,
        {
            "pyproject.toml": "[tool.pylinters.main-guard]\n"
            "require-test-runner = true\n",
            "t_test.py": "if __name__ == '__main__':\n    print('x')\n",
        },
    )
    monkeypatch.chdir(tmp_path)
    argv = ["--entry-source", "globs", "--test-glob", "*_test.py"]
    assert call(argv)[0] == 1
    assert call([*argv, "--no-require-test-runner"])[0] == 0


def test_notes_on_failure(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Notes on skipped sources are printed when the check fails too."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    code, out, _ = call([])
    assert code == 1
    assert "bazel: skipped (no workspace)" in out.splitlines()


def test_update_baseline_then_pass(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--update-baseline records debt; the next run passes with it."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call([*NO_BAZEL, "--update-baseline"])[0] == 0
    assert json.loads((tmp_path / BASELINE).read_text()) == {
        "checker": "py-main-guard",
        "entries": {"tool.py": {"limits": {"max_violations": 1}}},
        "version": 1,
    }
    code, out, _ = call(NO_BAZEL)
    assert code == 0
    assert "1 baselined violation(s)" in out


def test_stdin0_subset(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--stdin0 limits reporting to the given files."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call([*NO_BAZEL, "--stdin0"], stdin=b"lib.py\0")[0] == 0
    code, out, _ = call([*NO_BAZEL, "--stdin0"], stdin=b"tool.py\0")
    assert code == 1
    assert VIOLATION in out


def test_rewrite_with_paths_is_usage_error(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Baseline rewrites are whole-tree only."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call([*NO_BAZEL, "--update-baseline", "tool.py"])[0] == 2


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
