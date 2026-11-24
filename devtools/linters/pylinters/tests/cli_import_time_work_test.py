"""End-to-end tests for the py-import-time-work CLI."""

import io
import json
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from pylinters.cli import import_time_work as cli

pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git not on PATH"
)

SHEBANG = "#!/usr/bin/env python3\n"
GOOD = {
    "tool.py": (
        SHEBANG + "def main(): ...\nif __name__ == '__main__':\n    main()\n"
    ),
    "lib.py": "x = 1\n",
}
BAD = {
    **GOOD,
    "tool.py": (
        SHEBANG
        + 'import sys\nsys.path.insert(0, "x")\n'
        + "def main(): ...\nif __name__ == '__main__':\n    main()\n"
    ),
}
VIOLATION = (
    "tool.py:3: import-time-work: `sys.path.insert(...)` at module level "
    "runs on every import; move it into main() or under the __main__ guard"
)
BASELINE = ".baselines/py-import-time-work.json"
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
    """A clean tree exits 0 and prints scope, checked count, and entries."""
    make_repo(tmp_path, GOOD)
    monkeypatch.chdir(tmp_path)
    assert call(NO_BAZEL) == (
        0,
        (
            "PASSED: py-import-time-work (scope entry: 1 file(s) checked; "
            "1 entries: executable 1).\n"
        ),
        "",
    )


def test_violation_exit_1(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A violation exits 1 with the finding and the hints."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    code, out, err = call(NO_BAZEL)
    assert (code, err) == (1, "")
    assert out.splitlines()[0] == VIOLATION
    assert "py-import-time-work: allow" in out
    assert "--update-baseline" in out


def test_scope_all_checks_library(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """An unguarded library with work passes by default and fails in --scope all."""
    make_repo(tmp_path, {**GOOD, "lib.py": "setup()\n"})
    monkeypatch.chdir(tmp_path)
    assert call(NO_BAZEL)[0] == 0
    code, out, _ = call([*NO_BAZEL, "--scope", "all"])
    assert code == 1
    assert out.startswith("lib.py:1: import-time-work:")


def test_pure_call_flag(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--pure-call allows the named call at module level."""
    make_repo(
        tmp_path,
        {
            "tool.py": (
                SHEBANG
                + "import mylib\nX = mylib.make()\n"
                + "def main(): ...\nif __name__ == '__main__':\n    main()\n"
            ),
        },
    )
    monkeypatch.chdir(tmp_path)
    assert call(NO_BAZEL)[0] == 1
    assert call([*NO_BAZEL, "--pure-call", "mylib.make"])[0] == 0


def test_test_pattern_replaces(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--test-pattern replaces the default test patterns."""
    make_repo(
        tmp_path,
        {
            "t_test.py": (
                SHEBANG
                + "setup()\n"
                + "def main(): ...\nif __name__ == '__main__':\n    main()\n"
            ),
        },
    )
    monkeypatch.chdir(tmp_path)
    assert call(NO_BAZEL)[0] == 0
    code, out, _ = call([*NO_BAZEL, "--test-pattern", "none/*"])
    assert code == 1
    assert out.startswith("t_test.py:2: import-time-work:")


def test_update_baseline_then_pass(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--update-baseline records debt; the next run passes with it."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call([*NO_BAZEL, "--update-baseline"])[0] == 0
    assert json.loads((tmp_path / BASELINE).read_text()) == {
        "checker": "py-import-time-work",
        "entries": {"tool.py": {"limits": {"max_violations": 1}}},
        "version": 1,
    }
    code, out, _ = call(NO_BAZEL)
    assert code == 0
    assert "1 baselined violation(s)" in out


def test_file_leaving_scope_is_stale(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A baselined file excluded from scope is reported as stale."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call([*NO_BAZEL, "--update-baseline"])[0] == 0
    code, out, _ = call([*NO_BAZEL, "--exclude", "tool.py"])
    assert code == 1
    assert any(line.startswith("stale baseline:") for line in out.splitlines())


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


def test_bad_scope_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """An invalid --scope choice exits 2 via argparse."""
    make_repo(tmp_path, GOOD)
    monkeypatch.chdir(tmp_path)
    with pytest.raises(SystemExit) as exc:
        call([*NO_BAZEL, "--scope", "nope"])
    assert exc.value.code == 2


def test_notes_printed(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Default sources without a Bazel workspace print the skip note."""
    make_repo(tmp_path, GOOD)
    monkeypatch.chdir(tmp_path)
    code, out, _ = call([])
    assert code == 0
    assert "bazel: skipped (no workspace)" in out.splitlines()


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
