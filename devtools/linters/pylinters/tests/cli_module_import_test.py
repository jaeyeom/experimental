"""End-to-end tests for the py-module-import CLI."""

import io
import json
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from pylinters.cli import module_import as cli

pytestmark = pytest.mark.skipif(
    shutil.which("git") is None, reason="git not on PATH"
)

VIOLATION = (
    "main.py:1:23: from app.types import CheckResult: import the module "
    "instead (from app import types)"
)
BAD = {
    "app/__init__.py": "",
    "app/types.py": "",
    "main.py": "from app.types import CheckResult\n",
}
GOOD_MAIN = "from app import types\n"
BASELINE = ".baselines/py-module-import.json"
PASS_WITH_DEBT = (
    "PASSED: no new module symbol import "
    "(3 file(s) scanned, 1 baselined violation(s)).\n"
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


def test_pass_exit_0(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """A clean tree exits 0 and prints the scan count."""
    make_repo(tmp_path, {**BAD, "main.py": GOOD_MAIN})
    monkeypatch.chdir(tmp_path)
    assert call([]) == (
        0,
        "PASSED: no module symbol import (3 file(s) scanned).\n",
        "",
    )


def test_violation_exit_1_prints_line(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Violations exit 1 with the violation line and the directive hint."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    code, out, err = call([])
    assert (code, err) == (1, "")
    assert out.splitlines()[0] == VIOLATION
    assert "py-module-import: allow" in out
    assert "--update-baseline" in out


def test_parse_error_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A scanned file that does not parse exits 2 naming the file."""
    make_repo(tmp_path, {"bad.py": "import (\n"})
    monkeypatch.chdir(tmp_path)
    code, _, err = call([])
    assert code == 2
    assert err.startswith("py-module-import: error:")
    assert "bad.py" in err


def test_outside_repo_path_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A path outside the repo is a usage error."""
    repo = make_repo(tmp_path / "r", BAD)
    monkeypatch.chdir(repo)
    code, _, err = call([str(tmp_path / "elsewhere.py")])
    assert code == 2
    assert "outside" in err


def test_stdin0_skips_deleted_and_non_py(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Paths git diff lists but that cannot be checked are skipped."""
    make_repo(
        tmp_path,
        {
            **BAD,
            "a.py": GOOD_MAIN,
            "README.md": "",
            "gone.py": "from app.types import X\n",
        },
    )
    (tmp_path / "gone.py").unlink()
    monkeypatch.chdir(tmp_path / "app")
    stdin = b"a.py\0README.md\0gone.py\0"
    assert call(["--stdin0"], stdin=stdin) == (
        0,
        "PASSED: no module symbol import (1 file(s) scanned).\n",
        "",
    )
    code, out, _ = call(["--stdin0"], stdin=stdin + b"main.py\0")
    assert code == 1
    assert VIOLATION in out


def test_update_baseline_then_pass_then_stale(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--update-baseline records debt; fixing it makes the entry stale."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    assert json.loads((tmp_path / BASELINE).read_text()) == {
        "checker": "py-module-import",
        "entries": {"main.py": {"limits": {"max_violations": 1}}},
        "version": 1,
    }
    assert call([]) == (
        0,
        PASS_WITH_DEBT,
        "",
    )
    (tmp_path / "main.py").write_text(GOOD_MAIN)
    code, out, _ = call([])
    assert code == 1
    assert "stale baseline: main.py: now clean, remove baseline entry" in out
    assert VIOLATION not in out


def test_baseline_limit_exceeded_prints_violations(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A file above its baselined count prints its violations."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    (tmp_path / "main.py").write_text(
        "from app.types import CheckResult\nfrom app.types import Other\n"
    )
    code, out, _ = call([])
    assert code == 1
    assert VIOLATION in out
    assert "main.py:2:23: from app.types import Other" in out


def test_deleted_baselined_file_is_stale(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A baselined file removed from git is reported stale."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    subprocess.run(["git", "rm", "-qf", "main.py"], cwd=tmp_path, check=True)
    code, out, _ = call([])
    assert code == 1
    assert (
        "stale baseline: main.py: baseline entry refers to a file that is "
        "deleted or out of scope" in out
    )


def test_baselined_file_removed_from_work_tree_is_stale(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A baselined file deleted without git rm is stale and tightened away."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    (tmp_path / "main.py").unlink()
    want = (
        "stale baseline: main.py: baseline entry refers to a file that is "
        "deleted or out of scope"
    )
    code, out, _ = call([])
    assert code == 1
    assert want in out
    code, out, _ = call(["--stdin0"], stdin=b"main.py\0")
    assert code == 1
    assert want in out
    assert call(["--tighten-baseline"])[0] == 0
    assert json.loads((tmp_path / BASELINE).read_text())["entries"] == {}


def test_subset_ignores_stale_outside_paths(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A path-list run never reports baselined files outside the list."""
    make_repo(tmp_path, {**BAD, "other.py": GOOD_MAIN})
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    (tmp_path / "main.py").write_text(GOOD_MAIN)
    assert call(["other.py"]) == (
        0,
        "PASSED: no module symbol import (1 file(s) scanned).\n",
        "",
    )
    assert call(["--stdin0"], stdin=b"other.py\0")[0] == 0
    code, out, _ = call(["main.py"])
    assert code == 1
    assert "stale baseline: main.py" in out


def test_update_with_paths_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Rewriting the baseline is a whole-tree operation."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    for flag in ("--update-baseline", "--tighten-baseline"):
        code, _, err = call([flag, "main.py"])
        assert code == 2
        assert flag in err
        code, _, err = call([flag, "--stdin0"], stdin=b"main.py\0")
        assert code == 2
    assert not (tmp_path / BASELINE).exists()


def test_update_and_tighten_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--update-baseline and --tighten-baseline are mutually exclusive."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    with pytest.raises(SystemExit) as exc:
        call(["--update-baseline", "--tighten-baseline"])
    assert exc.value.code == 2


def test_tighten_removes_stale_and_never_adds(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--tighten-baseline drops clean entries and fails on new debt."""
    make_repo(tmp_path, {**BAD, "other.py": GOOD_MAIN})
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    (tmp_path / "main.py").write_text(GOOD_MAIN)
    (tmp_path / "other.py").write_text("from app.types import CheckResult\n")
    code, out, _ = call(["--tighten-baseline"])
    assert code == 1
    assert "other.py:1:23: from app.types import CheckResult" in out
    assert json.loads((tmp_path / BASELINE).read_text())["entries"] == {}


def test_strict_false_ignores_stale(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--strict-baseline=false passes with stale entries."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    (tmp_path / "main.py").write_text(GOOD_MAIN)
    assert call(["--strict-baseline=false"])[0] == 0
    assert call(["--strict-baseline", "true"])[0] == 1


def test_baseline_flag_relative_to_cwd(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """--baseline resolves a relative path against the cwd."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path / "app")
    assert call(["--baseline", "bl.json", "--update-baseline"])[0] == 0
    assert (tmp_path / "app" / "bl.json").exists()
    assert not (tmp_path / BASELINE).exists()
    assert call(["--baseline", "bl.json"])[0] == 0


def test_malformed_baseline_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A baseline that does not parse is a configuration error."""
    make_repo(tmp_path, BAD)
    (tmp_path / ".baselines").mkdir()
    (tmp_path / BASELINE).write_text("{")
    monkeypatch.chdir(tmp_path)
    code, _, err = call([])
    assert code == 2
    assert "invalid JSON" in err


def test_config_from_pyproject(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """exempt-markers in pyproject.toml exempts a Django tree."""
    make_repo(
        tmp_path,
        {
            "pyproject.toml": "[tool.pylinters.module-import]\n"
            'exempt-markers = ["manage.py"]\n',
            "site/manage.py": "",
            "site/app/__init__.py": "",
            "site/app/models.py": "",
            "site/app/views.py": "from app.models import Thing\n",
        },
    )
    monkeypatch.chdir(tmp_path)
    assert call([]) == (
        0,
        "PASSED: no module symbol import (0 file(s) scanned).\n",
        "",
    )
    code, out, _ = call(["--exempt-marker", "setup.py"])
    assert code == 1
    assert "site/app/views.py:1:24: from app.models import Thing" in out


def test_flag_overrides(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Override flags change the effective configuration."""
    make_repo(
        tmp_path,
        {
            **BAD,
            "main.py": "from app.types import CheckResult  # X\n",
            "tools/requests.py": "",
            "x.py": "from requests import get\n",
            "pyproject.toml": "[tool.pylinters]\nroots = []\n",
        },
    )
    monkeypatch.chdir(tmp_path)
    code, out, _ = call([])
    assert code == 1
    assert VIOLATION in out
    assert call(["--allow-directive", "X"])[0] == 0
    assert call(["--exclude", "main.py"])[0] == 0
    code, out, _ = call(["--allow-directive", "X", "--root-dir", "tools"])
    assert code == 1
    assert "x.py:1:22: from requests import get" in out
    assert (
        call(
            [
                "--allow-directive",
                "X",
                "--root-dir",
                "tools",
                "--third-party",
                "requests",
            ]
        )[0]
        == 0
    )
    assert call(["--generated", "main.py", "--allow-directive", "Y"])[0] == 0


def test_not_a_git_repo_exit_2(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Outside a git work tree the CLI exits 2."""
    monkeypatch.setenv("GIT_CEILING_DIRECTORIES", str(tmp_path.parent))
    monkeypatch.chdir(tmp_path)
    code, _, err = call([])
    assert code == 2
    assert err.startswith("py-module-import: error:")


def test_version_flag(capsys: pytest.CaptureFixture[str]) -> None:
    """--version prints a version and exits 0."""
    with pytest.raises(SystemExit) as exc:
        call(["--version"])
    assert exc.value.code == 0
    assert capsys.readouterr().out.strip()


def test_directory_path_expands(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A directory argument checks the tracked .py files below it."""
    make_repo(tmp_path, {**BAD, "pkg/main.py": BAD["main.py"]})
    monkeypatch.chdir(tmp_path)
    code, out, _ = call(["pkg"])
    assert code == 1
    assert "pkg/main.py:1:23: from app.types import CheckResult" in out
    assert VIOLATION not in out.splitlines()
    code, out, _ = call(["--stdin0"], stdin=b"pkg\0")
    assert code == 1
    assert "pkg/main.py:1:23" in out
    code, out, _ = call(["."])
    assert code == 1
    assert VIOLATION in out.splitlines()


def test_over_baseline_prints_reason(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A file above its baselined count says what the baseline allows."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    (tmp_path / "main.py").write_text(
        "from app.types import CheckResult\nfrom app.types import Other\n"
    )
    code, out, _ = call([])
    assert code == 1
    assert "main.py: 2 violation(s), baseline allows 1" in out
    code, out, _ = call(["--tighten-baseline"])
    assert code == 1
    assert "main.py: 2 violation(s), baseline allows 1" in out
    assert "main.py:2:23: from app.types import Other" in out


def test_pass_with_baselined_debt(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A pass with baselined debt says so; rewrites name the file.

    The written path is cwd-relative only when it lies under the cwd.
    """
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path / "app")
    abs_baseline = (tmp_path / BASELINE).resolve()
    assert call(["--update-baseline"]) == (
        0,
        f"baseline written to {abs_baseline}\n" + PASS_WITH_DEBT,
        "",
    )
    assert call([]) == (
        0,
        PASS_WITH_DEBT,
        "",
    )
    monkeypatch.chdir(tmp_path)
    code, out, _ = call(["--tighten-baseline"])
    assert code == 0
    assert out.startswith(
        "baseline written to .baselines/py-module-import.json\n"
    )


def test_stale_only_hint_suggests_tighten(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """With only stale entries, the hint points at --tighten-baseline."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    (tmp_path / "main.py").write_text(GOOD_MAIN)
    code, out, _ = call([])
    assert code == 1
    assert "--tighten-baseline" in out
    assert "--update-baseline" not in out
    assert "Import the module instead" not in out


@pytest.mark.parametrize("marker", ["manage", "sub/manage.py"])
def test_exempt_marker_flag_validated(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, marker: str
) -> None:
    """--exempt-marker takes plain .py file names, like the config."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    code, _, err = call(["--exempt-marker", marker])
    assert code == 2
    assert "--exempt-marker" in err


def test_excluded_baselined_file_is_out_of_scope(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """A baselined file that is now excluded is not called deleted."""
    make_repo(tmp_path, BAD)
    monkeypatch.chdir(tmp_path)
    assert call(["--update-baseline"])[0] == 0
    code, out, _ = call(["--exclude", "main.py"])
    assert code == 1
    assert (
        "stale baseline: main.py: baseline entry refers to a file that is "
        "deleted or out of scope" in out
    )


def test_exclude_help_mentions_configured(
    capsys: pytest.CaptureFixture[str],
) -> None:
    """--exclude help says it adds to the configured excludes."""
    with pytest.raises(SystemExit):
        call(["--help"])
    help_text = " ".join(capsys.readouterr().out.split())
    assert "added to the configured excludes (shared and checker)" in help_text


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
