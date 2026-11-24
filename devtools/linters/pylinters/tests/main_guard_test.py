"""Tests for pylinters.main_guard."""

import sys
from pathlib import Path

import pytest
from pylinters import main_guard
from pylinters.core import entries, errors, files

GUARD = "if __name__ == '__main__':\n"
REQUIRE = main_guard.Config(require_test_runner=True)


def run(
    tree: dict[str, str],
    found: dict[str, tuple[entries.Kind, tuple[str, ...]]] | None = None,
    cfg: main_guard.Config | None = None,
    paths: set[str] | None = None,
) -> main_guard.Result:
    """Check an in-memory tree whose entries are given directly."""
    fs = files.GitResolver("/repo", tree)
    disc = entries.Discovery(
        entries={
            p: entries.Entry(p, kind, reasons)
            for p, (kind, reasons) in (found or {}).items()
        }
    )
    return main_guard.check(
        fs,
        cfg or main_guard.Config(),
        disc,
        read=lambda p: tree[p].encode(),
        paths=paths,
    )


def rules(result: main_guard.Result) -> list[tuple[str, int, str]]:
    """Return (path, line, rule) per violation."""
    return [(v.path, v.line, v.rule) for v in result.violations]


SCRIPT: tuple[entries.Kind, tuple[str, ...]] = ("script", ("executable",))
TEST: tuple[entries.Kind, tuple[str, ...]] = ("test", ("bazel //pkg:t_test",))

# Rule 1: missing-guard


def test_entry_without_guard_flagged() -> None:
    """An entry with no module-level guard is flagged on line 1."""
    res = run({"a.py": "def main():\n    pass\n"}, {"a.py": SCRIPT})
    assert rules(res) == [("a.py", 1, "missing-guard")]
    assert "executable" in str(res.violations[0])
    assert str(res.violations[0]).startswith("a.py:1: missing-guard: ")


def test_entry_with_guard_ok() -> None:
    """An entry with a guard passes."""
    res = run(
        {"a.py": "def main(): ...\n" + GUARD + "    main()\n"},
        {"a.py": SCRIPT},
    )
    assert rules(res) == []


def test_non_entry_without_guard_ok() -> None:
    """A file nobody runs needs no guard."""
    assert rules(run({"lib.py": "x = 1\n"})) == []


def test_file_directive_exempts() -> None:
    """The directive before the first statement exempts the file."""
    src = '#!/usr/bin/env python3\n"""Doc."""\n# py-main-guard: allow -- exec()d\nx = 1\n'
    assert rules(run({"a.py": src}, {"a.py": SCRIPT})) == []


def test_directive_in_empty_file() -> None:
    """A file with only comments can carry the directive anywhere."""
    assert (
        rules(run({"a.py": "# py-main-guard: allow\n"}, {"a.py": SCRIPT}))
        == []
    )


def test_directive_after_first_statement_ignored() -> None:
    """The file directive must come before code."""
    src = "x = 1\n# py-main-guard: allow\n"
    assert rules(run({"a.py": src}, {"a.py": SCRIPT})) == [
        ("a.py", 1, "missing-guard")
    ]


def test_directive_in_string_ignored() -> None:
    """A string containing the directive is not a comment."""
    src = '"""py-main-guard: allow"""\nx = 1\n'
    assert rules(run({"a.py": src}, {"a.py": SCRIPT})) == [
        ("a.py", 1, "missing-guard")
    ]


def test_custom_directive() -> None:
    """The directive token is configurable."""
    cfg = main_guard.Config(allow_directive="ok-run")
    assert (
        rules(run({"a.py": "# ok-run\nx = 1\n"}, {"a.py": SCRIPT}, cfg)) == []
    )


# Rule 2: dropped-exit


def test_bare_pytest_main_flagged() -> None:
    """pytest.main() without propagation is flagged at the call."""
    src = "import pytest\n" + GUARD + "    pytest.main([__file__])\n"
    res = run({"a_test.py": src})
    assert rules(res) == [("a_test.py", 3, "dropped-exit")]
    assert "sys.exit(pytest.main(...))" in str(res.violations[0])


def test_sys_exit_pytest_main_ok() -> None:
    """sys.exit(pytest.main(...)) propagates."""
    src = (
        "import sys, pytest\n"
        + GUARD
        + "    sys.exit(pytest.main([__file__]))\n"
    )
    assert rules(run({"a_test.py": src})) == []


def test_raise_systemexit_ok() -> None:
    """Raise SystemExit(pytest.main(...)) propagates."""
    src = "import pytest\n" + GUARD + "    raise SystemExit(pytest.main())\n"
    assert rules(run({"a_test.py": src})) == []


@pytest.mark.parametrize("name", ["exit", "quit"])
def test_builtin_exit_ok(name: str) -> None:
    """The builtin exit() and quit() raise SystemExit with the result."""
    src = f"import pytest\n{GUARD}    {name}(pytest.main())\n"
    assert rules(run({"a_test.py": src})) == []


def test_imported_exit_shadowing_builtin_flagged() -> None:
    """An imported ``exit`` that is not sys.exit does not propagate."""
    src = (
        "import pytest\nfrom os import _exit as exit\n"
        + GUARD
        + "    exit(pytest.main())\n"
    )
    assert rules(run({"a_test.py": src})) == [("a_test.py", 4, "dropped-exit")]


def test_returned_from_function_in_guard_ok() -> None:
    """``return pytest.main()`` hands the result to the caller."""
    src = (
        "import sys, pytest\n"
        + GUARD
        + "    def run():\n        return pytest.main()\n"
        + "    sys.exit(run())\n"
    )
    assert rules(run({"a_test.py": src})) == []


def test_assigned_result_flagged() -> None:
    """Data flow is not followed: rc = pytest.main() is flagged."""
    src = (
        "import sys, pytest\n"
        + GUARD
        + "    rc = pytest.main()\n    sys.exit(rc)\n"
    )
    assert rules(run({"a_test.py": src})) == [("a_test.py", 3, "dropped-exit")]


def test_from_pytest_import_main_flagged() -> None:
    """A bare main imported from pytest is a runner too."""
    src = "from pytest import main\n" + GUARD + "    main()\n"
    assert rules(run({"a_test.py": src})) == [("a_test.py", 3, "dropped-exit")]


def test_unrelated_main_not_flagged() -> None:
    """A local main() is not pytest's."""
    src = "def main(): ...\n" + GUARD + "    main()\n"
    assert rules(run({"a.py": src})) == []


def test_unittest_exit_false_flagged() -> None:
    """unittest.main(exit=False) swallows the result."""
    src = "import unittest\n" + GUARD + "    unittest.main(exit=False)\n"
    assert rules(run({"a_test.py": src})) == [("a_test.py", 3, "dropped-exit")]


def test_unittest_default_ok() -> None:
    """unittest.main() exits by itself."""
    src = "import unittest\n" + GUARD + "    unittest.main()\n"
    assert rules(run({"a_test.py": src})) == []


def test_call_outside_guard_ignored() -> None:
    """Only guard bodies are checked."""
    src = "import pytest\ndef f():\n    pytest.main()\n"
    assert rules(run({"a.py": src})) == []


def test_nested_in_guard_flagged() -> None:
    """A runner nested in a block inside the guard is still checked."""
    src = "import pytest\n" + GUARD + "    if True:\n        pytest.main()\n"
    assert rules(run({"a_test.py": src})) == [("a_test.py", 4, "dropped-exit")]


def test_directive_on_closing_paren_line() -> None:
    """The directive on any line of the statement exempts it."""
    src = (
        "import pytest\n"
        + GUARD
        + "    pytest.main(\n        [__file__]\n    )  # py-main-guard: allow\n"
    )
    assert rules(run({"a_test.py": src})) == []


# Rule 3: missing-test-runner


def test_test_entry_missing_runner_flagged_when_required() -> None:
    """A test entry whose guard runs no runner is flagged at the guard."""
    src = "x = 1\n" + GUARD + "    print('hi')\n"
    res = run({"t_test.py": src}, {"t_test.py": TEST}, REQUIRE)
    assert rules(res) == [("t_test.py", 2, "missing-test-runner")]
    assert "bazel //pkg:t_test" in str(res.violations[0])


def test_test_entry_with_runner_ok() -> None:
    """sys.exit(pytest.main(...)) satisfies rule 3."""
    src = (
        "import sys, pytest\n"
        + GUARD
        + "    sys.exit(pytest.main([__file__]))\n"
    )
    assert rules(run({"t_test.py": src}, {"t_test.py": TEST}, REQUIRE)) == []


def test_unittest_main_satisfies_runner() -> None:
    """unittest.main() satisfies rule 3."""
    src = "import unittest\n" + GUARD + "    unittest.main()\n"
    assert rules(run({"t_test.py": src}, {"t_test.py": TEST}, REQUIRE)) == []


def test_absltest_main_satisfies_runner() -> None:
    """absltest.main() exits by itself, like unittest.main()."""
    src = (
        "from absl.testing import absltest\n" + GUARD + "    absltest.main()\n"
    )
    assert rules(run({"t_test.py": src}, {"t_test.py": TEST}, REQUIRE)) == []


def test_dropped_runner_is_rule2_only() -> None:
    """A dropped runner is reported once, as dropped-exit."""
    src = "import pytest\n" + GUARD + "    pytest.main()\n"
    res = run({"t_test.py": src}, {"t_test.py": TEST}, REQUIRE)
    assert rules(res) == [("t_test.py", 3, "dropped-exit")]


def test_not_flagged_when_not_required() -> None:
    """Rule 3 is off by default."""
    src = GUARD + "    print('hi')\n"
    assert rules(run({"t_test.py": src}, {"t_test.py": TEST})) == []


def test_test_entry_without_guard_only_rule1() -> None:
    """No guard at all: only missing-guard."""
    res = run({"t_test.py": "x = 1\n"}, {"t_test.py": TEST}, REQUIRE)
    assert rules(res) == [("t_test.py", 1, "missing-guard")]


def test_script_entry_not_rule3() -> None:
    """Rule 3 applies to test entries only."""
    src = GUARD + "    print('hi')\n"
    assert rules(run({"a.py": src}, {"a.py": SCRIPT}, REQUIRE)) == []


def test_file_directive_exempts_rule3() -> None:
    """The file directive also exempts rule 3."""
    src = "# py-main-guard: allow\n" + GUARD + "    print('hi')\n"
    assert rules(run({"t_test.py": src}, {"t_test.py": TEST}, REQUIRE)) == []


# check() scope


def test_check_counts_and_paths_subset() -> None:
    """Counts cover scanned files; paths limits what is scanned."""
    tree = {"a.py": "x = 1\n", "b.py": "y = 2\n"}
    found = {"a.py": SCRIPT, "b.py": SCRIPT}
    res = run(tree, found)
    assert res.counts == {"a.py": 1, "b.py": 1}
    assert res.files_scanned == 2
    sub = run(tree, found, paths={"b.py"})
    assert sub.counts == {"b.py": 1}


def test_excluded_and_generated_not_scanned() -> None:
    """Excluded and generated files are out of scope."""
    tree = {"gen/x_pb2.py": "x = 1\n", "skip/a.py": "x = 1\n"}
    found = {"gen/x_pb2.py": SCRIPT, "skip/a.py": SCRIPT}
    res = run(tree, found, main_guard.Config(exclude=("skip/**",)))
    assert res.violations == []
    assert res.files_scanned == 0


def test_parse_error_raises() -> None:
    """A file that does not parse is an error, not a skip."""
    with pytest.raises(errors.ParseError):
        run({"a.py": "def (\n"})


# Configuration


def _pyproject(tmp_path: Path, body: str) -> str:
    path = tmp_path / "pyproject.toml"
    path.write_text(body)
    return str(path)


def test_load_config_keys(tmp_path: Path) -> None:
    """Every key round-trips; exclude adds to the shared list."""
    cfg = main_guard.load_config(
        _pyproject(
            tmp_path,
            '[tool.pylinters]\nexclude = ["a/**"]\n'
            "[tool.pylinters.entries]\n"
            'entry-sources = ["globs"]\nscript-globs = ["bin/*.py"]\n'
            'test-globs = ["**/*_test.py"]\nbazel-dir = "sub"\n'
            "[tool.pylinters.main-guard]\n"
            'require-test-runner = true\nexclude = ["b/**"]\n'
            'allow-directive = "ok"\n',
        )
    )
    assert cfg == main_guard.Config(
        entries=entries.EntryConfig(
            sources=("globs",),
            script_globs=("bin/*.py",),
            test_globs=("**/*_test.py",),
            bazel_dir="sub",
        ),
        exclude=("a/**", "b/**"),
        require_test_runner=True,
        allow_directive="ok",
    )


@pytest.mark.parametrize(
    ("key", "value"),
    [
        ("entry-sources", '["globs"]'),
        ("script-globs", '["bin/*.py"]'),
        ("test-globs", '["*_test.py"]'),
        ("bazel-dir", '"sub"'),
    ],
)
def test_moved_entry_key_rejected(
    tmp_path: Path, key: str, value: str
) -> None:
    """Moved entry keys under main-guard raise ConfigError pointing to entries."""
    path = _pyproject(
        tmp_path, f"[tool.pylinters.main-guard]\n{key} = {value}\n"
    )
    with pytest.raises(
        errors.ConfigError, match=r"moved to \[tool\.pylinters\.entries\]"
    ):
        main_guard.load_config(path)


def test_load_config_defaults(tmp_path: Path) -> None:
    """A missing pyproject.toml yields the defaults."""
    assert (
        main_guard.load_config(str(tmp_path / "pyproject.toml"))
        == main_guard.Config()
    )


def test_unknown_key(tmp_path: Path) -> None:
    """A typo in the subtable is an error naming the key."""
    path = _pyproject(
        tmp_path, "[tool.pylinters.main-guard]\nrequire = true\n"
    )
    with pytest.raises(errors.ConfigError, match="require"):
        main_guard.load_config(path)


def test_require_test_runner_must_be_bool(tmp_path: Path) -> None:
    """require-test-runner must be a boolean."""
    path = _pyproject(
        tmp_path, '[tool.pylinters.main-guard]\nrequire-test-runner = "yes"\n'
    )
    with pytest.raises(errors.ConfigError, match="require-test-runner"):
        main_guard.load_config(path)


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
