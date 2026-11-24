"""Tests for pylinters.import_time_work."""

from __future__ import annotations

import ast
import sys
from collections.abc import Collection
from pathlib import Path

import pytest
from pylinters import import_time_work
from pylinters.core import entries, errors, files, guards, imports
from pylinters.import_time_work import (
    DEFAULT_DIRECTIVE,
    DEFAULT_PURE_CALLS,
    DEFAULT_TEST_PATTERNS,
    Config,
    Violation,
    find_violations,
    findings,
)

GUARD = "if __name__ == '__main__':\n"
SCRIPT: tuple[entries.Kind, tuple[str, ...]] = ("script", ("executable",))
TEST: tuple[entries.Kind, tuple[str, ...]] = ("test", ("bazel //pkg:t_test",))


def got(
    src: str, pure: Collection[str] = DEFAULT_PURE_CALLS
) -> list[tuple[int, str]]:
    """Return ``(lineno, what)`` pairs for findings in ``src``."""
    return [(f.node.lineno, f.what) for f in findings(ast.parse(src), pure)]


def check_src(src: str, cfg: Config | None = None) -> list[tuple[int, str]]:
    """Parse ``src`` and return ``(line, message)`` violation pairs."""
    data = src.encode()
    tree, comments = imports.parse_module(data, "m.py")
    colons = guards.colon_positions(data)
    viols = find_violations(tree, comments, colons, "m.py", cfg or Config())
    return [(v.line, v.message) for v in viols]


@pytest.mark.parametrize(
    "src",
    [
        "import os\nfrom sys import path\n",
        "@app.route(make())\ndef handler():\n    pass\n",
        "@dec(make())\nclass Thing:\n    pass\n",
        '"""Module docstring."""\n',
        '__all__ = ["a"]\nN = 42\nD = {"a": 1}\n',
        "X = A | B\n",
        "Alias = pkg.Thing\n",
        'import re\n_RE = re.compile("x")\n',
        "import logging\nlog = logging.getLogger(__name__)\n",
        'import re as r\nP = r.compile("x")\n',
        'from typing import TypeVar\nT = TypeVar("T")\n',
        "_S = object()\n",
        "from typing import TYPE_CHECKING\nif TYPE_CHECKING:\n    import os\n",
        "try:\n    import yaml\nexcept ImportError:\n    yaml = None\n",
        "x = 1\ndel x\n",
        "D = dict(k=lambda: load())\n",
        'if __name__ == "__main__":\n    main()\n',
    ],
)
def test_allowed(src: str) -> None:
    """Allowed top-level constructs produce no findings."""
    assert got(src) == []


@pytest.mark.skipif(
    sys.version_info < (3, 12), reason="type statement requires Python 3.12+"
)
def test_type_alias_allowed() -> None:
    """Python 3.12+ ``type`` alias statements are allowed."""
    assert got("type X = int\n") == []


def test_nested_guard_body_allowed() -> None:
    """A ``__main__`` guard nested in ``try`` still skips its body."""
    src = (
        "try:\n"
        '    if __name__ == "__main__":\n'
        "        main()\n"
        "except Exception:\n"
        "    pass\n"
    )
    assert got(src) == []


def test_flagged_sys_path_insert() -> None:
    """A bare impure call statement is flagged with its spelled callee."""
    assert got('import sys\nsys.path.insert(0, "x")\n') == [
        (2, "`sys.path.insert(...)`")
    ]


def test_flagged_assignment_call() -> None:
    """An assignment calling an impure function is flagged."""
    assert got("config = load()\n") == [(1, "`load(...)`")]


def test_flagged_pure_call_with_impure_arg() -> None:
    """Arguments of a pure call are still checked for impure calls."""
    assert got("frozenset(load())\n") == [(1, "`load(...)`")]


def test_flagged_chained_call() -> None:
    """A chained call reports the outer call first in pre-order."""
    assert got("Path(__file__).resolve()\n") == [
        (1, "`Path(...).resolve(...)`")
    ]


def test_flagged_comprehension() -> None:
    """Comprehensions are walked like any other expression."""
    assert got("X = [f(x) for x in Y]\n") == [(1, "`f(...)`")]


def test_flagged_if_test_and_body() -> None:
    """An ``if`` with an impure test flags both the test and its body."""
    src = 'import os\nif os.environ.get("A"):\n    f()\n'
    assert got(src) == [
        (2, "`if` on `os.environ.get(...)`"),
        (3, "`f(...)`"),
    ]


@pytest.mark.parametrize(
    ("src", "expected"),
    [
        ("for x in y:\n    pass\n", [(1, "a `for` statement")]),
        ("while cond:\n    pass\n", [(1, "a `while` statement")]),
        ("with ctx:\n    pass\n", [(1, "a `with` statement")]),
        ("assert cond\n", [(1, "a `assert` statement")]),
        ("raise RuntimeError\n", [(1, "a `raise` statement")]),
        ("global x\n", [(1, "a `global` statement")]),
        ("del a.b\n", [(1, "a `del` statement")]),
    ],
)
def test_flagged_statements(src: str, expected: list[tuple[int, str]]) -> None:
    """Imperative statements are flagged with their keyword."""
    assert got(src) == expected


def test_flagged_for_body_also_walked() -> None:
    """A flagged ``for`` statement whose body does work gives two findings."""
    src = "for x in y:\n    f()\n"
    assert got(src) == [(1, "a `for` statement"), (2, "`f(...)`")]


def test_flagged_except_type() -> None:
    """An ``except`` handler with an impure type expression is flagged."""
    src = "try:\n    pass\nexcept make():\n    pass\n"
    found = findings(ast.parse(src), DEFAULT_PURE_CALLS)
    assert len(found) == 1
    assert isinstance(found[0].node, ast.ExceptHandler)
    assert (found[0].node.lineno, found[0].what) == (
        3,
        "`except` on `make(...)`",
    )


def test_flagged_annassign_annotation() -> None:
    """An impure annotation is flagged unless future annotations are active."""
    assert got("x: f() = 1\n") == [(1, "`f(...)`")]
    assert got("from __future__ import annotations\nx: f() = 1\n") == []


def test_flagged_lambda_default() -> None:
    """Lambda default values are walked, though lambda bodies are not."""
    assert got("g = lambda a=load(): a\n") == [(1, "`load(...)`")]
    assert got("g = lambda *, a=load(): a\n") == [(1, "`load(...)`")]


def test_flagged_main_guard_else() -> None:
    """The ``else`` branch of a ``__main__`` guard is still walked."""
    src = 'if __name__ == "__main__":\n    main()\nelse:\n    setup()\n'
    assert got(src) == [(4, "`setup(...)`")]


def test_extra_pure_call() -> None:
    """Callers can extend the pure-calls allowlist."""
    src = "import mylib\nmylib.make()\n"
    assert got(src) == [(2, "`mylib.make(...)`")]
    assert got(src, DEFAULT_PURE_CALLS | {"mylib.make"}) == []


def test_checker_name() -> None:
    """CHECKER constant matches the expected checker name."""
    assert import_time_work.CHECKER == "import-time-work"


def test_defaults() -> None:
    """Default directive and test patterns match the specification."""
    assert DEFAULT_DIRECTIVE == "py-import-time-work: allow"
    assert DEFAULT_TEST_PATTERNS == (
        "**/test_*.py",
        "**/*_test.py",
        "**/conftest.py",
        "**/tests/**",
    )


@pytest.mark.parametrize(
    ("src", "line", "what"),
    [
        ('import sys\nsys.path += ["x"]\n', 2, "assignment to `sys.path`"),
        (
            'import sys\nsys.path[:0] = ["x"]\n',
            2,
            "assignment to `sys.path[:0]`",
        ),
        (
            'import os\nos.environ["X"] = "1"\n',
            2,
            "assignment to `os.environ['X']`",
        ),
        (
            'import sys\nsys.modules["m"] = s\n',
            2,
            "assignment to `sys.modules['m']`",
        ),
        (
            "import logging\nlogging.root.level = 10\n",
            2,
            "assignment to `logging.root.level`",
        ),
        (
            'import sys\na, sys.argv[0] = 1, "x"\n',
            2,
            "assignment to `sys.argv[0]`",
        ),
        (
            "import cfg\ncfg.x: int = 1\n",
            2,
            "assignment to `cfg.x`",
        ),
        (
            'import sys\n*sys.argv[:1], a = ["x", "y"]\n',
            2,
            "assignment to `sys.argv[:1]`",
        ),
    ],
)
def test_mutation_flagged(src: str, line: int, what: str) -> None:
    """Mutating an imported name's attribute or subscript is flagged."""
    viols = check_src(src)
    assert len(viols) == 1
    assert viols[0][0] == line
    assert viols[0][1].startswith(f"{what} at module level")


def test_mutation_of_relative_import_flagged() -> None:
    """Mutating a name bound by a relative import is flagged."""
    viols = check_src("from . import conf\nconf.x = 1\n")
    assert len(viols) == 1
    assert viols[0][0] == 2
    assert viols[0][1].startswith("assignment to `conf.x` at module level")


def test_mutation_of_imported_and_assigned_name_flagged() -> None:
    """A name that is both imported and assigned still counts as imported."""
    viols = check_src('import cfg\ncfg = {}\ncfg["k"] = 1\n')
    assert len(viols) == 1
    assert viols[0][0] == 3
    assert viols[0][1].startswith("assignment to `cfg['k']` at module level")


@pytest.mark.parametrize(
    "src",
    [
        '_REG = {}\n_REG["x"] = 1\n',
        '__all__ = []\n__all__ += ["y"]\n',
    ],
)
def test_mutation_of_local_name_allowed(src: str) -> None:
    """Mutating a module's own non-imported name is allowed."""
    assert check_src(src) == []


def test_mutation_of_local_assigned_from_call_only_flags_call() -> None:
    """Mutating a local variable created by a call only flags the call."""
    viols = check_src("CONFIG = Box()\nCONFIG.debug = True\n")
    assert len(viols) == 1
    assert viols[0][0] == 1
    assert viols[0][1].startswith("`Box(...)` at module level")


def test_one_finding_per_statement_prefers_impure_call() -> None:
    """An impure call in a mutating statement yields only the call finding."""
    viols = check_src("import sys\nsys.path[f()] = 1\n")
    assert len(viols) == 1
    assert viols[0][0] == 2
    assert viols[0][1].startswith("`f(...)` at module level")


def test_directive_exempts_multiline_statement() -> None:
    """An allow comment on a closing line exempts the simple statement."""
    src = 'import sys\nsys.path.insert(\n    0, "x"\n)  # py-import-time-work: allow\n'
    assert check_src(src) == []


def test_directive_in_string_ignored() -> None:
    """A directive inside a string literal does not exempt the statement."""
    viols = check_src('x = "# py-import-time-work: allow"; f()\n')
    assert len(viols) == 1
    assert viols[0][0] == 1
    assert viols[0][1].startswith("`f(...)` at module level")


def test_directive_on_compound_header_does_not_exempt_body() -> None:
    """An allow comment on a ``with`` header does not exempt its body."""
    src = "with open(p) as h:  # py-import-time-work: allow\n    data = h.read()\n"
    viols = check_src(src)
    assert len(viols) == 1
    assert viols[0][0] == 2
    assert viols[0][1].startswith("`h.read(...)` at module level")


def test_directive_in_body_does_not_exempt_header() -> None:
    """An allow comment on a body statement does not exempt the ``for``."""
    src = "for x in y:\n    f()  # py-import-time-work: allow\n"
    viols = check_src(src)
    assert len(viols) == 1
    assert viols[0][0] == 1
    assert viols[0][1].startswith("a `for` statement at module level")


def test_custom_allow_directive() -> None:
    """A custom ``allow_directive`` in ``Config`` is honoured."""
    src = "f()  # ok-here\n"
    assert len(check_src(src)) == 1
    assert check_src(src, Config(allow_directive="ok-here")) == []


def test_violation_str() -> None:
    """``Violation.__str__`` renders in the standard checker format."""
    assert str(Violation("a.py", 3, "m")) == "a.py:3: import-time-work: m"


def run(
    tree: dict[str, str],
    found: dict[str, tuple[entries.Kind, tuple[str, ...]]] | None = None,
    cfg: Config | None = None,
    paths: set[str] | None = None,
) -> import_time_work.Result:
    """Check an in-memory tree whose entries are given directly."""
    fs = files.GitResolver("/repo", tree)
    disc = entries.Discovery(
        entries={
            p: entries.Entry(p, kind, reasons)
            for p, (kind, reasons) in (found or {}).items()
        }
    )
    return import_time_work.check(
        fs,
        cfg or Config(),
        disc,
        read=lambda p: tree[p].encode(),
        paths=paths,
    )


def _pyproject(tmp_path: Path, body: str) -> str:
    path = tmp_path / "pyproject.toml"
    path.write_text(body)
    return str(path)


# Tree check: entry scope


def test_entry_scope_guarded_non_entry_flagged() -> None:
    """A guarded non-entry with top-level work is checked and flagged."""
    src = f"setup()\n{GUARD}    main()\n"
    res = run({"a.py": src})
    assert [(v.path, v.line) for v in res.violations] == [("a.py", 1)]
    assert res.counts == {"a.py": 1}
    assert res.files_checked == 1


def test_entry_scope_script_entry_without_guard_checked() -> None:
    """A script entry without a guard is checked in entry scope."""
    res = run({"a.py": "setup()\n"}, {"a.py": SCRIPT})
    assert [(v.path, v.line) for v in res.violations] == [("a.py", 1)]
    assert res.counts == {"a.py": 1}
    assert res.files_checked == 1


def test_entry_scope_unguarded_non_entry_not_checked() -> None:
    """An unguarded non-entry library is not checked in entry scope."""
    res = run({"lib.py": "setup()\n"})
    assert res.violations == []
    assert res.counts == {}
    assert res.files_checked == 0


def test_entry_scope_prefilter_false_positive_skipped() -> None:
    """A non-entry mentioning ``__main__`` without a guard is not checked."""
    res = run({"lib.py": "# __main__\nsetup()\n"})
    assert res.violations == []
    assert res.counts == {}
    assert res.files_checked == 0


# Tree check: all scope


def test_all_scope_checks_unguarded_library() -> None:
    """In ``all`` scope, an unguarded non-entry library is flagged."""
    res = run({"lib.py": "setup()\n"}, cfg=Config(scope="all"))
    assert [(v.path, v.line) for v in res.violations] == [("lib.py", 1)]
    assert res.counts == {"lib.py": 1}
    assert res.files_checked == 1


# Tree check: always skipped


@pytest.mark.parametrize("scope", ["entry", "all"])
@pytest.mark.parametrize(
    ("path", "found"),
    [
        ("pkg/tests/x.py", None),
        ("a_test.py", None),
        ("test_a.py", None),
        ("conftest.py", None),
        ("check_x.py", {"check_x.py": TEST}),
    ],
)
def test_tests_skipped_in_both_scopes(
    scope: str,
    path: str,
    found: dict[str, tuple[entries.Kind, tuple[str, ...]]] | None,
) -> None:
    """Test files and test-kind entries are skipped in both scopes."""
    src = f"setup()\n{GUARD}    main()\n"
    cfg = Config(scope="all" if scope == "all" else "entry")
    res = run({path: src}, found, cfg)
    assert res.violations == []
    assert res.counts == {}
    assert res.files_checked == 0


@pytest.mark.parametrize("scope", ["entry", "all"])
def test_dunder_main_skipped(scope: str) -> None:
    """``__main__.py`` files are skipped in both scopes."""
    cfg = Config(scope="all" if scope == "all" else "entry")
    res = run(
        {"pkg/__main__.py": "main()\n"},
        {"pkg/__main__.py": SCRIPT},
        cfg,
    )
    assert res.violations == []
    assert res.counts == {}
    assert res.files_checked == 0


# Tree check: filtering, compound statements, and errors


def test_paths_filter_limits_reported_files() -> None:
    """``paths`` limits checking to the specified files."""
    tree = {
        "a.py": f"setup()\n{GUARD}    main()\n",
        "b.py": f"setup()\n{GUARD}    main()\n",
    }
    res = run(tree, paths={"a.py"})
    assert [v.path for v in res.violations] == ["a.py"]
    assert res.counts == {"a.py": 1}
    assert res.files_checked == 1


def test_exclude_and_generated_skipped() -> None:
    """Excluded and generated files are skipped."""
    src = f"setup()\n{GUARD}    main()\n"
    tree = {"gen/x_pb2.py": src, "skip/a.py": src}
    res = run(tree, cfg=Config(exclude=("skip/**",)))
    assert res.violations == []
    assert res.counts == {}
    assert res.files_checked == 0


def test_parse_error_propagates() -> None:
    """An unparseable in-scope file raises ``ParseError``."""
    with pytest.raises(errors.ParseError):
        run({"a.py": "def (\n"}, {"a.py": SCRIPT})


def test_missing_file_on_disk_skipped() -> None:
    """A tracked file missing on disk is skipped."""
    fs = files.GitResolver("/repo", {"gone.py": ""})
    disc = entries.Discovery(
        entries={
            "gone.py": entries.Entry("gone.py", "script", ("executable",))
        }
    )

    def missing(_path: str) -> bytes:
        raise FileNotFoundError

    res = import_time_work.check(fs, Config(), disc, read=missing)
    assert res.violations == []
    assert res.counts == {}
    assert res.files_checked == 0


def test_files_checked_counts_checked_files() -> None:
    """``files_checked`` equals the number of checked files."""
    tree = {
        "clean.py": f"def main(): pass\n{GUARD}    main()\n",
        "dirty.py": f"setup()\n{GUARD}    main()\n",
        "lib.py": "setup()\n",
    }
    res = run(tree)
    assert res.counts == {"clean.py": 0, "dirty.py": 1}
    assert res.files_checked == 2


def test_check_compound_statement_without_directive() -> None:
    """Compound statements work when no allow directive is present."""
    src = f"for x in y:\n    pass\n{GUARD}    main()\n"
    res = run({"a.py": src})
    assert [(v.path, v.line) for v in res.violations] == [("a.py", 1)]


def test_check_compound_statement_with_directive() -> None:
    """Compound statements honour allow directives during ``check``."""
    src = (
        f"for x in y:  # py-import-time-work: allow\n"
        f"    pass\n{GUARD}    main()\n"
    )
    res = run({"a.py": src})
    assert res.violations == []
    assert res.counts == {"a.py": 0}


# Configuration


def test_load_config_defaults(tmp_path: Path) -> None:
    """A missing pyproject.toml yields the default Config."""
    assert (
        import_time_work.load_config(str(tmp_path / "pyproject.toml"))
        == Config()
    )


def test_load_config_all_keys(tmp_path: Path) -> None:
    """Every config key round-trips; exclude and pure-calls extend defaults."""
    cfg = import_time_work.load_config(
        _pyproject(
            tmp_path,
            '[tool.pylinters]\nexclude = ["a/**"]\n'
            "[tool.pylinters.entries]\n"
            'entry-sources = ["globs"]\nscript-globs = ["bin/*.py"]\n'
            'test-globs = ["t/*.py"]\nbazel-dir = "sub"\n'
            "[tool.pylinters.import-time-work]\n"
            'scope = "all"\n'
            'test-patterns = ["**/custom_test.py"]\n'
            'pure-calls = ["mylib.make"]\n'
            'exclude = ["b/**"]\n'
            'allow-directive = "ok"\n',
        )
    )
    assert cfg == Config(
        entries=entries.EntryConfig(
            sources=("globs",),
            script_globs=("bin/*.py",),
            test_globs=("t/*.py",),
            bazel_dir="sub",
        ),
        scope="all",
        test_patterns=("**/custom_test.py",),
        pure_calls=DEFAULT_PURE_CALLS | {"mylib.make"},
        exclude=("a/**", "b/**"),
        allow_directive="ok",
    )


def test_load_config_invalid_scope(tmp_path: Path) -> None:
    """An invalid ``scope`` value raises ``ConfigError``."""
    path = _pyproject(
        tmp_path, '[tool.pylinters.import-time-work]\nscope = "some"\n'
    )
    with pytest.raises(errors.ConfigError, match="scope"):
        import_time_work.load_config(path)


def test_load_config_unknown_key(tmp_path: Path) -> None:
    """An unknown key in the subtable raises ``ConfigError``."""
    path = _pyproject(
        tmp_path, '[tool.pylinters.import-time-work]\nunknown = "x"\n'
    )
    with pytest.raises(errors.ConfigError, match="unknown"):
        import_time_work.load_config(path)


def test_load_config_pure_calls_extends_defaults(tmp_path: Path) -> None:
    """``pure-calls`` is added to ``DEFAULT_PURE_CALLS``."""
    cfg = import_time_work.load_config(
        _pyproject(
            tmp_path,
            '[tool.pylinters.import-time-work]\npure-calls = ["mylib.make"]\n',
        )
    )
    assert cfg.pure_calls == DEFAULT_PURE_CALLS | {"mylib.make"}


def test_load_config_entries_section(tmp_path: Path) -> None:
    """``[tool.pylinters.entries]`` settings land in ``cfg.entries``."""
    cfg = import_time_work.load_config(
        _pyproject(
            tmp_path,
            "[tool.pylinters.entries]\n"
            'entry-sources = ["globs"]\n'
            'test-globs = ["t/*.py"]\n',
        )
    )
    assert cfg.entries == entries.EntryConfig(
        sources=("globs",),
        test_globs=("t/*.py",),
    )


def test_load_config_empty_allow_directive_rejected(tmp_path: Path) -> None:
    """An empty ``allow-directive`` raises ``ConfigError``."""
    path = _pyproject(
        tmp_path, '[tool.pylinters.import-time-work]\nallow-directive = ""\n'
    )
    with pytest.raises(errors.ConfigError, match="allow-directive"):
        import_time_work.load_config(path)


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
