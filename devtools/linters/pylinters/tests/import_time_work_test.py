"""Tests for pylinters.import_time_work."""

from __future__ import annotations

import ast
import sys
from collections.abc import Collection

import pytest
from pylinters import import_time_work
from pylinters.core import guards, imports
from pylinters.import_time_work import (
    DEFAULT_DIRECTIVE,
    DEFAULT_PURE_CALLS,
    DEFAULT_TEST_PATTERNS,
    Config,
    Violation,
    find_violations,
    findings,
)


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


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
