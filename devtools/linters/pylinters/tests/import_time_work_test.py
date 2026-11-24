"""Tests for pylinters.import_time_work."""

from __future__ import annotations

import ast
import sys
from collections.abc import Collection

import pytest
from pylinters import import_time_work
from pylinters.import_time_work import DEFAULT_PURE_CALLS, findings


def got(
    src: str, pure: Collection[str] = DEFAULT_PURE_CALLS
) -> list[tuple[int, str]]:
    """Return ``(lineno, what)`` pairs for findings in ``src``."""
    return [(f.node.lineno, f.what) for f in findings(ast.parse(src), pure)]


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


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
