"""Tests for pylinters.core.guards and imports.parse_module."""

import ast
import sys

import pytest
from pylinters.core import errors, guards, imports


def _guards(src: str) -> int:
    tree, _ = imports.parse_module(src.encode(), "x.py")
    return len(guards.module_guards(tree))


@pytest.mark.parametrize(
    "test",
    [
        "__name__ == '__main__'",
        '__name__ == "__main__"',
        "'__main__' == __name__",
    ],
)
def test_guard_forms(test: str) -> None:
    """Both operand orders and both quote styles are guards."""
    assert _guards(f"if {test}:\n    pass\n") == 1


@pytest.mark.parametrize(
    "src",
    [
        "if __name__ != '__main__':\n    pass\n",
        "if __name__ == 'x':\n    pass\n",
        "if a == b == '__main__':\n    pass\n",
        "if __main__ == '__name__':\n    pass\n",
        "def f():\n    if __name__ == '__main__':\n        pass\n",
    ],
)
def test_not_guards(src: str) -> None:
    """Other comparisons and nested guards are not module-level guards."""
    assert _guards(src) == 0


def test_parse_module_returns_comments() -> None:
    """Comments are keyed by line; strings are not comments."""
    _, comments = imports.parse_module(
        b'x = "# not a comment"\n# real\n', "x.py"
    )
    assert comments == {2: "# real"}


def test_comment_in() -> None:
    """comment_in looks for the directive on lines first..last."""
    comments = {3: "# py-main-guard: allow"}
    assert guards.comment_in(comments, 1, 3, "py-main-guard: allow")
    assert not guards.comment_in(comments, 1, 2, "py-main-guard: allow")


def test_parse_module_error() -> None:
    """Invalid source is a ParseError."""
    with pytest.raises(errors.ParseError):
        imports.parse_module(b"def (", "bad.py")


def _first_span(src: str) -> tuple[int, int]:
    tree = ast.parse(src)
    colons = guards.colon_positions(src.encode())
    return guards.directive_lines(tree.body[0], colons)


def test_simple_statement_span() -> None:
    """A simple statement spans all of its lines."""
    assert _first_span("f(\n    1,\n)\n") == (1, 3)


def test_with_header_span() -> None:
    """A compound ``with`` statement spans only its header."""
    assert _first_span("with open(\n    p\n) as h:\n    x = 1\n") == (1, 3)


def test_redundant_paren_on_own_line() -> None:
    """A header colon after a closing paren on its own line is included."""
    assert _first_span("if (\n    a\n):\n    pass\n") == (1, 3)


def test_for_header_single_line() -> None:
    """A single-line ``for`` header spans just its first line."""
    assert _first_span("for x in y:\n    f()\n") == (1, 1)


def test_except_handler_span() -> None:
    """An ``except`` handler spans from ``except`` through its header colon."""
    src = "try:\n    pass\nexcept (\n    E\n):\n    pass\n"
    tree = ast.parse(src)
    assert isinstance(tree.body[0], ast.Try)
    handler = tree.body[0].handlers[0]
    colons = guards.colon_positions(src.encode())
    assert guards.directive_lines(handler, colons) == (3, 5)


def test_directive_lines_missing_colon_raises() -> None:
    """A compound statement with no matching colon raises ValueError."""
    tree = ast.parse("if a:\n    pass\n")
    with pytest.raises(ValueError, match="colon|':'"):
        guards.directive_lines(tree.body[0], [])


def test_colon_positions_ignores_strings_and_comments() -> None:
    """Colons inside strings and comments are excluded."""
    assert guards.colon_positions(b"d = {1: 2}  # a: b\ns = 'x:y'\n") == [
        (1, 6)
    ]


def test_colon_positions_declared_encoding() -> None:
    """A PEP 263 coding cookie is honoured when tokenizing raw bytes."""
    src = b"# -*- coding: latin-1 -*-\nx = '\xe9'\nd = {1: 2}\n"
    assert guards.colon_positions(src) == [(3, 6)]


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
