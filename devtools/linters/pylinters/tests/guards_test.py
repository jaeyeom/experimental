"""Tests for pylinters.core.guards and imports.parse_module."""

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


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
