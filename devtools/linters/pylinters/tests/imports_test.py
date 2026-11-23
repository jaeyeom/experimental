"""Tests for pylinters.core.imports."""

import sys

import pytest
from pylinters.core import errors, imports


def test_collects_nested_imports() -> None:
    """Function-local, try and TYPE_CHECKING imports are all collected."""
    src = imports.parse(
        b"import a\n"
        b"def f():\n    import b\n"
        b"try:\n    import c\nexcept ImportError:\n    pass\n"
        b"if TYPE_CHECKING:\n    from d.e import f\n",
        "x.py",
    )
    assert [(s.line, s.module, s.names) for s in src.imports] == [
        (1, None, ("a",)),
        (3, None, ("b",)),
        (5, None, ("c",)),
        (9, "d.e", ("f",)),
    ]
    assert [s.is_from for s in src.imports] == [False, False, False, True]


def test_relative_level_and_aliases() -> None:
    """Relative level is kept; alias names, not as-names, are recorded."""
    s = imports.parse(b"from . import p\nimport os, q.r as z\n", "x.py")
    assert s.imports[0].level == 1 and s.imports[0].module is None
    assert s.imports[1].level == 0
    assert s.imports[1].names == ("os", "q.r")


def test_directive_on_any_line_of_statement() -> None:
    """A directive on the closing-paren line covers the statement."""
    s = imports.parse(b"from a import (\n    b,\n)  # tag: allow\n", "x.py")
    assert s.imports[0].end_line == 3
    assert s.has_directive(s.imports[0], "tag: allow")
    assert not s.has_directive(s.imports[0], "other")


def test_directive_in_string_does_not_count() -> None:
    """Only real comments carry directives."""
    s = imports.parse(b"import a; x = '# tag: allow'\n", "x.py")
    assert not s.has_directive(s.imports[0], "tag: allow")


def test_encoding_cookie() -> None:
    """A declared source encoding is honored."""
    data = "# -*- coding: latin-1 -*-\nimport a  # caf\xe9\n".encode("latin-1")
    assert imports.parse(data, "x.py").imports[0].names == ("a",)


def test_syntax_error_raises_parse_error() -> None:
    """Unparseable source raises ParseError naming the path."""
    with pytest.raises(errors.ParseError) as exc:
        imports.parse(b"import (\n", "bad.py")
    assert exc.value.path == "bad.py"


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
