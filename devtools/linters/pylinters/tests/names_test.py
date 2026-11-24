"""Tests for pylinters.core.names."""

import ast
import sys

import pytest
from pylinters.core import names


def _call_target(expr: str) -> ast.expr:
    stmt = ast.parse(f"{expr}()").body[0]
    assert isinstance(stmt, ast.Expr)
    assert isinstance(stmt.value, ast.Call)
    return stmt.value.func


def test_builtin_unbound() -> None:
    """An unbound bare name resolves to itself."""
    resolved = names.Names(ast.parse(""))
    assert resolved.qualified(_call_target("dict")) == "dict"


def test_import_alias() -> None:
    """An aliased module import resolves through the alias."""
    resolved = names.Names(ast.parse("import typing as t\n"))
    assert resolved.qualified(_call_target("t.TypeVar")) == "typing.TypeVar"


def test_from_import_alias() -> None:
    """An aliased from-import resolves to module.original."""
    resolved = names.Names(ast.parse("from re import compile as c\n"))
    assert resolved.qualified(_call_target("c")) == "re.compile"


def test_from_import_module_base() -> None:
    """A module imported via from-import resolves attribute chains."""
    resolved = names.Names(ast.parse("from absl.testing import absltest\n"))
    assert (
        resolved.qualified(_call_target("absltest.main"))
        == "absl.testing.absltest.main"
    )


def test_deep_attribute_chain() -> None:
    """Attribute chains of arbitrary depth resolve through the root."""
    aliased = names.Names(ast.parse("import numpy as np\n"))
    assert (
        aliased.qualified(_call_target("np.random.seed"))
        == "numpy.random.seed"
    )
    dotted = names.Names(ast.parse("import os.path\n"))
    assert dotted.qualified(_call_target("os.path.join")) == "os.path.join"


def test_call_result_is_none() -> None:
    """An attribute on a non-Name expression resolves to None."""
    resolved = names.Names(ast.parse("from pathlib import Path\n"))
    assert resolved.qualified(_call_target("Path(x).resolve")) is None


def test_relative_import_is_imported() -> None:
    """A relative import counts as imported but keeps its local name."""
    resolved = names.Names(ast.parse("from . import conf\n"))
    assert resolved.is_imported("conf")
    assert resolved.qualified(_call_target("conf.x")) == "conf.x"


def test_nested_import_is_imported() -> None:
    """An import nested inside a block still counts as imported."""
    src = "try:\n    import yaml\nexcept ImportError:\n    yaml = None\n"
    resolved = names.Names(ast.parse(src))
    assert resolved.is_imported("yaml")


def test_assigned_name_not_imported() -> None:
    """A name only bound by assignment is not imported."""
    resolved = names.Names(ast.parse("x = 1\n"))
    assert not resolved.is_imported("x")


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
