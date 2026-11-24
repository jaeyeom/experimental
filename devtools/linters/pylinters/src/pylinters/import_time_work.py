"""Detect module-level statements that do work at import time.

Pure syntax-tree analysis: walks the module body (and nested blocks
executed during import) and reports statements that invoke non-pure
callables or run imperative control flow outside an
``if __name__ == '__main__':`` guard.
"""

from __future__ import annotations

import ast
from collections.abc import Collection, Mapping, Sequence
from dataclasses import dataclass, field
from typing import Literal

from pylinters.core import config, entries, guards, names

CHECKER = "import-time-work"
DEFAULT_DIRECTIVE = "py-import-time-work: allow"
DEFAULT_TEST_PATTERNS: tuple[str, ...] = (
    "**/test_*.py",
    "**/*_test.py",
    "**/conftest.py",
    "**/tests/**",
)

DEFAULT_PURE_CALLS: frozenset[str] = frozenset(
    {
        "dict",
        "frozenset",
        "list",
        "set",
        "tuple",
        "object",
        "re.compile",
        "logging.getLogger",
        "collections.namedtuple",
        "typing.NamedTuple",
        "typing.NewType",
        "typing.TypeVar",
        "typing.ParamSpec",
        "typing.TypeVarTuple",
        "datetime.timedelta",
        "textwrap.dedent",
    }
)

_ALLOWED_STMTS = (
    ast.Import,
    ast.ImportFrom,
    ast.FunctionDef,
    ast.AsyncFunctionDef,
    ast.ClassDef,
    ast.Pass,
)

_STMT_KEYWORDS: dict[str, str] = {
    "For": "for",
    "AsyncFor": "async for",
    "While": "while",
    "With": "with",
    "AsyncWith": "async with",
    "Match": "match",
    "Global": "global",
    "Nonlocal": "nonlocal",
    "Assert": "assert",
    "Delete": "del",
    "Raise": "raise",
}


@dataclass(frozen=True)
class Config:
    """Settings for one run; ``exclude`` is shared plus checker excludes."""

    entries: entries.EntryConfig = field(default_factory=entries.EntryConfig)
    scope: Literal["entry", "all"] = "entry"
    test_patterns: tuple[str, ...] = DEFAULT_TEST_PATTERNS
    pure_calls: frozenset[str] = DEFAULT_PURE_CALLS
    exclude: tuple[str, ...] = ()
    generated: tuple[str, ...] = config.DEFAULT_GENERATED
    allow_directive: str = DEFAULT_DIRECTIVE


@dataclass(frozen=True)
class Violation:
    """One finding."""

    path: str
    line: int
    message: str

    def __str__(self) -> str:
        """Render as ``path:line: import-time-work: message``."""
        return f"{self.path}:{self.line}: {CHECKER}: {self.message}"


@dataclass(frozen=True)
class Finding:
    """A statement that does work at import time, and what the work is."""

    node: ast.stmt | ast.ExceptHandler
    what: str


def find_violations(
    tree: ast.Module,
    comments: Mapping[int, str],
    colons: Sequence[tuple[int, int]],
    path: str,
    cfg: Config,
) -> list[Violation]:
    """Return the violations for one parsed file."""
    out: list[Violation] = []
    for finding in findings(tree, cfg.pure_calls):
        first, last = guards.directive_lines(finding.node, colons)
        if guards.comment_in(comments, first, last, cfg.allow_directive):
            continue
        out.append(
            Violation(
                path,
                finding.node.lineno,
                f"{finding.what} at module level runs on every import; "
                "move it into main() or under the __main__ guard",
            )
        )
    return out


def findings(tree: ast.Module, pure_calls: Collection[str]) -> list[Finding]:
    """Return the module-level statements of ``tree`` that do work.

    Results are sorted by ``(lineno, col_offset)``.
    """
    bound = names.Names(tree)
    pure = (
        pure_calls
        if isinstance(pure_calls, (set, frozenset))
        else frozenset(pure_calls)
    )
    future_annotations = _has_future_annotations(tree)
    out: list[Finding] = []
    _walk_stmts(tree.body, bound, pure, future_annotations, out)
    out.sort(key=lambda f: (f.node.lineno, f.node.col_offset))
    return out


def _has_future_annotations(tree: ast.Module) -> bool:
    return any(
        isinstance(stmt, ast.ImportFrom)
        and stmt.level == 0
        and stmt.module == "__future__"
        and any(alias.name == "annotations" for alias in stmt.names)
        for stmt in tree.body
    )


def _walk_stmts(
    stmts: Sequence[ast.stmt],
    bound: names.Names,
    pure: Collection[str],
    future_annotations: bool,
    out: list[Finding],
) -> None:
    for stmt in stmts:
        _visit_stmt(stmt, bound, pure, future_annotations, out)


def _visit_stmt(
    stmt: ast.stmt,
    bound: names.Names,
    pure: Collection[str],
    future_annotations: bool,
    out: list[Finding],
) -> None:
    if (
        isinstance(stmt, _ALLOWED_STMTS)
        or type(stmt).__name__ == "TypeAlias"
        or _is_docstring(stmt)
    ):
        return

    if isinstance(stmt, (ast.Expr, ast.Assign, ast.AnnAssign, ast.AugAssign)):
        call = _first_impure_in_exprs(
            _exprs_for_stmt(stmt, future_annotations), bound, pure
        )
        if call is not None:
            out.append(Finding(stmt, f"`{_spelled(call.func)}(...)`"))
        elif not isinstance(stmt, ast.Expr):
            target = _first_imported_mutation(_targets_for_stmt(stmt), bound)
            if target is not None:
                out.append(
                    Finding(stmt, f"assignment to `{ast.unparse(target)}`")
                )
        return

    if isinstance(stmt, ast.Delete):
        if not all(isinstance(target, ast.Name) for target in stmt.targets):
            out.append(Finding(stmt, _stmt_what(stmt)))
        return

    if isinstance(stmt, ast.If):
        if guards.is_main_guard(stmt):
            _walk_stmts(stmt.orelse, bound, pure, future_annotations, out)
            return
        call = _first_impure_call(stmt.test, bound, pure)
        if call is not None:
            out.append(Finding(stmt, f"`if` on `{_spelled(call.func)}(...)`"))
        _walk_stmts(stmt.body, bound, pure, future_annotations, out)
        _walk_stmts(stmt.orelse, bound, pure, future_annotations, out)
        return

    if isinstance(stmt, (ast.Try, ast.TryStar)):
        _walk_stmts(stmt.body, bound, pure, future_annotations, out)
        for handler in stmt.handlers:
            if handler.type is not None:
                call = _first_impure_call(handler.type, bound, pure)
                if call is not None:
                    out.append(
                        Finding(
                            handler,
                            f"`except` on `{_spelled(call.func)}(...)`",
                        )
                    )
            _walk_stmts(handler.body, bound, pure, future_annotations, out)
        _walk_stmts(stmt.orelse, bound, pure, future_annotations, out)
        _walk_stmts(stmt.finalbody, bound, pure, future_annotations, out)
        return

    out.append(Finding(stmt, _stmt_what(stmt)))
    if isinstance(stmt, ast.Match):
        for case in stmt.cases:
            _walk_stmts(case.body, bound, pure, future_annotations, out)
        return
    body = getattr(stmt, "body", None)
    if isinstance(body, list):
        _walk_stmts(body, bound, pure, future_annotations, out)
    orelse = getattr(stmt, "orelse", None)
    if isinstance(orelse, list):
        _walk_stmts(orelse, bound, pure, future_annotations, out)


def _is_docstring(stmt: ast.stmt) -> bool:
    return (
        isinstance(stmt, ast.Expr)
        and isinstance(stmt.value, ast.Constant)
        and isinstance(stmt.value.value, str)
    )


def _exprs_for_stmt(
    stmt: ast.Expr | ast.Assign | ast.AnnAssign | ast.AugAssign,
    future_annotations: bool,
) -> list[ast.expr]:
    if isinstance(stmt, ast.Expr):
        return [stmt.value]
    if isinstance(stmt, ast.Assign):
        return [*stmt.targets, stmt.value]
    if isinstance(stmt, ast.AugAssign):
        return [stmt.target, stmt.value]
    exprs: list[ast.expr] = [stmt.target]
    if not future_annotations:
        exprs.append(stmt.annotation)
    if stmt.value is not None:
        exprs.append(stmt.value)
    return exprs


def _targets_for_stmt(
    stmt: ast.Assign | ast.AnnAssign | ast.AugAssign,
) -> Sequence[ast.expr]:
    if isinstance(stmt, ast.Assign):
        return stmt.targets
    return (stmt.target,)


def _first_imported_mutation(
    targets: Sequence[ast.expr], bound: names.Names
) -> ast.Attribute | ast.Subscript | None:
    for target in targets:
        if isinstance(target, (ast.Tuple, ast.List)):
            found = _first_imported_mutation(target.elts, bound)
            if found is not None:
                return found
        elif isinstance(target, ast.Starred):
            found = _first_imported_mutation((target.value,), bound)
            if found is not None:
                return found
        elif isinstance(target, (ast.Attribute, ast.Subscript)):
            base: ast.expr = target
            while isinstance(base, (ast.Attribute, ast.Subscript)):
                base = base.value
            if isinstance(base, ast.Name) and bound.is_imported(base.id):
                return target
    return None


def _first_impure_in_exprs(
    exprs: Sequence[ast.expr],
    bound: names.Names,
    pure: Collection[str],
) -> ast.Call | None:
    for expr in exprs:
        found = _first_impure_call(expr, bound, pure)
        if found is not None:
            return found
    return None


def _first_impure_call(
    node: ast.AST,
    bound: names.Names,
    pure: Collection[str],
) -> ast.Call | None:
    if isinstance(node, ast.Lambda):
        for default in node.args.defaults:
            found = _first_impure_call(default, bound, pure)
            if found is not None:
                return found
        for kw_default in node.args.kw_defaults:
            if kw_default is not None:
                found = _first_impure_call(kw_default, bound, pure)
                if found is not None:
                    return found
        return None
    if isinstance(node, ast.Call):
        qualified = bound.qualified(node.func)
        if qualified is None or qualified not in pure:
            return node
    for child in ast.iter_child_nodes(node):
        found = _first_impure_call(child, bound, pure)
        if found is not None:
            return found
    return None


def _spelled(expr: ast.expr) -> str:
    if isinstance(expr, ast.Name):
        return expr.id
    if isinstance(expr, ast.Attribute):
        return f"{_spelled(expr.value)}.{expr.attr}"
    if isinstance(expr, ast.Call):
        return f"{_spelled(expr.func)}(...)"
    return "?"


def _stmt_what(stmt: ast.stmt) -> str:
    name = type(stmt).__name__
    kw = _STMT_KEYWORDS.get(name, name.lower())
    return f"a `{kw}` statement"
