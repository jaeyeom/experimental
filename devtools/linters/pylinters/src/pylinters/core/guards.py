"""The ``if __name__ == '__main__':`` guard and comment directives."""

from __future__ import annotations

import ast
import bisect
import io
import tokenize
from collections.abc import Mapping, Sequence

_BODY_ATTRS = ("body", "orelse", "finalbody", "handlers", "cases")


def is_main_guard(node: ast.stmt) -> bool:
    """Report whether ``node`` is ``if __name__ == '__main__':``.

    Either operand order counts; any other comparison does not.
    """
    if not isinstance(node, ast.If):
        return False
    test = node.test
    if not (
        isinstance(test, ast.Compare)
        and len(test.ops) == 1
        and isinstance(test.ops[0], ast.Eq)
    ):
        return False
    pair = (test.left, test.comparators[0])
    return any(
        isinstance(name, ast.Name)
        and name.id == "__name__"
        and isinstance(const, ast.Constant)
        and const.value == "__main__"
        for name, const in (pair, pair[::-1])
    )


def module_guards(tree: ast.Module) -> list[ast.If]:
    """Return the module-level ``__main__`` guards of ``tree``."""
    return [
        node
        for node in tree.body
        if isinstance(node, ast.If) and is_main_guard(node)
    ]


def comment_in(
    comments: Mapping[int, str], first: int, last: int, directive: str
) -> bool:
    """Report whether a comment on lines ``first..last`` has ``directive``."""
    return any(
        directive in comments.get(line, "") for line in range(first, last + 1)
    )


def colon_positions(data: bytes) -> list[tuple[int, int]]:
    """Return sorted ``(line, col)`` of every ``:`` operator token."""
    return [
        tok.start
        for tok in tokenize.tokenize(io.BytesIO(data).readline)
        if tok.exact_type == tokenize.COLON
    ]


def directive_lines(
    node: ast.stmt | ast.ExceptHandler,
    colons: Sequence[tuple[int, int]],
) -> tuple[int, int]:
    """Return ``(first, last)`` lines where an allow comment exempts ``node``."""
    body_nodes: set[ast.AST] = set()
    for attr in _BODY_ATTRS:
        items = getattr(node, attr, None)
        if items:
            body_nodes.update(items)
    if not body_nodes:
        return (node.lineno, node.end_lineno or node.lineno)

    header_end = (node.lineno, node.col_offset)
    for child in ast.iter_child_nodes(node):
        if child in body_nodes:
            continue
        for desc in ast.walk(child):
            end_line = getattr(desc, "end_lineno", None)
            end_col = getattr(desc, "end_col_offset", None)
            if end_line is not None and end_col is not None:
                header_end = max(header_end, (end_line, end_col))

    idx = bisect.bisect_left(colons, header_end)
    if idx >= len(colons):
        raise ValueError(
            f"no ':' token found for {type(node).__name__} at line {node.lineno}"
        )
    return (node.lineno, colons[idx][0])
