"""The ``if __name__ == '__main__':`` guard and comment directives."""

from __future__ import annotations

import ast
from collections.abc import Mapping


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
