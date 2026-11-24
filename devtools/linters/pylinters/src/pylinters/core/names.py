"""Resolve local names bound by imports."""

from __future__ import annotations

import ast


class Names:
    """Local names bound by imports, for resolving call targets."""

    def __init__(self, tree: ast.Module) -> None:
        """Index import bindings across ``tree``."""
        self._imported: set[str] = set()
        self._bindings: dict[str, str] = {}
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                for alias in node.names:
                    if alias.asname:
                        self._imported.add(alias.asname)
                        self._bindings[alias.asname] = alias.name
                    else:
                        top = alias.name.partition(".")[0]
                        self._imported.add(top)
                        self._bindings[top] = top
            elif isinstance(node, ast.ImportFrom):
                for alias in node.names:
                    if alias.name == "*":
                        continue
                    local = alias.asname or alias.name
                    self._imported.add(local)
                    if node.level == 0 and node.module:
                        self._bindings[local] = f"{node.module}.{alias.name}"

    def qualified(self, func: ast.expr) -> str | None:
        """Return the dotted name ``func`` refers to, or ``None``."""
        attrs: list[str] = []
        cur: ast.expr = func
        while isinstance(cur, ast.Attribute):
            attrs.append(cur.attr)
            cur = cur.value
        if not isinstance(cur, ast.Name):
            return None
        root = self._bindings.get(cur.id, cur.id)
        if not attrs:
            return root
        attrs.reverse()
        return ".".join([root, *attrs])

    def is_imported(self, name: str) -> bool:
        """Report whether any import in the tree binds ``name``."""
        return name in self._imported
