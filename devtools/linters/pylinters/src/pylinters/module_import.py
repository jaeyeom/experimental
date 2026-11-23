"""Flag ``from pkg.mod import Symbol``; import the module instead.

Importing the module (``from pkg import mod``) keeps the call site
``mod.Symbol``, so the origin of a name is visible where it is used and
``monkeypatch.setattr(mod, ...)`` patches the object the code reads.

A ``from M import n`` is flagged when ``M`` resolves to a tracked module or
package and ``n`` is not itself a submodule of ``M``. Modules that do not
resolve (stdlib, third-party) are left alone.
"""

from __future__ import annotations

import sys
from collections.abc import Sequence
from dataclasses import dataclass

from pylinters.core import config, files, globs, imports

CHECKER = "module-import"
DEFAULT_DIRECTIVE = "py-module-import: allow"


@dataclass(frozen=True)
class Config:
    """Settings for one run; ``exclude`` is shared plus checker excludes."""

    roots: tuple[str, ...] | None = None
    exclude: tuple[str, ...] = ()
    generated: tuple[str, ...] = config.DEFAULT_GENERATED
    exempt_markers: tuple[str, ...] = ()
    third_party: tuple[str, ...] = ()
    allow_directive: str = DEFAULT_DIRECTIVE


@dataclass(frozen=True)
class Violation:
    """One symbol imported out of a module."""

    path: str
    line: int
    col: int
    module: str
    name: str

    def __str__(self) -> str:
        """Render as ``path:line:col: from M import n: message``."""
        return (
            f"{self.path}:{self.line}:{self.col}: from {self.module} import "
            f"{self.name}: import the module instead ({self.suggestion()})"
        )

    def suggestion(self) -> str:
        """Return the import statement that imports the module itself."""
        rest = self.module.lstrip(".")
        dots = self.module[: len(self.module) - len(rest)]
        head, _, last = rest.rpartition(".")
        if not head and not dots:
            return f"import {last}"
        return f"from {dots}{head} import {last}"


def find_violations(
    fs: files.FileSet,
    source: imports.Source,
    roots: Sequence[str],
    cfg: Config,
) -> list[Violation]:
    """Return the symbol imports in one file, sorted by position."""
    here = files.parent(source.path)
    in_init = source.path.rpartition("/")[2] == "__init__.py"
    own_prefix = here + "/" if here else ""
    out = []
    for stmt in source.imports:
        if not stmt.is_from or stmt.module is None:
            continue
        if source.statement_exempt(stmt, cfg.allow_directive):
            continue
        module_path = _resolve(fs, here, stmt, roots, cfg)
        if module_path is None:
            continue
        if in_init and module_path.startswith(own_prefix):
            continue  # re-export from the package's own submodule
        display = "." * stmt.level + stmt.module
        for i, name in enumerate(stmt.names):
            if name == "*" or source.name_exempt(stmt, i, cfg.allow_directive):
                continue
            target = files.join(module_path, name)
            if fs.resolves(target) or globs.match_any(
                cfg.generated, target + ".py"
            ):
                continue
            out.append(
                Violation(
                    source.path,
                    stmt.name_lines[i],
                    stmt.name_cols[i],
                    display,
                    name,
                )
            )
    out.sort(key=lambda v: (v.line, v.col))
    return out


def _resolve(
    fs: files.FileSet,
    here: str,
    stmt: imports.ImportStmt,
    roots: Sequence[str],
    cfg: Config,
) -> str | None:
    """Return the repo path of the imported module, or None if untracked."""
    parts = (stmt.module or "").split(".")
    if stmt.level:
        base = here
        for _ in range(stmt.level - 1):
            base = files.parent(base)
        path = files.join(base, *parts)
        return path if fs.resolves(path) else None
    candidates = files.search_roots(fs, here, roots)
    skip_extra = parts[0] in sys.stdlib_module_names or parts[0] in (
        cfg.third_party
    )
    own = {candidates[0], ""}
    for root in candidates:
        if skip_extra and root not in own:
            continue
        path = files.join(root, *parts)
        if fs.resolves(path):
            return path
    return None
