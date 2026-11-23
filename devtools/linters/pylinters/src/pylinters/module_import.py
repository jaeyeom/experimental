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
from collections.abc import Callable, Collection, Sequence
from dataclasses import dataclass

from pylinters.core import config, errors, files, globs, imports

CHECKER = "module-import"
DEFAULT_DIRECTIVE = "py-module-import: allow"

Reader = Callable[[str], bytes]


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
    out = []
    for stmt in source.imports:
        if not stmt.is_from or stmt.module is None:
            continue
        if source.statement_exempt(stmt, cfg.allow_directive):
            continue
        module_path = _resolve(fs, here, stmt, roots, cfg)
        if module_path is None:
            continue
        if in_init and _within(module_path, here):
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


def _within(path: str, dir: str) -> bool:
    """Report whether ``path`` is ``dir`` or below it (root: only itself)."""
    return path == dir or (bool(dir) and path.startswith(dir + "/"))


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
        depth = len(here.split("/")) if here else 0
        if stmt.level - 1 > depth:
            return None  # walks above the repo root: Python rejects it
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


@dataclass(frozen=True)
class Result:
    """The outcome of a check."""

    violations: list[Violation]
    counts: dict[str, int]
    files_scanned: int


def load_config(pyproject_path: str) -> Config:
    """Read the shared and ``module-import`` settings from pyproject.toml.

    Raises:
        ConfigError: Invalid TOML, an unknown key, or a wrong value type.
    """
    shared, table = config.load_table(pyproject_path, (CHECKER,))
    own = table.get(CHECKER, {})
    where = f"{pyproject_path} [tool.pylinters.{CHECKER}]"
    config.check_keys(
        own,
        ("exclude", "exempt-markers", "third-party", "allow-directive"),
        where,
    )
    markers = config.string_list(
        own.get("exempt-markers", []), "exempt-markers", where
    )
    check_exempt_markers(markers, f"{where} exempt-markers")
    directive = own.get("allow-directive", DEFAULT_DIRECTIVE)
    if not isinstance(directive, str) or not directive:
        raise errors.ConfigError(
            f"{where}: allow-directive must be a non-empty string"
        )
    return Config(
        roots=shared.roots,
        exclude=shared.exclude
        + config.string_list(own.get("exclude", []), "exclude", where),
        generated=shared.generated,
        exempt_markers=markers,
        third_party=config.string_list(
            own.get("third-party", []), "third-party", where
        ),
        allow_directive=directive,
    )


def check_exempt_markers(markers: Sequence[str], where: str) -> None:
    """Raise :class:`ConfigError` unless each marker is a ``.py`` basename."""
    for marker in markers:
        if not marker.endswith(".py") or "/" in marker:
            raise errors.ConfigError(
                f"{where}: entry {marker!r} must be a plain file name "
                "ending in .py"
            )


def in_scope(fs: files.FileSet, cfg: Config, path: str) -> bool:
    """Return whether ``path`` is subject to the check."""
    if globs.match_any(cfg.exclude, path) or globs.match_any(
        cfg.generated, path
    ):
        return False
    directory = files.parent(path)
    while True:
        if any(
            fs.has_file(files.join(directory, m)) for m in cfg.exempt_markers
        ):
            return False
        if not directory:
            return True
        directory = files.parent(directory)


def check(
    fs: files.FileSet,
    cfg: Config = Config(),
    read: Reader | None = None,
    paths: Collection[str] | None = None,
) -> Result:
    """Check the tree, reporting only files in ``paths`` when given.

    ``read`` maps a repo-relative path to file bytes; it defaults to reading
    from ``fs.root`` when ``fs`` is a :class:`files.GitResolver` (otherwise
    :class:`TypeError`).

    Raises:
        ConfigError: A configured root holds no tracked Python file.
        ParseError: A scanned file is not valid Python.
    """
    reader = read if read is not None else files.root_reader(fs)
    roots = (
        list(cfg.roots) if cfg.roots is not None else files.default_roots(fs)
    )
    files.validate_roots(fs, roots)

    violations: list[Violation] = []
    counts: dict[str, int] = {}
    for path in fs.py_files():
        if (paths is not None and path not in paths) or not in_scope(
            fs, cfg, path
        ):
            continue
        try:
            data = reader(path)
        except FileNotFoundError:
            continue  # tracked but gone from the work tree (mid-refactor)
        found = find_violations(fs, imports.parse(data, path), roots, cfg)
        counts[path] = len(found)
        violations.extend(found)
    violations.sort(key=lambda v: (v.path, v.line, v.col))
    return Result(violations, counts, len(counts))
