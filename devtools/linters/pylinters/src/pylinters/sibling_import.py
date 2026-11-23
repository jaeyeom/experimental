"""Flag a bare import of a sibling module inside a package.

Inside a package, siblings are imported package-qualified
(``from a.b import policy``), never bare (``import policy``). A bare import
does not fail: pytest and ``python a/b/x.py`` put the file's directory on
``sys.path``, so the bare name resolves -- to a second module object,
separate from the ``a.b.policy`` the code under test reads.
``monkeypatch.setattr(policy, ...)`` then patches the copy nobody reads.

A directory is a package when it has an ``__init__.py`` or when any tracked
file imports into it by a qualified name. Flat script directories are left
alone.
"""

from __future__ import annotations

from collections.abc import Callable, Collection
from dataclasses import dataclass, field

from pylinters.core import config, errors, files, globs, imports

CHECKER = "sibling-import"
DEFAULT_DIRECTIVE = "py-sibling-import: allow"

Reader = Callable[[str], bytes]


@dataclass(frozen=True)
class Config:
    """Settings for one run; ``exclude`` is shared plus checker excludes."""

    roots: tuple[str, ...] | None = None
    exclude: tuple[str, ...] = ()
    generated: tuple[str, ...] = config.DEFAULT_GENERATED
    allow_directive: str = DEFAULT_DIRECTIVE


@dataclass(frozen=True)
class Violation:
    """One bare import of a sibling module."""

    path: str
    line: int
    statement: str
    name: str
    suggestion: str

    def __str__(self) -> str:
        """Render as ``path:line: statement: message``."""
        return (
            f"{self.path}:{self.line}: {self.statement}: bare import of "
            f"sibling module; use `{self.suggestion}`"
        )


@dataclass(frozen=True)
class Result:
    """The outcome of a check."""

    violations: list[Violation]
    files_scanned: int
    package_dirs: dict[str, str] = field(default_factory=dict)


def load_config(pyproject_path: str) -> Config:
    """Read the shared and ``sibling-import`` settings from pyproject.toml.

    Raises:
        ConfigError: Invalid TOML, an unknown key, or a wrong value type.
    """
    shared, table = config.load_table(pyproject_path, (CHECKER,))
    own = table.get(CHECKER, {})
    where = f"{pyproject_path} [tool.pylinters.{CHECKER}]"
    config.check_keys(own, ("exclude", "allow-directive"), where)
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
        allow_directive=directive,
    )


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

    sources: dict[str, imports.Source] = {}
    scanned: list[str] = []
    for path in fs.py_files():
        skipped = globs.match_any(cfg.exclude, path) or globs.match_any(
            cfg.generated, path
        )
        wanted = not skipped and (paths is None or path in paths)
        try:
            data = reader(path)
        except FileNotFoundError:
            continue  # tracked but gone from the work tree (mid-refactor)
        try:
            sources[path] = imports.parse(data, path)
        except errors.ParseError:
            if wanted:
                raise
            continue
        if wanted:
            scanned.append(path)

    package_names = _package_dirs(fs, sources, roots)
    violations = [
        v
        for path in scanned
        for v in _violations(fs, sources[path], package_names, cfg)
    ]
    violations.sort(key=lambda v: (v.path, v.line, v.name))
    preferred = {pkg: names[0] for pkg, names in package_names.items()}
    return Result(violations, len(scanned), preferred)


def format_report(result: Result, cfg: Config) -> str:
    """Render violations plus a fix hint; ``""`` when there are none."""
    if not result.violations:
        return ""
    lines = [str(v) for v in result.violations]
    lines.append("")
    lines.append(
        "A bare import resolves to a second copy of the module when the "
        "file's directory is on sys.path (pytest, python file.py), so "
        "monkeypatch patches the copy the code under test never reads. "
        f"A genuine exception carries '# {cfg.allow_directive}' on the "
        "import statement."
    )
    return "\n".join(lines) + "\n"


def _package_dirs(
    fs: files.FileSet,
    sources: dict[str, imports.Source],
    roots: list[str],
) -> dict[str, list[str]]:
    """Map each package directory to its dotted names, preferred first.

    Preference: names used by qualified imports from outside the package,
    then the package's own ``__init__.py`` chain name, then names used from
    inside it. A file's own non-package directory is never a root here:
    resolving against it is exactly the bare import being reported.
    """
    outside: dict[str, list[str]] = {}
    inside: dict[str, list[str]] = {}
    for path, source in sources.items():
        here = files.parent(path)
        search = [
            r
            for r in files.search_roots(fs, here, roots)
            if r != here or r == ""
        ]
        for stmt in source.imports:
            if stmt.level:
                continue
            if stmt.is_from:
                chains = [
                    [*(stmt.module or "").split("."), n] for n in stmt.names
                ]
            else:
                chains = [n.split(".") for n in stmt.names]
            for root in search:
                for chain in chains:
                    for length in range(2, len(chain) + 1):
                        module = files.join(root, *chain[:length])
                        if not fs.resolves(module):
                            continue
                        pkg = files.parent(module)
                        name = files.dotted(pkg, root)
                        if name is None:
                            continue
                        within = here == pkg or here.startswith(pkg + "/")
                        _add(inside if within else outside, pkg, name)
    found: dict[str, list[str]] = {}
    for pkg, names in outside.items():
        for name in names:
            _add(found, pkg, name)
    for path in fs.py_files():
        if path.rpartition("/")[2] == "__init__.py":
            pkg = files.parent(path)
            name = files.dotted(pkg, files.own_source_root(fs, pkg))
            if name is not None:
                _add(found, pkg, name)
    for pkg, names in inside.items():
        for name in names:
            _add(found, pkg, name)
    return found


def _add(table: dict[str, list[str]], pkg: str, name: str) -> None:
    names = table.setdefault(pkg, [])
    if name not in names:
        names.append(name)


def _violations(
    fs: files.FileSet,
    source: imports.Source,
    package_names: dict[str, list[str]],
    cfg: Config,
) -> list[Violation]:
    pkg = files.parent(source.path)
    names = package_names.get(pkg)
    if not names:
        return []
    # A top-level name that starts one of the package's own dotted names is
    # the qualified import (``pkg/pkg.py`` beside ``from pkg import x``).
    # The file's own name is a shim importing the external module it
    # shadows. Neither is a bare sibling import.
    not_siblings = {name.split(".")[0] for name in names}
    not_siblings.add(source.path.rpartition("/")[2].removesuffix(".py"))
    out = []
    for stmt in source.imports:
        if stmt.level or source.has_directive(stmt, cfg.allow_directive):
            continue
        if stmt.is_from:
            pairs = [
                (
                    f"from {stmt.module} import ...",
                    (stmt.module or "").split(".")[0],
                )
            ]
        else:
            pairs = [(f"import {n}", n.split(".")[0]) for n in stmt.names]
        for label, top in pairs:
            if top not in not_siblings and _is_sibling(fs, pkg, top):
                out.append(
                    Violation(
                        source.path,
                        stmt.line,
                        label,
                        top,
                        f"from {names[0]} import {top}",
                    )
                )
    return out


def _is_sibling(fs: files.FileSet, pkg: str, name: str) -> bool:
    if name == "__init__":
        return False
    module = files.join(pkg, name)
    return fs.has_file(module + ".py") or fs.has_py_under(module)
