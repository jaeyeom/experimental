"""Entry modules: tracked ``.py`` files that are run directly.

Sources, each optional: the git executable bit or a ``#!`` first line,
Bazel ``py_binary`` / ``py_test`` mains, configured globs, and a list
supplied by any other build system.
"""

from __future__ import annotations

from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from typing import Literal

from pylinters.core import bazel, errors, files, globs

Kind = Literal["script", "test"]
SOURCES = ("executable", "bazel", "globs")

Reader = Callable[[str], bytes]


@dataclass(frozen=True)
class EntryConfig:
    """Which sources to use, and their settings."""

    sources: tuple[str, ...] = ("executable", "bazel")
    script_globs: tuple[str, ...] = ()
    test_globs: tuple[str, ...] = ()
    bazel_dir: str = "."
    bazel: str = "bazel"


@dataclass(frozen=True)
class Entry:
    """A file run directly; ``reasons`` say which sources found it."""

    path: str
    kind: Kind
    reasons: tuple[str, ...]


@dataclass(frozen=True)
class Discovery:
    """Entries by path, per-source counts, and notes on skipped sources."""

    entries: dict[str, Entry] = field(default_factory=dict)
    counts: dict[str, int] = field(default_factory=dict)
    notes: list[str] = field(default_factory=list)


def discover(
    fs: files.GitResolver,
    cfg: EntryConfig,
    read: Reader,
    *,
    entry_list: Sequence[tuple[Kind, str]] | None = None,
    run: bazel.Runner = bazel.run_subprocess,
) -> Discovery:
    """Find every entry module.

    ``read`` needs to return only a file's first two bytes.

    Raises:
        ConfigError: No source and no list, an unknown source, ``globs``
            without patterns, or a ``bazel_dir`` outside the repo.
        SourceError: The Bazel query failed or a main is ambiguous.
    """
    if not cfg.sources and entry_list is None:
        raise errors.ConfigError(
            "no entry source and no entry list: nothing would be checked"
        )
    for name in cfg.sources:
        if name not in SOURCES:
            raise errors.ConfigError(
                f"unknown entry source {name!r} (known: {', '.join(SOURCES)})"
            )
    out = Discovery()
    found: list[tuple[str, Kind, str]] = []
    for name in cfg.sources:
        before = len(found)
        if name == "executable":
            found += [
                (p, "script", "executable")
                for p in fs.py_files()
                if _is_script(fs, read, p)
            ]
        elif name == "bazel":
            found += _from_bazel(fs, cfg, run, out.notes)
        else:
            if not cfg.script_globs and not cfg.test_globs:
                raise errors.ConfigError(
                    "entry source 'globs' needs script-globs or test-globs"
                )
            for p in fs.py_files():
                if globs.match_any(cfg.test_globs, p):
                    found.append((p, "test", "glob"))
                elif globs.match_any(cfg.script_globs, p):
                    found.append((p, "script", "glob"))
        out.counts[name] = len(found) - before
    if entry_list is not None:
        found += [(p, kind, "list") for kind, p in entry_list]
        out.counts["list"] = len(entry_list)
    for path, kind, reason in found:
        prev = out.entries.get(path)
        if prev is None:
            out.entries[path] = Entry(path, kind, (reason,))
        else:
            merged: Kind = "test" if "test" in (prev.kind, kind) else "script"
            out.entries[path] = Entry(path, merged, (*prev.reasons, reason))
    return out


def parse_entry_list(
    text: str, where: str, fs: files.FileSet
) -> list[tuple[Kind, str]]:
    """Parse ``script PATH`` / ``test PATH`` lines (repo-relative paths).

    Raises:
        ConfigError: A bad kind, or a path that is not a tracked ``.py``.
    """
    out: list[tuple[Kind, str]] = []
    for number, raw in enumerate(text.splitlines(), 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        kind, path = (line.split(None, 1) + [""])[:2]
        path = path.strip()
        if kind not in ("script", "test") or not path:
            raise errors.ConfigError(
                f"{where}:{number}: expected 'script PATH' or 'test PATH'"
            )
        if not fs.has_file(path):
            raise errors.ConfigError(
                f"{where}:{number}: {path} is not a tracked .py file"
            )
        out.append(("script" if kind == "script" else "test", path))
    return out


def _is_script(fs: files.FileSet, read: Reader, path: str) -> bool:
    if path.rpartition("/")[2] == "__main__.py":
        return False
    if fs.is_executable(path):
        return True
    try:
        return read(path).startswith(b"#!")
    except FileNotFoundError:
        return False


def _from_bazel(
    fs: files.GitResolver,
    cfg: EntryConfig,
    run: bazel.Runner,
    notes: list[str],
) -> list[tuple[str, Kind, str]]:
    if not bazel.has_workspace(fs, cfg.bazel_dir):
        notes.append("bazel: skipped (no workspace)")
        return []
    res = bazel.find_mains(fs, cfg.bazel_dir, cfg.bazel, run)
    for count, what in (
        (res.generated, "generated main(s)"),
        (res.module_main, "main_module target(s)"),
        (res.unresolved, "target(s) with no main file among srcs"),
        (res.external, "main(s) in another repository"),
    ):
        if count:
            notes.append(f"bazel: {count} {what} skipped")
    return [(t.path, t.kind, f"bazel {t.label}") for t in res.targets]
