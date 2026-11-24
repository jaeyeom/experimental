"""Command-line helpers shared by pylinters checkers."""

from __future__ import annotations

import argparse
import dataclasses
import importlib.metadata
import os
from collections.abc import Iterable, Sequence
from typing import BinaryIO

from pylinters.core import entries, errors, files

EXIT_OK = 0
EXIT_VIOLATIONS = 1
EXIT_ERROR = 2

EXCLUDE_HELP = (
    "exclude glob (repeatable; added to the configured excludes "
    "(shared and checker))"
)


def version() -> str:
    """Return the installed pylinters version, or ``"dev"``."""
    try:
        return importlib.metadata.version("pylinters")
    except importlib.metadata.PackageNotFoundError:
        return "dev"


def read_stdin0(stream: BinaryIO) -> list[str]:
    """Read NUL-separated paths (``git diff --name-only -z``)."""
    data = stream.read().decode("utf-8", "surrogateescape")
    return [p for p in data.split("\0") if p]


def to_repo_paths(args: Iterable[str], cwd: str, toplevel: str) -> list[str]:
    """Convert cwd-relative paths to repo-relative POSIX paths.

    Raises:
        ConfigError: A path lies outside ``toplevel``.
    """
    top = os.path.realpath(toplevel)
    out = []
    for arg in args:
        full = os.path.realpath(os.path.join(cwd, arg))
        rel = os.path.relpath(full, top)
        if rel == os.pardir or rel.startswith(os.pardir + os.sep):
            raise errors.ConfigError(f"{arg}: outside the repository {top}")
        out.append(rel.replace(os.sep, "/"))
    return out


def selected_paths(
    fs: files.GitResolver,
    args: Sequence[str],
    stdin: BinaryIO | None,
) -> set[str] | None:
    """Return the repo paths a run is limited to, or None for the tree.

    ``args`` are cwd-relative; ``stdin`` (when given) holds NUL-separated
    top-level-relative paths as ``git diff --name-only -z`` prints them.
    Directories expand to the tracked ``.py`` files below them.

    Raises:
        ConfigError: A path lies outside the repository, or a directory
            argument holds no tracked ``.py`` files.
    """
    if not args and stdin is None:
        return None
    paths = to_repo_paths(args, os.getcwd(), fs.root)
    for arg, path in zip(args, paths, strict=True):
        dir = "" if path == "." else path
        if os.path.isdir(os.path.join(fs.root, dir)) and not fs.has_py_under(
            dir
        ):
            raise errors.ConfigError(
                f"{arg}: no tracked .py files under directory"
            )
    if stdin is not None:
        paths += to_repo_paths(read_stdin0(stdin), fs.root, fs.root)
    return fs.expand_dirs(paths)


def add_entry_arguments(parser: argparse.ArgumentParser) -> None:
    """Add shared entry-discovery flags to ``parser``."""
    parser.add_argument(
        "--entry-source",
        action="append",
        choices=entries.SOURCES,
        help="entry source (repeatable; replaces config)",
    )
    parser.add_argument(
        "--script-glob",
        action="append",
        metavar="GLOB",
        help="glob of script entries (repeatable; replaces config)",
    )
    parser.add_argument(
        "--test-glob",
        action="append",
        metavar="GLOB",
        help="glob of test entries (repeatable; replaces config)",
    )
    parser.add_argument(
        "--entry-list",
        metavar="FILE",
        help="file of 'script PATH' / 'test PATH' lines, paths relative to "
        "the repo top-level",
    )
    parser.add_argument(
        "--bazel-dir", metavar="DIR", help="Bazel workspace directory"
    )
    parser.add_argument(
        "--bazel", metavar="PATH", help="bazel binary (default: bazel)"
    )


def entry_config(
    cfg: entries.EntryConfig, args: argparse.Namespace
) -> entries.EntryConfig:
    """Apply entry-discovery CLI flags over ``cfg``."""
    changes: dict[str, object] = {}
    if args.entry_source is not None:
        changes["sources"] = tuple(args.entry_source)
    if args.script_glob is not None:
        changes["script_globs"] = tuple(args.script_glob)
    if args.test_glob is not None:
        changes["test_globs"] = tuple(args.test_glob)
    if args.bazel_dir is not None:
        changes["bazel_dir"] = args.bazel_dir
    if args.bazel is not None:
        changes["bazel"] = args.bazel
    return dataclasses.replace(cfg, **changes)  # type: ignore[arg-type]


def entry_list(
    args: argparse.Namespace, fs: files.FileSet
) -> list[tuple[entries.Kind, str]] | None:
    """Read and parse ``--entry-list``, or return ``None`` when unset."""
    if args.entry_list is None:
        return None
    with open(args.entry_list, encoding="utf-8") as handle:
        return entries.parse_entry_list(handle.read(), args.entry_list, fs)
