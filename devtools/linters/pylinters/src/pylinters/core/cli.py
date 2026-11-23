"""Command-line helpers shared by pylinters checkers."""

from __future__ import annotations

import importlib.metadata
import os
from collections.abc import Iterable, Sequence
from typing import BinaryIO

from pylinters.core import errors, files

EXIT_OK = 0
EXIT_VIOLATIONS = 1
EXIT_ERROR = 2


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
        ConfigError: A path lies outside the repository.
    """
    if not args and stdin is None:
        return None
    paths = to_repo_paths(args, os.getcwd(), fs.root)
    if stdin is not None:
        paths += to_repo_paths(read_stdin0(stdin), fs.root, fs.root)
    return fs.expand_dirs(paths)
