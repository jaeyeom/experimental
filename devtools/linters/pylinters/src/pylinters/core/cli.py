"""Command-line helpers shared by pylinters checkers."""

from __future__ import annotations

import importlib.metadata
import os
from collections.abc import Iterable
from typing import BinaryIO

from pylinters.core import errors

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
