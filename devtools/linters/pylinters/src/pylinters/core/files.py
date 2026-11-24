"""The tracked-file universe and module-path resolution.

Every existence question goes to the git-tracked file list, never the
filesystem, so the answer is the same on every machine for a commit. Paths
are repo-relative POSIX strings; the repo root is ``""``.
"""

from __future__ import annotations

import bisect
import os
import subprocess
from collections.abc import Callable, Iterable, Sequence

from pylinters.core import errors


def join(*parts: str) -> str:
    """Join path parts, treating ``""`` as the repo root."""
    return "/".join(p for p in parts if p)


def parent(path: str) -> str:
    """Return the directory of ``path`` (``""`` for a top-level entry)."""
    return path.rpartition("/")[0]


class FileSet:
    """A sorted set of tracked ``.py`` paths, plus every tracked path."""

    def __init__(
        self, paths: Iterable[str], executable: Iterable[str] = ()
    ) -> None:
        """Keep ``.py`` paths sorted; remember all paths and executables."""
        self._all = frozenset(paths)
        self._files = sorted(p for p in self._all if p.endswith(".py"))
        self._set = frozenset(self._files)
        self._executable = frozenset(executable)

    def py_files(self) -> list[str]:
        """Return every tracked ``.py`` path, sorted."""
        return list(self._files)

    def has_file(self, path: str) -> bool:
        """Report whether ``path`` is a tracked ``.py`` file."""
        return path in self._set

    def is_tracked(self, path: str) -> bool:
        """Report whether ``path`` is any tracked file, ``.py`` or not."""
        return path in self._all

    def is_executable(self, path: str) -> bool:
        """Report whether ``path`` has git mode ``100755``."""
        return path in self._executable

    def has_py_under(self, dir: str) -> bool:
        """Report whether a tracked ``.py`` lies strictly below ``dir``."""
        prefix = dir + "/" if dir else ""
        i = bisect.bisect_left(self._files, prefix)
        return i < len(self._files) and self._files[i].startswith(prefix)

    def expand_dirs(self, paths: Iterable[str]) -> set[str]:
        """Replace each directory path with the tracked ``.py`` below it.

        ``"."`` names the repo root. Other paths are kept as given.
        """
        out: set[str] = set()
        for path in paths:
            dir = "" if path == "." else path
            if not self.has_py_under(dir):
                out.add(path)
                continue
            prefix = dir + "/" if dir else ""
            i = bisect.bisect_left(self._files, prefix)
            while i < len(self._files) and self._files[i].startswith(prefix):
                out.add(self._files[i])
                i += 1
        return out

    def resolves(self, module_path: str) -> bool:
        """Report whether ``module_path`` names a module or package."""
        return self.has_file(module_path + ".py") or self.has_py_under(
            module_path
        )

    def is_package(self, dir: str) -> bool:
        """Report whether ``dir`` has a tracked ``__init__.py``."""
        return self.has_file(join(dir, "__init__.py"))


class GitResolver(FileSet):
    """A :class:`FileSet` read from ``git ls-files`` of a work tree."""

    def __init__(
        self, root: str, paths: Iterable[str], executable: Iterable[str] = ()
    ) -> None:
        """Hold the absolute work-tree ``root`` and its tracked paths."""
        super().__init__(paths, executable)
        self.root = root

    @classmethod
    def from_repo(cls, path: str) -> GitResolver:
        """Build the resolver for the work tree containing ``path``."""
        root = _git(path, "rev-parse", "--show-toplevel").strip()
        # Records are ``<mode> <object> <stage>\t<path>``.
        listing = _git(root, "ls-files", "-s", "-z")
        paths: list[str] = []
        executable: list[str] = []
        for record in listing.split("\0"):
            if not record:
                continue
            meta, _, path = record.partition("\t")
            paths.append(path)
            if meta.split(" ", 1)[0] == "100755":
                executable.append(path)
        return cls(root, paths, executable)


def _git(cwd: str, *args: str) -> str:
    try:
        out = subprocess.run(
            ["git", *args],
            cwd=cwd,
            check=True,
            capture_output=True,
        )
    except FileNotFoundError as err:
        raise errors.GitError(f"cannot run git: {err}") from err
    except subprocess.CalledProcessError as err:
        msg = err.stderr.decode("utf-8", "replace").strip()
        raise errors.GitError(f"git {' '.join(args)} in {cwd}: {msg}") from err
    return out.stdout.decode("utf-8", "surrogateescape")


def root_reader(fs: FileSet) -> Callable[[str], bytes]:
    """Return a reader of repo-relative paths under the work-tree root."""
    if not isinstance(fs, GitResolver):
        raise TypeError("read is required unless fs is a GitResolver")
    root = fs.root

    def read(path: str) -> bytes:
        with open(os.path.join(root, path), "rb") as handle:
            return handle.read()

    return read


def default_roots(fs: FileSet) -> list[str]:
    """Return ``["src"]`` when ``src/`` holds Python files, else ``[]``."""
    return ["src"] if fs.has_py_under("src") else []


def validate_roots(fs: FileSet, roots: Sequence[str]) -> None:
    """Raise :class:`ConfigError` for a root holding no tracked ``.py``."""
    for root in roots:
        if not fs.has_py_under(root):
            raise errors.ConfigError(
                f"configured root {root!r} holds no tracked .py file"
            )


def own_source_root(fs: FileSet, dir: str) -> str:
    """Return the parent of the topmost ``__init__.py`` package above dir."""
    while dir and fs.is_package(dir):
        dir = parent(dir)
    return dir


def search_roots(fs: FileSet, dir: str, roots: Sequence[str]) -> list[str]:
    """Return the roots an import in ``dir`` resolves against, in order."""
    ordered = [own_source_root(fs, dir), "", *roots]
    return list(dict.fromkeys(ordered))


def dotted(dir: str, root: str) -> str | None:
    """Return the dotted name of ``dir`` below ``root``, if it has one."""
    if root:
        if not dir.startswith(root + "/"):
            return None
        rel = dir[len(root) + 1 :]
    else:
        rel = dir
    if not rel:
        return None
    parts = rel.split("/")
    if not all(p.isidentifier() for p in parts):
        return None
    return ".".join(parts)
