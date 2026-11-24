"""Tests for pylinters.core.files."""

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from pylinters.core import errors, files

needs_git = pytest.mark.skipif(
    shutil.which("git") is None, reason="git not on PATH"
)


def test_fileset_filters_and_sorts() -> None:
    """Only .py paths are kept, sorted and deduplicated."""
    fs = files.FileSet(["b.py", "a/x.py", "README.md", "a/x.py"])
    assert fs.py_files() == ["a/x.py", "b.py"]


def test_resolves_module_file_and_namespace_dir() -> None:
    """A module path resolves to a file or a dir holding .py below it."""
    fs = files.FileSet(["a/b/c.py", "n/deep/m.py"])
    assert fs.resolves("a/b/c") and fs.resolves("a/b") and fs.resolves("n")
    assert not fs.resolves("a/b/zz") and not fs.resolves("a/b/c/d")


def test_own_source_root_walks_init_chain() -> None:
    """The source root is the parent of the topmost __init__ package."""
    fs = files.FileSet(
        [
            "codelab/proto/gen/__init__.py",
            "codelab/proto/gen/buf/validate/v_pb2.py",
            "codelab/proto/server.py",
        ]
    )
    assert files.own_source_root(fs, "codelab/proto/gen") == "codelab/proto"
    assert files.own_source_root(fs, "codelab/proto") == "codelab/proto"


def test_search_roots_order_and_dedup() -> None:
    """Own source root, repo root, then configured roots, deduplicated."""
    fs = files.FileSet(["src/p/__init__.py", "src/p/m.py"])
    assert files.search_roots(fs, "src/p", ["src"]) == ["src", ""]


def test_default_roots() -> None:
    """The default extra root is src/ when it holds Python files."""
    assert files.default_roots(files.FileSet(["src/a.py"])) == ["src"]
    assert files.default_roots(files.FileSet(["a.py"])) == []


def test_validate_roots_rejects_empty_root() -> None:
    """A configured root holding no tracked .py is an error."""
    with pytest.raises(errors.ConfigError, match="lib"):
        files.validate_roots(files.FileSet(["a.py"]), ["lib"])


def test_dotted() -> None:
    """Dotted names exist only strictly below the root, for identifiers."""
    assert files.dotted("a/b", "") == "a.b"
    assert files.dotted("src/a", "src") == "a"
    assert files.dotted("src", "src") is None
    assert files.dotted("my-tool", "") is None
    assert files.dotted("x/a", "src") is None


def test_join_and_parent() -> None:
    """The repo root is the empty string."""
    assert files.join("", "a", "b.py") == "a/b.py"
    assert files.parent("a/b.py") == "a"
    assert files.parent("b.py") == ""


def _git(cwd: Path, *args: str) -> None:
    subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True)


@needs_git
def test_git_resolver_lists_tracked_only(tmp_path: Path) -> None:
    """Untracked files are not part of the universe."""
    (tmp_path / "a").mkdir()
    (tmp_path / "a" / "x.py").write_text("")
    _git(tmp_path, "init", "-q")
    _git(tmp_path, "add", "a/x.py")
    (tmp_path / "a" / "y.py").write_text("")
    fs = files.GitResolver.from_repo(str(tmp_path / "a"))
    assert fs.py_files() == ["a/x.py"]
    assert fs.root == os.path.realpath(tmp_path)


def test_is_executable_in_memory() -> None:
    """Executable paths are given explicitly to an in-memory set."""
    fs = files.FileSet(["a.py", "b.py"], executable=["a.py"])
    assert fs.is_executable("a.py")
    assert not fs.is_executable("b.py")


def test_is_tracked_any_file() -> None:
    """Non-.py paths are tracked but not part of py_files."""
    fs = files.FileSet(["MODULE.bazel", "a.py"])
    assert fs.is_tracked("MODULE.bazel")
    assert fs.is_tracked("a.py")
    assert not fs.is_tracked("WORKSPACE")
    assert fs.py_files() == ["a.py"]


@needs_git
def test_git_resolver_reads_mode(tmp_path: Path) -> None:
    """The git index mode 100755 marks a file executable."""
    (tmp_path / "a.py").write_text("")
    (tmp_path / "b.py").write_text("")
    (tmp_path / "MODULE.bazel").write_text("")
    _git(tmp_path, "init", "-q")
    _git(tmp_path, "add", "a.py", "b.py", "MODULE.bazel")
    _git(tmp_path, "update-index", "--chmod=+x", "a.py")
    fs = files.GitResolver.from_repo(str(tmp_path))
    assert fs.is_executable("a.py")
    assert not fs.is_executable("b.py")
    assert fs.is_tracked("MODULE.bazel")
    assert fs.py_files() == ["a.py", "b.py"]


@needs_git
def test_git_resolver_outside_repo_raises(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Outside a git work tree the resolver raises GitError."""
    monkeypatch.setenv("GIT_CEILING_DIRECTORIES", str(tmp_path.parent))
    with pytest.raises(errors.GitError):
        files.GitResolver.from_repo(str(tmp_path))


def test_root_reader_requires_git_resolver() -> None:
    """A plain FileSet has no work-tree root to read from."""
    with pytest.raises(TypeError):
        files.root_reader(files.FileSet([]))


def test_root_reader_limit(tmp_path: Path) -> None:
    """A limit reads only the first bytes of the file."""
    (tmp_path / "a.py").write_bytes(b"#!/usr/bin/env python3\n")
    fs = files.GitResolver(str(tmp_path), ["a.py"])
    assert files.root_reader(fs, limit=2)("a.py") == b"#!"
    assert files.root_reader(fs)("a.py") == b"#!/usr/bin/env python3\n"


def test_expand_dirs() -> None:
    """Directory paths expand to the tracked .py files below them."""
    fs = files.FileSet(["a/x.py", "a/b/y.py", "ab.py", "c.py"])
    assert fs.expand_dirs(["a", "c.py", "gone.py"]) == {
        "a/x.py",
        "a/b/y.py",
        "c.py",
        "gone.py",
    }
    assert fs.expand_dirs(["."]) == {"a/x.py", "a/b/y.py", "ab.py", "c.py"}


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
