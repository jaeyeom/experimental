"""Bazel test verifying every Go file is listed in a BUILD.bazel Go rule."""

from __future__ import annotations

import ast
import os
import sys
from pathlib import Path, PurePosixPath

import pytest

_SKIP_DIRS: frozenset[str] = frozenset(
    {
        ".git",
        "vendor",
        ".cache",
        ".claude",
        ".tmp",
        "node_modules",
    }
)
_SKIP_PREFIXES: tuple[str, ...] = ("bazel-", ".bazel-")
_GO_SRC_ATTRS: frozenset[str] = frozenset({"srcs", "embedsrcs"})


def _should_skip_dir(name: str) -> bool:
    """Return True if a directory name should be skipped during repo walks."""
    return name in _SKIP_DIRS or name.startswith(_SKIP_PREFIXES)


def _has_skipped_segment(rel_posix: str) -> bool:
    """Return True if any directory segment in rel_posix is skipped."""
    parts = PurePosixPath(rel_posix).parts[:-1]
    return any(_should_skip_dir(part) for part in parts)


def _is_checked_go_filename(name: str) -> bool:
    """Return True if filename is a checked .go file (not test_*.go)."""
    return name.endswith(".go") and not name.startswith("test_")


def find_go_files(root: Path) -> set[str]:
    """Return repo-relative POSIX paths of all checked .go files under root."""
    found: set[str] = set()
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if not _should_skip_dir(d)]
        rel_dir = Path(dirpath).relative_to(root)
        for filename in filenames:
            if _is_checked_go_filename(filename):
                rel = (
                    PurePosixPath(filename)
                    if rel_dir == Path(".")
                    else PurePosixPath(*rel_dir.parts, filename)
                )
                found.add(rel.as_posix())
    return found


def _extract_string_list(expr: ast.expr) -> list[str]:
    """Extract literal string elements from a list or tuple AST node."""
    if not isinstance(expr, (ast.List, ast.Tuple)):
        return []
    result: list[str] = []
    for elt in expr.elts:
        if isinstance(elt, ast.Constant) and isinstance(elt.value, str):
            result.append(elt.value)
    return result


def _eval_glob(call: ast.Call, pkg_dir: Path) -> set[str]:
    """Evaluate a Starlark glob(include, exclude=...) call in pkg_dir."""
    include_patterns: list[str] = []
    exclude_patterns: list[str] = []
    if call.args:
        include_patterns = _extract_string_list(call.args[0])
    if len(call.args) >= 2:
        exclude_patterns = _extract_string_list(call.args[1])
    for kw in call.keywords:
        if kw.arg == "include":
            include_patterns = _extract_string_list(kw.value)
        elif kw.arg == "exclude":
            exclude_patterns = _extract_string_list(kw.value)

    excluded: set[str] = set()
    for pattern in exclude_patterns:
        for match in pkg_dir.glob(pattern):
            if match.is_file():
                excluded.add(match.relative_to(pkg_dir).as_posix())

    matched: set[str] = set()
    for pattern in include_patterns:
        for match in pkg_dir.glob(pattern):
            if match.is_file():
                rel = match.relative_to(pkg_dir).as_posix()
                if rel not in excluded and rel.endswith(".go"):
                    matched.add(rel)
    return matched


def _extract_go_srcs_from_expr(expr: ast.expr, pkg_dir: Path) -> set[str]:
    """Extract package-relative .go source paths from a Starlark expression."""
    if isinstance(expr, ast.Constant) and isinstance(expr.value, str):
        value = expr.value.removeprefix(":")
        if value.endswith(".go"):
            return {value}
        return set()
    if isinstance(expr, (ast.List, ast.Tuple, ast.Set)):
        srcs: set[str] = set()
        for elt in expr.elts:
            srcs.update(_extract_go_srcs_from_expr(elt, pkg_dir))
        return srcs
    if isinstance(expr, ast.BinOp) and isinstance(expr.op, ast.Add):
        return _extract_go_srcs_from_expr(
            expr.left, pkg_dir
        ) | _extract_go_srcs_from_expr(expr.right, pkg_dir)
    if isinstance(expr, ast.Call) and isinstance(expr.func, ast.Name):
        if expr.func.id == "select" and expr.args:
            arg = expr.args[0]
            if isinstance(arg, ast.Dict):
                srcs = set()
                for val in arg.values:
                    srcs.update(_extract_go_srcs_from_expr(val, pkg_dir))
                return srcs
        elif expr.func.id == "glob":
            return _eval_glob(expr, pkg_dir)
    return set()


def _parse_build_file(build_file: Path, root: Path) -> set[str]:
    """Return repo-relative .go paths referenced by go_* rules in a BUILD."""
    pkg_dir = build_file.parent
    rel_dir = pkg_dir.relative_to(root)
    tree = ast.parse(
        build_file.read_text(encoding="utf-8"),
        filename=str(build_file),
    )
    found: set[str] = set()
    for node in ast.walk(tree):
        if not (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Name)
            and node.func.id.startswith("go_")
        ):
            continue
        for kw in node.keywords:
            if kw.arg not in _GO_SRC_ATTRS:
                continue
            for src in _extract_go_srcs_from_expr(kw.value, pkg_dir):
                rel = (
                    PurePosixPath(src)
                    if rel_dir == Path(".")
                    else PurePosixPath(*rel_dir.parts, src)
                )
                rel_posix = rel.as_posix()
                if not _has_skipped_segment(
                    rel_posix
                ) and _is_checked_go_filename(rel.name):
                    found.add(rel_posix)
    return found


def find_bazel_go_files(root: Path) -> set[str]:
    """Return repo-relative POSIX paths of .go files in BUILD go_* rules."""
    found: set[str] = set()
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if not _should_skip_dir(d)]
        build_name = (
            "BUILD.bazel"
            if "BUILD.bazel" in filenames
            else ("BUILD" if "BUILD" in filenames else None)
        )
        if build_name is None:
            continue
        found.update(_parse_build_file(Path(dirpath) / build_name, root))
    return found


def find_missing_go_files(root: Path) -> list[str]:
    """Return sorted repo-relative .go files missing from BUILD go_* rules."""
    fs_files = find_go_files(root)
    bazel_files = find_bazel_go_files(root)
    return sorted(fs_files - bazel_files)


def _repo_root() -> Path:
    """Resolve the real checkout root from the go.mod data file symlink."""
    cur = Path.cwd()
    while True:
        candidate = cur / "go.mod"
        if candidate.exists():
            return candidate.resolve().parent
        if cur.parent == cur:
            raise RuntimeError("go.mod not found")
        cur = cur.parent


def test_all_go_files_listed(tmp_path: Path) -> None:
    """Passes when every .go file is listed in srcs, embedsrcs, or select."""
    pkg = tmp_path / "pkg"
    fixture = pkg / "testdata" / "src" / "a"
    fixture.mkdir(parents=True)
    (pkg / "lib.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "extra.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "embedded.go").write_text("package pkg\n", encoding="utf-8")
    (fixture / "a.go").write_text("package a\n", encoding="utf-8")
    (pkg / "BUILD.bazel").write_text(
        'go_library(\n'
        '    name = "go_default_library",\n'
        '    srcs = ["lib.go"] + select({\n'
        '        "//conditions:default": [":extra.go"],\n'
        '    }),\n'
        '    embedsrcs = ["embedded.go"],\n'
        ')\n'
        'go_library(\n'
        '    name = "fixture",\n'
        '    srcs = ["testdata/src/a/a.go"],\n'
        ')\n',
        encoding="utf-8",
    )

    assert find_missing_go_files(tmp_path) == []


def test_glob_srcs_supported(tmp_path: Path) -> None:
    """Supports glob() in go_* srcs with optional exclude."""
    pkg = tmp_path / "pkg"
    pkg.mkdir()
    (pkg / "a.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "ignored.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "BUILD.bazel").write_text(
        'go_library(\n'
        '    name = "go_default_library",\n'
        '    srcs = glob(["*.go"], exclude = ["ignored.go"]),\n'
        ')\n',
        encoding="utf-8",
    )

    assert find_missing_go_files(tmp_path) == ["pkg/ignored.go"]


def test_missing_go_file_is_reported(tmp_path: Path) -> None:
    """Reports .go files not listed in any go_* rule or missing BUILD.bazel."""
    pkg = tmp_path / "pkg"
    pkg.mkdir()
    (pkg / "listed.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "unlisted.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "BUILD.bazel").write_text(
        'go_library(\n'
        '    name = "go_default_library",\n'
        '    srcs = ["listed.go"],\n'
        ')\n',
        encoding="utf-8",
    )
    orphan_dir = tmp_path / "orphan"
    orphan_dir.mkdir()
    (orphan_dir / "main.go").write_text("package main\n", encoding="utf-8")

    assert find_missing_go_files(tmp_path) == [
        "orphan/main.go",
        "pkg/unlisted.go",
    ]


def test_excluded_files_and_dirs_are_ignored(tmp_path: Path) -> None:
    """Ignores test_*.go files and skipped directories."""
    (tmp_path / "test_helper.go").write_text(
        "package main\n", encoding="utf-8"
    )
    for skipped in (
        ".git",
        "vendor",
        ".cache",
        ".claude",
        ".tmp",
        "node_modules",
        "bazel-out",
        ".bazel-cache",
    ):
        d = tmp_path / skipped
        d.mkdir()
        (d / "skip.go").write_text("package skip\n", encoding="utf-8")

    assert find_missing_go_files(tmp_path) == []


def test_non_go_rule_does_not_satisfy_check(tmp_path: Path) -> None:
    """Listing a .go file only in a filegroup does not satisfy the check."""
    pkg = tmp_path / "pkg"
    pkg.mkdir()
    (pkg / "only_fg.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "BUILD.bazel").write_text(
        'filegroup(\n    name = "go_srcs",\n    srcs = ["only_fg.go"],\n)\n',
        encoding="utf-8",
    )

    assert find_missing_go_files(tmp_path) == ["pkg/only_fg.go"]


def test_repo_go_files_in_bazel() -> None:
    """Every checked .go file in the checkout is in a BUILD.bazel Go rule."""
    root = _repo_root()
    go_files = find_go_files(root)
    assert go_files, f"no .go files found under {root}"
    missing = find_missing_go_files(root)
    assert not missing, (
        "Go files not listed in any BUILD.bazel go_* target "
        "(run `bazel run //:gazelle` or add them to BUILD.bazel):\n"
        + "\n".join(f"  - {path}" for path in missing)
    )


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
