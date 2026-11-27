"""Bazel test verifying every Semgrep-scanned file is wired into Bazel inputs."""

from __future__ import annotations

import ast
import os
import re
import sys
from dataclasses import dataclass
from pathlib import Path, PurePosixPath

import pytest

_SKIP_DIRS: frozenset[str] = frozenset(
    {
        ".git",
        "vendor",
        ".cache",
        ".claude",
        ".tmp",
        ".venv",
        "__pycache__",
        "node_modules",
    }
)
_SKIP_PREFIXES: tuple[str, ...] = ("bazel-", ".bazel-")
_TEMPLATE_SUFFIXES: tuple[str, ...] = (
    ".gohtml",
    ".html",
    ".thtml",
    ".tmpl",
    ".tpl",
)


@dataclass(frozen=True)
class CategorySpec:
    """Configuration for one Semgrep input category."""

    name: str
    filegroup_name: str
    aggregator_build: str
    aggregator_pkg: str
    aggregator_target: str


_CATEGORIES: tuple[CategorySpec, ...] = (
    CategorySpec(
        name="go",
        filegroup_name="go_srcs",
        aggregator_build="tools/lint/BUILD.bazel",
        aggregator_pkg="tools/lint",
        aggregator_target="repo_go_srcs",
    ),
    CategorySpec(
        name="python",
        filegroup_name="py_srcs",
        aggregator_build="devtools/linters/pylinters/BUILD.bazel",
        aggregator_pkg="devtools/linters/pylinters",
        aggregator_target="repo_py_srcs",
    ),
    CategorySpec(
        name="yaml",
        filegroup_name="yaml_srcs",
        aggregator_build="tools/lint/BUILD.bazel",
        aggregator_pkg="tools/lint",
        aggregator_target="repo_yaml_srcs",
    ),
    CategorySpec(
        name="template",
        filegroup_name="template_srcs",
        aggregator_build="tools/lint/BUILD.bazel",
        aggregator_pkg="tools/lint",
        aggregator_target="repo_template_srcs",
    ),
)


def _should_skip_dir(name: str) -> bool:
    """Return True if a directory name should be skipped during repo walks."""
    return name in _SKIP_DIRS or name.startswith(_SKIP_PREFIXES)


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


def _classify_file(name: str) -> str | None:
    """Return the Semgrep category for a filename, or None if unscanned."""
    if name.endswith(".go") and not name.endswith("_test.go"):
        return "go"
    if name.endswith(".py"):
        return "python"
    if name.endswith((".yml", ".yaml")):
        return "yaml"
    if name.endswith(_TEMPLATE_SUFFIXES):
        return "template"
    return None


def find_scanned_files(root: Path) -> dict[str, set[str]]:
    """Return repo-relative POSIX paths grouped by Semgrep category."""
    found: dict[str, set[str]] = {spec.name: set() for spec in _CATEGORIES}
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if not _should_skip_dir(d)]
        rel_dir = Path(dirpath).relative_to(root)
        for filename in filenames:
            cat = _classify_file(filename)
            if cat is None:
                continue
            rel = (
                PurePosixPath(filename)
                if rel_dir == Path(".")
                else PurePosixPath(*rel_dir.parts, filename)
            )
            found[cat].add(rel.as_posix())
    return found


def _owning_package(root: Path, rel_path: str) -> str:
    """Return the Bazel package owning rel_path ('.' for root)."""
    parent = PurePosixPath(rel_path).parent
    while str(parent) != ".":
        pkg_dir = root / Path(*parent.parts)
        if (pkg_dir / "BUILD.bazel").is_file() or (
            pkg_dir / "BUILD"
        ).is_file():
            return parent.as_posix()
        parent = parent.parent
    return "."


def _extract_string_list(expr: ast.expr) -> list[str]:
    """Extract literal string elements from a list or tuple AST node."""
    if not isinstance(expr, (ast.List, ast.Tuple)):
        return []
    result: list[str] = []
    for elt in expr.elts:
        if isinstance(elt, ast.Constant) and isinstance(elt.value, str):
            result.append(elt.value)
    return result


def _glob_to_regex(pattern: str) -> re.Pattern[str]:
    """Convert a Bazel glob pattern to an anchored POSIX path regex."""
    parts = ["^"]
    i = 0
    n = len(pattern)
    while i < n:
        if pattern[i : i + 3] == "**/":
            parts.append("(?:.+/)?")
            i += 3
        elif pattern[i : i + 2] == "**":
            parts.append(".*")
            i += 2
        elif pattern[i] == "*":
            parts.append("[^/]*")
            i += 1
        elif pattern[i] == "?":
            parts.append("[^/]")
            i += 1
        else:
            parts.append(re.escape(pattern[i]))
            i += 1
    parts.append("$")
    return re.compile("".join(parts))


def _walk_package_files(pkg_dir: Path) -> list[str]:
    """Return package-relative POSIX file paths owned by pkg_dir."""
    files: list[str] = []
    for dirpath, dirnames, filenames in os.walk(pkg_dir):
        cur = Path(dirpath)
        kept: list[str] = []
        for d in dirnames:
            if _should_skip_dir(d):
                continue
            sub = cur / d
            if (sub / "BUILD.bazel").is_file() or (sub / "BUILD").is_file():
                continue
            kept.append(d)
        dirnames[:] = kept
        rel_dir = cur.relative_to(pkg_dir)
        for filename in filenames:
            rel = (
                PurePosixPath(filename)
                if rel_dir == Path(".")
                else PurePosixPath(*rel_dir.parts, filename)
            )
            files.append(rel.as_posix())
    return files


def _eval_glob(call: ast.Call, pkg_dir: Path) -> set[str]:
    """Evaluate a Starlark glob(include, exclude=...) within pkg_dir only."""
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

    inc_res = [_glob_to_regex(p) for p in include_patterns]
    exc_res = [_glob_to_regex(p) for p in exclude_patterns]

    matched: set[str] = set()
    for rel in _walk_package_files(pkg_dir):
        if any(r.match(rel) for r in inc_res) and not any(
            r.match(rel) for r in exc_res
        ):
            matched.add(rel)
    return matched


def _extract_filegroup_srcs(expr: ast.expr, pkg_dir: Path) -> set[str]:
    """Extract package-relative file paths or labels from a filegroup srcs."""
    if isinstance(expr, ast.Constant) and isinstance(expr.value, str):
        return {expr.value}
    if isinstance(expr, (ast.List, ast.Tuple, ast.Set)):
        out: set[str] = set()
        for elt in expr.elts:
            out.update(_extract_filegroup_srcs(elt, pkg_dir))
        return out
    if isinstance(expr, ast.BinOp) and isinstance(expr.op, ast.Add):
        return _extract_filegroup_srcs(
            expr.left, pkg_dir
        ) | _extract_filegroup_srcs(expr.right, pkg_dir)
    if (
        isinstance(expr, ast.Call)
        and isinstance(expr.func, ast.Name)
        and expr.func.id == "glob"
    ):
        return _eval_glob(expr, pkg_dir)
    return set()


def _read_filegroup_srcs(
    build_file: Path, target_name: str
) -> set[str] | None:
    """Return raw srcs entries for filegroup(name=target_name), or None."""
    if not build_file.is_file():
        return None
    tree = ast.parse(
        build_file.read_text(encoding="utf-8"),
        filename=str(build_file),
    )
    pkg_dir = build_file.parent
    for node in ast.walk(tree):
        if not (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Name)
            and node.func.id == "filegroup"
        ):
            continue
        name: str | None = None
        srcs_expr: ast.expr | None = None
        for kw in node.keywords:
            if kw.arg == "name" and isinstance(kw.value, ast.Constant):
                if isinstance(kw.value.value, str):
                    name = kw.value.value
            elif kw.arg == "srcs":
                srcs_expr = kw.value
        if name == target_name and srcs_expr is not None:
            return _extract_filegroup_srcs(srcs_expr, pkg_dir)
    return None


def _expected_label(pkg: str, fg_name: str, aggregator_pkg: str) -> str:
    """Return the Bazel label referencing pkg's filegroup from aggregator."""
    if pkg == aggregator_pkg:
        return f":{fg_name}"
    if pkg == ".":
        return f"//:{fg_name}"
    return f"//{pkg}:{fg_name}"


def _build_file_for_pkg(root: Path, pkg: str) -> Path:
    """Return BUILD.bazel or BUILD path for a package."""
    pkg_dir = root if pkg == "." else root / Path(*PurePosixPath(pkg).parts)
    bazel_file = pkg_dir / "BUILD.bazel"
    if bazel_file.is_file():
        return bazel_file
    return pkg_dir / "BUILD"


def find_missing_semgrep_inputs(root: Path) -> list[str]:
    """Return human-readable descriptions of Semgrep inputs missing in Bazel."""
    files_by_cat = find_scanned_files(root)
    errors: list[str] = []

    for spec in _CATEGORIES:
        agg_build = root / Path(*PurePosixPath(spec.aggregator_build).parts)
        agg_refs = _read_filegroup_srcs(agg_build, spec.aggregator_target)
        if agg_refs is None:
            errors.append(
                f"missing filegroup {spec.aggregator_target!r} in "
                f"{spec.aggregator_build}"
            )
            continue

        by_pkg: dict[str, list[str]] = {}
        for rel in sorted(files_by_cat[spec.name]):
            pkg = _owning_package(root, rel)
            by_pkg.setdefault(pkg, []).append(rel)

        for pkg, rels in sorted(by_pkg.items()):
            build_file = _build_file_for_pkg(root, pkg)
            pkg_srcs = _read_filegroup_srcs(build_file, spec.filegroup_name)
            if pkg_srcs is None:
                errors.append(
                    f"{pkg}: missing filegroup {spec.filegroup_name!r} in "
                    f"{build_file.relative_to(root).as_posix()} "
                    f"(covers {len(rels)} {spec.name} file(s), e.g. {rels[0]})"
                )
                continue

            prefix = "" if pkg == "." else f"{pkg}/"
            resolved = {f"{prefix}{s.removeprefix(':')}" for s in pkg_srcs}
            for rel in rels:
                if rel not in resolved:
                    errors.append(
                        f"{rel}: not matched by {spec.filegroup_name!r} in "
                        f"{build_file.relative_to(root).as_posix()}"
                    )

            label = _expected_label(
                pkg, spec.filegroup_name, spec.aggregator_pkg
            )
            if label not in agg_refs:
                errors.append(
                    f"{label}: missing from {spec.aggregator_target!r} in "
                    f"{spec.aggregator_build}"
                )

    return errors


def _setup_minimal_repo(root: Path) -> None:
    """Create minimal aggregator BUILD files for unit tests."""
    lint_dir = root / "tools" / "lint"
    lint_dir.mkdir(parents=True, exist_ok=True)
    (lint_dir / "BUILD.bazel").write_text(
        'filegroup(name = "repo_go_srcs", srcs = ["//pkg:go_srcs"])\n'
        'filegroup(name = "repo_yaml_srcs", srcs = ["//:yaml_srcs"])\n'
        'filegroup(name = "repo_template_srcs", srcs = ["//:template_srcs"])\n',
        encoding="utf-8",
    )
    py_dir = root / "devtools" / "linters" / "pylinters"
    py_dir.mkdir(parents=True, exist_ok=True)
    (py_dir / "BUILD.bazel").write_text(
        'filegroup(name = "repo_py_srcs", srcs = ["//pkg:py_srcs"])\n',
        encoding="utf-8",
    )
    (root / "BUILD.bazel").write_text(
        'filegroup(name = "yaml_srcs", srcs = glob(["*.yaml"]))\n'
        'filegroup(name = "template_srcs", srcs = glob(["*.html"]))\n',
        encoding="utf-8",
    )
    (root / "cfg.yaml").write_text("rules: []\n", encoding="utf-8")
    (root / "index.html").write_text("<html/>\n", encoding="utf-8")
    pkg = root / "pkg"
    pkg.mkdir()
    (pkg / "a.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "a_test.go").write_text("package pkg\n", encoding="utf-8")
    (pkg / "a.py").write_text("x = 1\n", encoding="utf-8")
    (pkg / "BUILD.bazel").write_text(
        'filegroup(name = "go_srcs", srcs = glob(["*.go"], '
        'exclude = ["*_test.go"]))\n'
        'filegroup(name = "py_srcs", srcs = glob(["*.py"]))\n',
        encoding="utf-8",
    )


def test_complete_repo_passes(tmp_path: Path) -> None:
    """Passes when every scanned file and package is wired."""
    _setup_minimal_repo(tmp_path)
    assert find_missing_semgrep_inputs(tmp_path) == []


def test_detects_missing_package_filegroup_and_aggregator_entry(
    tmp_path: Path,
) -> None:
    """Reports missing filegroup in a new Go package and missing wire-up."""
    _setup_minimal_repo(tmp_path)
    extra = tmp_path / "extra"
    extra.mkdir()
    (extra / "lib.go").write_text("package extra\n", encoding="utf-8")
    (extra / "BUILD.bazel").write_text("", encoding="utf-8")

    missing = find_missing_semgrep_inputs(tmp_path)
    assert any("extra: missing filegroup 'go_srcs'" in m for m in missing)

    (extra / "BUILD.bazel").write_text(
        'filegroup(name = "go_srcs", srcs = glob(["*.go"]))\n',
        encoding="utf-8",
    )
    missing = find_missing_semgrep_inputs(tmp_path)
    assert missing == [
        (
            "//extra:go_srcs: missing from 'repo_go_srcs' in "
            "tools/lint/BUILD.bazel"
        ),
    ]


def test_glob_does_not_cross_child_package_boundary(tmp_path: Path) -> None:
    """A parent glob does not satisfy a child package with its own BUILD."""
    _setup_minimal_repo(tmp_path)
    child = tmp_path / "pkg" / "sub"
    child.mkdir()
    (child / "sub.go").write_text("package sub\n", encoding="utf-8")
    (child / "BUILD.bazel").write_text("", encoding="utf-8")

    missing = find_missing_semgrep_inputs(tmp_path)
    assert any("pkg/sub: missing filegroup 'go_srcs'" in m for m in missing)


def test_skipped_dirs_and_go_tests_are_ignored(tmp_path: Path) -> None:
    """Ignores *_test.go and skipped directories like node_modules."""
    _setup_minimal_repo(tmp_path)
    nm = tmp_path / "node_modules" / "dep"
    nm.mkdir(parents=True)
    (nm / "ignored.yaml").write_text("a: 1\n", encoding="utf-8")
    (nm / "ignored.html").write_text("<html/>\n", encoding="utf-8")
    assert find_missing_semgrep_inputs(tmp_path) == []


def test_repo_semgrep_inputs_complete() -> None:
    """Every Semgrep-scanned file in the checkout is wired into Bazel."""
    root = _repo_root()
    missing = find_missing_semgrep_inputs(root)
    assert not missing, (
        "Semgrep-scanned files missing from Bazel filegroups:\n"
        + "\n".join(f"  - {item}" for item in missing)
    )


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
