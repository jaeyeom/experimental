"""Hermetic Bazel test runner and unit tests for the four pylinters."""

from __future__ import annotations

import dataclasses
import os
import stat
import sys
from collections.abc import Sequence
from pathlib import Path

import pytest
from pylinters import (
    import_time_work,
    main_guard,
    module_import,
    sibling_import,
)
from pylinters.core import baseline, entries, files


def build_fileset(root: str, rel_paths: Sequence[str]) -> files.GitResolver:
    """Build a GitResolver from declared repo-relative paths without git."""
    executable = [
        p
        for p in rel_paths
        if os.access(os.path.join(root, p), os.X_OK)
        and os.path.isfile(os.path.join(root, p))
    ]
    return files.GitResolver(root, rel_paths, executable)


def load_entry_lists(
    root: str,
    manifest_paths: Sequence[str],
    fs: files.FileSet,
) -> list[tuple[entries.Kind, str]]:
    """Read and parse one or more pylinters entry-list manifest files."""
    out: list[tuple[entries.Kind, str]] = []
    for rel in manifest_paths:
        full = os.path.join(root, rel)
        with open(full, encoding="utf-8") as handle:
            out.extend(entries.parse_entry_list(handle.read(), rel, fs))
    if manifest_paths and not out:
        raise ValueError("entry-list manifests contained no entries")
    return out


def _entries_without_bazel(cfg: entries.EntryConfig) -> entries.EntryConfig:
    """Replace the ``bazel`` source (supplied via entry_list instead)."""
    sources = tuple(s for s in cfg.sources if s != "bazel")
    return dataclasses.replace(cfg, sources=sources)


def run_checker(
    checker: str,
    root: str,
    rel_paths: Sequence[str],
    entry_list_paths: Sequence[str] = (),
) -> list[str]:
    """Run one pylinters checker hermetically and return failure lines."""
    fs = build_fileset(root, rel_paths)
    pyproject = os.path.join(root, "pyproject.toml")
    read = files.root_reader(fs)

    if checker == "import-time-work":
        itw_cfg = import_time_work.load_config(pyproject)
        itw_cfg = dataclasses.replace(
            itw_cfg, entries=_entries_without_bazel(itw_cfg.entries)
        )
        entry_list = load_entry_lists(root, entry_list_paths, fs)
        found = entries.discover(
            fs,
            itw_cfg.entries,
            files.root_reader(fs, limit=2),
            entry_list=entry_list,
        )
        bl_path = os.path.join(root, ".baselines", "py-import-time-work.json")
        bl = baseline.load(bl_path, "py-import-time-work")
        itw_result = import_time_work.check(fs, itw_cfg, found, read)
        if itw_result.files_checked == 0:
            return ["py-import-time-work checked 0 files"]
        existing = [
            p
            for p in fs.py_files()
            if import_time_work.in_scope(itw_cfg, p, found)
            and os.path.isfile(os.path.join(fs.root, p))
        ]
        ev = baseline.evaluate(bl, itw_result.counts, existing)
        return _format_baseline_failures(
            itw_result.violations, itw_result.counts, bl, ev
        )

    if checker == "main-guard":
        mg_cfg = main_guard.load_config(pyproject)
        mg_cfg = dataclasses.replace(
            mg_cfg, entries=_entries_without_bazel(mg_cfg.entries)
        )
        entry_list = load_entry_lists(root, entry_list_paths, fs)
        found = entries.discover(
            fs,
            mg_cfg.entries,
            files.root_reader(fs, limit=2),
            entry_list=entry_list,
        )
        bl_path = os.path.join(root, ".baselines", "py-main-guard.json")
        bl = baseline.load(bl_path, "py-main-guard")
        mg_result = main_guard.check(fs, mg_cfg, found, read)
        if mg_result.files_scanned == 0:
            return ["py-main-guard scanned 0 files"]
        existing = [
            p
            for p in fs.py_files()
            if main_guard.in_scope(mg_cfg, p)
            and os.path.isfile(os.path.join(fs.root, p))
        ]
        ev = baseline.evaluate(bl, mg_result.counts, existing)
        return _format_baseline_failures(
            mg_result.violations, mg_result.counts, bl, ev
        )

    if checker == "module-import":
        mi_cfg = module_import.load_config(pyproject)
        bl_path = os.path.join(root, ".baselines", "py-module-import.json")
        bl = baseline.load(bl_path, "py-module-import")
        mi_result = module_import.check(fs, mi_cfg, read)
        if mi_result.files_scanned == 0:
            return ["py-module-import scanned 0 files"]
        existing = [
            p
            for p in fs.py_files()
            if module_import.in_scope(fs, mi_cfg, p)
            and os.path.isfile(os.path.join(fs.root, p))
        ]
        ev = baseline.evaluate(bl, mi_result.counts, existing)
        return _format_baseline_failures(
            mi_result.violations, mi_result.counts, bl, ev
        )

    if checker == "sibling-import":
        si_cfg = sibling_import.load_config(pyproject)
        si_result = sibling_import.check(fs, si_cfg, read)
        if si_result.files_scanned == 0:
            return ["py-sibling-import scanned 0 files"]
        return [str(v) for v in si_result.violations]

    raise ValueError(f"unknown checker: {checker!r}")


def _format_baseline_failures(
    violations: Sequence[
        import_time_work.Violation
        | main_guard.Violation
        | module_import.Violation
    ],
    counts: dict[str, int],
    bl: baseline.File,
    ev: baseline.Evaluation,
) -> list[str]:
    if not ev.failures and not ev.stale:
        return []
    out = [
        str(v)
        for v in violations
        if counts[v.path] > baseline.allowed(bl, v.path)
    ]
    if bl.entries:
        out.extend(f"baseline exceeded: {line}" for line in ev.failures)
    out.extend(f"stale baseline: {line}" for line in ev.stale)
    return out


def _write_tree(root: Path, tree: dict[str, str]) -> list[str]:
    paths: list[str] = []
    for rel, content in tree.items():
        full = root / rel
        full.parent.mkdir(parents=True, exist_ok=True)
        full.write_text(content, encoding="utf-8")
        paths.append(rel)
    return paths


def test_import_time_work_passes_and_fails(tmp_path: Path) -> None:
    """import-time-work passes on clean entry and fails on module-level call."""
    paths = _write_tree(
        tmp_path,
        {
            "app.py": (
                "def main() -> None:\n    pass\n\n"
                'if __name__ == "__main__":\n    main()\n'
            ),
            "entries.txt": "script app.py\n",
        },
    )
    assert (
        run_checker("import-time-work", str(tmp_path), paths, ["entries.txt"])
        == []
    )

    (tmp_path / "app.py").write_text(
        'import sys\nsys.path.insert(0, ".")\n'
        'if __name__ == "__main__":\n    pass\n',
        encoding="utf-8",
    )
    failures = run_checker(
        "import-time-work", str(tmp_path), paths, ["entries.txt"]
    )
    assert any("sys.path.insert" in line for line in failures)


def test_main_guard_passes_and_fails(tmp_path: Path) -> None:
    """main-guard passes on guarded entry and fails on missing guard."""
    paths = _write_tree(
        tmp_path,
        {
            "tool.py": 'if __name__ == "__main__":\n    pass\n',
            "entries.txt": "script tool.py\n",
        },
    )
    assert (
        run_checker("main-guard", str(tmp_path), paths, ["entries.txt"]) == []
    )

    (tmp_path / "tool.py").write_text("x = 1\n", encoding="utf-8")
    failures = run_checker("main-guard", str(tmp_path), paths, ["entries.txt"])
    assert any("missing-guard" in line for line in failures)


def test_main_guard_detects_executable_file_without_git(
    tmp_path: Path,
) -> None:
    """Executable bit on disk marks a script entry without git ls-files."""
    paths = _write_tree(tmp_path, {"run.py": "x = 1\n"})
    script = tmp_path / "run.py"
    script.chmod(script.stat().st_mode | stat.S_IXUSR)
    failures = run_checker("main-guard", str(tmp_path), paths)
    assert any("missing-guard" in line for line in failures)


def test_module_import_passes_and_fails(tmp_path: Path) -> None:
    """module-import passes on module import and fails on symbol import."""
    paths = _write_tree(
        tmp_path,
        {
            "pkg/__init__.py": "",
            "pkg/helper.py": "VALUE = 1\n",
            "pkg/user.py": "from pkg import helper\n_ = helper.VALUE\n",
        },
    )
    assert run_checker("module-import", str(tmp_path), paths) == []

    (tmp_path / "pkg/user.py").write_text(
        "from pkg.helper import VALUE\n_ = VALUE\n", encoding="utf-8"
    )
    failures = run_checker("module-import", str(tmp_path), paths)
    assert any("from pkg.helper import VALUE" in line for line in failures)


def test_sibling_import_passes_and_fails(tmp_path: Path) -> None:
    """sibling-import passes on qualified import and fails on bare sibling."""
    paths = _write_tree(
        tmp_path,
        {
            "pkg/__init__.py": "",
            "pkg/helper.py": "VALUE = 1\n",
            "pkg/user.py": "from pkg import helper\n_ = helper.VALUE\n",
        },
    )
    assert run_checker("sibling-import", str(tmp_path), paths) == []

    (tmp_path / "pkg/user.py").write_text(
        "import helper\n_ = helper.VALUE\n", encoding="utf-8"
    )
    failures = run_checker("sibling-import", str(tmp_path), paths)
    assert any("bare import of sibling module" in line for line in failures)


def test_repo_lint() -> None:
    """Run the configured pylinters checker across declared repo inputs."""
    checker = os.environ.get("PYLINTERS_CHECKER")
    if not checker:
        pytest.skip("PYLINTERS_CHECKER not set (run via bazel test)")
        return
    raw_files = os.environ.get("PYLINTERS_FILES", "").split()
    assert raw_files, "PYLINTERS_FILES must list repo Python files"
    raw_entries = os.environ.get("PYLINTERS_ENTRY_LISTS", "").split()
    failures = run_checker(checker, os.getcwd(), raw_files, raw_entries)
    assert not failures, "\n".join(failures)


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
