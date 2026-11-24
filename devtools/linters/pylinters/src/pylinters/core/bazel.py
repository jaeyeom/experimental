"""Main files of Bazel ``py_binary`` and ``py_test`` targets.

``bazel query`` sees targets after macros and list comprehensions expand,
which parsing BUILD files cannot. Main resolution follows rules_python:
``main`` if given, else ``<name>.py``, matched against ``srcs`` by exact
path, else by trailing path.
"""

from __future__ import annotations

import json
import os
import posixpath
import subprocess
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from typing import Any, Literal

from pylinters.core import errors, files

QUERY = 'kind("py_binary rule|py_test rule", //...)'

Runner = Callable[[Sequence[str], str], str]

_KINDS: dict[str, Literal["script", "test"]] = {
    "py_binary": "script",
    "py_test": "test",
}
_WORKSPACE_FILES = ("MODULE.bazel", "WORKSPACE", "WORKSPACE.bazel")


@dataclass(frozen=True)
class Target:
    """A target and the tracked file it runs as ``__main__``."""

    label: str
    kind: Literal["script", "test"]
    path: str


@dataclass(frozen=True)
class Found:
    """Resolved targets, and counts of targets skipped by design."""

    targets: list[Target] = field(default_factory=list)
    generated: int = 0
    module_main: int = 0
    select_main: int = 0


def run_subprocess(argv: Sequence[str], cwd: str) -> str:
    """Run ``argv`` in ``cwd`` and return stdout.

    Raises:
        SourceError: The binary is missing or exits non-zero.
    """
    try:
        out = subprocess.run(
            list(argv), cwd=cwd, check=True, capture_output=True
        )
    except FileNotFoundError as err:
        raise errors.SourceError(f"cannot run {argv[0]}: {err}") from err
    except subprocess.CalledProcessError as err:
        msg = err.stderr.decode("utf-8", "replace").strip()
        raise errors.SourceError(
            f"{' '.join(argv)} failed in {cwd}: {msg}"
        ) from err
    return out.stdout.decode("utf-8", "surrogateescape")


def has_workspace(fs: files.FileSet, bazel_dir: str) -> bool:
    """Report whether ``bazel_dir`` holds a tracked Bazel workspace file."""
    base = _dir(bazel_dir)
    return any(fs.is_tracked(files.join(base, n)) for n in _WORKSPACE_FILES)


def find_mains(
    fs: files.GitResolver,
    bazel_dir: str,
    bazel: str = "bazel",
    run: Runner = run_subprocess,
) -> Found:
    """Return the main file of every ``py_binary`` and ``py_test``.

    Raises:
        SourceError: The query fails, or a target's main matches zero or
            several srcs.
    """
    base = _dir(bazel_dir)
    cwd = os.path.join(fs.root, base) if base else fs.root
    out = run([bazel, "query", QUERY, "--output=streamed_jsonproto"], cwd)
    targets: list[Target] = []
    counts = {"generated": 0, "module_main": 0, "select_main": 0}
    for line in out.splitlines():
        if not line.strip():
            continue
        try:
            rule = json.loads(line)["rule"]
        except (ValueError, KeyError, TypeError) as err:
            raise errors.SourceError(
                f"unexpected bazel query output: {line[:200]!r}"
            ) from err
        kind = _KINDS.get(rule.get("ruleClass", ""))
        if kind is None:
            continue
        outcome = _resolve(fs, base, rule)
        if isinstance(outcome, str):
            counts[outcome] += 1
        elif outcome is not None:
            targets.append(Target(rule["name"], kind, outcome[0]))
    return Found(targets, **counts)


def _resolve(
    fs: files.FileSet, base: str, rule: dict[str, Any]
) -> tuple[str] | str | None:
    """Return ``(path,)``, a skip-counter name, or None for external."""
    label = rule["name"]
    attrs = {a["name"]: a for a in rule.get("attribute", [])}
    if _string(attrs.get("main_module")):
        return "module_main"
    package = _package(label)
    main_attr = attrs.get("main")
    if main_attr is not None and main_attr.get("explicitlySpecified"):
        main_label = _string(main_attr)
        if not main_label:
            return "select_main"
        main = _label_path(main_label, package)
        if main is None:
            return None
        suffix = main_label.rpartition(":")[2] or main
    else:
        name = _string(attrs.get("name")) or label.rpartition(":")[2]
        suffix = name + ".py"
        main = posixpath.join(package, suffix)
    srcs = [
        p
        for p in (
            _label_path(s, package)
            for s in attrs.get("srcs", {}).get("stringListValue", [])
        )
        if p is not None
    ]
    matches = [p for p in srcs if p == main] or [
        p for p in srcs if ("/" + p).endswith("/" + suffix)
    ]
    if len(matches) != 1:
        raise errors.SourceError(
            f"{label}: main {suffix} matches {len(matches)} srcs"
        )
    path = files.join(base, matches[0])
    return (path,) if fs.has_file(path) else "generated"


def _string(attr: dict[str, Any] | None) -> str:
    return str(attr.get("stringValue", "")) if attr else ""


def _package(label: str) -> str:
    return label.removeprefix("//").partition(":")[0]


def _label_path(label: str, package: str) -> str | None:
    """Map a main-repo file label to a path relative to the workspace."""
    if label.startswith("@"):
        return None
    if label.startswith("//"):
        pkg, _, name = label[2:].partition(":")
        if not name:
            return None
    else:
        pkg, name = package, label.removeprefix(":")
    return posixpath.normpath(posixpath.join(pkg, name))


def _dir(bazel_dir: str) -> str:
    norm = posixpath.normpath(bazel_dir)
    return "" if norm == "." else norm
