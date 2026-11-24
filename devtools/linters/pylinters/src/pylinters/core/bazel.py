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
    unresolved: int = 0


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
        SourceError: The query fails, or a target's main matches several
            srcs.
    """
    base = _dir(bazel_dir)
    cwd = os.path.join(fs.root, base) if base else fs.root
    # Unflattened, a select() keeps every branch, so a main behind one is
    # checked on every platform.
    out = run(
        [
            bazel,
            "query",
            QUERY,
            "--output=streamed_jsonproto",
            "--noproto:flatten_selects",
        ],
        cwd,
    )
    targets: list[Target] = []
    counts = {"generated": 0, "module_main": 0, "unresolved": 0}
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
        for outcome in _resolve(fs, base, rule):
            if isinstance(outcome, str):
                counts[outcome] += 1
            else:
                targets.append(Target(rule["name"], kind, outcome[0]))
    return Found(targets, **counts)


def _resolve(
    fs: files.FileSet, base: str, rule: dict[str, Any]
) -> list[tuple[str] | str]:
    """Return ``(path,)`` per main file, or skip-counter names.

    A ``select()`` on ``main`` yields one main per branch. Mains in other
    repositories yield nothing.
    """
    label = rule["name"]
    attrs = {a["name"]: a for a in rule.get("attribute", [])}
    if _string(attrs.get("main_module")):
        return ["module_main"]
    package = _package(label)
    srcs = [
        p
        for p in (_label_path(s, package) for s in _values(attrs.get("srcs")))
        if p is not None
    ]
    main_attr = attrs.get("main")
    if main_attr is not None and main_attr.get("explicitlySpecified"):
        mains = [
            (path, main_label.rpartition(":")[2] or path)
            for main_label in _values(main_attr)
            if (path := _label_path(main_label, package)) is not None
        ]
    else:
        name = _string(attrs.get("name")) or label.rpartition(":")[2]
        mains = [(posixpath.join(package, name + ".py"), name + ".py")]
    out: list[tuple[str] | str] = []
    for main, suffix in mains:
        matches = [p for p in srcs if p == main] or [
            p for p in srcs if ("/" + p).endswith("/" + suffix)
        ]
        if not matches:
            # srcs made by another rule (a filegroup, a genrule) are labels,
            # not files; the main cannot be pinned to a tracked file.
            out.append("unresolved")
            continue
        if len(matches) > 1:
            raise errors.SourceError(
                f"{label}: main {suffix} matches {len(matches)} srcs"
            )
        path = files.join(base, matches[0])
        out.append((path,) if fs.has_file(path) else "generated")
    return out


def _values(attr: dict[str, Any] | None) -> list[str]:
    """Return an attribute's labels, the union of all branches of a select()."""
    if not attr:
        return []
    if "selectorList" in attr:
        return [
            v
            for element in attr["selectorList"].get("elements", [])
            for entry in element.get("entries", [])
            for v in _entry_values(entry)
        ]
    return _entry_values(attr)


def _entry_values(item: dict[str, Any]) -> list[str]:
    if "stringListValue" in item:
        return [str(v) for v in item["stringListValue"]]
    value = item.get("stringValue")
    return [str(value)] if value else []


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
