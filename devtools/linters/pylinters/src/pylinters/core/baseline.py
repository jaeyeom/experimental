"""Checker baseline support for the ``max_violations`` metric.

Implements the repository checker-baseline contract
(``devtools/docs/checker-baseline.md``) for checkers whose only metric is
a per-file violation count with a default maximum of zero.
"""

from __future__ import annotations

import json
import posixpath
from collections.abc import Collection, Mapping
from dataclasses import dataclass, field, replace
from pathlib import Path
from typing import Any

from pylinters.core import errors, globs

METRIC = "max_violations"
VERSION = 1
_TOP_FIELDS = frozenset({"version", "checker", "entries"})
_ENTRY_FIELDS = frozenset({"limits", "reason", "issue"})


@dataclass
class Entry:
    """One baseline entry; no ``limits`` means a pure exemption."""

    limits: dict[str, int] = field(default_factory=dict)
    reason: str = ""
    issue: str = ""


@dataclass
class File:
    """A parsed baseline file."""

    checker: str
    entries: dict[str, Entry]
    version: int = VERSION


@dataclass(frozen=True)
class Evaluation:
    """Result of checking counts against a baseline."""

    failures: list[str]
    stale: list[str]


def load(path: str, checker: str) -> File:
    """Load the baseline at ``path``; a missing file is an empty baseline."""
    try:
        text = Path(path).read_text(encoding="utf-8")
    except FileNotFoundError:
        return File(checker=checker, entries={})
    except OSError as e:
        raise errors.ConfigError(f"{path}: {e}") from e
    try:
        raw = json.loads(text)
    except json.JSONDecodeError as e:
        raise errors.ConfigError(f"{path}: invalid JSON: {e}") from e
    try:
        return _parse(raw, checker)
    except errors.ConfigError as e:
        raise errors.ConfigError(f"{path}: {e}") from e


def dumps(f: File) -> str:
    """Serialize ``f`` as canonical JSON."""
    entries = {key: _entry_json(e) for key, e in f.entries.items()}
    doc = {"version": f.version, "checker": f.checker, "entries": entries}
    return json.dumps(doc, indent=2, sort_keys=True, ensure_ascii=False) + "\n"


def save(path: str, f: File) -> None:
    """Write ``f`` to ``path``, creating parent directories."""
    p = Path(path)
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(dumps(f), encoding="utf-8")


def evaluate(
    f: File,
    counts: Mapping[str, int],
    existing: Collection[str],
    scanned: Collection[str] | None = None,
) -> Evaluation:
    """Compare per-path violation ``counts`` with the baseline.

    ``existing`` holds every in-scope path of the whole tree. ``scanned``
    restricts staleness checks to a subset of paths (None for full runs).
    """
    failures = [
        _failure(path, count, allowed(f, path))
        for path, count in sorted(counts.items())
        if count > allowed(f, path)
    ]
    stale: list[str] = []
    in_scope = set(scanned) if scanned is not None else None
    existing_set = set(existing)
    for key, entry in sorted(f.entries.items()):
        if _is_glob(key):
            if in_scope is None and not any(
                globs.match(key, p) for p in existing_set
            ):
                stale.append(f"{key}: glob matches no files")
            continue
        if in_scope is not None and key not in in_scope:
            continue
        if key not in existing_set:
            stale.append(
                f"{key}: baseline entry refers to a file that is deleted "
                "or out of scope"
            )
        elif entry.limits and key in counts:
            limit, count = entry.limits[METRIC], counts[key]
            if count == 0:
                stale.append(f"{key}: now clean, remove baseline entry")
            elif count < limit:
                stale.append(
                    f"{key}: can tighten to {count} (currently {limit})"
                )
    return Evaluation(failures=failures, stale=stale)


def updated(
    f: File, counts: Mapping[str, int], existing: Collection[str]
) -> File:
    """Return ``f`` ratcheted to ``counts``, recording new violations."""
    return _rewrite(f, counts, existing, add=True)


def tightened(
    f: File, counts: Mapping[str, int], existing: Collection[str]
) -> tuple[File, list[str]]:
    """Return ``f`` with only stale or loose entries fixed.

    Also returns the failure messages for violations the baseline does not
    allow, which tightening never records.
    """
    new = _rewrite(f, counts, existing, add=False)
    return new, evaluate(f, counts, existing).failures


def _rewrite(
    f: File, counts: Mapping[str, int], existing: Collection[str], *, add: bool
) -> File:
    existing_set = set(existing)
    entries: dict[str, Entry] = {}
    for key, entry in f.entries.items():
        if _is_glob(key) or not entry.limits:
            if _is_glob(key) or key in existing_set:
                entries[key] = entry
            continue
        if key not in existing_set:
            continue
        limit, count = entry.limits[METRIC], counts.get(key)
        if count is None:
            entries[key] = entry
        elif count == 0:
            continue
        elif count < limit or (add and count > limit):
            entries[key] = replace(entry, limits={METRIC: count})
        else:
            entries[key] = entry
    if add:
        for path, count in counts.items():
            if path not in f.entries and count > allowed(f, path):
                entries[path] = Entry(limits={METRIC: count})
    return File(checker=f.checker, entries=entries, version=f.version)


def allowed(f: File, path: str) -> int:
    """Return the max violations allowed in ``path`` (unbounded if exempt)."""
    entry = _resolve(f, path)
    if entry is None:
        return 0
    if not entry.limits:
        return 2**63 - 1
    return entry.limits[METRIC]


def _resolve(f: File, path: str) -> Entry | None:
    """Pick the entry for ``path``: exact, else longest-prefix glob."""
    if path in f.entries:
        return f.entries[path]
    matching = [k for k in f.entries if _is_glob(k) and globs.match(k, path)]
    if not matching:
        return None
    return f.entries[min(matching, key=lambda k: (-_prefix_len(k), k))]


def _is_glob(key: str) -> bool:
    return any(c in key for c in "*?[")


def _prefix_len(pattern: str) -> int:
    for i, c in enumerate(pattern):
        if c in "*?[":
            return i
    return len(pattern)


def _failure(path: str, count: int, allowed: int) -> str:
    return f"{path}: {count} violation(s), baseline allows {allowed}"


def _entry_json(e: Entry) -> dict[str, Any]:
    out: dict[str, Any] = {}
    if e.limits:
        out["limits"] = dict(e.limits)
    if e.reason:
        out["reason"] = e.reason
    if e.issue:
        out["issue"] = e.issue
    return out


def _parse(raw: object, checker: str) -> File:
    if not isinstance(raw, dict):
        raise errors.ConfigError("top level must be an object")
    _reject_unknown(raw, _TOP_FIELDS, "baseline")
    if type(raw.get("version")) is not int or raw["version"] != VERSION:
        raise errors.ConfigError(
            f"unsupported version {raw.get('version')!r}, want {VERSION}"
        )
    if raw.get("checker") != checker:
        raise errors.ConfigError(
            f"checker is {raw.get('checker')!r}, want {checker!r}"
        )
    raw_entries = raw.get("entries")
    if not isinstance(raw_entries, dict):
        raise errors.ConfigError("entries must be an object")
    entries = {}
    for key, value in raw_entries.items():
        _check_key(key)
        entries[key] = _parse_entry(key, value)
    return File(checker=checker, entries=entries)


def _parse_entry(key: str, raw: object) -> Entry:
    if not isinstance(raw, dict):
        raise errors.ConfigError(f"entry {key!r} must be an object")
    _reject_unknown(raw, _ENTRY_FIELDS, f"entry {key!r}")
    limits = raw.get("limits", {})
    if not isinstance(limits, dict):
        raise errors.ConfigError(f"entry {key!r}: limits must be an object")
    for metric, value in limits.items():
        if metric != METRIC:
            raise errors.ConfigError(
                f"entry {key!r}: unknown metric {metric!r}"
            )
        if not isinstance(value, int) or isinstance(value, bool):
            raise errors.ConfigError(
                f"entry {key!r}: {metric} must be an integer"
            )
    reason, issue = raw.get("reason", ""), raw.get("issue", "")
    if not isinstance(reason, str) or not isinstance(issue, str):
        raise errors.ConfigError(
            f"entry {key!r}: reason and issue must be strings"
        )
    return Entry(limits=dict(limits), reason=reason, issue=issue)


def _reject_unknown(
    raw: dict[str, Any], allowed: frozenset[str], what: str
) -> None:
    unknown = sorted(set(raw) - allowed)
    if unknown:
        raise errors.ConfigError(
            f"{what}: unknown field(s) {', '.join(unknown)}"
        )


def _check_key(key: str) -> None:
    bad = (
        not key
        or key == "."
        or key.startswith("/")
        or "\\" in key
        or ".." in key.split("/")
        or posixpath.normpath(key) != key
    )
    if bad:
        raise errors.ConfigError(
            f"entry key {key!r} is not a normalized repo-relative path"
        )
