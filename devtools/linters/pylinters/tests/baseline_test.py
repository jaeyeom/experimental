"""Tests for pylinters.core.baseline."""

import json
import sys
from pathlib import Path

import pytest
from pylinters.core import baseline
from pylinters.core.baseline import Entry, File
from pylinters.core.errors import ConfigError

CHECKER = "py-module-import"


def _file(entries: dict[str, Entry]) -> File:
    return File(checker=CHECKER, entries=entries)


def _limit(n: int, **kw: str) -> Entry:
    return Entry(limits={baseline.METRIC: n}, **kw)


def _write(tmp_path: Path, obj: object) -> str:
    p = tmp_path / "b.json"
    p.write_text(json.dumps(obj), encoding="utf-8")
    return str(p)


def test_dumps_matches_spec_example() -> None:
    """Dumps matches spec example."""
    f = _file(
        {
            "legacy/a.py": Entry(
                limits={baseline.METRIC: 3}, reason="Legacy", issue="#1069"
            ),
            "legacy/b.py": Entry(),
        }
    )
    assert baseline.dumps(f) == (
        "{\n"
        '  "checker": "py-module-import",\n'
        '  "entries": {\n'
        '    "legacy/a.py": {\n'
        '      "issue": "#1069",\n'
        '      "limits": {\n'
        '        "max_violations": 3\n'
        "      },\n"
        '      "reason": "Legacy"\n'
        "    },\n"
        '    "legacy/b.py": {}\n'
        "  },\n"
        '  "version": 1\n'
        "}\n"
    )


def test_save_load_round_trip(tmp_path: Path) -> None:
    """Save load round trip."""
    f = _file({"a.py": _limit(2, reason="r")})
    path = str(tmp_path / "sub" / "dir" / "b.json")
    baseline.save(path, f)
    assert baseline.load(path, CHECKER) == f


def test_load_missing_returns_empty(tmp_path: Path) -> None:
    """Load missing returns empty."""
    f = baseline.load(str(tmp_path / "nope.json"), CHECKER)
    assert f == File(checker=CHECKER, entries={})


def test_load_rejects_bad_json(tmp_path: Path) -> None:
    """Load rejects bad json."""
    p = tmp_path / "b.json"
    p.write_text("{", encoding="utf-8")
    with pytest.raises(ConfigError):
        baseline.load(str(p), CHECKER)


@pytest.mark.parametrize(
    "obj",
    [
        {"version": 1, "checker": CHECKER, "entries": {}, "extra": 1},
        {"version": 1, "checker": CHECKER, "entries": {"a.py": {"limit": 1}}},
        {
            "version": 1,
            "checker": CHECKER,
            "entries": {"a.py": {"limits": {"max_bytes": 1}}},
        },
        {
            "version": 1,
            "checker": CHECKER,
            "entries": {"a.py": {"limits": {"max_violations": "1"}}},
        },
        {
            "version": 1,
            "checker": CHECKER,
            "entries": {"a.py": {"limits": {"max_violations": True}}},
        },
    ],
)
def test_load_rejects_unknown_field(tmp_path: Path, obj: object) -> None:
    """Load rejects unknown field."""
    with pytest.raises(ConfigError):
        baseline.load(_write(tmp_path, obj), CHECKER)


@pytest.mark.parametrize("version", [2, 1.0, True])
def test_load_rejects_bad_version(tmp_path: Path, version: object) -> None:
    """Load rejects any version other than the integer 1."""
    obj = {"version": version, "checker": CHECKER, "entries": {}}
    with pytest.raises(ConfigError, match="version"):
        baseline.load(_write(tmp_path, obj), CHECKER)


@pytest.mark.parametrize(
    "key", ["/abs", "a/../b", "./a", "a/", "a\\b", "a//b", "."]
)
def test_load_rejects_unnormalized_key(tmp_path: Path, key: str) -> None:
    """Load rejects unnormalized key."""
    obj = {"version": 1, "checker": CHECKER, "entries": {key: {}}}
    with pytest.raises(ConfigError):
        baseline.load(_write(tmp_path, obj), CHECKER)


def test_load_rejects_checker_mismatch(tmp_path: Path) -> None:
    """Load rejects checker mismatch."""
    obj = {"version": 1, "checker": "other", "entries": {}}
    with pytest.raises(ConfigError, match="checker"):
        baseline.load(_write(tmp_path, obj), CHECKER)


def test_evaluate_new_violation_fails() -> None:
    """Evaluate new violation fails."""
    ev = baseline.evaluate(_file({}), {"a.py": 1, "b.py": 0}, {"a.py", "b.py"})
    assert ev.failures == ["a.py: 1 violation(s), baseline allows 0"]
    assert ev.stale == []


def test_evaluate_within_limit_passes() -> None:
    """Evaluate within limit passes."""
    f = _file({"a.py": _limit(3)})
    ev = baseline.evaluate(f, {"a.py": 3}, {"a.py"})
    assert ev.failures == []
    assert ev.stale == []


def test_evaluate_over_limit_fails() -> None:
    """Evaluate over limit fails."""
    f = _file({"a.py": _limit(3)})
    ev = baseline.evaluate(f, {"a.py": 4}, {"a.py"})
    assert ev.failures == ["a.py: 4 violation(s), baseline allows 3"]


def test_evaluate_stale_conditions() -> None:
    """Evaluate stale conditions."""
    f = _file({"t.py": _limit(3), "c.py": _limit(3), "gone.py": _limit(3)})
    ev = baseline.evaluate(f, {"t.py": 1, "c.py": 0}, {"t.py", "c.py"})
    assert ev.failures == []
    assert len(ev.stale) == 3
    by_path = {m.split(":")[0]: m for m in ev.stale}
    assert "tighten" in by_path["t.py"]
    assert "clean" in by_path["c.py"]
    assert "deleted" in by_path["gone.py"]


def test_evaluate_scanned_limits_stale() -> None:
    """Evaluate scanned limits stale."""
    f = _file({"a.py": _limit(3), "b.py": _limit(3)})
    counts = {"a.py": 3}
    ev = baseline.evaluate(f, counts, {"a.py", "b.py"}, scanned={"a.py"})
    assert ev.stale == []
    ev = baseline.evaluate(f, {"a.py": 3, "b.py": 0}, {"a.py", "b.py"})
    assert len(ev.stale) == 1
    assert ev.stale[0].startswith("b.py")


def test_glob_pure_exemption_and_precedence() -> None:
    """Glob pure exemption and precedence."""
    f = _file(
        {
            "legacy/**": Entry(),
            "legacy/x.py": _limit(1),
            "unused/**": Entry(),
        }
    )
    existing = {"legacy/x.py", "legacy/y.py"}
    counts = {"legacy/x.py": 2, "legacy/y.py": 5}
    ev = baseline.evaluate(f, counts, existing, scanned=set(counts))
    assert ev.failures == ["legacy/x.py: 2 violation(s), baseline allows 1"]
    assert ev.stale == []
    ev = baseline.evaluate(f, counts, existing)
    assert len(ev.stale) == 1
    assert ev.stale[0].startswith("unused/**")


def test_glob_longest_prefix_then_lexicographic() -> None:
    """Glob longest prefix then lexicographic."""
    f = _file(
        {
            "**/*.py": _limit(9),
            "a/**": _limit(5),
            "a/b/*.py": _limit(1),
            "a/b/?.py": _limit(7),
        }
    )
    # "a/b/*.py" and "a/b/?.py" tie on prefix length; "a/b/*.py" < "a/b/?.py".
    ev = baseline.evaluate(f, {"a/b/c.py": 2}, {"a/b/c.py"})
    assert ev.failures == ["a/b/c.py: 2 violation(s), baseline allows 1"]
    ev = baseline.evaluate(f, {"a/b/c/d.py": 6}, {"a/b/c/d.py"})
    assert ev.failures == ["a/b/c/d.py: 6 violation(s), baseline allows 5"]
    ev = baseline.evaluate(f, {"z/d.py": 10}, {"z/d.py"})
    assert ev.failures == ["z/d.py: 10 violation(s), baseline allows 9"]


def test_updated_ratchets_and_preserves_metadata() -> None:
    """Updated ratchets and preserves metadata."""
    f = _file(
        {
            "tight.py": _limit(5, reason="r", issue="#1"),
            "worse.py": _limit(1, reason="w"),
            "clean.py": _limit(2),
            "gone.py": _limit(2),
            "keep.py": Entry(),
            "g/**": Entry(reason="manual"),
        }
    )
    counts = {
        "tight.py": 2,
        "worse.py": 4,
        "clean.py": 0,
        "new.py": 3,
        "g/z.py": 9,
    }
    existing = {
        "tight.py",
        "worse.py",
        "clean.py",
        "keep.py",
        "new.py",
        "g/z.py",
    }
    got = baseline.updated(f, counts, existing)
    assert got.checker == CHECKER
    assert got.entries == {
        "tight.py": _limit(2, reason="r", issue="#1"),
        "worse.py": _limit(4, reason="w"),
        "new.py": _limit(3),
        "keep.py": Entry(),
        "g/**": Entry(reason="manual"),
    }
    assert f.entries["tight.py"] == _limit(5, reason="r", issue="#1")


def test_tightened_never_adds_or_loosens() -> None:
    """Tightened never adds or loosens."""
    f = _file(
        {
            "tight.py": _limit(5, reason="r"),
            "worse.py": _limit(1),
            "clean.py": _limit(2),
            "gone.py": _limit(2),
        }
    )
    counts = {"tight.py": 2, "worse.py": 4, "clean.py": 0, "new.py": 3}
    existing = {"tight.py", "worse.py", "clean.py", "new.py"}
    got, failures = baseline.tightened(f, counts, existing)
    assert got.entries == {
        "tight.py": _limit(2, reason="r"),
        "worse.py": _limit(1),
    }
    assert sorted(failures) == [
        "new.py: 3 violation(s), baseline allows 0",
        "worse.py: 4 violation(s), baseline allows 1",
    ]


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
