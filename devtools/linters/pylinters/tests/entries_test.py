"""Tests for pylinters.core.entries."""

import json
import sys
from collections.abc import Sequence

import pytest
from pylinters.core import bazel, entries, errors, files

NO_BAZEL = entries.EntryConfig(sources=("executable",))


def _fs(*paths: str, executable: Sequence[str] = ()) -> files.GitResolver:
    return files.GitResolver("/repo", paths, executable)


def _reader(data: dict[str, bytes]):  # type: ignore[no-untyped-def]
    def read(path: str) -> bytes:
        if path not in data:
            raise FileNotFoundError(path)
        return data[path]

    return read


def _bazel(*targets: tuple[str, str, str]) -> bazel.Runner:
    """Return a Runner printing py rules for (label, class, src)."""
    lines = [
        json.dumps(
            {
                "type": "RULE",
                "rule": {
                    "name": label,
                    "ruleClass": cls,
                    "attribute": [
                        {"name": "srcs", "stringListValue": [src]},
                    ],
                },
            }
        )
        for label, cls, src in targets
    ]

    def run(argv: Sequence[str], cwd: str) -> str:
        return "".join(line + "\n" for line in lines)

    return run


def test_executable_bit_and_shebang() -> None:
    """Mode 100755 or a #! first line makes a script entry."""
    fs = _fs("a.py", "b.py", "c.py", executable=["a.py"])
    read = _reader(
        {"a.py": b"", "b.py": b"#!/usr/bin/env python3\n", "c.py": b"x = 1\n"}
    )
    found = entries.discover(fs, NO_BAZEL, read)
    assert found.entries == {
        "a.py": entries.Entry("a.py", "script", ("executable",)),
        "b.py": entries.Entry("b.py", "script", ("executable",)),
    }
    assert found.counts == {"executable": 2}


def test_shebang_binary_and_empty() -> None:
    """Undecodable or empty files are not entries and do not crash."""
    fs = _fs("a.py", "b.py", "gone.py")
    read = _reader({"a.py": b"\xff\xfe", "b.py": b""})
    assert entries.discover(fs, NO_BAZEL, read).entries == {}


def test_dunder_main_not_executable_entry() -> None:
    """__main__.py is not an entry even when executable."""
    fs = _fs("pkg/__main__.py", executable=["pkg/__main__.py"])
    assert entries.discover(fs, NO_BAZEL, _reader({})).entries == {}


def test_bazel_kinds_and_reason() -> None:
    """py_binary mains are scripts, py_test mains tests, with the label."""
    fs = _fs("MODULE.bazel", "tools/a.py", "tools/a_test.py")
    run = _bazel(
        ("//tools:a", "py_binary", "//tools:a.py"),
        ("//tools:a_test", "py_test", "//tools:a_test.py"),
    )
    found = entries.discover(
        fs, entries.EntryConfig(sources=("bazel",)), _reader({}), run=run
    )
    assert found.entries == {
        "tools/a.py": entries.Entry(
            "tools/a.py", "script", ("bazel //tools:a",)
        ),
        "tools/a_test.py": entries.Entry(
            "tools/a_test.py", "test", ("bazel //tools:a_test",)
        ),
    }
    assert found.counts == {"bazel": 2}


def test_bazel_skipped_without_workspace() -> None:
    """No tracked workspace file: the source is skipped with a note."""

    def run(argv: Sequence[str], cwd: str) -> str:
        raise AssertionError("bazel must not run")

    found = entries.discover(
        _fs("a.py"), entries.EntryConfig(), _reader({"a.py": b""}), run=run
    )
    assert found.counts["bazel"] == 0
    assert "bazel: skipped (no workspace)" in found.notes


def test_bazel_skip_counts_become_notes() -> None:
    """Generated mains are reported in the notes."""
    fs = _fs("MODULE.bazel")
    run = _bazel(("//pkg:gen", "py_binary", "//pkg:gen.py"))
    found = entries.discover(
        fs, entries.EntryConfig(sources=("bazel",)), _reader({}), run=run
    )
    assert found.notes == ["bazel: 1 generated main(s) skipped"]


def test_bazel_error_propagates() -> None:
    """A failing query is a SourceError."""

    def run(argv: Sequence[str], cwd: str) -> str:
        raise errors.SourceError("boom")

    with pytest.raises(errors.SourceError):
        entries.discover(
            _fs("MODULE.bazel"), entries.EntryConfig(), _reader({}), run=run
        )


def test_globs() -> None:
    """script-globs and test-globs select entries by path."""
    cfg = entries.EntryConfig(
        sources=("globs",),
        script_globs=("bin/*.py",),
        test_globs=("**/*_test.py",),
    )
    fs = _fs("bin/x.py", "lib/y.py", "lib/y_test.py")
    found = entries.discover(fs, cfg, _reader({}))
    assert {p: e.kind for p, e in found.entries.items()} == {
        "bin/x.py": "script",
        "lib/y_test.py": "test",
    }
    assert found.counts == {"globs": 2}


def test_globs_source_requires_patterns() -> None:
    """Selecting globs with no patterns is a configuration error."""
    with pytest.raises(errors.ConfigError, match="globs"):
        entries.discover(
            _fs(), entries.EntryConfig(sources=("globs",)), _reader({})
        )


def test_unknown_source() -> None:
    """An unknown source name is a configuration error."""
    with pytest.raises(errors.ConfigError, match="nope"):
        entries.discover(
            _fs(), entries.EntryConfig(sources=("nope",)), _reader({})
        )


def test_both_kinds_is_test() -> None:
    """Found as script and as test: the entry is a test with both reasons."""
    fs = _fs("MODULE.bazel", "t_test.py", executable=["t_test.py"])
    run = _bazel(("//:t_test", "py_test", "//:t_test.py"))
    found = entries.discover(
        fs, entries.EntryConfig(), _reader({"t_test.py": b""}), run=run
    )
    assert found.entries["t_test.py"] == entries.Entry(
        "t_test.py", "test", ("executable", "bazel //:t_test")
    )


def test_entry_list() -> None:
    """List entries are added with reason 'list'."""
    fs = _fs("tools/a.py", "t/b_test.py", "pkg/__main__.py")
    pairs = entries.parse_entry_list(
        "# c\nscript tools/a.py\n\ntest t/b_test.py\nscript pkg/__main__.py\n",
        "list.txt",
        fs,
    )
    found = entries.discover(
        fs, entries.EntryConfig(sources=()), _reader({}), entry_list=pairs
    )
    assert {p: e.kind for p, e in found.entries.items()} == {
        "tools/a.py": "script",
        "t/b_test.py": "test",
        "pkg/__main__.py": "script",
    }
    assert found.counts == {"list": 3}


def test_entry_list_bad_kind() -> None:
    """An unknown kind is an error naming the line."""
    with pytest.raises(errors.ConfigError, match="list.txt:1:"):
        entries.parse_entry_list("bin x.py\n", "list.txt", _fs("x.py"))


def test_entry_list_untracked() -> None:
    """A path that is not a tracked .py is an error naming the line."""
    with pytest.raises(errors.ConfigError, match="list.txt:2:.*nope.py"):
        entries.parse_entry_list(
            "script x.py\nscript nope.py\n", "list.txt", _fs("x.py")
        )


def test_bazel_external_main_note() -> None:
    """A main in another repository is noted, not silently dropped."""
    fs = _fs("MODULE.bazel")
    run = _bazel(("//pkg:a", "py_binary", "@ext//x:a.py"))

    def run_with_main(argv: Sequence[str], cwd: str) -> str:
        rule = json.loads(run(argv, cwd))
        rule["rule"]["attribute"].append(
            {
                "name": "main",
                "explicitlySpecified": True,
                "stringValue": "@ext//x:a.py",
            }
        )
        return json.dumps(rule) + "\n"

    found = entries.discover(
        fs,
        entries.EntryConfig(sources=("bazel",)),
        _reader({}),
        run=run_with_main,
    )
    assert found.notes == ["bazel: 1 main(s) in another repository skipped"]


def test_no_sources_and_no_list_is_error() -> None:
    """With every source turned off and no list, nothing would be checked."""
    with pytest.raises(errors.ConfigError, match="entry source"):
        entries.discover(
            _fs("a.py"), entries.EntryConfig(sources=()), _reader({})
        )


def test_entry_list_any_whitespace() -> None:
    """Kind and path may be separated by tabs or several spaces."""
    pairs = entries.parse_entry_list(
        "script\tx.py\ntest   y_test.py\n",
        "list.txt",
        _fs("x.py", "y_test.py"),
    )
    assert pairs == [("script", "x.py"), ("test", "y_test.py")]


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
