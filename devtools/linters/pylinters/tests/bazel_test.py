"""Tests for pylinters.core.bazel."""

import json
import sys
from collections.abc import Sequence

import pytest
from pylinters.core import bazel, errors, files


def rule(label: str, cls: str, **attrs: object) -> str:
    """Return one streamed_jsonproto line for a rule.

    String attrs become ``stringValue``; list attrs ``stringListValue``;
    ``None`` is an explicitly specified attribute with no value, which is
    how ``bazel query`` prints a ``select()`` on a single-label attribute.
    """
    name = label.rpartition(":")[2]
    attributes: list[dict[str, object]] = [
        {"name": "name", "type": "STRING", "stringValue": name}
    ]
    for key, value in attrs.items():
        attr: dict[str, object] = {"name": key, "explicitlySpecified": True}
        if isinstance(value, list):
            attr["type"] = "LABEL_LIST"
            attr["stringListValue"] = value
        elif isinstance(value, str):
            attr["type"] = "STRING" if key == "main_module" else "LABEL"
            attr["stringValue"] = value
        else:
            attr["type"] = "LABEL"
        attributes.append(attr)
    return json.dumps(
        {
            "type": "RULE",
            "rule": {"name": label, "ruleClass": cls, "attribute": attributes},
        }
    )


class FakeRun:
    """A Runner returning canned output and recording calls."""

    def __init__(self, *lines: str) -> None:
        """Hold the output lines to return."""
        self.out = "".join(line + "\n" for line in lines)
        self.calls: list[tuple[list[str], str]] = []

    def __call__(self, argv: Sequence[str], cwd: str) -> str:
        """Record the call and return the canned output."""
        self.calls.append((list(argv), cwd))
        return self.out


def _fs(*paths: str) -> files.GitResolver:
    return files.GitResolver("/repo", paths)


def test_explicit_main() -> None:
    """An explicit main label in srcs is the target's main file."""
    run = FakeRun(
        rule(
            "//scripts:strip",
            "py_binary",
            main="//scripts:strip.py",
            srcs=["//scripts:strip.py", "//scripts:lib.py"],
        )
    )
    found = bazel.find_mains(
        _fs("scripts/strip.py", "scripts/lib.py"), ".", run=run
    )
    assert found.targets == [
        bazel.Target("//scripts:strip", "script", "scripts/strip.py")
    ]
    argv, cwd = run.calls[0]
    assert argv == [
        "bazel",
        "query",
        bazel.QUERY,
        "--output=streamed_jsonproto",
    ]
    assert cwd == "/repo"


def test_default_name_main() -> None:
    """Without main, <name>.py is matched against srcs by trailing path."""
    run = FakeRun(
        rule("//pkg:foo_test", "py_test", srcs=["//pkg:sub/foo_test.py"])
    )
    found = bazel.find_mains(_fs("pkg/sub/foo_test.py"), ".", run=run)
    assert found.targets == [
        bazel.Target("//pkg:foo_test", "test", "pkg/sub/foo_test.py")
    ]


def test_root_package_label() -> None:
    """//:f.py names a file in the workspace root."""
    run = FakeRun(rule("//:tool", "py_binary", srcs=["//:tool.py"]))
    found = bazel.find_mains(_fs("tool.py"), ".", run=run)
    assert [t.path for t in found.targets] == ["tool.py"]


def test_ambiguous_is_error() -> None:
    """Two srcs ending in the main's name is an error naming the target."""
    run = FakeRun(
        rule(
            "//pkg:foo", "py_binary", srcs=["//pkg:a/foo.py", "//pkg:b/foo.py"]
        )
    )
    with pytest.raises(errors.SourceError, match="//pkg:foo"):
        bazel.find_mains(_fs("pkg/a/foo.py", "pkg/b/foo.py"), ".", run=run)


def test_no_match_is_error() -> None:
    """A main matching no src is an error naming the target."""
    run = FakeRun(rule("//pkg:foo", "py_binary", srcs=["//pkg:other.py"]))
    with pytest.raises(errors.SourceError, match="//pkg:foo"):
        bazel.find_mains(_fs("pkg/other.py"), ".", run=run)


def test_generated_main_counted() -> None:
    """A main that matches an untracked src is generated: counted, skipped."""
    run = FakeRun(
        rule(
            "//pkg:gen",
            "py_binary",
            main="//pkg:gen.py",
            srcs=["//pkg:gen.py"],
        )
    )
    found = bazel.find_mains(_fs("pkg/other.py"), ".", run=run)
    assert found.targets == []
    assert found.generated == 1


def test_main_module_skipped() -> None:
    """A target run by main_module has no main file: counted, skipped."""
    run = FakeRun(
        rule(
            "//pkg:app",
            "py_binary",
            main_module="pkg.app",
            srcs=["//pkg:app.py"],
        )
    )
    found = bazel.find_mains(_fs("pkg/app.py"), ".", run=run)
    assert found.targets == []
    assert found.module_main == 1


def test_select_union() -> None:
    """select() on srcs arrives as the union; each default main resolves."""
    run = FakeRun(
        rule(
            "//pkg:a",
            "py_binary",
            srcs=["//pkg:common.py", "//pkg:a.py"],
        )
    )
    found = bazel.find_mains(_fs("pkg/common.py", "pkg/a.py"), ".", run=run)
    assert [t.path for t in found.targets] == ["pkg/a.py"]


def test_select_main_skipped() -> None:
    """select() on main has no value in query output: counted, skipped."""
    run = FakeRun(
        rule("//pkg:a", "py_binary", main=None, srcs=["//pkg:lin/a.py"])
    )
    found = bazel.find_mains(_fs("pkg/lin/a.py"), ".", run=run)
    assert found.targets == []
    assert found.select_main == 1


def test_external_labels_ignored() -> None:
    """Sources from other repositories never become entries."""
    run = FakeRun(
        rule(
            "//pkg:a", "py_binary", main="@ext//x:a.py", srcs=["@ext//x:a.py"]
        )
    )
    found = bazel.find_mains(_fs("pkg/x.py"), ".", run=run)
    assert found.targets == []


def test_nested_bazel_dir() -> None:
    """Labels are relative to bazel_dir; the query runs there."""
    run = FakeRun(rule("//pkg:t", "py_binary", srcs=["//pkg:t.py"]))
    found = bazel.find_mains(_fs("sub/pkg/t.py"), "sub", bazel="bz", run=run)
    assert [t.path for t in found.targets] == ["sub/pkg/t.py"]
    argv, cwd = run.calls[0]
    assert argv[0] == "bz"
    assert cwd == "/repo/sub"


def test_runner_failure_propagates() -> None:
    """A failing query is a SourceError, never an empty result."""

    def run(argv: Sequence[str], cwd: str) -> str:
        raise errors.SourceError("bazel: boom")

    with pytest.raises(errors.SourceError, match="boom"):
        bazel.find_mains(_fs("a.py"), ".", run=run)


def test_run_subprocess_missing_binary() -> None:
    """A missing bazel binary is a SourceError."""
    with pytest.raises(errors.SourceError):
        bazel.run_subprocess(["/nonexistent/bazel", "query"], "/")


def test_has_workspace() -> None:
    """A tracked MODULE.bazel, WORKSPACE or WORKSPACE.bazel marks one."""
    assert bazel.has_workspace(files.FileSet(["MODULE.bazel"]), ".")
    assert bazel.has_workspace(files.FileSet(["sub/WORKSPACE"]), "sub")
    assert not bazel.has_workspace(files.FileSet(["a.py"]), ".")


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
