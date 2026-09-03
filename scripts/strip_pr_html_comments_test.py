"""Tests for stripping HTML comments from pull request bodies."""

import io
import json
import pathlib
import sys

import pytest
import strip_pr_html_comments as strippr


@pytest.mark.parametrize(
    ("body", "want"),
    [
        ("", ""),
        ("   \n\n", "   \n\n"),
        (
            "Keep\n\n\n\nKeep\n",
            "Keep\n\n\n\nKeep\n",
        ),
        (
            "## Summary\n\nShipped the fix.\n",
            "## Summary\n\nShipped the fix.\n",
        ),
        (
            "Hello <!-- hidden --> world\n",
            "Hello  world\n",
        ),
        (
            "## Summary\n\n<!-- line 1\n     line 2 -->\n\nShipped the fix.\n",
            "## Summary\n\nShipped the fix.\n",
        ),
        (
            "<!-- a -->\n\n\n\n<!-- b -->\nKeep\n",
            "Keep\n",
        ),
        (
            "  <!-- leading -->\nKeep\n<!-- trailing -->  \n",
            "Keep\n",
        ),
        (
            "<!-- unclosed comment",
            "<!-- unclosed comment",
        ),
        (
            "Already clean.\n",
            "Already clean.\n",
        ),
    ],
    ids=[
        "empty",
        "whitespace_only",
        "blank_runs_without_comments_kept",
        "no_comments",
        "inline_comment",
        "multiline_comment",
        "comments_leave_blank_runs",
        "trim_comment_only_edges",
        "unclosed_comment_kept",
        "idempotent_clean_body",
    ],
)
def test_canonicalize_strips_html_comments(body: str, want: str) -> None:
    """Canonicalize removes HTML comments and extra blank lines."""
    assert strippr.canonicalize(body) == want
    assert strippr.canonicalize(want) == want


def _pull_request_template() -> pathlib.Path:
    """Return the repo PR template, resolving Bazel runfiles or a source tree."""
    here = pathlib.Path(__file__).resolve().parent
    for root in (here.parent, *here.parents, pathlib.Path.cwd()):
        candidate = root / ".github" / "pull_request_template.md"
        if candidate.is_file():
            return candidate
    raise FileNotFoundError("pull_request_template.md")


def test_canonicalize_strips_pr_template_comments() -> None:
    """Filled PR template comments must not survive into the squash message."""
    template = _pull_request_template().read_text()
    assert "<!--" in template
    body = template.replace("Resolves #", "Resolves #288").replace(
        "- [ ] `make check` is green",
        "- [x] `make check` is green",
    )
    body = body.replace(
        "## Summary\n\n",
        "## Summary\n\n- Wait for a post-send working transition.\n\n",
        1,
    )
    got = strippr.canonicalize(body)
    assert "<!--" not in got
    assert "-->" not in got
    assert "## Summary" in got
    assert "Resolves #288" in got
    assert "- Wait for a post-send working transition." in got
    assert "- [x] `make check` is green" in got
    assert "\n\n\n" not in got
    assert got == strippr.canonicalize(got)


def test_main_reads_stdin_and_writes_canonical_body() -> None:
    """The CLI writes the canonical body to stdout."""
    stdin = io.StringIO("Hello <!-- hidden --> world\n")
    stdout = io.StringIO()
    strippr.main(stdin=stdin, stdout=stdout, argv=[])
    assert stdout.getvalue() == "Hello  world\n"


def test_write_canonical_pr_body_from_gh_json(tmp_path: pathlib.Path) -> None:
    """JSON from `gh pr view --json body` is rewritten without comments."""
    src = tmp_path / "pr.json"
    dest = tmp_path / "body.md"
    src.write_text(json.dumps({"body": "Hi <!-- x -->\n"}))
    assert strippr.write_canonical_pr_body(src, dest) is True
    assert dest.read_text() == "Hi\n"


def test_write_canonical_pr_body_reports_unchanged(
    tmp_path: pathlib.Path,
) -> None:
    """Already-canonical bodies are not treated as a change."""
    src = tmp_path / "pr.json"
    dest = tmp_path / "body.md"
    src.write_text(json.dumps({"body": "Hi\n"}))
    assert strippr.write_canonical_pr_body(src, dest) is False
    assert dest.read_text() == "Hi\n"


def test_write_canonical_pr_body_treats_null_as_empty(
    tmp_path: pathlib.Path,
) -> None:
    """A missing PR body is canonicalized to an empty file."""
    src = tmp_path / "pr.json"
    dest = tmp_path / "body.md"
    src.write_text(json.dumps({"body": None}))
    assert strippr.write_canonical_pr_body(src, dest) is False
    assert dest.read_text() == ""


def test_main_json_paths_print_changed(tmp_path: pathlib.Path) -> None:
    """The CLI JSON mode prints whether the body changed."""
    src = tmp_path / "pr.json"
    dest = tmp_path / "body.md"
    src.write_text(json.dumps({"body": "Hi <!-- x -->\n"}))
    stdout = io.StringIO()
    strippr.main(argv=[str(src), str(dest)], stdout=stdout)
    assert stdout.getvalue() == "changed\n"
    assert dest.read_text() == "Hi\n"


def test_main_json_paths_print_unchanged(tmp_path: pathlib.Path) -> None:
    """The CLI JSON mode leaves comment-free bodies alone."""
    src = tmp_path / "pr.json"
    dest = tmp_path / "body.md"
    src.write_text(json.dumps({"body": "Hi\n\n\nThere\n"}))
    stdout = io.StringIO()
    strippr.main(argv=[str(src), str(dest)], stdout=stdout)
    assert stdout.getvalue() == "unchanged\n"
    assert dest.read_text() == "Hi\n\n\nThere\n"


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
