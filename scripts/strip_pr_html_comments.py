"""Strip HTML comments from a GitHub pull request body.

GitHub squash-merge with "PR title and description" copies the raw PR body
into the commit message. Template ``<!-- ... -->`` hints are hidden in the
Markdown preview but would otherwise appear in ``git log``.
"""

import io
import json
import pathlib
import re
import sys

_COMMENT = re.compile(r"<!--.*?-->", re.DOTALL)
_BLANK_RUN = re.compile(r"\n{3,}")


def canonicalize(body: str) -> str:
    """Return body with HTML comments removed and extra blank lines collapsed.

    Bodies that contain no HTML comments are returned unchanged so the
    workflow never rewrites a comment-free description. After comments are
    removed, a non-empty result ends with a single newline so the output is
    stable across GitHub API round-trips.
    """
    if _COMMENT.search(body) is None:
        return body
    text = _COMMENT.sub("", body)
    text = _BLANK_RUN.sub("\n\n", text).strip()
    if not text:
        return ""
    return text + "\n"


def write_canonical_pr_body(
    pr_json: pathlib.Path,
    dest: pathlib.Path,
) -> bool:
    """Write a comment-stripped body from `gh pr view --json body` output.

    Returns True if the canonical body differs from the stored body.
    """
    body = json.loads(pr_json.read_text(encoding="utf-8")).get("body") or ""
    canonical = canonicalize(body)
    dest.write_text(canonical, encoding="utf-8")
    return canonical != body


def main(
    stdin: io.TextIOBase | None = None,
    stdout: io.TextIOBase | None = None,
    argv: list[str] | None = None,
) -> None:
    """Read a PR body from stdin, or canonicalize `gh` JSON given two paths."""
    dest = sys.stdout if stdout is None else stdout
    args = sys.argv[1:] if argv is None else argv
    if len(args) == 2:
        changed = write_canonical_pr_body(
            pathlib.Path(args[0]),
            pathlib.Path(args[1]),
        )
        dest.write("changed\n" if changed else "unchanged\n")
        return
    source = sys.stdin if stdin is None else stdin
    dest.write(canonicalize(source.read()))


if __name__ == "__main__":
    main()
