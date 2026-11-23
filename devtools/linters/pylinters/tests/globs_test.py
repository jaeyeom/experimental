"""Tests for pylinters.core.globs."""

import sys

import pytest
from pylinters.core import globs


@pytest.mark.parametrize(
    ("pat", "path", "want"),
    [
        ("**/*_pb2.py", "a_pb2.py", True),
        ("**/*_pb2.py", "x/y/a_pb2.py", True),
        ("**/*_pb2.py", "x/a_pb2_grpc.py", False),
        ("**/testdata/**", "a/testdata/b/c.py", True),
        ("**/testdata/**", "testdata/c.py", True),
        ("a/*.py", "a/b/c.py", False),
        ("a/*.py", "a/c.py", True),
        ("a/?.py", "a/b.py", True),
        ("a/?.py", "a/bc.py", False),
        ("a/[bc].py", "a/c.py", True),
        ("a/[bc].py", "a/d.py", False),
        ("a/[a-c].py", "a/b.py", True),
        ("a/[!a-c].py", "a/b.py", False),
        ("a/[!a-c].py", "a/d.py", True),
        ("a/**", "a/b/c.py", True),
        ("a/**", "ab/c.py", False),
        ("a.py", "xa.py", False),
    ],
)
def test_match(pat: str, path: str, want: bool) -> None:
    """Patterns follow the checker-baseline glob syntax."""
    assert globs.match(pat, path) is want


def test_match_any() -> None:
    """match_any is true when any pattern matches."""
    assert globs.match_any(["x/**", "**/*_pb2.py"], "a/b_pb2.py")
    assert not globs.match_any([], "a.py")


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
