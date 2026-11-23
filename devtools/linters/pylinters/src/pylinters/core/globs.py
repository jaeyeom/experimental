"""Path globs with the checker-baseline syntax.

``*`` matches zero or more non-``/`` characters, ``?`` one non-``/``
character, ``[...]`` one non-``/`` character from a class, and ``**`` zero
or more whole path segments.
"""

from __future__ import annotations

import functools
import re
from collections.abc import Iterable


def match(pattern: str, path: str) -> bool:
    """Report whether repo-relative ``path`` matches ``pattern``."""
    return _compile(pattern).fullmatch(path) is not None


def match_any(patterns: Iterable[str], path: str) -> bool:
    """Report whether ``path`` matches any of ``patterns``."""
    return any(match(p, path) for p in patterns)


@functools.lru_cache(maxsize=256)
def _compile(pattern: str) -> re.Pattern[str]:
    out = []
    i, n = 0, len(pattern)
    while i < n:
        c = pattern[i]
        if pattern.startswith("**/", i):
            out.append("(?:[^/]+/)*")
            i += 3
        elif pattern.startswith("/**", i) and i + 3 == n:
            out.append("(?:/[^/]+)*")
            i += 3
        elif pattern.startswith("**", i):
            out.append(".*")
            i += 2
        elif c == "*":
            out.append("[^/]*")
            i += 1
        elif c == "?":
            out.append("[^/]")
            i += 1
        elif c == "[" and "]" in pattern[i + 1 :]:
            end = pattern.index("]", i + 1)
            body = pattern[i + 1 : end]
            if body[:1] in ("!", "^"):
                out.append("[^/" + _class_body(body[1:]) + "]")
            else:
                out.append("(?!/)[" + _class_body(body) + "]")
            i = end + 1
        else:
            out.append(re.escape(c))
            i += 1
    return re.compile("".join(out))


def _class_body(body: str) -> str:
    """Escape regex specials inside a character class, keeping ranges."""
    return body.replace("\\", "\\\\").replace("[", "\\[").replace("^", "\\^")
