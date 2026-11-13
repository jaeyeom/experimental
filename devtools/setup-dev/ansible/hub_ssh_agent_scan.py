"""Choose the login rc for setup-hub-client's stable ssh-agent socket.

Prints one JSON object, ``{"rc": "<path>", "warn": "<path>"}``. ``warn``
is empty unless ``hub_ssh_agent_rc`` names a startup file that runs
before the agent starts. zsh reads ``~/.zprofile`` before ``~/.zshrc``,
so the managed block has to follow the agent-start line.
"""

from __future__ import annotations

import json
import os
import re
import sys

# Earliest startup file first. The last agent-start line wins.
_ORDER = (".profile", ".bash_profile", ".bashrc", ".zprofile", ".zshrc")
_DEFAULT = (".zprofile", ".bash_profile", ".profile")
_PATTERNS = ("keychain", "ssh-agent", 'eval "$(ssh-agent', "SSH_AUTH_SOCK=")
_BEGIN = "BEGIN setup-dev hub ssh agent"
_END = "END setup-dev hub ssh agent"
_SOURCE = re.compile(r"(?:^|&&|\|\||;|\||\()\s*(?:source|\.)\s+(\S+)")


def _expand(token: str, home: str, base: str) -> str:
    """Return token as a path, or empty when it is not one file."""
    token = token.strip().strip("\"'")
    if token.startswith("~/"):
        token = os.path.join(home, token[2:])
    elif token == "~":
        token = home
    token = token.replace("${HOME}", home).replace("$HOME", home)
    if token == "" or any(ch in token for ch in "*?["):
        return ""
    if not os.path.isabs(token):
        token = os.path.join(base, token)
    return os.path.normpath(token)


def _under_home(path: str, home: str) -> bool:
    home = os.path.normpath(home)
    path = os.path.normpath(path)
    return path == home or path.startswith(home + os.sep)


def _lines(path: str) -> list[str] | None:
    try:
        with open(path, encoding="utf-8", errors="replace") as handle:
            return handle.read().splitlines()
    except OSError:
        return None


def _agent_line(line: str) -> bool:
    body = line.strip()
    if body == "" or body.startswith("#"):
        return False
    return any(pattern in line for pattern in _PATTERNS)


def _walk(
    path: str,
    home: str,
    follow: bool,
    rank: int,
    hits: list[tuple[int, str]],
) -> None:
    """Record files whose agent-start line runs at rank."""
    text = _lines(path)
    if text is None:
        return
    skip = False
    for line in text:
        if _BEGIN in line:
            skip = True
            continue
        if skip:
            if _END in line:
                skip = False
            continue
        if _agent_line(line):
            # A sourced path outside home is not ours to edit. The
            # startup file that reached it still runs after that line.
            dest = path if _under_home(path, home) else ""
            if dest != "":
                hits.append((rank, os.path.normpath(dest)))
        if not follow:
            continue
        match = _SOURCE.search(line.strip())
        if match is None:
            continue
        child = _expand(match.group(1), home, os.path.dirname(path))
        if child == "" or os.path.normpath(child) == os.path.normpath(path):
            continue
        _walk(child, home, False, rank, hits)


def _default_rc(home: str) -> str:
    for name in _DEFAULT:
        path = os.path.join(home, name)
        if os.path.exists(path):
            return path
    return os.path.join(home, ".profile")


def choose(home: str, explicit: str) -> dict[str, str]:
    """Return the rc path and a later agent-start file, if any."""
    home = os.path.normpath(home)
    hits: list[tuple[int, str]] = []
    for rank, name in enumerate(_ORDER):
        _walk(os.path.join(home, name), home, True, rank, hits)
    if explicit == "":
        rc = hits[-1][1] if hits else _default_rc(home)
        return {"rc": rc, "warn": ""}
    rc = _expand(explicit, home, home)
    base = os.path.basename(rc)
    rc_rank = _ORDER.index(base) if base in _ORDER else None
    later: list[str] = []
    if rc_rank is not None:
        for hit_rank, hit_path in hits:
            if (
                hit_rank > rc_rank
                and hit_path != os.path.normpath(rc)
                and hit_path not in later
            ):
                later.append(hit_path)
    return {"rc": rc, "warn": ", ".join(later)}


def main(argv: list[str]) -> None:
    """Print the choice for HOME and an optional explicit rc."""
    home = argv[1] if len(argv) > 1 else ""
    explicit = argv[2] if len(argv) > 2 else ""
    if home == "":
        sys.exit("usage: hub_ssh_agent_scan.py HOME [RC]")
    json.dump(choose(home, explicit), sys.stdout)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main(sys.argv)
