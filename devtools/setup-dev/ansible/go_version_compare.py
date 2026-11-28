#!/usr/bin/env python3
"""Compare installed and target Go module versions without downgrading."""

from __future__ import annotations

import argparse
import json
import re
import sys
from typing import NamedTuple

_IDENT_RE = re.compile(r"^[0-9A-Za-z-]+$")


class SemVer(NamedTuple):
    """Parsed Go module semantic version (without build metadata)."""

    major: int
    minor: int
    patch: int
    pre: tuple[str, ...]


def _valid_core_part(part: str) -> bool:
    return (
        bool(part)
        and part.isdigit()
        and (len(part) == 1 or not part.startswith("0"))
    )


def parse_go_semver(raw: str) -> SemVer | None:
    """Parse a Go module semver string (e.g. v0.1.1-0.20261125-sha) or return None."""
    v = raw.strip()
    if not v.startswith("v"):
        return None
    main_part, has_plus, build_meta = v[1:].partition("+")
    if has_plus:
        if not build_meta:
            return None
        for ident in build_meta.split("."):
            if not _IDENT_RE.match(ident):
                return None

    core_str, has_pre, pre_str = main_part.partition("-")
    core_parts = core_str.split(".")
    if not 1 <= len(core_parts) <= 3:
        return None
    if not all(_valid_core_part(p) for p in core_parts):
        return None
    nums = [int(p) for p in core_parts]
    while len(nums) < 3:
        nums.append(0)

    pre: tuple[str, ...] = ()
    if has_pre:
        if not pre_str:
            return None
        pre_parts = pre_str.split(".")
        for ident in pre_parts:
            if not _IDENT_RE.match(ident):
                return None
            if ident.isdigit() and len(ident) > 1 and ident.startswith("0"):
                return None
        pre = tuple(pre_parts)

    return SemVer(nums[0], nums[1], nums[2], pre)


def _compare_pre(a: tuple[str, ...], b: tuple[str, ...]) -> int:
    if not a and not b:
        return 0
    if not a:
        return 1
    if not b:
        return -1
    for i in range(min(len(a), len(b))):
        pa, pb = a[i], b[i]
        if pa == pb:
            continue
        a_num = pa.isdigit()
        b_num = pb.isdigit()
        if a_num and b_num:
            return -1 if int(pa) < int(pb) else 1
        if a_num:
            return -1
        if b_num:
            return 1
        return -1 if pa < pb else 1
    if len(a) == len(b):
        return 0
    return -1 if len(a) < len(b) else 1


def compare_semver(a: SemVer, b: SemVer) -> int:
    """Return -1 if a < b, 0 if equal precedence, 1 if a > b."""
    core_a = (a.major, a.minor, a.patch)
    core_b = (b.major, b.minor, b.patch)
    if core_a != core_b:
        return -1 if core_a < core_b else 1
    return _compare_pre(a.pre, b.pre)


def _has_dirty_build(raw: str) -> bool:
    _, has_plus, build_meta = raw.strip().partition("+")
    return has_plus and "dirty" in build_meta.split(".")


def _modules_match(installed_module: str, target_module: str) -> bool:
    """Return True when target_module belongs to installed_module."""
    return target_module == installed_module or target_module.startswith(
        installed_module + "/"
    )


def decide_upgrade(
    installed: str,
    target: str,
    installed_go: str = "",
    toolchain_go: str = "",
    installed_module: str = "",
    target_module: str = "",
) -> dict[str, str]:
    """Decide whether to install/upgrade a Go tool or skip."""
    inst = installed.strip()
    targ = target.strip()
    inst_go = installed_go.strip()
    tool_go = toolchain_go.strip()
    inst_mod = installed_module.strip()
    targ_mod = target_module.strip()

    if not inst:
        return {"action": "install", "reason": "not installed"}
    if inst_mod and targ_mod and not _modules_match(inst_mod, targ_mod):
        return {
            "action": "install",
            "reason": f"module path changed ({inst_mod} -> {targ_mod})",
        }
    if not targ:
        return {
            "action": "skip_unresolved",
            "reason": f"target version could not be resolved; keeping installed {inst}",
        }

    t_ver = parse_go_semver(targ)
    if t_ver is None:
        return {
            "action": "skip_unresolved",
            "reason": f"target version {targ!r} is not a valid Go version; keeping installed {inst}",
        }

    i_ver = parse_go_semver(inst)
    if i_ver is None:
        return {
            "action": "install",
            "reason": f"installed version {inst!r} has no semver; installing {targ}",
        }

    cmp = compare_semver(i_ver, t_ver)
    if cmp > 0:
        return {
            "action": "skip_older",
            "reason": f"installed version {inst} is newer than target {targ}",
        }
    if cmp < 0:
        return {
            "action": "install",
            "reason": f"target version {targ} is newer than installed {inst}",
        }

    if _has_dirty_build(inst) and not _has_dirty_build(targ):
        return {
            "action": "skip_older",
            "reason": f"installed local build {inst} is at or ahead of target {targ}",
        }
    if tool_go and inst_go != tool_go:
        return {
            "action": "install",
            "reason": f"Go toolchain changed ({inst_go or 'unknown'} -> {tool_go})",
        }
    return {
        "action": "skip_current",
        "reason": f"installed version {inst} matches target {targ}",
    }


def main(argv: list[str] | None = None) -> int:
    """CLI entry point outputting JSON upgrade decision."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("installed", help="Installed module version")
    parser.add_argument("target", help="Target module version")
    parser.add_argument(
        "installed_go",
        nargs="?",
        default="",
        help="Go toolchain version used to build installed binary",
    )
    parser.add_argument(
        "toolchain_go",
        nargs="?",
        default="",
        help="Current Go toolchain version",
    )
    parser.add_argument(
        "--installed-module",
        default="",
        help="Module path of the currently installed binary",
    )
    parser.add_argument(
        "--target-module",
        default="",
        help="Configured target module or package path",
    )
    ns = parser.parse_args(argv)
    json.dump(
        decide_upgrade(
            ns.installed,
            ns.target,
            ns.installed_go,
            ns.toolchain_go,
            ns.installed_module,
            ns.target_module,
        ),
        sys.stdout,
    )
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
