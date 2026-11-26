"""Write decide Table and Gauge panels for running coding-agent RSS memory.

Works on Linux (reading ``/proc``) and macOS (running ``ps``), filtering
processes by a configurable command-name regex pattern and reporting total
resident memory against a configurable maximum.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import pathlib
import re
import subprocess
import sys
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from typing import Any

_DURATION_RE = re.compile(
    r"^\s*(?:(\d+)d)?(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?\s*$"
)
_SIZE_RE = re.compile(
    r"^\s*([0-9]+(?:\.[0-9]+)?)\s*(K|Ki|KiB|M|Mi|MiB|G|Gi|GiB|T|Ti|TiB)?\s*$",
    re.IGNORECASE,
)
_VM_RSS_RE = re.compile(r"^VmRSS:\s*(\d+)\s*kB\s*$", re.MULTILINE)

_DEFAULT_PATTERN = r"(claude|codex|gemini|opencode|aider|agy)"
_DEFAULT_MAX = "8Gi"
_DEFAULT_INTERVAL = "1m"
_DEFAULT_TABLE_ID = "agent-memory"
_DEFAULT_GAUGE_ID = "agent-memory-total"
_DEFAULT_TABLE_TITLE = "Agent Memory"
_DEFAULT_GAUGE_TITLE = "Agent Memory Total"
_DEFAULT_SOURCE = "agent-memory.sh"


@dataclass(frozen=True)
class ProcessInfo:
    """Resident memory snapshot for a single running process."""

    pid: int
    name: str
    command: str
    args: str
    rss_kib: int


def parse_memory_mib(raw: str) -> float:
    """Parse a positive memory limit (plain MiB or K/M/G/T suffix) to MiB."""
    match = _SIZE_RE.match(raw)
    if not match:
        raise ValueError(f"invalid memory size {raw!r}")
    num = float(match.group(1))
    if not math.isfinite(num) or num <= 0:
        raise ValueError(f"memory size {raw!r} must be positive")
    suffix = (match.group(2) or "mi").lower()
    factors = {
        "k": 1.0 / 1024.0,
        "ki": 1.0 / 1024.0,
        "kib": 1.0 / 1024.0,
        "m": 1.0,
        "mi": 1.0,
        "mib": 1.0,
        "g": 1024.0,
        "gi": 1024.0,
        "gib": 1024.0,
        "t": 1024.0 * 1024.0,
        "ti": 1024.0 * 1024.0,
        "tib": 1024.0 * 1024.0,
    }
    return num * factors[suffix]


def _double_duration(raw: str) -> str:
    match = _DURATION_RE.match(raw)
    if not match or not any(match.groups()):
        raise ValueError(f"invalid duration {raw!r}")
    days, hours, mins, secs = (int(g or 0) for g in match.groups())
    total = (days * 86400 + hours * 3600 + mins * 60 + secs) * 2
    if total <= 0:
        raise ValueError(f"duration {raw!r} must be positive")
    h, rem = divmod(total, 3600)
    m, s = divmod(rem, 60)
    parts: list[str] = []
    if h:
        parts.append(f"{h}h")
    if m:
        parts.append(f"{m}m")
    if s:
        parts.append(f"{s}s")
    return "".join(parts)


def format_rss(rss_kib: int) -> str:
    """Format KiB as human-readable KiB, MiB, or GiB."""
    if rss_kib < 1024:
        return f"{rss_kib} KiB"
    mib = rss_kib / 1024.0
    if mib >= 1024.0:
        return f"{mib / 1024.0:.2f} GiB"
    return f"{mib:.1f} MiB"


def read_proc_processes(
    proc_root: pathlib.Path,
    ignore_pids: set[int] | None = None,
) -> list[ProcessInfo]:
    """Read processes and RSS from a Linux ``/proc`` filesystem."""
    if not proc_root.is_dir():
        return []
    skip = ignore_pids or set()
    procs: list[ProcessInfo] = []
    for entry in proc_root.iterdir():
        if not entry.name.isdigit():
            continue
        pid = int(entry.name)
        if pid in skip:
            continue
        try:
            status = (entry / "status").read_text(
                encoding="utf-8", errors="replace"
            )
            comm = (
                (entry / "comm")
                .read_text(encoding="utf-8", errors="replace")
                .strip()
            )
            cmdline_raw = (entry / "cmdline").read_bytes()
        except OSError:
            continue
        rss_match = _VM_RSS_RE.search(status)
        if not rss_match:
            continue
        rss_kib = int(rss_match.group(1))
        argv = [
            part.decode("utf-8", errors="replace")
            for part in cmdline_raw.split(b"\x00")
            if part
        ]
        command = argv[0] if argv else comm
        name = pathlib.Path(command).name if command else comm
        args = " ".join(argv) if argv else comm
        procs.append(
            ProcessInfo(
                pid=pid,
                name=name or comm,
                command=command,
                args=args,
                rss_kib=rss_kib,
            )
        )
    return procs


def parse_ps_output(
    text: str,
    ignore_pids: set[int] | None = None,
) -> list[ProcessInfo]:
    """Parse ``ps -axo pid=,rss=,comm=`` or ``args=`` output."""
    skip = ignore_pids or set()
    procs: list[ProcessInfo] = []
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        parts = line.split(None, 2)
        if len(parts) < 3:
            continue
        pid_str, rss_str, cmd = parts
        if not pid_str.isdigit() or not rss_str.isdigit():
            continue
        pid = int(pid_str)
        if pid in skip:
            continue
        rss_kib = int(rss_str)
        first_tok = cmd.split(None, 1)[0]
        name = (
            pathlib.Path(cmd).name
            if "/" in cmd and " -" not in cmd
            else (pathlib.Path(first_tok).name)
        )
        procs.append(
            ProcessInfo(
                pid=pid,
                name=name,
                command=cmd,
                args=cmd,
                rss_kib=rss_kib,
            )
        )
    return procs


def read_ps_processes(
    ps_bin: str,
    match_args: bool,
    ignore_pids: set[int] | None = None,
) -> list[ProcessInfo]:
    """Read running processes on macOS (or BSD) using ``ps``."""
    col = "args=" if match_args else "comm="
    proc = subprocess.run(
        [ps_bin, "-axo", f"pid=,rss=,{col}"],
        check=True,
        capture_output=True,
        text=True,
        timeout=15,
    )
    return parse_ps_output(proc.stdout, ignore_pids=ignore_pids)


def filter_processes(
    procs: Sequence[ProcessInfo],
    pattern: str,
    *,
    match_args: bool = False,
) -> list[ProcessInfo]:
    """Filter processes by regex pattern and sort by RSS descending."""
    compiled = re.compile(pattern, re.IGNORECASE)
    matched: list[ProcessInfo] = []
    for proc in procs:
        if match_args:
            target = proc.args
        elif "/" in pattern:
            target = proc.command
        else:
            target = proc.name
        if compiled.search(target):
            matched.append(proc)
    matched.sort(key=lambda p: (-p.rss_kib, p.pid))
    return matched


def build_table_data(procs: Sequence[ProcessInfo]) -> dict[str, Any]:
    """Build a decide Table payload (Name, PID, RSS)."""
    return {
        "columns": ["Name", "PID", "RSS"],
        "rows": [[p.name, p.pid, format_rss(p.rss_kib)] for p in procs],
    }


def build_gauge_data(
    procs: Sequence[ProcessInfo],
    *,
    max_mib: float,
    warn_pct: float,
    fail_pct: float,
) -> dict[str, Any]:
    """Build a decide Gauge payload summing matched RSS against max_mib."""
    total_mib = round(sum(p.rss_kib for p in procs) / 1024.0, 1)
    data: dict[str, Any] = {
        "value": total_mib,
        "max": round(max_mib, 1),
        "unit": "MiB",
    }
    if warn_pct > 0:
        data["warn"] = round(max_mib * (warn_pct / 100.0), 1)
    if fail_pct > 0:
        data["fail"] = round(max_mib * (fail_pct / 100.0), 1)
    return data


def _write_panel(
    *,
    decide_bin: str,
    remote: str,
    panel_id: str,
    title: str,
    kind: str,
    data: Mapping[str, Any],
    stale_after: str,
    source: str,
) -> None:
    cmd = [decide_bin]
    if remote:
        cmd.extend(["--remote", remote])
    cmd.extend(
        [
            "panel",
            "set",
            panel_id,
            "--title",
            title,
            "--kind",
            kind,
            "--file",
            "-",
            "--stale-after",
            stale_after,
            "--source",
            source,
        ]
    )
    subprocess.run(
        cmd,
        input=json.dumps(data) + "\n",
        text=True,
        check=True,
    )


def _build_parser(environ: Mapping[str, str]) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Write decide Table and Gauge panels for agent RSS memory."
    )
    parser.add_argument(
        "--pattern",
        default=environ.get("DECIDE_AGENT_MEMORY_PATTERN", _DEFAULT_PATTERN),
        help="Regex matching agent process command names.",
    )
    parser.add_argument(
        "--match-args",
        action="store_true",
        help="Match --pattern against full command args instead of name.",
    )
    parser.add_argument(
        "--max",
        default=environ.get("DECIDE_AGENT_MEMORY_MAX", _DEFAULT_MAX),
        help="Gauge maximum memory (e.g. 8Gi, 4096Mi; default: 8Gi).",
    )
    parser.add_argument(
        "--warn-pct",
        type=float,
        default=float(environ.get("DECIDE_AGENT_MEMORY_WARN_PCT", "75")),
        help="Gauge warn threshold percent of max (default: 75).",
    )
    parser.add_argument(
        "--fail-pct",
        type=float,
        default=float(environ.get("DECIDE_AGENT_MEMORY_FAIL_PCT", "90")),
        help="Gauge fail threshold percent of max (default: 90).",
    )
    parser.add_argument(
        "--interval",
        default=environ.get("DECIDE_AGENT_MEMORY_INTERVAL", _DEFAULT_INTERVAL),
        help="Cron interval; sets staleAfter to 2x (default: 1m).",
    )
    parser.add_argument(
        "--table-id",
        default=environ.get("DECIDE_AGENT_MEMORY_TABLE_ID", _DEFAULT_TABLE_ID),
        help="Panel ID for the Table panel (default: agent-memory).",
    )
    parser.add_argument(
        "--gauge-id",
        default=environ.get("DECIDE_AGENT_MEMORY_GAUGE_ID", _DEFAULT_GAUGE_ID),
        help="Panel ID for the Gauge panel (default: agent-memory-total).",
    )
    parser.add_argument(
        "--table-title",
        default=environ.get(
            "DECIDE_AGENT_MEMORY_TABLE_TITLE", _DEFAULT_TABLE_TITLE
        ),
        help="Title for the Table panel.",
    )
    parser.add_argument(
        "--gauge-title",
        default=environ.get(
            "DECIDE_AGENT_MEMORY_GAUGE_TITLE", _DEFAULT_GAUGE_TITLE
        ),
        help="Title for the Gauge panel.",
    )
    parser.add_argument(
        "--source",
        default=environ.get("DECIDE_AGENT_MEMORY_SOURCE", _DEFAULT_SOURCE),
        help="Panel source label (default: agent-memory.sh).",
    )
    parser.add_argument(
        "--decide-bin",
        default=environ.get("DECIDE_BIN", "decide"),
        help="Path to decide binary.",
    )
    parser.add_argument(
        "--remote",
        default=environ.get("DECIDE_REMOTE", ""),
        help="Optional decide hub SSH target (--remote).",
    )
    parser.add_argument(
        "--proc-root",
        default="/proc",
        help="Linux procfs root (default: /proc).",
    )
    parser.add_argument(
        "--ps-bin",
        default=environ.get("PS_BIN", "ps"),
        help="macOS ps binary path (default: ps).",
    )
    parser.add_argument(
        "--platform",
        default=sys.platform,
        help="Override platform detection (linux or darwin).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print panel payloads JSON instead of calling decide.",
    )
    return parser


def main(
    argv: Sequence[str] | None = None,
    environ: Mapping[str, str] | None = None,
) -> int:
    """CLI entrypoint for the agent memory panel writer."""
    env = os.environ if environ is None else environ
    parser = _build_parser(env)
    args = parser.parse_args(argv)

    try:
        max_mib = parse_memory_mib(args.max)
        stale_after = _double_duration(args.interval)
        re.compile(args.pattern)
    except (ValueError, re.error) as err:
        print(f"agent-memory: {err}", file=sys.stderr)
        return 2

    ignore_pids = {os.getpid(), os.getppid()}
    plat = args.platform.lower()
    try:
        if plat.startswith("linux"):
            procs = read_proc_processes(
                pathlib.Path(args.proc_root), ignore_pids=ignore_pids
            )
        elif plat == "darwin":
            procs = read_ps_processes(
                args.ps_bin, args.match_args, ignore_pids=ignore_pids
            )
        else:
            print(
                f"agent-memory: unsupported platform {args.platform!r}",
                file=sys.stderr,
            )
            return 2
    except (OSError, subprocess.CalledProcessError) as err:
        print(
            f"agent-memory: failed to read processes: {err}", file=sys.stderr
        )
        return 1

    matched = filter_processes(procs, args.pattern, match_args=args.match_args)
    table_data = build_table_data(matched)
    gauge_data = build_gauge_data(
        matched,
        max_mib=max_mib,
        warn_pct=args.warn_pct,
        fail_pct=args.fail_pct,
    )

    if args.dry_run:
        payload = [
            {
                "id": args.table_id,
                "title": args.table_title,
                "kind": "Table",
                "data": table_data,
                "staleAfter": stale_after,
                "source": args.source,
            },
            {
                "id": args.gauge_id,
                "title": args.gauge_title,
                "kind": "Gauge",
                "data": gauge_data,
                "staleAfter": stale_after,
                "source": args.source,
            },
        ]
        print(json.dumps(payload, indent=2))
        return 0

    try:
        _write_panel(
            decide_bin=args.decide_bin,
            remote=args.remote,
            panel_id=args.table_id,
            title=args.table_title,
            kind="Table",
            data=table_data,
            stale_after=stale_after,
            source=args.source,
        )
        _write_panel(
            decide_bin=args.decide_bin,
            remote=args.remote,
            panel_id=args.gauge_id,
            title=args.gauge_title,
            kind="Gauge",
            data=gauge_data,
            stale_after=stale_after,
            source=args.source,
        )
    except (OSError, subprocess.CalledProcessError) as err:
        print(f"agent-memory: decide panel set failed: {err}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
