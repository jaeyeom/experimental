"""Write the decide 'tokens' Chart panel from claude-usage, quotapace, and logs.

Intended for cron: builds daily cumulative usage for the current month with a
projection to the configured monthly limit and crossing date, then updates the
panel via ``decide panel set``.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import math
import os
import pathlib
import re
import subprocess
import sys
from collections.abc import Mapping, Sequence
from typing import Any

# Support system /usr/bin/python3 (<3.11) when invoked from cron.
_UTC = dt.timezone.utc  # noqa: UP017

_DURATION_RE = re.compile(
    r"^\s*(?:(\d+)d)?(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?\s*$"
)
_DEFAULT_INTERVAL = "30m"
_DEFAULT_UNIT = "tokens"
_DEFAULT_PANEL_ID = "tokens"
_DEFAULT_TITLE = "Tokens"
_DEFAULT_SOURCE = "tokens.sh"


def resolve_limit(raw: str | None, environ: Mapping[str, str]) -> float:
    """Resolve and validate the required monthly token limit."""
    value = raw if raw is not None else environ.get("DECIDE_TOKENS_LIMIT", "")
    text = value.strip()
    if not text:
        raise ValueError(
            "--limit or DECIDE_TOKENS_LIMIT must be set (no plan default)"
        )
    try:
        num = float(text)
    except ValueError as err:
        raise ValueError(f"limit {text!r} must be a number") from err
    if not math.isfinite(num):
        raise ValueError(f"limit {text!r} must be finite")
    if num <= 0:
        raise ValueError(f"limit {text!r} must be positive")
    return num


def parse_duration_seconds(raw: str) -> int:
    """Parse a positive duration like '30m', '1h30m', or '1d' into seconds."""
    match = _DURATION_RE.match(raw)
    if not match or not any(match.groups()):
        raise ValueError(f"invalid duration {raw!r}")
    days, hours, mins, secs = (int(g or 0) for g in match.groups())
    total = days * 86400 + hours * 3600 + mins * 60 + secs
    if total <= 0:
        raise ValueError(f"duration {raw!r} must be positive")
    return total


def format_duration(seconds: int) -> str:
    """Format positive seconds as a concise Go duration string."""
    if seconds <= 0:
        raise ValueError(f"duration seconds {seconds} must be positive")
    hours, rem = divmod(seconds, 3600)
    mins, secs = divmod(rem, 60)
    parts: list[str] = []
    if hours:
        parts.append(f"{hours}h")
    if mins:
        parts.append(f"{mins}m")
    if secs:
        parts.append(f"{secs}s")
    return "".join(parts)


def double_duration(raw: str) -> str:
    """Return twice the given duration formatted for decide --stale-after."""
    return format_duration(parse_duration_seconds(raw) * 2)


def parse_rfc3339(text: str) -> dt.datetime | None:
    """Parse an RFC 3339 timestamp string into a UTC-aware datetime."""
    cleaned = text.strip()
    if not cleaned:
        return None
    if cleaned.endswith("Z"):
        cleaned = cleaned[:-1] + "+00:00"
    try:
        parsed = dt.datetime.fromisoformat(cleaned)
    except ValueError:
        return None
    if parsed.tzinfo is None:
        return parsed.replace(tzinfo=_UTC)
    return parsed.astimezone(_UTC)


def format_rfc3339(when: dt.datetime) -> str:
    """Format a datetime as UTC RFC 3339 with Z suffix."""
    return when.astimezone(_UTC).strftime("%Y-%m-%dT%H:%M:%SZ")


def fetch_claude_usage(binary: str) -> dict[str, Any] | None:
    """Run ``claude-usage --json`` and return parsed JSON if available."""
    try:
        proc = subprocess.run(
            [binary, "--json"],
            check=False,
            capture_output=True,
            text=True,
            timeout=15,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if proc.returncode != 0 or not proc.stdout.strip():
        return None
    try:
        data = json.loads(proc.stdout)
    except json.JSONDecodeError:
        return None
    return data if isinstance(data, dict) else None


def fetch_quotapace_status(
    binary: str,
    profile: str = "",
) -> dict[str, Any] | None:
    """Run ``quotapace status --json`` and return parsed snapshot JSON."""
    cmd = [binary, "status", "--json"]
    if profile:
        cmd.extend(["--profile", profile])
    try:
        proc = subprocess.run(
            cmd,
            check=False,
            capture_output=True,
            text=True,
            timeout=15,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    # quotapace status exits 0=green, 1=amber, 2=red, 3=missing/stale.
    if proc.returncode not in (0, 1, 2, 3) or not proc.stdout.strip():
        return None
    try:
        data = json.loads(proc.stdout)
    except json.JSONDecodeError:
        return None
    return data if isinstance(data, dict) else None


def _select_quotapace_window(
    windows: Sequence[dict[str, Any]],
    preferred: str,
) -> dict[str, Any] | None:
    if preferred:
        for win in windows:
            if win.get("window") == preferred:
                return win
        return None
    for candidate in ("30d", "monthly", "7d"):
        for win in windows:
            proj = win.get("projection")
            if (
                win.get("window") == candidate
                and isinstance(proj, dict)
                and proj.get("hoursTo100") is not None
            ):
                return win
    for win in windows:
        proj = win.get("projection")
        if isinstance(proj, dict) and proj.get("hoursTo100") is not None:
            return win
    return None


def quotapace_crossing_at(
    status: Mapping[str, Any] | None,
    window: str = "",
    now: dt.datetime | None = None,
) -> dt.datetime | None:
    """Compute the projected crossing timestamp from quotapace status JSON."""
    if not status:
        return None
    profiles = status.get("profiles")
    if not isinstance(profiles, list):
        return None
    snap_now = (
        parse_rfc3339(str(status.get("now", "")))
        or now
        or dt.datetime.now(_UTC)
    )
    for prof in profiles:
        if not isinstance(prof, dict):
            continue
        windows = prof.get("windows")
        if not isinstance(windows, list):
            continue
        selected = _select_quotapace_window(windows, window)
        if not selected:
            continue
        proj = selected.get("projection")
        if not isinstance(proj, dict):
            continue
        hours = proj.get("hoursTo100")
        if not isinstance(hours, (int, float)) or not math.isfinite(hours):
            continue
        anchor = parse_rfc3339(str(proj.get("at", ""))) or snap_now
        return anchor + dt.timedelta(hours=float(hours))
    return None


def _extract_usage_tokens(usage: Any) -> int:
    if not isinstance(usage, dict):
        return 0
    fields = (
        "input_tokens",
        "output_tokens",
        "cache_creation_input_tokens",
        "cache_read_input_tokens",
    )
    total = 0
    for key in fields:
        val = usage.get(key, 0)
        if isinstance(val, (int, float)) and val > 0:
            total += int(val)
    return total


def _event_date(event: Mapping[str, Any], fallback: dt.date) -> dt.date:
    raw_ts = event.get("timestamp")
    if isinstance(raw_ts, str):
        parsed = parse_rfc3339(raw_ts)
        if parsed is not None:
            return parsed.date()
    return fallback


def _read_session_file(
    path: pathlib.Path,
) -> list[tuple[dt.date, int]]:
    try:
        mtime = dt.datetime.fromtimestamp(path.stat().st_mtime, tz=_UTC)
    except OSError:
        return []
    fallback_date = mtime.date()
    seen_msgs: dict[str, tuple[dt.date, int]] = {}
    unnamed: list[tuple[dt.date, int]] = []
    result_fallback: tuple[dt.date, int] | None = None

    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return []

    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        if not isinstance(event, dict):
            continue
        ev_type = event.get("type")
        ev_date = _event_date(event, fallback_date)
        if ev_type == "assistant":
            msg = event.get("message")
            if not isinstance(msg, dict):
                continue
            tokens = _extract_usage_tokens(msg.get("usage"))
            msg_id = msg.get("id")
            if isinstance(msg_id, str) and msg_id:
                prev = seen_msgs.get(msg_id)
                if prev is None or tokens >= prev[1]:
                    seen_msgs[msg_id] = (ev_date, tokens)
            elif tokens > 0:
                unnamed.append((ev_date, tokens))
        elif ev_type == "result":
            tokens = _extract_usage_tokens(event.get("usage"))
            if tokens > 0:
                result_fallback = (ev_date, tokens)

    records = [*seen_msgs.values(), *unnamed]
    if records:
        return records
    return [result_fallback] if result_fallback is not None else []


def default_logs_dir(environ: Mapping[str, str]) -> pathlib.Path:
    """Return the Claude Code projects directory from env or HOME."""
    cfg_dir = environ.get("CLAUDE_CONFIG_DIR", "").strip()
    if cfg_dir:
        return pathlib.Path(cfg_dir) / "projects"
    home = environ.get("HOME", "").strip()
    if home:
        return pathlib.Path(home) / ".claude" / "projects"
    return pathlib.Path(".claude") / "projects"


def read_daily_tokens(
    projects_dir: pathlib.Path,
    year: int,
    month: int,
    today: dt.date,
) -> dict[dt.date, int]:
    """Aggregate local Claude Code JSONL token usage by day for the month."""
    if not projects_dir.is_dir():
        return {}
    daily: dict[dt.date, int] = {}
    for path in sorted(projects_dir.rglob("*.jsonl")):
        if not path.is_file():
            continue
        for ev_date, tokens in _read_session_file(path):
            if (
                ev_date.year == year
                and ev_date.month == month
                and ev_date <= today
                and tokens > 0
            ):
                daily[ev_date] = daily.get(ev_date, 0) + tokens
    return daily


def _linear_crossing_at(
    series: Sequence[dict[str, Any]],
    limit: float,
    now: dt.datetime,
) -> dt.datetime | None:
    if not series:
        return None
    total = float(series[-1]["v"])
    if total <= 0:
        return None
    if total >= limit:
        for point in series:
            if float(point["v"]) >= limit:
                crossed_day = dt.date.fromisoformat(str(point["t"]))
                if crossed_day == now.date():
                    return now
                return dt.datetime.combine(
                    crossed_day, dt.time(23, 59, 59), tzinfo=_UTC
                )
        return now
    first_day = dt.date.fromisoformat(str(series[0]["t"]))
    start = dt.datetime.combine(first_day, dt.time.min, tzinfo=_UTC)
    elapsed_hours = max((now - start).total_seconds() / 3600.0, 1.0)
    rate_per_hour = total / elapsed_hours
    if rate_per_hour <= 0:
        return None
    hours_left = (limit - total) / rate_per_hour
    return now + dt.timedelta(hours=hours_left)


def build_chart_data(
    daily_tokens: Mapping[dt.date, int],
    *,
    limit: float,
    unit: str,
    today: dt.date,
    now: dt.datetime,
    qp_crossing: dt.datetime | None = None,
) -> dict[str, Any]:
    """Build a decide Chart payload with cumulative daily series and forecast."""
    active_days = sorted(d for d, v in daily_tokens.items() if v > 0)
    series: list[dict[str, Any]] = []
    if active_days:
        cur = active_days[0]
        running = 0
        while cur <= today:
            running += daily_tokens.get(cur, 0)
            series.append({"t": cur.isoformat(), "v": running})
            cur += dt.timedelta(days=1)
    else:
        series.append({"t": today.isoformat(), "v": 0})

    crossing = qp_crossing or _linear_crossing_at(series, limit, now)
    data: dict[str, Any] = {
        "series": series,
        "unit": unit,
        "limit": limit,
    }
    if crossing is not None:
        data["projection"] = {
            "to": limit,
            "at": format_rfc3339(crossing),
        }
    return data


def build_title(
    explicit: str | None,
    claude_usage: Mapping[str, Any] | None,
) -> str:
    """Format the panel title, surfacing current window usage if available."""
    if explicit:
        return explicit
    if not claude_usage:
        return _DEFAULT_TITLE
    parts: list[str] = []
    if claude_usage.get("fiveHour") is not None:
        parts.append(f"5h {claude_usage['fiveHour']}%")
    if claude_usage.get("sevenDay") is not None:
        parts.append(f"7d {claude_usage['sevenDay']}%")
    if not parts:
        return _DEFAULT_TITLE
    return f"{_DEFAULT_TITLE} ({', '.join(parts)})"


def write_panel(
    *,
    decide_bin: str,
    remote: str,
    panel_id: str,
    title: str,
    data: Mapping[str, Any],
    stale_after: str,
    source: str,
) -> None:
    """Invoke ``decide panel set`` with panel data JSON on stdin."""
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
            "Chart",
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
        description="Write a decide Chart panel for monthly token usage."
    )
    parser.add_argument(
        "--limit",
        default=None,
        help="Monthly limit (or set DECIDE_TOKENS_LIMIT). No plan default.",
    )
    parser.add_argument(
        "--unit",
        default=environ.get("DECIDE_TOKENS_UNIT", _DEFAULT_UNIT),
        help="Chart unit label (default: tokens).",
    )
    parser.add_argument(
        "--interval",
        default=environ.get("DECIDE_TOKENS_INTERVAL", _DEFAULT_INTERVAL),
        help="Cron interval; sets staleAfter to 2x (default: 30m).",
    )
    parser.add_argument(
        "--panel-id",
        default=environ.get("DECIDE_TOKENS_PANEL_ID", _DEFAULT_PANEL_ID),
        help="Decide panel ID (default: tokens).",
    )
    parser.add_argument(
        "--title",
        default=environ.get("DECIDE_TOKENS_TITLE") or None,
        help="Override panel title.",
    )
    parser.add_argument(
        "--source",
        default=environ.get("DECIDE_TOKENS_SOURCE", _DEFAULT_SOURCE),
        help="Panel source label (default: tokens.sh).",
    )
    parser.add_argument(
        "--profile",
        default=environ.get("DECIDE_TOKENS_PROFILE", ""),
        help="Optional quotapace profile name.",
    )
    parser.add_argument(
        "--quotapace-window",
        default=environ.get("DECIDE_TOKENS_QUOTAPACE_WINDOW", ""),
        help="Optional quotapace window name (e.g. 30d, 7d).",
    )
    parser.add_argument(
        "--logs-dir",
        default=None,
        help="Claude Code projects directory (default: ~/.claude/projects).",
    )
    parser.add_argument(
        "--claude-usage-bin",
        default=environ.get("CLAUDE_USAGE_BIN", "claude-usage"),
        help="Path to claude-usage binary.",
    )
    parser.add_argument(
        "--quotapace-bin",
        default=environ.get("QUOTAPACE_BIN", "quotapace"),
        help="Path to quotapace binary.",
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
        "--now",
        default="",
        help="Override current UTC RFC 3339 time (for tests).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print panel payload JSON instead of calling decide.",
    )
    return parser


def main(
    argv: Sequence[str] | None = None,
    environ: Mapping[str, str] | None = None,
) -> int:
    """CLI entrypoint for the tokens panel writer."""
    env = os.environ if environ is None else environ
    parser = _build_parser(env)
    args = parser.parse_args(argv)

    try:
        limit = resolve_limit(args.limit, env)
        stale_after = double_duration(args.interval)
    except ValueError as err:
        print(f"tokens: {err}", file=sys.stderr)
        return 2

    now = (parse_rfc3339(args.now) if args.now else None) or dt.datetime.now(
        _UTC
    )
    if args.now and parse_rfc3339(args.now) is None:
        print(f"tokens: invalid --now timestamp {args.now!r}", file=sys.stderr)
        return 2

    usage = fetch_claude_usage(args.claude_usage_bin)
    qp_status = fetch_quotapace_status(args.quotapace_bin, args.profile)
    qp_cross = quotapace_crossing_at(
        qp_status, window=args.quotapace_window, now=now
    )

    logs_dir = (
        pathlib.Path(args.logs_dir) if args.logs_dir else default_logs_dir(env)
    )
    today = now.date()
    daily = read_daily_tokens(logs_dir, today.year, today.month, today)
    data = build_chart_data(
        daily,
        limit=limit,
        unit=args.unit,
        today=today,
        now=now,
        qp_crossing=qp_cross,
    )
    title = build_title(args.title, usage)

    if args.dry_run:
        payload = {
            "id": args.panel_id,
            "title": title,
            "kind": "Chart",
            "data": data,
            "staleAfter": stale_after,
            "source": args.source,
        }
        print(json.dumps(payload, indent=2))
        return 0

    try:
        write_panel(
            decide_bin=args.decide_bin,
            remote=args.remote,
            panel_id=args.panel_id,
            title=title,
            data=data,
            stale_after=stale_after,
            source=args.source,
        )
    except (OSError, subprocess.CalledProcessError) as err:
        print(f"tokens: decide panel set failed: {err}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
