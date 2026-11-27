"""Unit tests for the decide tokens panel writer."""

import datetime as dt
import json
import pathlib
import sys

import pytest
import tokens


def test_parse_limit_requires_explicit_value() -> None:
    """No default limit exists; missing or invalid limits fail."""
    with pytest.raises(ValueError, match="limit"):
        tokens.resolve_limit(None, {})
    with pytest.raises(ValueError, match="positive"):
        tokens.resolve_limit("0", {})
    with pytest.raises(ValueError, match="positive"):
        tokens.resolve_limit("-5", {})
    with pytest.raises(ValueError, match="finite"):
        tokens.resolve_limit("nan", {})
    assert tokens.resolve_limit(None, {"DECIDE_TOKENS_LIMIT": "2500000"}) == (
        2500000.0
    )
    assert tokens.resolve_limit("1000000", {"DECIDE_TOKENS_LIMIT": "50"}) == (
        1000000.0
    )


def test_double_interval_duration() -> None:
    """StaleAfter is twice the cron interval in Go duration syntax."""
    assert tokens.double_duration("30m") == "1h"
    assert tokens.double_duration("15m") == "30m"
    assert tokens.double_duration("1h30m") == "3h"
    assert tokens.double_duration("45s") == "1m30s"
    assert tokens.double_duration("1d") == "48h"
    with pytest.raises(ValueError, match="duration"):
        tokens.double_duration("0s")


def test_quotapace_crossing_at_prefers_window_and_anchor() -> None:
    """Projection timestamp uses quotapace's anchor plus hoursTo100."""
    snap = {
        "version": 1,
        "now": "2026-11-26T12:00:00Z",
        "profiles": [
            {
                "profile": "work",
                "windows": [
                    {
                        "window": "5h",
                        "projection": {
                            "at": "2026-11-26T11:00:00Z",
                            "hoursTo100": 4.0,
                        },
                    },
                    {
                        "window": "7d",
                        "projection": {
                            "at": "2026-11-26T10:00:00Z",
                            "hoursTo100": 48.5,
                        },
                    },
                ],
            }
        ],
    }
    got = tokens.quotapace_crossing_at(snap)
    assert got == dt.datetime(2026, 11, 28, 10, 30, 0, tzinfo=dt.UTC)
    assert tokens.quotapace_crossing_at(snap, window="5h") == dt.datetime(
        2026, 11, 26, 15, 0, 0, tzinfo=dt.UTC
    )


def test_read_daily_tokens_dedupes_streaming_and_filters_month(
    tmp_path: pathlib.Path,
) -> None:
    """Streaming assistant chunks share message.id and are counted once."""
    proj = tmp_path / "proj-a"
    proj.mkdir()
    lines = [
        json.dumps(
            {
                "type": "assistant",
                "timestamp": "2026-10-31T23:59:00Z",
                "message": {
                    "id": "msg_prev",
                    "usage": {"input_tokens": 999, "output_tokens": 1},
                },
            }
        ),
        json.dumps(
            {
                "type": "assistant",
                "timestamp": "2026-11-24T10:00:00Z",
                "message": {
                    "id": "msg_1",
                    "usage": {"input_tokens": 100, "output_tokens": 10},
                },
            }
        ),
        json.dumps(
            {
                "type": "assistant",
                "timestamp": "2026-11-24T10:00:05Z",
                "message": {
                    "id": "msg_1",
                    "usage": {
                        "input_tokens": 100,
                        "output_tokens": 50,
                        "cache_read_input_tokens": 200,
                        "cache_creation_input_tokens": 50,
                    },
                },
            }
        ),
        "{broken-json-line",
        json.dumps(
            {
                "type": "assistant",
                "timestamp": "2026-11-26T08:00:00Z",
                "message": {
                    "id": "msg_2",
                    "usage": {"input_tokens": 300, "output_tokens": 100},
                },
            }
        ),
    ]
    (proj / "sess1.jsonl").write_text("\n".join(lines) + "\n")

    daily = tokens.read_daily_tokens(tmp_path, 2026, 11, dt.date(2026, 11, 26))
    assert daily == {
        dt.date(2026, 11, 24): 400,
        dt.date(2026, 11, 26): 400,
    }


def test_build_chart_data_uses_quotapace_and_fills_days() -> None:
    """Chart series accumulates daily tokens and includes projection."""
    now = dt.datetime(2026, 11, 26, 12, 0, 0, tzinfo=dt.UTC)
    qp_cross = dt.datetime(2026, 11, 29, 6, 0, 0, tzinfo=dt.UTC)
    daily = {
        dt.date(2026, 11, 24): 1000,
        dt.date(2026, 11, 26): 500,
    }
    chart = tokens.build_chart_data(
        daily,
        limit=5000.0,
        unit="tokens",
        today=now.date(),
        now=now,
        qp_crossing=qp_cross,
    )
    assert chart == {
        "series": [
            {"t": "2026-11-24", "v": 1000},
            {"t": "2026-11-25", "v": 1000},
            {"t": "2026-11-26", "v": 1500},
        ],
        "unit": "tokens",
        "limit": 5000.0,
        "projection": {"to": 5000.0, "at": "2026-11-29T06:00:00Z"},
    }


def test_build_chart_data_linear_fallback_when_quotapace_absent() -> None:
    """When quotapace has no projection, daily pace estimates crossing."""
    now = dt.datetime(2026, 11, 25, 12, 0, 0, tzinfo=dt.UTC)
    daily = {dt.date(2026, 11, 25): 500}
    chart = tokens.build_chart_data(
        daily,
        limit=1000.0,
        unit="tokens",
        today=now.date(),
        now=now,
        qp_crossing=None,
    )
    assert chart["projection"]["to"] == 1000.0
    assert chart["projection"]["at"] == "2026-11-26T00:00:00Z"


def test_build_title_with_claude_usage() -> None:
    """Default title incorporates claude-usage percentages when present."""
    usage = {"planName": "Team", "fiveHour": 2, "sevenDay": 12}
    assert tokens.build_title(None, usage) == "Tokens (5h 2%, 7d 12%)"
    frac_usage = {
        "planName": "Team",
        "fiveHour": 2.4,
        "sevenDay": 33.3,
        "fetchedAt": "2026-11-26T12:00:00Z",
        "source": "keychain:/Users/alice/.claude",
    }
    assert tokens.build_title(None, frac_usage) == "Tokens (5h 2.4%, 7d 33.3%)"
    assert tokens.build_title("Custom Title", usage) == "Custom Title"
    assert tokens.build_title(None, None) == "Tokens"


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
