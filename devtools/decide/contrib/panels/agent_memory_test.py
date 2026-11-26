"""Unit tests for the decide agent-memory panel writer."""

import pathlib
import sys

import agent_memory
import pytest


def test_parse_memory_mib() -> None:
    """Memory limits accept plain MiB and binary/decimal unit suffixes."""
    assert agent_memory.parse_memory_mib("4096") == 4096.0
    assert agent_memory.parse_memory_mib("8Gi") == 8192.0
    assert agent_memory.parse_memory_mib("2GiB") == 2048.0
    assert agent_memory.parse_memory_mib("512Mi") == 512.0
    assert agent_memory.parse_memory_mib("1024Ki") == 1.0
    with pytest.raises(ValueError, match="positive"):
        agent_memory.parse_memory_mib("0")


def test_read_proc_processes(tmp_path: pathlib.Path) -> None:
    """Linux /proc parser reads comm, cmdline, and VmRSS kB."""
    p1 = tmp_path / "101"
    p1.mkdir()
    (p1 / "comm").write_text("claude\n")
    (p1 / "cmdline").write_bytes(b"/usr/local/bin/claude\x00--resume\x00")
    (p1 / "status").write_text("Name:\tclaude\nVmRSS:\t  524288 kB\n")

    p2 = tmp_path / "202"
    p2.mkdir()
    (p2 / "comm").write_text("kthreadd\n")
    (p2 / "cmdline").write_bytes(b"")
    (p2 / "status").write_text("Name:\tkthreadd\n")

    p3 = tmp_path / "303"
    p3.mkdir()
    (p3 / "comm").write_text("codex\n")
    (p3 / "cmdline").write_bytes(b"codex\x00")
    (p3 / "status").write_text("Name:\tcodex\nVmRSS:\t  102400 kB\n")

    (tmp_path / "self").mkdir()

    procs = agent_memory.read_proc_processes(tmp_path, ignore_pids={303})
    assert procs == [
        agent_memory.ProcessInfo(
            pid=101,
            name="claude",
            command="/usr/local/bin/claude",
            args="/usr/local/bin/claude --resume",
            rss_kib=524288,
        )
    ]


def test_parse_ps_output() -> None:
    """MacOS ps output parser handles full paths and ignores self PIDs."""
    text = (
        "  PID   RSS COMM\n"
        " 1001 409600 /Users/me/.local/bin/claude\n"
        " 1002  10240 /bin/zsh\n"
        " 1003 204800 /opt/homebrew/bin/codex\n"
        " 1004 102400 /Applications/Claude Helper.app/Contents/MacOS/claude\n"
    )
    procs = agent_memory.parse_ps_output(text, ignore_pids={1003})
    assert [p.pid for p in procs] == [1001, 1002, 1004]
    assert procs[0].name == "claude"
    assert procs[0].rss_kib == 409600
    assert procs[2].name == "claude"


def test_filter_sort_and_build_panels() -> None:
    """Matched processes produce a sorted Table and a thresholded Gauge."""
    procs = [
        agent_memory.ProcessInfo(
            pid=22,
            name="codex",
            command="/usr/bin/codex",
            args="/usr/bin/codex",
            rss_kib=204800,
        ),
        agent_memory.ProcessInfo(
            pid=11,
            name="bash",
            command="/bin/bash",
            args="/bin/bash /tmp/claude-notes.txt",
            rss_kib=10240,
        ),
        agent_memory.ProcessInfo(
            pid=33,
            name="claude",
            command="/usr/bin/claude",
            args="/usr/bin/claude --model opus",
            rss_kib=1048576,
        ),
    ]
    matched = agent_memory.filter_processes(
        procs, r"(claude|codex)", match_args=False
    )
    assert [p.pid for p in matched] == [33, 22]

    table = agent_memory.build_table_data(matched)
    assert table == {
        "columns": ["Name", "PID", "RSS"],
        "rows": [
            ["claude", 33, "1.00 GiB"],
            ["codex", 22, "200.0 MiB"],
        ],
    }

    gauge = agent_memory.build_gauge_data(
        matched, max_mib=4096.0, warn_pct=75.0, fail_pct=90.0
    )
    assert gauge == {
        "value": 1224.0,
        "max": 4096.0,
        "warn": 3072.0,
        "fail": 3686.4,
        "unit": "MiB",
    }


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
