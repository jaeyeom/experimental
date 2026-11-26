#!/bin/sh
# End-to-end fixture tests for the decide example panel writer scripts.
# Runs the real decide binary with fake claude-usage, quotapace, /proc, and ps
# fixtures, verifying decide panel set accepts the emitted JSON.
set -eu

DECIDE_BIN=$1
TOKENS_SH=$2
AGENT_MEM_SH=$3

if ! command -v python3 >/dev/null 2>&1; then
  printf '%s\n' "FAIL: python3 is required" >&2
  exit 1
fi

TMP_ROOT=$(mktemp -d "${TEST_TMPDIR:-/tmp}/decide_panels_test.XXXXXX")
trap 'rm -rf "$TMP_ROOT"' EXIT INT TERM

STATE_HOME="$TMP_ROOT/state"
CONFIG_HOME="$TMP_ROOT/config"
FAKE_BIN="$TMP_ROOT/bin"
CLAUDE_DIR="$TMP_ROOT/claude"
PROC_DIR="$TMP_ROOT/proc"
mkdir -p "$STATE_HOME" "$CONFIG_HOME" "$FAKE_BIN" "$CLAUDE_DIR/projects/demo" "$PROC_DIR/101" "$PROC_DIR/202"

export XDG_STATE_HOME="$STATE_HOME"
export XDG_CONFIG_HOME="$CONFIG_HOME"
export CLAUDE_CONFIG_DIR="$CLAUDE_DIR"
export DECIDE_BIN="$DECIDE_BIN"

# --- Fake claude-usage ---
cat >"$FAKE_BIN/claude-usage" <<'EOF'
#!/bin/sh
set -eu
printf '%s\n' '{"planName":"Team","fiveHour":14,"sevenDay":42,"fiveHourResetAt":"2026-11-26T17:00:00Z","sevenDayResetAt":"2026-11-30T23:00:00Z"}'
EOF
chmod +x "$FAKE_BIN/claude-usage"

# --- Fake quotapace (exits 1 for amber to verify non-zero status handling) ---
cat >"$FAKE_BIN/quotapace" <<'EOF'
#!/bin/sh
set -eu
cat <<'JSON'
{
  "version": 1,
  "now": "2026-11-26T12:00:00Z",
  "profiles": [
    {
      "profile": "work",
      "offset": 0,
      "windows": [
        {
          "window": "7d",
          "state": "amber",
          "projection": {
            "at": "2026-11-26T12:00:00Z",
            "percent": 42,
            "hoursTo100": 72.0,
            "state": "amber"
          }
        }
      ]
    }
  ]
}
JSON
exit 1
EOF
chmod +x "$FAKE_BIN/quotapace"

# --- Fixture Claude Code local JSONL logs ---
cat >"$CLAUDE_DIR/projects/demo/session.jsonl" <<'EOF'
{"type":"assistant","timestamp":"2026-11-24T09:00:00Z","message":{"id":"m1","usage":{"input_tokens":100000,"output_tokens":20000}}}
{"type":"assistant","timestamp":"2026-11-26T11:30:00Z","message":{"id":"m2","usage":{"input_tokens":150000,"output_tokens":50000,"cache_read_input_tokens":80000}}}
EOF

# --- Test 1: tokens.sh requires explicit limit ---
if DECIDE_TOKENS_LIMIT="" "$TOKENS_SH" --now "2026-11-26T12:00:00Z" >/dev/null 2>&1; then
  printf '%s\n' "FAIL: expected tokens.sh to fail without --limit or DECIDE_TOKENS_LIMIT" >&2
  exit 1
fi

# --- Test 2: tokens.sh writes a valid Chart panel via decide panel set ---
PATH="$FAKE_BIN:$PATH" \
  "$TOKENS_SH" \
  --limit 1000000 \
  --interval 30m \
  --now "2026-11-26T12:00:00Z"

# --- Test 3: agent-memory.sh (Linux /proc fixture) writes Table + Gauge ---
printf '%s\n' "claude" >"$PROC_DIR/101/comm"
printf '%s\0%s\0' "/usr/local/bin/claude" "-p" >"$PROC_DIR/101/cmdline"
printf 'Name:\tclaude\nVmRSS:\t  524288 kB\n' >"$PROC_DIR/101/status"

printf '%s\n' "unrelated" >"$PROC_DIR/202/comm"
printf '%s\0' "/usr/bin/unrelated" >"$PROC_DIR/202/cmdline"
printf 'Name:\tunrelated\nVmRSS:\t  102400 kB\n' >"$PROC_DIR/202/status"

"$AGENT_MEM_SH" \
  --platform linux \
  --proc-root "$PROC_DIR" \
  --pattern "claude|codex" \
  --max 4Gi \
  --interval 2m

# --- Test 4: agent-memory.sh (macOS ps fixture) writes Table + Gauge ---
cat >"$FAKE_BIN/ps" <<'EOF'
#!/bin/sh
set -eu
printf '  PID   RSS COMM\n'
printf ' 5001 262144 /usr/local/bin/codex\n'
printf ' 5002  10240 /bin/sh\n'
EOF
chmod +x "$FAKE_BIN/ps"

PATH="$FAKE_BIN:$PATH" \
  "$AGENT_MEM_SH" \
  --platform darwin \
  --table-id agent-memory-mac \
  --gauge-id agent-memory-mac-total \
  --pattern "codex" \
  --max 2048

# --- Verify stored panels via decide panel ls --json ---
PANELS_JSON="$TMP_ROOT/panels.json"
"$DECIDE_BIN" panel ls --json >"$PANELS_JSON"

python3 - "$PANELS_JSON" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as fh:
    items = {item["id"]: item for item in json.load(fh)}

assert set(items) == {
    "tokens",
    "agent-memory",
    "agent-memory-total",
    "agent-memory-mac",
    "agent-memory-mac-total",
}, sorted(items)

for pid, item in items.items():
    assert not item.get("unreadable"), f"{pid} unreadable: {item}"
    assert not item.get("stale"), f"{pid} unexpected stale: {item}"

tok = items["tokens"]
assert tok["kind"] == "Chart"
assert tok["staleAfter"] == "1h"
assert "5h 14%" in tok["title"] and "7d 42%" in tok["title"]
assert tok["data"]["limit"] == 1000000
assert tok["data"]["series"] == [
    {"t": "2026-11-24", "v": 120000},
    {"t": "2026-11-25", "v": 120000},
    {"t": "2026-11-26", "v": 400000},
]
assert tok["data"]["projection"] == {
    "to": 1000000,
    "at": "2026-11-29T12:00:00Z",
}

tbl = items["agent-memory"]
assert tbl["kind"] == "Table"
assert tbl["staleAfter"] == "4m"
assert tbl["data"] == {
    "columns": ["Name", "PID", "RSS"],
    "rows": [["claude", 101, "512.0 MiB"]],
}

gauge = items["agent-memory-total"]
assert gauge["kind"] == "Gauge"
assert gauge["data"] == {
    "value": 512.0,
    "max": 4096.0,
    "warn": 3072.0,
    "fail": 3686.4,
    "unit": "MiB",
}

mac_tbl = items["agent-memory-mac"]
assert mac_tbl["data"]["rows"] == [["codex", 5001, "256.0 MiB"]]
mac_gauge = items["agent-memory-mac-total"]
assert mac_gauge["data"]["value"] == 256.0
assert mac_gauge["data"]["max"] == 2048.0
PY
