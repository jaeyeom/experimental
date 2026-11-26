#!/bin/sh
# Cron panel writer for coding-agent resident memory (Table + Gauge).
set -eu

dir=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
python_bin=${PYTHON_BIN:-python3}

if ! command -v "$python_bin" >/dev/null 2>&1; then
  printf '%s\n' "agent-memory.sh: $python_bin is required" >&2
  exit 1
fi

exec "$python_bin" "$dir/agent_memory.py" "$@"
