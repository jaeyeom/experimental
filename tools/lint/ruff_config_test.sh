#!/usr/bin/env bash
# Verifies that the hermetic Ruff binary respects //:pyproject.toml settings
# (UP, D, line-length 79, extend-exclude) and does not auto-fix with --no-fix.
set -euo pipefail

ruff_bin="$(realpath "$1")"
pyproject="$(realpath "$2")"

workdir="$(mktemp -d "${TEST_TMPDIR:-/tmp}/ruff_config_test.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT

cp "$pyproject" "$workdir/pyproject.toml"

# 1. Catch UP and D (Google pydocstyle) violations without modifying the file.
cat >"$workdir/bad.py" <<'EOF'
from typing import List


def bad(items: List[str]) -> str:
    return ",".join(items)
EOF
before="$(cat "$workdir/bad.py")"

output=""
if output=$("$ruff_bin" check --force-exclude --no-fix --output-format=concise "$workdir/bad.py" 2>&1); then
  echo "FAIL: expected ruff to report violations on bad.py" >&2
  exit 1
fi

for code in UP035 UP006 D100 D103; do
  if ! printf '%s\n' "$output" | grep -q "$code"; then
    echo "FAIL: expected rule $code in ruff output:" >&2
    printf '%s\n' "$output" >&2
    exit 1
  fi
done

if [ "$before" != "$(cat "$workdir/bad.py")" ]; then
  echo "FAIL: ruff --no-fix modified bad.py" >&2
  exit 1
fi

# 2. Respect extend-exclude (*_pb2.py, *_pb2_grpc.py).
cat >"$workdir/example_pb2.py" <<'EOF'
import os
x = 1
EOF

"$ruff_bin" check --force-exclude --no-fix "$workdir/example_pb2.py"
