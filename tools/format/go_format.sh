#!/usr/bin/env bash
# Combined Go formatter running both goimports (FormatOnly) and gofumpt.
# Implements the -l (list unformatted) and -w (write in-place) interface
# expected by aspect_rules_lint's Go formatter wrapper.

# --- begin runfiles.bash initialization v3 ---
set -uo pipefail
set +e
f=bazel_tools/tools/bash/runfiles/runfiles.bash
# shellcheck disable=SC1090
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null ||
  source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null ||
  source "$0.runfiles/$f" 2>/dev/null ||
  source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null ||
  source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null ||
  {
    echo >&2 "ERROR: cannot find $f"
    exit 1
  }
f=
set -e
# --- end runfiles.bash initialization v3 ---

goimports_bin="$(rlocation "@@GOIMPORTS_RLOCATION@@")"
gofumpt_bin="$(rlocation "@@GOFUMPT_RLOCATION@@")"

if [[ ! -x "$goimports_bin" ]]; then
  echo >&2 "ERROR: goimports binary not found at $goimports_bin"
  exit 1
fi
if [[ ! -x "$gofumpt_bin" ]]; then
  echo >&2 "ERROR: gofumpt binary not found at $gofumpt_bin"
  exit 1
fi

if [[ $# -lt 1 ]]; then
  echo >&2 "Usage: $0 (-l|-w) [file ...]"
  exit 2
fi

mode="$1"
shift

case "$mode" in
  -l)
    {
      "$goimports_bin" -l "$@"
      "$gofumpt_bin" -l "$@"
    } | LC_ALL=C sort -u
    ;;
  -w)
    "$goimports_bin" -w "$@"
    "$gofumpt_bin" -w "$@"
    ;;
  *)
    echo >&2 "ERROR: unsupported flag: $mode (expected -l or -w)"
    exit 2
    ;;
esac
