#!/bin/sh
# Tests for session_id: a 16-character id from a bounded urandom read.
#
# `tr | head -c` reports "tr: write error: Broken pipe" when SIGPIPE is
# ignored. These tests lock in a finite dd read so Linux, macOS, and
# Termux stay quiet, including shells that inherit an ignored SIGPIPE.
set -eu

fail() {
    echo "FAIL: $1" >&2
    exit 1
}

assert_eq() {
    _label="$1"
    _got="$2"
    _want="$3"
    if [ "$_got" != "$_want" ]; then
        fail "$_label: got '$_got', want '$_want'"
    fi
}

assert_match() {
    _label="$1"
    _got="$2"
    _re="$3"
    if ! printf '%s\n' "$_got" | grep -Eq "$_re"; then
        fail "$_label: got '$_got', want match /$_re/"
    fi
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
sh -n "$SCRIPT_DIR/session_id.sh"
sh -n "$SCRIPT_DIR/session_id_test.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/session_id.sh"

_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp"' EXIT

# Fixed alphanumeric input is truncated to 16 characters. No broken pipe.
printf 'AAAABBBBCCCCDDDDEEEE' >"$_tmp/fixed"
assert_eq "truncate fixed input" "$(session_id "$_tmp/fixed")" "AAAABBBBCCCCDDDD"

# A short file is re-read until 16 characters accumulate.
printf 'ABab' >"$_tmp/short"
assert_eq "reread short input" "$(session_id "$_tmp/short")" "ABabABabABabABab"

# C locale: bytes between 'Z' and 'a' are not alphanumeric.
# A UTF-8 collation range on BSD tr would keep [\^_`.
printf 'AB[\\]^_`ab' >"$_tmp/range"
assert_eq "C locale range" "$(session_id "$_tmp/range")" "ABabABabABabABab"

# NUL bytes stay inside the dd|tr pipe. Capturing dd output in the shell
# would truncate the chunk at the first NUL.
printf 'AAAA\000BBBBCCCCDDDD' >"$_tmp/nul"
assert_eq "NUL inside block" "$(session_id "$_tmp/nul")" "AAAABBBBCCCCDDDD"

# No alphanumeric bytes: fall back to epoch-pid.
printf '\000\001!!!' >"$_tmp/none"
assert_match "no alnum falls back" "$(session_id "$_tmp/none")" '^[0-9]+-[0-9]+$'

# Missing and unreadable sources use the same fallback.
assert_match "missing source" "$(session_id "$_tmp/missing")" '^[0-9]+-[0-9]+$'
printf 'AAAAAAAA' >"$_tmp/unreadable"
if chmod 000 "$_tmp/unreadable" 2>/dev/null && [ ! -r "$_tmp/unreadable" ]; then
    assert_match "unreadable source" "$(session_id "$_tmp/unreadable")" '^[0-9]+-[0-9]+$'
fi

# Real urandom, with SIGPIPE ignored the way proot leaves it.
# The old `tr | head -c` pipeline prints "Broken pipe" here.
_i=0
while [ "$_i" -lt 20 ]; do
    _err=$(
        trap '' PIPE
        session_id 2>&1 >/dev/null
    ) || fail "session_id failed under ignored SIGPIPE"
    if [ -n "$_err" ]; then
        fail "stderr under ignored SIGPIPE: $_err"
    fi
    _id=$(
        trap '' PIPE
        session_id
    ) || fail "session_id failed under ignored SIGPIPE"
    assert_match "urandom id" "$_id" '^[A-Za-z0-9]{16}$'
    _i=$((_i + 1))
done

echo "All session_id tests passed."
