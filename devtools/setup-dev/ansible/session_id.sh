#!/bin/sh
# Session id helper sourced by ensure.sh.
#
# Do not pipe an endless /dev/urandom stream into `head -c`. head exits
# after 16 bytes and closes the pipe while tr is still writing. When
# SIGPIPE is ignored (proot, and shells that inherit that disposition),
# GNU tr prints "tr: write error: Broken pipe" on stderr.
#
# dd copies one fixed block and exits, so tr reads EOF. That is the same
# on Linux (GNU), macOS (BSD), and Termux. Do not use GNU-only
# `dd status=none`; BSD dd rejects it. A device read may be shorter than
# the block size, and a block may contain fewer than 16 alphanumeric
# bytes, so accumulate until 16 characters are available.

# Print a 16-character [A-Za-z0-9] id drawn from PATH (default /dev/urandom).
# Falls back to "<epoch>-<pid>" when the source is unreadable or cannot
# supply 16 alphanumeric bytes within a few reads.
# Usage: session_id [PATH]
session_id() {
    _sid_src=${1:-/dev/urandom}
    if [ ! -r "$_sid_src" ]; then
        date +%s-"$$"
        return 0
    fi

    _sid=
    _sid_try=0
    while [ "${#_sid}" -lt 16 ]; do
        _sid_try=$((_sid_try + 1))
        if [ "$_sid_try" -gt 5 ]; then
            date +%s-"$$"
            return 0
        fi
        # || true: a missing dd/tr must not abort callers that use set -e.
        _sid_chunk=$(dd if="$_sid_src" bs=128 count=1 2>/dev/null | LC_ALL=C tr -dc 'A-Za-z0-9') || true
        _sid=${_sid}${_sid_chunk}
    done
    printf '%.16s\n' "$_sid"
}
