#!/bin/sh
# Tests for cache_expired: a portable 24-hour (86400s) freshness check.
#
# GNU find -mtime +1 is NOT 24 hours: it ignores the fractional day, so a
# file must be at least two days old to match. These tests lock in second-
# based comparison so Linux, macOS, and Termux share a real 24-hour TTL.
set -eu

fail() {
    echo "FAIL: $1" >&2
    exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
sh -n "$SCRIPT_DIR/cache_expired.sh"
sh -n "$SCRIPT_DIR/cache_expired_test.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/cache_expired.sh"

_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp"' EXIT

_f="$_tmp/cache"
touch "$_f"
_mtime=$(stat -c %Y "$_f" 2>/dev/null || stat -f %m "$_f")

# Missing file is always expired (refresh).
if ! cache_expired "$_tmp/missing"; then
    fail "missing cache file must be expired"
fi

# Fresh file is valid for a 24-hour TTL.
if cache_expired "$_f" 86400 "$_mtime"; then
    fail "just-created cache must not be expired at mtime"
fi
if cache_expired "$_f" 86400 "$((_mtime + 23 * 3600))"; then
    fail "23-hour-old cache must still be valid for a 24-hour TTL"
fi

# Exactly 24 hours and beyond is expired.
if ! cache_expired "$_f" 86400 "$((_mtime + 86400))"; then
    fail "exactly 24-hour-old cache must be expired"
fi
if ! cache_expired "$_f" 86400 "$((_mtime + 25 * 3600))"; then
    fail "25-hour-old cache must be expired"
fi

# Regression: GNU find -mtime +1 treats 30 hours as still "today+1" (not
# old enough). A real 24-hour TTL must refresh.
if ! cache_expired "$_f" 86400 "$((_mtime + 30 * 3600))"; then
    fail "30-hour-old cache must be expired (find -mtime +1 would skip this)"
fi

# Default TTL is 24 hours when max-age is omitted.
if cache_expired "$_f" "" "$((_mtime + 23 * 3600))"; then
    fail "default TTL must keep a 23-hour-old cache valid"
fi
if ! cache_expired "$_f" "" "$((_mtime + 86400))"; then
    fail "default TTL must expire a 24-hour-old cache"
fi

# Custom TTL: 30-hour-old cache is still valid if the limit is 48 hours.
if cache_expired "$_f" $((48 * 3600)) "$((_mtime + 30 * 3600))"; then
    fail "30-hour-old cache must still be valid for a 48-hour TTL"
fi

# Live clock: a file touched just now is not expired for a 24-hour TTL.
if cache_expired "$_f"; then
    fail "just-created cache must not be expired against the live clock"
fi

echo "All cache_expired tests passed."
