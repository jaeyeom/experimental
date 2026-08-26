#!/bin/sh
# Portable cache freshness helper sourced by ensure.sh.
#
# Do not use `find FILE -mtime +1` for a 24-hour TTL. GNU find ignores the
# fractional 24-hour period, so -mtime +1 means "at least two days old".
# Compare epoch seconds instead so Linux (GNU), macOS (BSD), and Termux
# share a real 24-hour window.

# Print FILE's mtime as epoch seconds. GNU stat first, BSD stat fallback.
_cache_file_mtime() {
    _cfm_out=$(stat -c %Y "$1" 2>/dev/null) \
        || _cfm_out=$(stat -f %m "$1" 2>/dev/null) \
        || return 1
    case "$_cfm_out" in
        '' | *[!0-9]*) return 1 ;;
    esac
    printf '%s\n' "$_cfm_out"
}

# True if FILE is missing or at least MAX_AGE seconds old.
# Usage: cache_expired FILE [MAX_AGE_SEC] [NOW_EPOCH]
# MAX_AGE_SEC defaults to 86400 (24 hours). NOW_EPOCH is for tests.
cache_expired() {
    _ce_file="$1"
    _ce_max_age="${2:-86400}"
    _ce_now="${3:-}"
    if [ ! -f "$_ce_file" ]; then
        return 0
    fi
    if [ -z "$_ce_now" ]; then
        _ce_now=$(date +%s) || return 0
    fi
    _ce_mtime=$(_cache_file_mtime "$_ce_file") || return 0
    [ "$((_ce_now - _ce_mtime))" -ge "$_ce_max_age" ]
}
