#!/bin/sh
# Tests for traced_by_proot and the Termux shims that use it.
#
# proot-distro treats a process as nested when TracerPid is non-zero and
# that tracer's Name contains "proot". The Termux grok and codex shims
# must make the same decision before they exec proot.
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

# 0 when traced_by_proot returns success, 1 otherwise.
probe() {
    if traced_by_proot "$@"; then
        printf '%s\n' 0
    else
        printf '%s\n' 1
    fi
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
sh -n "$SCRIPT_DIR/proot_tracer.sh"
sh -n "$SCRIPT_DIR/proot_tracer_test.sh"
sh -n "$SCRIPT_DIR/grok_termux_wrapper.sh"
sh -n "$SCRIPT_DIR/codex_termux_wrapper.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/proot_tracer.sh"

_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp"' EXIT

printf 'Name:\tgrok\nTracerPid:\t42\n' >"$_tmp/self"
printf 'Name:\tproot\nTracerPid:\t0\n' >"$_tmp/proot"
printf 'Name:\tgrok\nTracerPid:\t0\n' >"$_tmp/grok"
printf 'Name:\tproof\n' >"$_tmp/proof"
printf 'Name:\tproot \n' >"$_tmp/padded"
printf 'TracerPid:\t0\n' >"$_tmp/zero"
printf 'TracerPid: \t 42\n' >"$_tmp/spaced"
printf 'TracerPid:\t42x\n' >"$_tmp/badpid"
: >"$_tmp/empty"

printf 'Name:\tproot-distro\n' >"$_tmp/contains"

assert_eq "proot tracer" "$(probe "$_tmp/self" "$_tmp/proot")" 0
assert_eq "padded proot name" "$(probe "$_tmp/self" "$_tmp/padded")" 0
assert_eq "name containing proot" "$(probe "$_tmp/self" "$_tmp/contains")" 0
assert_eq "spaced tracer pid" "$(probe "$_tmp/spaced" "$_tmp/proot")" 0
assert_eq "other tracer" "$(probe "$_tmp/self" "$_tmp/grok")" 1
# "proof" shares a prefix with "proot" and must not match.
assert_eq "proof is not proot" "$(probe "$_tmp/self" "$_tmp/proof")" 1
assert_eq "tracer pid 0" "$(probe "$_tmp/zero" "$_tmp/proot")" 1
assert_eq "non-numeric tracer pid" "$(probe "$_tmp/badpid" "$_tmp/proot")" 1
assert_eq "empty status" "$(probe "$_tmp/empty" "$_tmp/proot")" 1
assert_eq "missing status" "$(probe "$_tmp/missing" "$_tmp/proot")" 1
assert_eq "no args" "$(probe)" 1

# Default tracer path is /proc/<pid>/status. This pid is not running.
printf 'TracerPid:\t999999999\n' >"$_tmp/absent"
assert_eq "missing default tracer status" "$(probe "$_tmp/absent")" 1

printf 'ok\n' >"$_tmp/unreadable"
if chmod 000 "$_tmp/unreadable" 2>/dev/null && [ ! -r "$_tmp/unreadable" ]; then
    assert_eq "unreadable tracer status" "$(probe "$_tmp/self" "$_tmp/unreadable")" 1
    assert_eq "unreadable pid status" "$(probe "$_tmp/unreadable")" 1
fi

# Real /proc status, checked against an independent parser so the fixture
# format cannot drift away from what Linux writes.
if [ -r /proc/$$/status ] && command -v awk >/dev/null 2>&1; then
    _tracer=$(awk '/^TracerPid:/{print $2; exit}' /proc/$$/status)
    _name=
    if [ -n "$_tracer" ] && [ "$_tracer" != 0 ] && [ -r "/proc/$_tracer/status" ]; then
        _name=$(awk '/^Name:/{print $2; exit}' "/proc/$_tracer/status")
    fi
    _want=1
    case $_name in
        *proot*) _want=0 ;;
    esac
    assert_eq "live /proc/$$/status" "$(probe /proc/$$/status)" "$_want"
fi

# The shim must consult the helper before it starts proot. A direct exec
# of the real binary does not contain the word proot.
assert_guard() {
    _file=$1
    _seen=0
    _line=0
    _proot_exec=0
    while IFS= read -r _l || [ -n "$_l" ]; do
        _line=$((_line + 1))
        _trim=$_l
        _tab=$(printf '\t')
        while :; do
            case $_trim in
                " "* | "$_tab"*) _trim=${_trim#?} ;;
                *) break ;;
            esac
        done
        case $_trim in
            \#*) continue ;;
        esac
        case $_trim in
            *'traced_by_proot /proc/$$/status'*)
                _seen=1
                ;;
            *exec*proot*)
                _proot_exec=1
                if [ "$_seen" -ne 1 ]; then
                    fail "$_file: proot exec at line $_line is above traced_by_proot"
                fi
                ;;
        esac
    done < "$SCRIPT_DIR/$_file"
    if [ "$_seen" -ne 1 ]; then
        fail "$_file: missing traced_by_proot guard"
    fi
    if [ "$_proot_exec" -ne 1 ]; then
        fail "$_file: missing proot exec"
    fi
    if ! grep -q '/.local/shims/lib/proot_tracer.sh' "$SCRIPT_DIR/$_file"; then
        fail "$_file: does not source the installed helper"
    fi
}

assert_guard grok_termux_wrapper.sh
assert_guard codex_termux_wrapper.sh

for _pair in \
    'setup-grok-termux.yml grok_termux_wrapper.sh' \
    'setup-codex-termux.yml codex_termux_wrapper.sh'; do
    # shellcheck disable=SC2086 # Pair is two fields.
    set -- $_pair
    if ! grep -q "/.local/shims/lib/proot_tracer.sh" "$SCRIPT_DIR/$1"; then
        fail "$1: does not install proot_tracer.sh"
    fi
    if ! grep -q "src: $2" "$SCRIPT_DIR/$1"; then
        fail "$1: does not install $2"
    fi
done

echo "All proot_tracer tests passed."
