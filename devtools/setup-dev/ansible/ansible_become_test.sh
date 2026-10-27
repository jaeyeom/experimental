#!/bin/sh
# Tests for ansible become password detection helpers.
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

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
sh -n "$SCRIPT_DIR/ansible_become.sh"
sh -n "$SCRIPT_DIR/ansible_become_test.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/ansible_become.sh"

# --- ansible_flags_ask_become ---

if ansible_flags_ask_become; then
    fail "empty flags must not ask become pass"
fi
if ansible_flags_ask_become -v --check; then
    fail "-v --check must not ask become pass"
fi
if ! ansible_flags_ask_become -K; then
    fail "-K must ask become pass"
fi
if ! ansible_flags_ask_become -v -K --check; then
    fail "-K among other flags must ask become pass"
fi
if ! ansible_flags_ask_become --ask-become-pass; then
    fail "--ask-become-pass must ask become pass"
fi
if ansible_flags_ask_become --keep; then
    fail "--keep must not be treated as --ask-become-pass"
fi

# --- ansible_become_has_password ---

if ansible_become_has_password; then
    fail "no flags and no env must not have a become password"
fi
if ! ansible_become_has_password -K; then
    fail "-K must count as having a become password"
fi
export ANSIBLE_BECOME_PASS=secret
if ! ansible_become_has_password; then
    fail "ANSIBLE_BECOME_PASS must count as having a become password"
fi
unset ANSIBLE_BECOME_PASS
export ANSIBLE_SUDO_PASS=secret
if ! ansible_become_has_password; then
    fail "ANSIBLE_SUDO_PASS must count as having a become password"
fi
unset ANSIBLE_SUDO_PASS

# --- ansible_become_extra_flags ---
# Args: INTERACTIVE ALREADY_HAS_PASSWORD PASSWORDLESS
# Prints -K when an interactive session needs a become password.

_got=$(ansible_become_extra_flags 1 1 0) || fail "already-has-password must succeed"
assert_eq "already has password" "$_got" ""

_got=$(ansible_become_extra_flags 1 0 1) || fail "passwordless must succeed"
assert_eq "passwordless" "$_got" ""

_got=$(ansible_become_extra_flags 0 0 1) || fail "non-interactive passwordless must succeed"
assert_eq "non-interactive passwordless" "$_got" ""

_got=$(ansible_become_extra_flags 1 0 0) || fail "interactive without passwordless must succeed"
assert_eq "interactive asks" "$_got" "-K"

if _got=$(ansible_become_extra_flags 0 0 0); then
    fail "non-interactive without passwordless must fail, got '$_got'"
fi

# --- ansible_sudo_passwordless with PATH stubs ---

_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp"' EXIT

# Fake sudo that succeeds: treat as passwordless.
cat > "$_tmp/sudo" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$_tmp/sudo"

# Fake setsid -w that execs the rest of the command line.
cat > "$_tmp/setsid" <<'EOF'
#!/bin/sh
if [ "$1" = "-w" ] || [ "$1" = "--wait" ]; then
    shift
fi
exec "$@"
EOF
chmod +x "$_tmp/setsid"

_old_path=$PATH
PATH="$_tmp:$PATH"
export PATH
if ! ansible_sudo_passwordless; then
    PATH="$_old_path"
    fail "successful sudo -n must count as passwordless"
fi

# Fake sudo that requires a password.
cat > "$_tmp/sudo" <<'EOF'
#!/bin/sh
echo "sudo: a password is required" >&2
exit 1
EOF
chmod +x "$_tmp/sudo"
if ansible_sudo_passwordless; then
    PATH="$_old_path"
    fail "sudo -n failure must not count as passwordless"
fi
PATH="$_old_path"

# --- sudo-rs become executable ---

_rs_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp" "$_rs_tmp"' EXIT

cat > "$_rs_tmp/sudo" <<'EOF'
#!/bin/sh
if [ "$1" = "-V" ]; then
    echo "sudo-rs 0.2.13"
    exit 0
fi
echo "sudo: a password is required" >&2
exit 1
EOF
cat > "$_rs_tmp/sudo.ws" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$_rs_tmp/sudo" "$_rs_tmp/sudo.ws"

PATH="$_rs_tmp:$PATH"
export PATH
unset ANSIBLE_BECOME_EXE
ansible_use_classic_sudo_for_become
assert_eq "sudo-rs selects sudo.ws" "${ANSIBLE_BECOME_EXE:-}" "$_rs_tmp/sudo.ws"

ANSIBLE_BECOME_EXE=/custom/sudo
export ANSIBLE_BECOME_EXE
ansible_use_classic_sudo_for_become
assert_eq "explicit become exe kept" "$ANSIBLE_BECOME_EXE" "/custom/sudo"
unset ANSIBLE_BECOME_EXE

cat > "$_rs_tmp/sudo" <<'EOF'
#!/bin/sh
if [ "$1" = "-V" ]; then
    echo "Sudo version 1.9.17p2"
    exit 0
fi
exit 1
EOF
chmod +x "$_rs_tmp/sudo"
ansible_use_classic_sudo_for_become
if [ -n "${ANSIBLE_BECOME_EXE:-}" ]; then
    fail "classic sudo must not set ANSIBLE_BECOME_EXE, got '$ANSIBLE_BECOME_EXE'"
fi

rm -f "$_rs_tmp/sudo.ws"
cat > "$_rs_tmp/sudo" <<'EOF'
#!/bin/sh
if [ "$1" = "-V" ]; then
    echo "sudo-rs 0.2.13"
    exit 0
fi
exit 1
EOF
chmod +x "$_rs_tmp/sudo"
# Drop the rest of PATH so the system sudo.ws cannot satisfy command -v.
PATH="$_rs_tmp"
ansible_use_classic_sudo_for_become
if [ -n "${ANSIBLE_BECOME_EXE:-}" ]; then
    fail "missing sudo.ws must leave ANSIBLE_BECOME_EXE unset, got '$ANSIBLE_BECOME_EXE'"
fi

PATH="$_old_path"
unset ANSIBLE_BECOME_EXE

echo "All ansible_become tests passed."
