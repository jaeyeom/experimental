#!/bin/sh
# Ansible become password helpers. Sourced by ensure.sh.
#
# Ansible's sudo become plugin defaults to `sudo -H -S -n` and does not reuse
# a terminal sudo ticket. `sudo true` succeeding in the user's shell is not
# enough: timestamps are TTY-specific, and Ansible invokes sudo without that
# TTY. Probe passwordless sudo in a new session so a TTY ticket cannot mask
# a missing NOPASSWD rule.

# True if ansible-playbook flags already request a become password.
# Usage: ansible_flags_ask_become [flag...]
ansible_flags_ask_become() {
    for _afab in "$@"; do
        case "$_afab" in
            -K|--ask-become-pass) return 0 ;;
        esac
    done
    return 1
}

# True if a become password is already available via flags or environment.
# Usage: ansible_become_has_password [flag...]
ansible_become_has_password() {
    [ -n "${ANSIBLE_BECOME_PASS:-}" ] && return 0
    [ -n "${ANSIBLE_SUDO_PASS:-}" ] && return 0
    ansible_flags_ask_become "$@"
}

# Extra ansible-playbook flags needed for become.
# $1 interactive (0/1), $2 already has password (0/1), $3 passwordless (0/1).
# Prints -K when an interactive session needs a become password.
# Returns 1 when a password is required and the session is non-interactive.
ansible_become_extra_flags() {
    _abef_interactive="$1"
    _abef_already="$2"
    _abef_passwordless="$3"
    if [ "$_abef_already" = 1 ] || [ "$_abef_passwordless" = 1 ]; then
        return 0
    fi
    if [ "$_abef_interactive" = 1 ]; then
        printf '%s\n' "-K"
        return 0
    fi
    return 1
}

# True if sudo can run non-interactively without a TTY ticket (NOPASSWD or root).
ansible_sudo_passwordless() {
    if [ "$(id -u)" -eq 0 ]; then
        return 0
    fi
    if ! command -v sudo >/dev/null 2>&1; then
        return 1
    fi
    # Detach from the controlling TTY so a cached ticket from this terminal
    # cannot satisfy Ansible's sudo, which uses a different TTY (or none).
    if command -v setsid >/dev/null 2>&1 && setsid -w true </dev/null >/dev/null 2>&1; then
        setsid -w sudo -n -S true </dev/null >/dev/null 2>&1
        return $?
    fi
    sudo -n -S true </dev/null >/dev/null 2>&1
}
