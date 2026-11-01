#!/bin/sh
# Review herdr --skill before setup-herdr.yml writes it. Sourced by ensure.sh.
#
# The Ansible local command module has no controlling terminal, so a prompt
# inside the playbook cannot be answered. This shell has the terminal.
# herdr.yml runs first because it may upgrade the binary whose stdout is
# hashed. pre_approve_urls stays URL-only and runs before that upgrade.

# approve_herdr_skill SCRIPT_DIR FLAGS ORIG_PLAYBOOKS
# FLAGS and ORIG_PLAYBOOKS are word-split strings, as ensure.sh stores them.
# Returns 0 when there is nothing to install or the text is already approved.
# Returns non-zero when herdr.yml fails, the user declines, or no terminal
# is available for unapproved text. Does not write skill files.
approve_herdr_skill() {
    _hs_dir=$1
    _hs_flags=$2
    _hs_orig=${3-}

    _hs_log=${ANSIBLE_RUN_LOG:-${HOME}/.cache/setup-dev/ensure_sh.ansible.log}
    _hs_status=${ANSIBLE_RUN_STATUS:-${HOME}/.cache/setup-dev/ensure_sh.ansible.status}
    mkdir -p "$(dirname "$_hs_log")" "$(dirname "$_hs_status")"

    printf '%s\n' "==> Running playbook: herdr.yml"
    (
        # shellcheck disable=SC2086  # Intentional word splitting — flags never contain spaces
        ansible-playbook -i inventory.ini $_hs_flags herdr.yml
        printf '%s\n' "$?" >"$_hs_status"
    ) 2>&1 | tee "$_hs_log"
    _hs_rc=1
    if [ -f "$_hs_status" ]; then
        _hs_rc=$(cat "$_hs_status")
    fi
    if [ "$_hs_rc" -ne 0 ]; then
        if command -v print_playbook_failure >/dev/null 2>&1; then
            print_playbook_failure "herdr.yml" "$_hs_log" "$_hs_orig"
        else
            printf '%s\n' "Error: playbook herdr.yml failed." >&2
        fi
        return 1
    fi

    # ansible-playbook --check skips the skill task, so a dry run must not
    # prompt or store an approval. -C is Ansible's short form of --check.
    _hs_check=0
    # shellcheck disable=SC2086  # Intentional word splitting — flags never contain spaces
    for _hs_flag in $_hs_flags; do
        case "$_hs_flag" in
            --check|-C) _hs_check=1 ;;
        esac
    done
    if [ "$_hs_check" -eq 1 ]; then
        return 0
    fi

    _hs_path="${HOME}/.local/bin:${PATH}"
    if ! PATH="$_hs_path" command -v herdr >/dev/null 2>&1; then
        printf '%s\n' "WARNING: Skipping herdr skill install because herdr is not on PATH. Nothing was written."
        return 0
    fi

    _hs_out=$(mktemp)
    _hs_skill_rc=0
    PATH="$_hs_path" herdr --skill >"$_hs_out" || _hs_skill_rc=$?
    if [ "$_hs_skill_rc" -ne 0 ]; then
        printf '%s\n' "WARNING: Skipping herdr skill install because \`herdr --skill\` exited ${_hs_skill_rc}. Nothing was written."
        rm -f "$_hs_out"
        return 0
    fi
    # Match the playbook: whitespace-only stdout is "printed nothing".
    if ! grep -q '[^[:space:]]' "$_hs_out"; then
        printf '%s\n' "WARNING: Skipping herdr skill install because \`herdr --skill\` printed nothing. Nothing was written."
        rm -f "$_hs_out"
        return 0
    fi
    rm -f "$_hs_out"

    if PATH="$_hs_path" "${_hs_dir}/verified-run" check-text herdr-skill -- herdr --skill; then
        return 0
    fi

    printf '%s\n' "herdr skill text needs approval."
    if ! PATH="$_hs_path" "${_hs_dir}/verified-run" review-text herdr-skill -- herdr --skill; then
        return 1
    fi
    return 0
}
