#!/bin/sh
# Review `factory skill` before setup-factory.yml writes it. Sourced by ensure.sh.
#
# The Ansible local command module has no controlling terminal, so a prompt
# inside the playbook cannot be answered. This shell has the terminal.
# factory.yml runs first because it may upgrade the binary whose stdout is
# hashed. pre_approve_urls stays URL-only and runs before that upgrade.

# approve_factory_skill SCRIPT_DIR FLAGS ORIG_PLAYBOOKS
# FLAGS and ORIG_PLAYBOOKS are word-split strings, as ensure.sh stores them.
# Returns 0 when there is nothing to install or the text is already approved.
# Returns non-zero when factory.yml fails, the user declines, or no terminal
# is available for unapproved text. Does not write skill files.
approve_factory_skill() {
    _fs_dir=$1
    _fs_flags=$2
    _fs_orig=${3-}

    _fs_log=${ANSIBLE_RUN_LOG:-${HOME}/.cache/setup-dev/ensure_sh.ansible.log}
    _fs_status=${ANSIBLE_RUN_STATUS:-${HOME}/.cache/setup-dev/ensure_sh.ansible.status}
    mkdir -p "$(dirname "$_fs_log")" "$(dirname "$_fs_status")"

    printf '%s\n' "==> Running playbook: factory.yml"
    (
        # shellcheck disable=SC2086  # Intentional word splitting — flags never contain spaces
        ansible-playbook -i inventory.ini $_fs_flags factory.yml
        printf '%s\n' "$?" >"$_fs_status"
    ) 2>&1 | tee "$_fs_log"
    _fs_rc=1
    if [ -f "$_fs_status" ]; then
        _fs_rc=$(cat "$_fs_status")
    fi
    if [ "$_fs_rc" -ne 0 ]; then
        if command -v print_playbook_failure >/dev/null 2>&1; then
            print_playbook_failure "factory.yml" "$_fs_log" "$_fs_orig"
        else
            printf '%s\n' "Error: playbook factory.yml failed." >&2
        fi
        return 1
    fi

    # ansible-playbook --check skips the skill task, so a dry run must not
    # prompt or store an approval. -C is Ansible's short form of --check.
    _fs_check=0
    # shellcheck disable=SC2086  # Intentional word splitting — flags never contain spaces
    for _fs_flag in $_fs_flags; do
        case "$_fs_flag" in
            --check|-C) _fs_check=1 ;;
        esac
    done
    if [ "$_fs_check" -eq 1 ]; then
        return 0
    fi

    # go install writes the binary to GOPATH/bin. Ansible command modules do
    # not use the PATH that setup-user-go-bin-directory.yml puts on ansible_env.
    _fs_path="$(go env GOPATH)/bin:${PATH}"
    if ! PATH="$_fs_path" command -v factory >/dev/null 2>&1; then
        printf '%s\n' "WARNING: Skipping factory skill install because factory is not on PATH. Nothing was written."
        return 0
    fi

    _fs_out=$(mktemp)
    _fs_skill_rc=0
    PATH="$_fs_path" factory skill >"$_fs_out" || _fs_skill_rc=$?
    if [ "$_fs_skill_rc" -ne 0 ]; then
        printf '%s\n' "WARNING: Skipping factory skill install because \`factory skill\` exited ${_fs_skill_rc}. Nothing was written."
        rm -f "$_fs_out"
        return 0
    fi
    # Match the playbook: whitespace-only stdout is "printed nothing".
    if ! grep -q '[^[:space:]]' "$_fs_out"; then
        printf '%s\n' "WARNING: Skipping factory skill install because \`factory skill\` printed nothing. Nothing was written."
        rm -f "$_fs_out"
        return 0
    fi
    rm -f "$_fs_out"

    if PATH="$_fs_path" "${_fs_dir}/verified-run" check-text factory-skill -- factory skill; then
        return 0
    fi

    printf '%s\n' "factory skill text needs approval."
    if ! PATH="$_fs_path" "${_fs_dir}/verified-run" review-text factory-skill -- factory skill; then
        return 1
    fi
    return 0
}
