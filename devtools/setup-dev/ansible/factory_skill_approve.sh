#!/bin/sh
# Review `factory skill` and write it after the playbooks. Sourced by ensure.sh.
#
# The Ansible local command module has no controlling terminal, so a prompt
# inside the playbook cannot be answered. ensure.sh installs factory in the
# main loop, then calls this function. This function does not run a playbook
# and does not use become. pre_approve_urls stays URL-only.

# approve_factory_skill SCRIPT_DIR FLAGS
# FLAGS is a word-split string, as ensure.sh stores it.
# Returns 0 when there is nothing to install, check mode, or the skill file
# was written. Returns non-zero when the user declines or apply-text fails.
# Check mode does not prompt, store an approval, or write the skill file.
approve_factory_skill() {
    _fs_dir=$1
    _fs_flags=$2

    # A --check or -C dry run must not prompt, store an approval, or write
    # the skill file. -C is the short form of --check.
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

    if ! PATH="$_fs_path" "${_fs_dir}/verified-run" check-text factory-skill -- factory skill; then
        printf '%s\n' "factory skill text needs approval."
        if ! PATH="$_fs_path" "${_fs_dir}/verified-run" review-text factory-skill -- factory skill; then
            return 1
        fi
    fi

    # Same destination as factory_skill_paths in setup-factory.yml.
    # apply-text refuses to create the parent directory.
    _fs_dest="${HOME}/.claude/skills/factory/SKILL.md"
    mkdir -p "$(dirname "$_fs_dest")"
    if ! PATH="$_fs_path" "${_fs_dir}/verified-run" apply-text factory-skill "$_fs_dest" -- factory skill; then
        return 1
    fi
    return 0
}
