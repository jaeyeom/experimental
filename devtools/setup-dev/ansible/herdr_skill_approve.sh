#!/bin/sh
# Review herdr --skill and write it after the playbooks. Sourced by ensure.sh.
#
# The Ansible local command module has no controlling terminal, so a prompt
# inside the playbook cannot be answered. ensure.sh installs herdr in the
# main loop, then calls this function. This function does not run a playbook
# and does not use become. pre_approve_urls stays URL-only.

# approve_herdr_skill SCRIPT_DIR FLAGS
# FLAGS is a word-split string, as ensure.sh stores it.
# Returns 0 when there is nothing to install, check mode, or the skill file
# was written. Returns non-zero when the user declines or apply-text fails.
# Check mode does not prompt, store an approval, or write the skill file.
approve_herdr_skill() {
    _hs_dir=$1
    _hs_flags=$2

    # A --check or -C dry run must not prompt, store an approval, or write
    # the skill file. -C is the short form of --check.
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

    if ! PATH="$_hs_path" "${_hs_dir}/verified-run" check-text herdr-skill -- herdr --skill; then
        printf '%s\n' "herdr skill text needs approval."
        if ! PATH="$_hs_path" "${_hs_dir}/verified-run" review-text herdr-skill -- herdr --skill; then
            return 1
        fi
    fi

    # Same destination as herdr_skill_paths in setup-herdr.yml.
    # apply-text refuses to create the parent directory.
    _hs_dest="${HOME}/.claude/skills/herdr/SKILL.md"
    mkdir -p "$(dirname "$_hs_dest")"
    if ! PATH="$_hs_path" "${_hs_dir}/verified-run" apply-text herdr-skill "$_hs_dest" -- herdr --skill; then
        return 1
    fi
    return 0
}
