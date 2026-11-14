#!/bin/sh
# Review `decide skill` and write it after the playbooks. Sourced by
# ensure.sh.
#
# The Ansible local command module has no controlling terminal, so a prompt
# inside the playbook cannot be answered. ensure.sh installs decide in the
# main loop, then calls this function. This function does not run a playbook
# and does not use become. pre_approve_urls stays URL-only.

# approve_decide_skill SCRIPT_DIR FLAGS
# FLAGS is a word-split string, as ensure.sh stores it.
# Returns 0 when there is nothing to install, check mode, or the skill file
# was written. Returns non-zero when the user declines or apply-text fails.
# Check mode does not prompt, store an approval, or write the skill file.
approve_decide_skill() {
    _dc_dir=$1
    _dc_flags=$2

    # A --check or -C dry run must not prompt, store an approval, or write
    # the skill file. -C is the short form of --check.
    _dc_check=0
    # shellcheck disable=SC2086  # Intentional word splitting — flags never contain spaces
    for _dc_flag in $_dc_flags; do
        case "$_dc_flag" in
            --check|-C) _dc_check=1 ;;
        esac
    done
    if [ "$_dc_check" -eq 1 ]; then
        return 0
    fi

    # go install writes the binary to GOPATH/bin. Ansible command modules do
    # not use the PATH that setup-user-go-bin-directory.yml puts on ansible_env.
    _dc_path="$(go env GOPATH)/bin:${PATH}"
    if ! PATH="$_dc_path" command -v decide >/dev/null 2>&1; then
        printf '%s\n' "WARNING: Skipping decide skill install because decide is not on PATH. Nothing was written."
        return 0
    fi

    _dc_out=$(mktemp)
    _dc_skill_rc=0
    PATH="$_dc_path" decide skill >"$_dc_out" || _dc_skill_rc=$?
    if [ "$_dc_skill_rc" -ne 0 ]; then
        printf '%s\n' "WARNING: Skipping decide skill install because \`decide skill\` exited ${_dc_skill_rc}. Nothing was written."
        rm -f "$_dc_out"
        return 0
    fi
    # Match the playbook: whitespace-only stdout is "printed nothing".
    if ! grep -q '[^[:space:]]' "$_dc_out"; then
        printf '%s\n' "WARNING: Skipping decide skill install because \`decide skill\` printed nothing. Nothing was written."
        rm -f "$_dc_out"
        return 0
    fi
    rm -f "$_dc_out"

    if ! PATH="$_dc_path" "${_dc_dir}/verified-run" check-text decide-skill -- decide skill; then
        printf '%s\n' "decide skill text needs approval."
        if ! PATH="$_dc_path" "${_dc_dir}/verified-run" review-text decide-skill -- decide skill; then
            return 1
        fi
    fi

    # Same destination as decide_skill_paths in setup-decide.yml.
    # apply-text refuses to create the parent directory.
    _dc_dest="${HOME}/.claude/skills/decide/SKILL.md"
    mkdir -p "$(dirname "$_dc_dest")"
    if ! PATH="$_dc_path" "${_dc_dir}/verified-run" apply-text decide-skill "$_dc_dest" -- decide skill; then
        return 1
    fi
    return 0
}
