#!/bin/sh
# Review `slackwatch skill` and write it after the playbooks. Sourced by
# ensure.sh.
#
# The Ansible local command module has no controlling terminal, so a prompt
# inside the playbook cannot be answered. ensure.sh installs slackwatch in the
# main loop, then calls this function. This function does not run a playbook
# and does not use become. pre_approve_urls stays URL-only.

# approve_slackwatch_skill SCRIPT_DIR FLAGS
# FLAGS is a word-split string, as ensure.sh stores it.
# Returns 0 when there is nothing to install, check mode, or the skill file
# was written. Returns non-zero when the user declines or apply-text fails.
# Check mode does not prompt, store an approval, or write the skill file.
approve_slackwatch_skill() {
    _sw_dir=$1
    _sw_flags=$2

    # A --check or -C dry run must not prompt, store an approval, or write
    # the skill file. -C is the short form of --check.
    _sw_check=0
    # shellcheck disable=SC2086  # Intentional word splitting — flags never contain spaces
    for _sw_flag in $_sw_flags; do
        case "$_sw_flag" in
            --check|-C) _sw_check=1 ;;
        esac
    done
    if [ "$_sw_check" -eq 1 ]; then
        return 0
    fi

    # go install writes the binary to GOPATH/bin. Ansible command modules do
    # not use the PATH that setup-user-go-bin-directory.yml puts on ansible_env.
    _sw_path="$(go env GOPATH)/bin:${PATH}"
    if ! PATH="$_sw_path" command -v slackwatch >/dev/null 2>&1; then
        printf '%s\n' "WARNING: Skipping slackwatch skill install because slackwatch is not on PATH. Nothing was written."
        return 0
    fi

    _sw_out=$(mktemp)
    _sw_skill_rc=0
    PATH="$_sw_path" slackwatch skill >"$_sw_out" || _sw_skill_rc=$?
    if [ "$_sw_skill_rc" -ne 0 ]; then
        printf '%s\n' "WARNING: Skipping slackwatch skill install because \`slackwatch skill\` exited ${_sw_skill_rc}. Nothing was written."
        rm -f "$_sw_out"
        return 0
    fi
    # Match the playbook: whitespace-only stdout is "printed nothing".
    if ! grep -q '[^[:space:]]' "$_sw_out"; then
        printf '%s\n' "WARNING: Skipping slackwatch skill install because \`slackwatch skill\` printed nothing. Nothing was written."
        rm -f "$_sw_out"
        return 0
    fi
    rm -f "$_sw_out"

    if ! PATH="$_sw_path" "${_sw_dir}/verified-run" check-text slackwatch-skill -- slackwatch skill; then
        printf '%s\n' "slackwatch skill text needs approval."
        if ! PATH="$_sw_path" "${_sw_dir}/verified-run" review-text slackwatch-skill -- slackwatch skill; then
            return 1
        fi
    fi

    # Same destination as slackwatch_skill_paths in setup-slackwatch.yml.
    # apply-text refuses to create the parent directory.
    _sw_dest="${HOME}/.claude/skills/slackwatch/SKILL.md"
    mkdir -p "$(dirname "$_sw_dest")"
    if ! PATH="$_sw_path" "${_sw_dir}/verified-run" apply-text slackwatch-skill "$_sw_dest" -- slackwatch skill; then
        return 1
    fi
    return 0
}
