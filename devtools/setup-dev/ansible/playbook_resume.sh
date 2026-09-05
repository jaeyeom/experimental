#!/bin/sh
# Playbook failure parsing and --from include-guard lookup. Sourced by ensure.sh.

# Print basename:line from the last "task path: FILE:LINE" in LOG.
# Usage: failed_task_path LOG
failed_task_path() {
    _ftp_log="$1"
    _ftp_line=$(grep '^task path: ' "$_ftp_log" 2>/dev/null | sed -n '$p') || true
    if [ -z "$_ftp_line" ]; then
        return 1
    fi
    _ftp_path=${_ftp_line#task path: }
    printf '%s\n' "${_ftp_path##*/}"
}

# Print the last TASK [name] from LOG.
# Usage: failed_task_name LOG
failed_task_name() {
    _ftn_log="$1"
    _ftn_line=$(grep '^TASK \[' "$_ftn_log" 2>/dev/null | sed -n '$p') || true
    if [ -z "$_ftn_line" ]; then
        return 1
    fi
    _ftn_name=${_ftn_line#TASK [}
    _ftn_name=${_ftn_name%%]*}
    printf '%s\n' "$_ftn_name"
}

# Print the include-guard task name from PLAYBOOK (.yml optional).
# Reads the file; does not construct the name from the filename.
# Usage: include_guard_task_name PLAYBOOK
include_guard_task_name() {
    _igt_pb="$1"
    case "$_igt_pb" in
        *.yml) ;;
        *) _igt_pb="${_igt_pb}.yml" ;;
    esac
    if [ ! -f "$_igt_pb" ]; then
        printf '%s\n' "Error: playbook $_igt_pb not found." >&2
        return 1
    fi
    _igt_name=$(sed -n 's/^[[:space:]]*- name: \(Stop early if the .* playbook is already included\)$/\1/p' "$_igt_pb" | sed -n '1p')
    if [ -z "$_igt_name" ]; then
        printf '%s\n' "Error: no include-guard task in ${_igt_pb##*/}." >&2
        return 1
    fi
    printf '%s\n' "$_igt_name"
}

# Print a failure summary and --from resume command to stderr.
# ORIG_PLAYBOOKS is the ensure.sh playbook list, including a leading space.
# Usage: print_playbook_failure TOP_PLAYBOOK LOG ORIG_PLAYBOOKS
print_playbook_failure() {
    _ppf_pb="$1"
    _ppf_log="$2"
    _ppf_orig="$3"
    printf '%s\n' "Error: playbook ${_ppf_pb} failed." >&2
    _ppf_path=$(failed_task_path "$_ppf_log") || _ppf_path=""
    _ppf_task=$(failed_task_name "$_ppf_log") || _ppf_task=""
    if [ -n "$_ppf_path" ]; then
        printf '%s\n' "Failed in: $_ppf_path" >&2
    fi
    if [ -n "$_ppf_task" ]; then
        printf '%s\n' "Task: $_ppf_task" >&2
    fi
    if [ -n "$_ppf_path" ]; then
        _ppf_from=${_ppf_path%%:*}
        _ppf_from=${_ppf_from%.yml}
        printf '%s\n' "Resume from that play and continue:" >&2
        printf '%s\n' "  ./ensure.sh --from ${_ppf_from} --${_ppf_orig}" >&2
    fi
}
