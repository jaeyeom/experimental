#!/bin/sh
# Argument parsing for ensure.sh. Sourced by ensure.sh.
#
# POSIX sh has no arrays, so arguments for ansible-playbook are kept in
# $ansible_args as a string of single-quoted words. Expand it with eval so
# each value stays one argument, even with spaces, quotes, JSON, or globs:
#   eval "ansible-playbook $ansible_args \"\$playbook\""

# ensure_shell_quote VALUE prints VALUE as one single-quoted shell word.
ensure_shell_quote() {
    printf "'%s'" "$(printf '%s' "$1" | sed "s/'/'\\\\''/g")"
}

# ensure_args_append ARG... appends ARGs to $ansible_args.
ensure_args_append() {
    for _eaa in "$@"; do
        ansible_args="$ansible_args $(ensure_shell_quote "$_eaa")"
    done
}

# ensure_parse_args ARG... splits ensure.sh arguments and sets:
#   playbooks      space-separated playbook names
#   ansible_args   quoted words for ansible-playbook (see above)
#   flags          standalone flags without spaces, word-split by callers
#                  that look for --check, -C, or -K
#   from_playbook  the value of --from (not passed to ansible-playbook)
# Value-taking flags read the next argument as their value, so
#   ./ensure.sh -e foo=bar --tags setup all
# runs only all.yml. Arguments after -- are always playbooks.
# Returns 1 when a flag is missing its value.
ensure_parse_args() {
    flags=""
    ansible_args=""
    playbooks=""
    from_playbook=""
    _epa_past_separator=false
    _epa_expect=""
    for _epa_arg in "$@"; do
        if [ -n "$_epa_expect" ]; then
            if [ "$_epa_expect" = "--from" ]; then
                # shellcheck disable=SC2034  # Read by ensure.sh
                from_playbook="$_epa_arg"
            else
                ensure_args_append "$_epa_expect" "$_epa_arg"
            fi
            _epa_expect=""
            continue
        fi
        if [ "$_epa_past_separator" = true ]; then
            playbooks="$playbooks $_epa_arg"
            continue
        fi
        case "$_epa_arg" in
            --)
                _epa_past_separator=true
                ;;
            --from | -e | --extra-vars | -t | --tags | --skip-tags | \
                -l | --limit | -i | --inventory)
                _epa_expect="$_epa_arg"
                ;;
            -*)
                ensure_args_append "$_epa_arg"
                case "$_epa_arg" in
                    *[[:space:]]*) ;;
                    *) flags="$flags $_epa_arg" ;;
                esac
                ;;
            *)
                playbooks="$playbooks $_epa_arg"
                ;;
        esac
    done
    if [ -n "$_epa_expect" ]; then
        if [ "$_epa_expect" = "--from" ]; then
            echo "Error: --from requires a playbook name." >&2
        else
            echo "Error: $_epa_expect requires a value." >&2
        fi
        return 1
    fi
    return 0
}
