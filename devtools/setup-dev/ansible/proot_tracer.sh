#!/bin/sh
# Report whether a process is traced by proot.
#
# proot-distro reads TracerPid from /proc/<pid>/status and treats the
# process as nested when that tracer's Name contains "proot". An
# unreadable status is not proot: skipping the resolv.conf bind on a
# guess would break musl DNS on raw Termux.
#
# Usage: traced_by_proot PID_STATUS [TRACER_STATUS]
# PID_STATUS is a /proc/<pid>/status file. When TRACER_STATUS is omitted,
# the tracer is /proc/<TracerPid>/status. TracerPid must be a decimal
# integer. Returns 0 when the tracer name contains "proot".

_tbp_field_value() {
    _tbp_file=$1
    _tbp_key=$2
    _tbp_val=
    while IFS= read -r _tbp_line || [ -n "$_tbp_line" ]; do
        case $_tbp_line in
            "$_tbp_key":*)
                _tbp_val=${_tbp_line#"$_tbp_key":}
                break
                ;;
        esac
    done < "$_tbp_file" || return 0

    _tbp_tab=$(printf '\t')
    while :; do
        case $_tbp_val in
            " "*|"$_tbp_tab"*) _tbp_val=${_tbp_val#?} ;;
            *) break ;;
        esac
    done
    while :; do
        case $_tbp_val in
            *" "|*"$_tbp_tab") _tbp_val=${_tbp_val%?} ;;
            *) break ;;
        esac
    done
    printf '%s\n' "$_tbp_val"
}

traced_by_proot() {
    if [ $# -lt 1 ] || [ ! -r "$1" ]; then
        return 1
    fi

    _tbp_tracer=$(_tbp_field_value "$1" TracerPid) || return 1
    case $_tbp_tracer in
        '' | 0 | *[!0-9]*) return 1 ;;
    esac

    _tbp_tracer_status=${2-}
    if [ -z "$_tbp_tracer_status" ]; then
        _tbp_tracer_status=/proc/${_tbp_tracer}/status
    fi
    if [ ! -r "$_tbp_tracer_status" ]; then
        return 1
    fi

    _tbp_name=$(_tbp_field_value "$_tbp_tracer_status" Name) || return 1
    case $_tbp_name in
        *proot*) return 0 ;;
    esac
    return 1
}
