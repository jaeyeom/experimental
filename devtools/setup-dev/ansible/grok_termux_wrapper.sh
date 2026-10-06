#!/data/data/com.termux/files/usr/bin/sh
# Bind Termux's resolv.conf over /etc/resolv.conf for the official
# static musl Grok binary. ~/.grok/bin stays the real binary so
# `grok update` can replace it.
#
# Start proot only when this process is not already traced by proot.
# Nested proot does not attach: the outer proot keeps the trace, and
# proot-distro refuses to run. A direct exec stays under that outer
# proot, so its resolv.conf bind still covers the binary.
set -eu

_helper=${HOME}/.local/shims/lib/proot_tracer.sh
if [ ! -r "$_helper" ]; then
    printf '%s\n' "grok termux wrapper: missing $_helper" >&2
    exit 1
fi
# shellcheck disable=SC1090,SC1091 # Installed path depends on $HOME.
. "$_helper"

name=$(basename -- "$0")
case $name in
    grok | agent) ;;
    *)
        printf '%s\n' "grok termux wrapper: unexpected command name: $name" >&2
        exit 1
        ;;
esac

if traced_by_proot /proc/$$/status; then
    exec "$HOME/.grok/bin/$name" "$@"
fi

prefix=${PREFIX:-/data/data/com.termux/files/usr}
resolv=$prefix/etc/resolv.conf
if [ ! -s "$resolv" ]; then
    printf '%s\n' "grok termux wrapper: $resolv is missing or empty" >&2
    exit 1
fi

exec "$prefix/bin/proot" -b "$resolv:/etc/resolv.conf" "$HOME/.grok/bin/$name" "$@"
