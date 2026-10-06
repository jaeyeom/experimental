#!/data/data/com.termux/files/usr/bin/sh
# Wrapper for Codex on Termux. The static musl Linux/aarch64 binary cannot
# resolve DNS on Android (no /etc/resolv.conf) and does not find native CA
# roots at standard Linux paths. Bubblewrap cannot create user namespaces
# on Android, so the codex sandbox must be disabled.
#
# Start proot only when this process is not already traced by proot.
# Nested proot does not attach. A direct exec keeps the outer proot's
# resolv.conf bind.
set -eu

_helper=${HOME}/.local/shims/lib/proot_tracer.sh
if [ ! -r "$_helper" ]; then
    printf '%s\n' "codex termux wrapper: missing $_helper" >&2
    exit 1
fi
# shellcheck disable=SC1090,SC1091 # Installed path depends on $HOME.
. "$_helper"

# Find the real codex (this wrapper is at ~/.local/shims/bin which precedes
# $PREFIX/bin in PATH; the second `which -a` hit is the npm-installed binary).
# shellcheck disable=SC2230 # which -a lists every PATH hit; command -v returns one.
REAL_CODEX=$(which -a codex 2>/dev/null | sed -n '2p')
if [ -z "$REAL_CODEX" ] || [ "$REAL_CODEX" = "$0" ]; then
    REAL_CODEX=/data/data/com.termux/files/usr/bin/codex
fi

export SSL_CERT_FILE="${SSL_CERT_FILE:-/data/data/com.termux/files/usr/etc/tls/cert.pem}"

if traced_by_proot /proc/$$/status; then
    exec "$REAL_CODEX" -c 'sandbox_mode="danger-full-access"' "$@"
fi

exec proot -b /data/data/com.termux/files/usr/etc/resolv.conf:/etc/resolv.conf \
    "$REAL_CODEX" -c 'sandbox_mode="danger-full-access"' "$@"
