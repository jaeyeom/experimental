#!/bin/sh
# Remove desktop bloat from a Fedora Qubes TemplateVM.
#
# Source: fedora-*-xfce (stock Qubes template).
# Target: trimmed xfce — run default-fedora.sh next to reach fedora-*-xfce-default.
#
# Do not remove xterm on Fedora 43+: dnf cascades other removals that break sudo.
# xterm was safe to drop on Fedora 42 only.
set -eu

if [ "$(id -u)" -ne 0 ]; then
    echo "Error: This script must be run as root (or with sudo)." >&2
    exit 1
fi

dnf remove -y \
    asunder \
    claws-mail \
    firefox \
    geany \
    keepassxc \
    pidgin \
    thunderbird \
    xfburn