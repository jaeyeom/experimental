#!/bin/sh
# Remove desktop bloat from a Debian Qubes TemplateVM.
#
# Source: debian-*-xfce (stock Qubes template).
# Target: trimmed xfce — run default-debian.sh next to reach debian-*-xfce-default.
set -eu

if [ "$(id -u)" -ne 0 ]; then
    echo "Error: This script must be run as root (or with sudo)." >&2
    exit 1
fi

apt remove --auto-remove \
    firefox-esr \
    "gnome-terminal*" \
    "keepassx*" \
    "libreoffice*" \
    thunderbird \
    xterm