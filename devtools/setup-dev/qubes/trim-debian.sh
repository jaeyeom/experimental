#!/bin/sh
# Remove desktop bloat from a Debian Qubes TemplateVM.
#
# Assumes a template derived from debian-*-minimum or debian-*-default.
# Run default-debian.sh afterward to add packages for a default template.
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