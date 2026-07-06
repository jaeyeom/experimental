#!/bin/sh
# Remove desktop bloat from a Fedora Qubes TemplateVM.
#
# Assumes a template derived from fedora-*-minimum or fedora-*-default.
# Run default-fedora.sh afterward to add packages for a default template.
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