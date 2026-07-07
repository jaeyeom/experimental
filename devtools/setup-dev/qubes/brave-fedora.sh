#!/bin/sh
# Add Brave browser to a fedora-*-xfce-default Qubes TemplateVM.
#
# Source: fedora-*-xfce-default (after trim-fedora.sh and default-fedora.sh).
# Target: fedora-*-xfce-brave.
#
# Repo setup follows https://brave.com/linux/ (Fedora 41+ uses dnf5 syntax).
set -eu

if [ "$(id -u)" -ne 0 ]; then
    echo "Error: This script must be run as root (or with sudo)." >&2
    exit 1
fi

dnf install -y dnf-plugins-core

FEDORA_VER="$(rpm -E %fedora)"
BRAVE_REPO_URL="https://brave-browser-rpm-release.s3.brave.com/brave-browser.repo"

if [ "${FEDORA_VER}" -ge 41 ]; then
    dnf config-manager addrepo --from-repofile="${BRAVE_REPO_URL}"
else
    dnf config-manager --add-repo "${BRAVE_REPO_URL}"
fi

dnf install -y brave-browser