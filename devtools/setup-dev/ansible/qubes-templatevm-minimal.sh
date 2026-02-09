#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: minimal
# Install apt packages and configure their sources.
set -euo pipefail

# Add Debian backports (no-op on Ubuntu).
CODENAME=$(lsb_release -cs)
if grep -q '^ID=debian' /etc/os-release 2>/dev/null; then
  echo "deb http://deb.debian.org/debian ${CODENAME}-backports main contrib non-free non-free-firmware" \
    > /etc/apt/sources.list.d/backports.list
fi

apt-get update
apt-get install -y \
  curl \
  fzf \
  gh \
  git \
  git-delta \
  gpg \
  gpg-agent \
  grep \
  htop \
  jq \
  keychain \
  libssl-dev \
  make \
  man \
  openssh-client \
  pkg-config \
  plocate \
  sed \
  sshpass \
  which \
  zoxide
