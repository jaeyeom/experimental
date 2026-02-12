#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: cloudflare-dev
# Install apt packages and configure their sources.
set -euo pipefail

# Add Debian backports (no-op on Ubuntu).
CODENAME=$(lsb_release -cs)
if grep -q '^ID=debian' /etc/os-release 2>/dev/null; then
  echo "deb http://deb.debian.org/debian ${CODENAME}-backports main contrib non-free non-free-firmware" \
    > /etc/apt/sources.list.d/backports.list
fi

# Add custom repo for cloudflared.
curl -fsSL https://pkg.cloudflare.com/cloudflare-main.gpg | gpg --dearmor -o /usr/share/keyrings/cloudflare-main.gpg
echo "deb [signed-by=/usr/share/keyrings/cloudflare-main.gpg] https://pkg.cloudflare.com/cloudflared any main" > /etc/apt/sources.list.d/cloudflared.list

apt-get update
apt-get install -y \
  cloudflared \
  curl \
  direnv \
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
  npm \
  openssh-client \
  openssl \
  pkg-config \
  plocate \
  ripgrep \
  rustup \
  sed \
  sshpass \
  tmux \
  which \
  zoxide
