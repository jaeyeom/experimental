#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: emacs-daily
# Install apt packages and configure their sources.
set -euo pipefail

# Add Debian backports (no-op on Ubuntu).
CODENAME=$(lsb_release -cs)
if grep -q '^ID=debian' /etc/os-release 2>/dev/null; then
  echo "deb http://deb.debian.org/debian ${CODENAME}-backports main contrib non-free non-free-firmware" \
    > /etc/apt/sources.list.d/backports.list
fi

# Add Ubuntu PPAs (no-op on Debian).
if command -v add-apt-repository >/dev/null 2>&1; then
  add-apt-repository -y ppa:ubuntuhandbook1/emacs
fi

apt-get update
apt-get install -y \
  cmake \
  curl \
  direnv \
  emacs \
  ffmpegthumbnailer \
  fzf \
  gh \
  git \
  git-delta \
  golang \
  gpg \
  gpg-agent \
  grep \
  htop \
  jq \
  keychain \
  libssl-dev \
  libtool \
  libvips-tools \
  libvterm-dev \
  make \
  man \
  notmuch \
  openssh-client \
  p7zip-full \
  pandoc \
  pass \
  perl \
  pkg-config \
  plocate \
  python3-notmuch2 \
  ripgrep \
  rustup \
  sed \
  sshpass \
  tmux \
  which \
  zoxide
