#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: emacs-daily
# Install apt packages and configure their sources.
set -euo pipefail

# Add Ubuntu PPAs (no-op on Debian).
if command -v add-apt-repository >/dev/null 2>&1; then
  add-apt-repository -y ppa:ubuntuhandbook1/emacs
fi

apt-get update
apt-get install -y \
  buf \
  clang-format \
  cmake \
  curl \
  direnv \
  emacs \
  ffmpegthumbnailer \
  fzf \
  gh \
  git \
  git-delta \
  gpg \
  gpg-agent \
  grep \
  htop \
  keychain \
  libssl-dev \
  libtool \
  libvips-tools \
  libvterm-dev \
  man \
  notmuch \
  openssh-client \
  p7zip-full \
  pandoc \
  pass \
  perl \
  pkg-config \
  plocate \
  protobuf-compiler \
  python3-notmuch2 \
  ripgrep \
  sed \
  sshpass \
  tmux \
  which \
  zoxide
