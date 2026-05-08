#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: emacs-daily
# Install apt packages and configure their sources.
set -euo pipefail

# Add custom repo for emacs.
curl -fsSL 'https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xF4E48910A020E77056748B745738AE8480447DDF&options=mr' | gpg --dearmor -o /etc/apt/keyrings/ubuntuhandbook1-emacs-ppa.gpg
echo "deb [signed-by=/etc/apt/keyrings/ubuntuhandbook1-emacs-ppa.gpg] https://ppa.launchpadcontent.net/ubuntuhandbook1/emacs/ubuntu $(lsb_release -cs) main" > /etc/apt/sources.list.d/emacs.list

apt-get update
apt-get install -y \
  cmake \
  curl \
  direnv \
  e-wrapper \
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
  libtool-bin \
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
  unzip \
  which \
  zoxide
