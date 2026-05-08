#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: minimal
# Install apt packages and configure their sources.
set -euo pipefail

apt-get update
apt-get install -y \
  curl \
  direnv \
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
  make \
  man \
  openssh-client \
  pkg-config \
  plocate \
  ripgrep \
  rustup \
  sed \
  sshpass \
  tmux \
  unzip \
  which \
  zoxide
