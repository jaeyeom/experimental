#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: minimal
# Install apt packages and configure their sources.
set -euo pipefail

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
  mlocate \
  openssh-client \
  pkg-config \
  sed \
  sshpass \
  which \
  zoxide
