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
  make \
  man \
  mlocate \
  openssh-client \
  sed \
  sshpass \
  which \
  zoxide
