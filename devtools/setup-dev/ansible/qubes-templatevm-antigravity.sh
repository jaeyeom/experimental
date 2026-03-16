#!/bin/bash
# Auto-generated TemplateVM bootstrap for profile: antigravity
# Install apt packages and configure their sources.
set -euo pipefail

# Add Debian backports (no-op on Ubuntu).
CODENAME=$(lsb_release -cs)
if grep -q '^ID=debian' /etc/os-release 2>/dev/null; then
  echo "deb http://deb.debian.org/debian ${CODENAME}-backports main contrib non-free non-free-firmware" \
    > /etc/apt/sources.list.d/backports.list
fi

# Add custom repo for antigravity.
base64 -d << 'GPGKEY' | gpg --dearmor -o /etc/apt/keyrings/antigravity-repo-key.gpg
LS0tLS1CRUdJTiBQR1AgUFVCTElDIEtFWSBCTE9DSy0tLS0tCgp4c0JOQkdDUnQ3TUJDQURrWUpISFFRb0w2dEtyVy9MYm1mUjlsano3aWIyYVdubzRKTzNWS1F2THdqeVVNUHBxCi9TWFhNT254OGpYd2dXaXpwUHhRWURSSjBTUVhTOVVMSjFoWFJML09nTW5aQVl2WURlVjJqQm5Lc0FJRWRpRy8KZTFxbThQNFc5cXBXSmMraE5xN0ZPVDEzUnpHV1J4NTdTZExXU1hvMEtlWTM4cjlsdmpqT21UL2N1T2NtandsRApUOVhZZi9SU08reUovQXN5TWRBcitaYkRlUVVkOUhZSmlQZEkwNGxHYUdNMDJNakRNbngrbW9uYyt5NTR0K1orCnJ5MVd0UWR6b1F0OWRIbElQbFYxdFIreFY1REhIc2VqQ1p4dTlUV3p6U2xMNXdmQkJlRXo3Ui9PSXppdkdKcFcKUWRKemQrMlFEWFNSZzlxMlhZV1A1WlZ0U2dqVlZKak5sYjZaQUJFQkFBSE5WRUZ5ZEdsbVlXTjBJRkpsWjJsegpkSEo1SUZKbGNHOXphWFJ2Y25rZ1UybG5ibVZ5SUR4aGNuUnBabUZqZEMxeVpXZHBjM1J5ZVMxeVpYQnZjMmwwCmIzSjVMWE5wWjI1bGNrQm5iMjluYkdVdVkyOXRQc0xBamdRVEFRb0FPQlloQkRXNm9MTStuck9XOVp5b09NQzYKWE9iY1l4V2pCUUpna2JlekFoc0RCUXNKQ0FjQ0JoVUtDUWdMQWdRV0FnTUJBaDRCQWhlQUFBb0pFTUM2WE9iYwpZeFdqK2lnSUFNRmg2RHJBWU1lcTlzYloxWkc2b0FNcmluVWhlR1FiRXFlNzZuSURRTnNabmhEd1oyd1dxZ1ZDCjdEZ09NcWxoUW1PbXptN002TnptcTJkdlB3cTN4QzJPZUk5ZlF5empUNzJkZUJUekxQN1BKb2s5UEpGT01kTGYKSUxTc1VubU1zaGVRdDREVU8wallBWDJLVXVXT0lYWEphWjMxOVF5b1JOQlBZYTVxejdxWFM3d0hMT1k4OUlEcQpmSHQ2QXVkOEVSNXpoeU95aHl0Y1lNZWFHQzFnMUlLV21nZXduaEVxMDJGYW50TUpHbG1tRmkyZUEwRVBEMDJHCkMzNzQyUUdxUnhMd2pXc201L1RweXVVMjRFWUtSR0NSbTdRZFZJbzN1Z0ZTZXRLcm4wYnlPeFdHQnZ0dTRmSDgKWFd2WmtSVCt1K3l6SDFzNXlGWUJxYzJKVHJySnZSVT0KPVFudk4KLS0tLS1FTkQgUEdQIFBVQkxJQyBLRVkgQkxPQ0stLS0tLQo=
GPGKEY
echo "deb [signed-by=/etc/apt/keyrings/antigravity-repo-key.gpg] https://us-central1-apt.pkg.dev/projects/antigravity-auto-updater-dev/ antigravity-debian main" > /etc/apt/sources.list.d/antigravity.list

apt-get update
apt-get install -y \
  antigravity \
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
