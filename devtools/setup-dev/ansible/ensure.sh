#!/bin/sh

# Script ensure.sh runs the provided playbooks with the provided arguments.

LOG_FILE="$HOME/.cache/setup-dev/ensure_sh.log"
LOG_DIR=$(dirname "$LOG_FILE")
mkdir -p "$LOG_DIR"

# Generate a simple session ID. Prefer /dev/urandom when available, fall back to time+pid.
if [ -r /dev/urandom ]; then
    SESSION_ID=$(LC_ALL=C tr -dc 'A-Za-z0-9' </dev/urandom | head -c 16)
else
    SESSION_ID="$(date +%s)-$$"
fi

finish() {
    exit_code=$?
    finish_reason="Success"
    if [ "$exit_code" -ne 0 ]; then
        finish_reason="Failure (Exit Code: $exit_code)"
    fi

    {
        echo "---"
        echo "Session ID: $SESSION_ID"
        echo "Finish Time: $(date)"
        echo "Finish Reason: $finish_reason"
    } >>"$LOG_FILE"
}
trap 'finish' EXIT

{
    echo "---"
    echo "Session ID: $SESSION_ID"
    echo "Start Time: $(date)"
    echo "Arguments: $*"
} >>"$LOG_FILE"

CACHE_DIR="$HOME/.cache/last_upgrade"
mkdir -p "$CACHE_DIR"

PKG_CACHE="$CACHE_DIR/termux-pkg-upgrade"
BREW_CACHE="$CACHE_DIR/brew-update"
NALA_CACHE="$CACHE_DIR/nala-upgrade"
PIP_CACHE="$CACHE_DIR/pip"
RUSTUP_CACHE="$CACHE_DIR/rustup-update"
ANSIBLE_GALAXY_CACHE="$CACHE_DIR/ansible-galaxy-collection"

# Detect OS
OS="$(uname -s)"

# Pre-check for SSH key authentication
if ! ssh-add -l >/dev/null 2>&1; then
    if [ -z "$SSHPASS" ]; then
        echo "Error: No SSH keys are added to the agent, and the SSHPASS environment variable is not set." >&2
        echo "Please add your SSH keys using 'ssh-add' or set SSHPASS before running this script." >&2
        exit 1
    fi
fi

# Set GITHUB_TOKEN from gh CLI if not already set (for higher API rate limits)
if [ -z "$GITHUB_TOKEN" ] && command -v gh >/dev/null 2>&1; then
    if gh_token=$(gh auth token 2>/dev/null); then
        export GITHUB_TOKEN="$gh_token"
    fi
fi

if [ -n "$TERMUX_VERSION" ]; then
    # Ansible remote temp directory is messed up on Termux like
    # /data/.ansible/tmp, so we need to set it to a writable directory.
    export ANSIBLE_REMOTE_TEMP=$HOME/.ansible/tmp

    # Upgrading pkg is necessary to avoid issues on Termux. Instead of handling
    # that in Ansible, we do it here.
    # Only upgrade if not done in the last 24 hours
    if [ ! -f "$PKG_CACHE" ] || [ "$(find "$PKG_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        pkg upgrade -y
        pkg install -y rust python-pip
        touch "$PKG_CACHE"
    fi

    if ! command -v rustc >/dev/null 2>&1; then
        echo "Installing Rust..."
        pkg install -y rust
    fi
    if ! command -v pip >/dev/null 2>&1; then
        echo "Installing pip..."
        pkg install -y python-pip
    fi

    # Install necessary packages for Ansible and also install ansible.
    if [ ! -f "$PIP_CACHE" ] || [ "$(find "$PIP_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        pip install -U ansible
    fi
elif [ "$OS" = "Darwin" ]; then
    # Check if Homebrew is installed
    if ! command -v brew >/dev/null 2>&1; then
        echo "Installing Homebrew..."
        BREW_URL="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
        SCRIPT_DIR="$(dirname "$0")"

        # Use verified-run with interactive mode for on-the-fly approval
        if ! "$SCRIPT_DIR/verified-run" exec --interactive "$BREW_URL"; then
            echo "Homebrew installation cancelled or failed."
            exit 1
        fi

        # Add Homebrew to PATH based on chip architecture
        if [ "$(uname -m)" = "arm64" ]; then
            # For Apple Silicon Macs
            # shellcheck disable=SC2016  # Single quotes intentional - write literal string to .zprofile
            echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> "$HOME/.zprofile"
            eval "$(/opt/homebrew/bin/brew shellenv)"
        else
            # For Intel Macs
            # shellcheck disable=SC2016  # Single quotes intentional - write literal string to .zprofile
            echo 'eval "$(/usr/local/bin/brew shellenv)"' >> "$HOME/.zprofile"
            eval "$(/usr/local/bin/brew shellenv)"
        fi
    fi

    # Update Homebrew
    # Only update if not done in the last 24 hours
    if [ ! -f "$BREW_CACHE" ] || [ "$(find "$BREW_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        echo "Updating Homebrew..."
        brew update && brew upgrade
        touch "$BREW_CACHE"
    fi

    # Install Ansible if not already installed
    if ! command -v ansible >/dev/null 2>&1; then
        echo "Installing Ansible..."
        brew install ansible
    fi

    # Install python-packaging if not already installed
    if ! python -c "import packaging" >/dev/null 2>&1; then
        echo "Installing python-packaging..."
        brew install python-packaging
    fi
else
    # Check if we're on a Debian/Ubuntu system and handle nala
    if command -v apt >/dev/null 2>&1; then
        # Check if nala is installed, install if not
        if ! command -v nala >/dev/null 2>&1; then
            echo "Installing nala..."
            sudo apt update
            sudo apt install -y nala
        fi

        # Upgrade packages with nala
        # Only upgrade if not done in the last 24 hours
        if [ ! -f "$NALA_CACHE" ] || [ "$(find "$NALA_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
            echo "Upgrading packages with nala..."
            sudo nala upgrade -y
            touch "$NALA_CACHE"
        fi
    fi

    # Install Ansible if not already installed on Linux systems
    if ! command -v ansible >/dev/null 2>&1; then
        echo "Installing Ansible..."
        if command -v nala >/dev/null 2>&1; then
            sudo nala install -y ansible
        elif command -v apt >/dev/null 2>&1; then
            sudo apt update
            sudo apt install -y ansible
        elif command -v yum >/dev/null 2>&1; then
            sudo yum install -y epel-release
            sudo yum install -y ansible
        elif command -v dnf >/dev/null 2>&1; then
            sudo dnf install -y ansible
        else
            echo "Unable to install Ansible - unsupported package manager"
            exit 1
        fi
    fi
fi

# Update rustup if installed (upgrades rust toolchain and all components)
# Only update if not done in the last 24 hours
if command -v rustup >/dev/null 2>&1; then
    if [ ! -f "$RUSTUP_CACHE" ] || [ "$(find "$RUSTUP_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        echo "Updating Rust toolchain via rustup..."
        rustup update
        touch "$RUSTUP_CACHE"
    fi
fi

# Install community.general collection if not already installed
if [ ! -f "$ANSIBLE_GALAXY_CACHE" ] || [ "$(find "$ANSIBLE_GALAXY_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
    ansible-galaxy collection install community.general
    touch "$ANSIBLE_GALAXY_CACHE"
fi

# Take all flags that starts with a hyphen.
flags=$(echo " " "$@" | grep -o -- ' -[^ ]*')

# Run playbook with the provided args with the .yml suffix for each arg.

for playbook in "$@"; do
    if [ "$(echo "$playbook" | head -c 1)" = "-" ]; then
        continue
    fi
    # Add .yml suffix only if it doesn't already exist
    # shellcheck disable=SC2086  # Intentional word splitting for flags - quoted empty string causes errors
    case "$playbook" in
        *.yml) ansible-playbook -i inventory.ini $flags "$playbook" ;;
        *) ansible-playbook -i inventory.ini $flags "$playbook.yml" ;;
    esac
done
