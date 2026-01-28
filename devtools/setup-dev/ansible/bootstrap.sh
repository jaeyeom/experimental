#!/bin/sh
# Bootstrap script for setting up development environment.
# Usage: curl -fsSL https://raw.githubusercontent.com/jaeyeom/experimental/main/devtools/setup-dev/ansible/bootstrap.sh | sh
#
# This script:
# 1. Ensures git is installed
# 2. Clones the repository (if needed)
# 3. Runs ensure.sh to set up the development environment

set -e

# Clone location - must match paths expected by ansible playbooks
REPO_DIR="$HOME/go/src/github.com/jaeyeom/experimental"
REPO_URL="https://github.com/jaeyeom/experimental.git"
REPO_SSH_URL="git@github.com:jaeyeom/experimental.git"

# Detect OS
OS="$(uname -s)"

echo "==> Bootstrap: Setting up development environment"
echo "    OS: $OS"
echo "    Target directory: $REPO_DIR"

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to install git based on platform
install_git() {
    echo "==> Installing git..."
    if [ -n "$TERMUX_VERSION" ]; then
        pkg install -y git
    elif [ "$OS" = "Darwin" ]; then
        # On macOS, git comes with Xcode Command Line Tools
        echo "    Installing Xcode Command Line Tools (includes git)..."
        xcode-select --install 2>/dev/null || true
        # Wait for installation to complete
        echo "    Please complete the Xcode Command Line Tools installation if prompted,"
        echo "    then run this script again."
        exit 1
    elif command_exists apt; then
        sudo apt update
        sudo apt install -y git
    elif command_exists yum; then
        sudo yum install -y git
    elif command_exists dnf; then
        sudo dnf install -y git
    elif command_exists pacman; then
        sudo pacman -S --noconfirm git
    else
        echo "Error: Unable to install git - unsupported package manager"
        echo "Please install git manually and run this script again."
        exit 1
    fi
}

# Ensure git is installed
if ! command_exists git; then
    install_git
fi

echo "==> git is available: $(git --version)"

# Check for SSH key or SSHPASS (required by ensure.sh)
if ! ssh-add -l >/dev/null 2>&1; then
    if [ -z "$SSHPASS" ]; then
        echo ""
        echo "Warning: No SSH keys are added to the agent."
        echo "The ensure.sh script requires SSH authentication."
        echo ""
        echo "Options:"
        echo "  1. Add your SSH key: ssh-add ~/.ssh/id_ed25519"
        echo "  2. Set SSHPASS environment variable"
        echo "  3. Generate a new SSH key: ssh-keygen -t ed25519"
        echo ""
        echo "Continuing with HTTPS clone (you may need to set up SSH before running ensure.sh)..."
        USE_HTTPS=1
    fi
fi

# Clone or update the repository
if [ -d "$REPO_DIR/.git" ]; then
    echo "==> Repository already exists at $REPO_DIR"
    echo "    Pulling latest changes..."
    cd "$REPO_DIR"
    git pull --ff-only || echo "    Warning: Could not pull (you may have local changes)"
else
    echo "==> Cloning repository..."
    mkdir -p "$(dirname "$REPO_DIR")"

    if [ -n "$USE_HTTPS" ]; then
        git clone "$REPO_URL" "$REPO_DIR"
    else
        # Try SSH first, fall back to HTTPS
        if git clone "$REPO_SSH_URL" "$REPO_DIR" 2>/dev/null; then
            echo "    Cloned via SSH"
        else
            echo "    SSH clone failed, trying HTTPS..."
            git clone "$REPO_URL" "$REPO_DIR"
        fi
    fi
fi

# Change to ansible directory
cd "$REPO_DIR/devtools/setup-dev/ansible"

echo ""
echo "==> Repository ready at: $REPO_DIR"
echo ""
echo "Next steps:"
echo "  1. Ensure SSH keys are set up: ssh-add -l"
echo "  2. Run the setup: cd $REPO_DIR/devtools/setup-dev/ansible && ./ensure.sh all"
echo ""

# If SSH is ready, offer to run ensure.sh
if ssh-add -l >/dev/null 2>&1 || [ -n "$SSHPASS" ]; then
    echo "SSH authentication is available."
    echo ""

    # Check if running interactively (stdin is a terminal)
    if [ -t 0 ]; then
        printf "Run ensure.sh now? (y/N): "
        read -r REPLY
        if [ "$REPLY" = "y" ] || [ "$REPLY" = "Y" ]; then
            echo ""
            echo "==> Running ensure.sh all..."
            exec ./ensure.sh all
        fi
    else
        echo "Non-interactive mode detected (piped from curl)."
        echo "To run the full setup, execute:"
        echo "  cd $REPO_DIR/devtools/setup-dev/ansible && ./ensure.sh all"
    fi
else
    echo "Please set up SSH authentication before running ensure.sh."
fi

echo ""
echo "Bootstrap complete!"
