#!/bin/sh

# Script ensure.sh runs the provided playbooks with the provided arguments.

CACHE_DIR="$HOME/.cache/last_upgrade"
mkdir -p $CACHE_DIR

PKG_CACHE="$CACHE_DIR/termux-pkg-upgrade"
BREW_CACHE="$CACHE_DIR/brew-update"
NALA_CACHE="$CACHE_DIR/nala-upgrade"
PIP_CACHE="$CACHE_DIR/pip"
ANSIBLE_GALAXY_CACHE="$CACHE_DIR/ansible-galaxy-collection"

# Detect OS
OS="$(uname -s)"

if [ -n "$TERMUX_VERSION" ]; then
    # Ansible remote temp directory is messed up on Termux like
    # /data/.ansible/tmp, so we need to set it to a writable directory.
    export ANSIBLE_REMOTE_TEMP=$HOME/.ansible/tmp

    # Upgrading pkg is necessary to avoid issues on Termux. Instead of handling
    # that in Ansible, we do it here.
    # Only upgrade if not done in the last 24 hours
    if [ ! -f $PKG_CACHE ] || [ $(find $PKG_CACHE -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
        pkg upgrade -y
        pkg install -y rust python-pip
        touch $PKG_CACHE
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
    if [ ! -f $PIP_CACHE ] || [ $(find $PIP_CACHE -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
        pip install -U ansible
    fi
elif [ "$OS" = "Darwin" ]; then
    # Check if Homebrew is installed
    if ! command -v brew >/dev/null 2>&1; then
        echo "Installing Homebrew..."
        echo "Fetching recent changes to Homebrew install script..."

        # Show recent commits to the install script
        echo ""
        echo "Recent commits to install.sh:"
        curl -s "https://api.github.com/repos/Homebrew/install/commits?path=install.sh&per_page=10" | \
            jq -r '.[] | "\(.commit.author.date | split("T")[0]) [\(.sha[0:7])] \(.commit.message | split("\n")[0])"' 2>/dev/null || \
            echo "(Unable to fetch commit history)"

        # Download the install script
        BREW_INSTALL_SCRIPT=$(mktemp)
        trap "rm -f $BREW_INSTALL_SCRIPT" EXIT

        echo ""
        echo "Downloading Homebrew install script..."
        if ! curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh -o "$BREW_INSTALL_SCRIPT"; then
            echo "Failed to download Homebrew installation script"
            exit 1
        fi

        echo ""
        echo "SECURITY: Please review the installation script before proceeding:"
        echo "  Downloaded to: $BREW_INSTALL_SCRIPT"
        echo "  View online: https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
        echo "  Commit history: https://github.com/Homebrew/install/commits/HEAD/install.sh"
        echo ""
        echo "You can review the script with: less $BREW_INSTALL_SCRIPT"
        echo ""
        read -p "Proceed with Homebrew installation? (y/N): " -n 1 -r
        echo

        if [[ $REPLY =~ ^[Yy]$ ]]; then
            /bin/bash "$BREW_INSTALL_SCRIPT"
        else
            echo "Homebrew installation cancelled. Ansible may fail without Homebrew."
            exit 1
        fi

        # Add Homebrew to PATH based on chip architecture
        if [ "$(uname -m)" = "arm64" ]; then
            # For Apple Silicon Macs
            echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> $HOME/.zprofile
            eval "$(/opt/homebrew/bin/brew shellenv)"
        else
            # For Intel Macs
            echo 'eval "$(/usr/local/bin/brew shellenv)"' >> $HOME/.zprofile
            eval "$(/usr/local/bin/brew shellenv)"
        fi
    fi

    # Update Homebrew
    # Only update if not done in the last 24 hours
    if [ ! -f $BREW_CACHE ] || [ $(find $BREW_CACHE -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
        echo "Updating Homebrew..."
        brew update && brew upgrade
        touch $BREW_CACHE
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
        if [ ! -f $NALA_CACHE ] || [ $(find $NALA_CACHE -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
            echo "Upgrading packages with nala..."
            sudo nala upgrade -y
            touch $NALA_CACHE
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

# Install community.general collection if not already installed
if [ ! -f $ANSIBLE_GALAXY_CACHE ] || [ $(find $ANSIBLE_GALAXY_CACHE -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
    ansible-galaxy collection install community.general
    touch $ANSIBLE_GALAXY_CACHE
fi

# Take all flags that starts with a hyphen.
flags=$(echo " " "$@" | grep -o -- ' -[^ ]*')

# Run playbook with the provided args with the .yml suffix for each arg.

for playbook in "$@"; do
    if [ "$(echo $playbook | head -c 1)" = "-" ]; then
        continue
    fi
    # Add .yml suffix only if it doesn't already exist
    case "$playbook" in
        *.yml) ansible-playbook -i inventory.ini $flags "$playbook" ;;
        *) ansible-playbook -i inventory.ini $flags "$playbook.yml" ;;
    esac
done
