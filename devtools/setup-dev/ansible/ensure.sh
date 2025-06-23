#!/bin/sh

# Script ensure.sh runs the provided playbooks with the provided arguments.

# Detect OS
OS="$(uname -s)"

if [ -n "$TERMUX_VERSION" ]; then
    # Ansible remote temp directory is messed up on Termux like
    # /data/.ansible/tmp, so we need to set it to a writable directory.
    export ANSIBLE_REMOTE_TEMP=$HOME/.ansible/tmp

    # Upgrading pkg is necessary to avoid issues on Termux. Instead of handling
    # that in Ansible, we do it here.
    # Only upgrade if not done in the last 24 hours
    mkdir -p ~/.cache
    if [ ! -f ~/.cache/last_pkg_upgrade ] || [ $(find ~/.cache/last_pkg_upgrade -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
        pkg upgrade -y
        touch ~/.cache/last_pkg_upgrade
    fi

    # Install necessary packages for Ansible and also install ansible.
    pkg install -y rust python-pip
    pip install -U ansible
elif [ "$OS" = "Darwin" ]; then
    # Check if Homebrew is installed
    if ! command -v brew >/dev/null 2>&1; then
        echo "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

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
    mkdir -p ~/.cache
    if [ ! -f ~/.cache/last_brew_update ] || [ $(find ~/.cache/last_brew_update -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
        echo "Updating Homebrew..."
        brew update
        touch ~/.cache/last_brew_update
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

    # Install community.general collection if not already installed
    ansible-galaxy collection install community.general
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
        mkdir -p ~/.cache
        if [ ! -f ~/.cache/last_nala_upgrade ] || [ $(find ~/.cache/last_nala_upgrade -mtime +1 2>/dev/null | wc -l) -gt 0 ]; then
            echo "Upgrading packages with nala..."
            sudo nala upgrade -y
            touch ~/.cache/last_nala_upgrade
        fi
    fi

    # Install Ansible if not already installed on Linux systems
    if ! command -v ansible >/dev/null 2>&1; then
        echo "Installing Ansible..."
        if command -v apt >/dev/null 2>&1; then
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

    # Install community.general collection if not already installed
    ansible-galaxy collection install community.general
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
