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
    pkg upgrade -y

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
