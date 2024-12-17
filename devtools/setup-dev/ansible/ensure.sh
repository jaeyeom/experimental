#!/bin/sh

# Script ensure.sh runs the provided playbooks with the provided arguments.

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
fi

# Take all flags that starts with a hyphen.
flags=$(echo " " "$@" | grep -o -- ' -[^ ]*')

# Run playbook with the provided args with the .yml suffix for each arg.

for playbook in "$@"; do
    if [ "$(echo $playbook | head -c 1)" = "-" ]; then
        continue
    fi
    ansible-playbook -i inventory.ini $flags $playbook.yml
done
