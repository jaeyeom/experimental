#!/bin/sh

# Script ensure.sh runs the provided playbooks with the provided arguments.

if [ -n "$TERMUX_VERSION" ]; then
    export ANSIBLE_REMOTE_TEMP=$HOME/.ansible/tmp
fi

# Run playbook with the provided args with the .yml suffix for each arg.

for playbook in "$@"; do
    ansible-playbook -i inventory.ini $playbook.yml
done
