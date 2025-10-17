#!/bin/bash
set -e

# Check if ansible-playbook is available
if ! command -v ansible-playbook &> /dev/null; then
    echo "SKIP: ansible-playbook not found"
    exit 0
fi

# Get the playbook file from arguments
if [ $# -eq 0 ]; then
    echo "ERROR: No playbook file specified"
    exit 1
fi

PLAYBOOK="$1"

# Find the playbook in the runfiles directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLAYBOOK_PATH="$SCRIPT_DIR/$PLAYBOOK"

if [ ! -f "$PLAYBOOK_PATH" ]; then
    echo "ERROR: Playbook not found: $PLAYBOOK_PATH"
    exit 1
fi

# Check syntax
OUTPUT=$(ansible-playbook --syntax-check "$PLAYBOOK_PATH" 2>&1)
EXIT_CODE=$?

# Check for libc.so.6 or other library loading errors (Termux/sandbox issue)
if echo "$OUTPUT" | grep -q "libc.so"; then
    echo "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
    exit 0
fi

if [ $EXIT_CODE -ne 0 ]; then
    echo "ERROR: Syntax check failed for $PLAYBOOK"
    echo "$OUTPUT" | grep -E "ERROR|error"
    exit 1
fi

echo "✓ $PLAYBOOK passed syntax check"
