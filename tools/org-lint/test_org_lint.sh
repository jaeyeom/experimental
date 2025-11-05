#!/bin/bash
set -e

# Check if org-lint is available
if ! command -v org-lint &> /dev/null; then
    echo "SKIP: org-lint not found"
    exit 0
fi

# Get the org file from arguments
if [ $# -eq 0 ]; then
    echo "ERROR: No org file specified"
    exit 1
fi

ORG_FILE="$1"

# Find the org file in the runfiles directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORG_FILE_PATH="$SCRIPT_DIR/$ORG_FILE"

if [ ! -f "$ORG_FILE_PATH" ]; then
    echo "ERROR: Org file not found: $ORG_FILE_PATH"
    exit 1
fi

# Run org-lint with --skip-if-no-emacs for CI compatibility
OUTPUT=$(org-lint --skip-if-no-emacs "$ORG_FILE_PATH" 2>&1)
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
    echo "ERROR: org-lint failed for $ORG_FILE"
    echo "$OUTPUT"
    exit 1
fi

# Check if there were any warnings/issues in the output
if [ -n "$OUTPUT" ]; then
    echo "ERROR: org-lint found issues in $ORG_FILE"
    echo "$OUTPUT"
    exit 1
fi

echo "✓ $ORG_FILE passed org-lint check"
