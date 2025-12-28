#!/bin/bash
set -e

# Check that all .org files have corresponding org-lint test targets
# Similar to check-bazel-go-files.sh

echo "Checking that all .org files have org-lint test targets..."

EXIT_CODE=0

# Find all .org files
ORG_FILES=$(find . -name "*.org" -type f ! -path "./.git/*" ! -path "*/bazel-*" | sort)

for ORG_FILE in $ORG_FILES; do
    DIR=$(dirname "$ORG_FILE")
    BASENAME=$(basename "$ORG_FILE")
    BUILD_FILE="$DIR/BUILD.bazel"

    # Check if BUILD.bazel exists
    if [ ! -f "$BUILD_FILE" ]; then
        echo "ERROR: $ORG_FILE - No BUILD.bazel file found in $DIR"
        echo "  Create $BUILD_FILE with:"
        echo "  load(\"//tools/org-lint:defs.bzl\", \"org_lint_tests\")"
        echo "  org_lint_tests([\"$BASENAME\"])"
        EXIT_CODE=1
        continue
    fi

    # Check if the file is referenced in org_lint_tests macro
    if ! grep -q "\"$BASENAME\"" "$BUILD_FILE"; then
        echo "ERROR: $ORG_FILE - Not included in BUILD.bazel"
        echo "  Add to org_lint_tests([...]) in $BUILD_FILE"
        EXIT_CODE=1
        continue
    fi

    # Verify the BUILD file loads the macro
    if ! grep -q "load(\"//tools/org-lint:defs.bzl\"" "$BUILD_FILE"; then
        echo "WARNING: $BUILD_FILE - Uses org file but doesn't load org_lint_tests macro"
        echo "  Add: load(\"//tools/org-lint:defs.bzl\", \"org_lint_tests\")"
        EXIT_CODE=1
    fi
done

if [ $EXIT_CODE -eq 0 ]; then
    echo "✓ All .org files have org-lint test targets"
else
    echo ""
    echo "FAILED: Some .org files are missing org-lint test targets"
    echo ""
    echo "To fix, add to the corresponding BUILD.bazel:"
    echo "  load(\"//tools/org-lint:defs.bzl\", \"org_lint_tests\")"
    echo "  org_lint_tests([\"filename.org\"])"
fi

exit $EXIT_CODE
