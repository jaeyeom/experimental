#!/bin/bash

set -euo pipefail

# Script to check if all Go files are included in Bazel targets
# Usage: ./check-bazel-go-files.sh [--fix]
#
# Options:
#   --fix    Show suggested BUILD.bazel fixes for missing files

SHOW_FIXES=false
if [[ "${1:-}" == "--fix" ]]; then
    SHOW_FIXES=true
fi

TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

FILESYSTEM_GO_FILES="$TEMP_DIR/filesystem_go_files.txt"
BAZEL_GO_FILES="$TEMP_DIR/bazel_go_files.txt"

echo "🔍 Checking if all Go files are included in Bazel targets..."
echo

# Find all Go files in the filesystem (excluding certain directories and patterns)
# Note: test_*.go files are excluded as they are often temporary testing files
echo "📂 Finding all Go files in filesystem..."
find . -name "*.go" \
    -not -path "./.git/*" \
    -not -path "./bazel-*" \
    -not -path "./.bazel-*" \
    -not -path "./vendor/*" \
    -not -path "./.cache/*" \
    -not -name "test_*.go" \
    | sed 's|^\./||' \
    | sort > "$FILESYSTEM_GO_FILES"

FILESYSTEM_COUNT=$(wc -l < "$FILESYSTEM_GO_FILES")
echo "Found $FILESYSTEM_COUNT Go files in filesystem"

# Extract Go files from Bazel targets (remove duplicates)
echo "🏗️  Extracting Go files from Bazel targets..."
bazel query "kind(go_.*, //...)" --output=streamed_jsonproto 2>/dev/null | \
    jq -r 'select(.type == "RULE") | select(.rule.ruleClass | startswith("go_")) | (.rule.attribute[] | select(.name == "srcs" and has("stringListValue")) | .stringListValue[]) | select(endswith(".go"))' | \
    sed 's|^//||' | \
    sed 's|:|/|' | \
    sort -u > "$BAZEL_GO_FILES"

BAZEL_COUNT=$(wc -l < "$BAZEL_GO_FILES")
echo "Found $BAZEL_COUNT Go files in Bazel targets"
echo

# Compare the files
MISSING_FROM_BAZEL="$TEMP_DIR/missing_from_bazel.txt"
EXTRA_IN_BAZEL="$TEMP_DIR/extra_in_bazel.txt"

comm -23 "$FILESYSTEM_GO_FILES" "$BAZEL_GO_FILES" > "$MISSING_FROM_BAZEL"
comm -13 "$FILESYSTEM_GO_FILES" "$BAZEL_GO_FILES" > "$EXTRA_IN_BAZEL"

MISSING_COUNT=$(wc -l < "$MISSING_FROM_BAZEL")
EXTRA_COUNT=$(wc -l < "$EXTRA_IN_BAZEL")

# Report results
if [[ $MISSING_COUNT -eq 0 && $EXTRA_COUNT -eq 0 ]]; then
    echo "✅ All Go files are properly included in Bazel targets!"
    exit 0
fi

echo "❌ Found differences between filesystem and Bazel targets:"
echo

if [[ $MISSING_COUNT -gt 0 ]]; then
    echo "🚨 Go files NOT included in Bazel targets ($MISSING_COUNT files):"
    while IFS= read -r file; do
        echo "  - $file"
    done < "$MISSING_FROM_BAZEL"
    echo

    if [[ "$SHOW_FIXES" == true ]]; then
        echo "🔧 Suggested fixes:"
        while IFS= read -r file; do
            dir=$(dirname "$file")
            build_file="$dir/BUILD.bazel"
            if [[ -f "$build_file" ]]; then
                echo "  Add '$file' to srcs in $build_file"
            else
                echo "  Create $build_file and add go_library/go_binary rule for '$file'"
            fi
        done < "$MISSING_FROM_BAZEL"
        echo
    fi
fi

if [[ $EXTRA_COUNT -gt 0 ]]; then
    echo "⚠️  Go files in Bazel targets but NOT found in filesystem ($EXTRA_COUNT files):"
    echo "   (These might be generated files or moved/deleted files)"
    while IFS= read -r file; do
        echo "  - $file"
    done < "$EXTRA_IN_BAZEL"
    echo
fi

# Summary
echo "📊 Summary:"
echo "  Filesystem Go files: $FILESYSTEM_COUNT"
echo "  Bazel target Go files: $BAZEL_COUNT"
echo "  Missing from Bazel: $MISSING_COUNT"
echo "  Extra in Bazel: $EXTRA_COUNT"

if [[ $MISSING_COUNT -gt 0 ]]; then
    exit 1
fi