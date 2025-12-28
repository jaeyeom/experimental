#!/bin/bash

set -euo pipefail

# Script to check if all source files are included in Bazel targets
# Usage: ./check-bazel-src-files.sh <language> [--fix]
#
# Supported languages: go, py, java, cc, rust
#
# Options:
#   --fix    Show suggested BUILD.bazel fixes for missing files

SUPPORTED_LANGS="go, py, java, cc, rust"

usage() {
    echo "Usage: $0 <language> [--fix]"
    echo ""
    echo "Supported languages: $SUPPORTED_LANGS"
    echo ""
    echo "Options:"
    echo "  --fix    Show suggested BUILD.bazel fixes for missing files"
    exit 1
}

# Get language-specific configuration
# EXCLUDE_PATS is a space-separated list of patterns to exclude
get_config() {
    local lang="$1"
    case "$lang" in
        go)
            EXT="*.go"
            RULE_PREFIX="go_"
            EXCLUDE_PATS="test_*.go"
            DISPLAY_NAME="Go"
            ;;
        py)
            EXT="*.py"
            RULE_PREFIX="py_"
            EXCLUDE_PATS="test_*.py *_pb2.py *_pb2_grpc.py"
            DISPLAY_NAME="Python"
            ;;
        java)
            EXT="*.java"
            RULE_PREFIX="java_"
            EXCLUDE_PATS=""
            DISPLAY_NAME="Java"
            ;;
        cc)
            EXT="*.cc"
            RULE_PREFIX="cc_"
            EXCLUDE_PATS=""
            DISPLAY_NAME="C++"
            ;;
        rust)
            EXT="*.rs"
            RULE_PREFIX="rust_"
            EXCLUDE_PATS=""
            DISPLAY_NAME="Rust"
            ;;
        *)
            echo "❌ Unsupported language: $lang"
            echo "Supported languages: $SUPPORTED_LANGS"
            exit 1
            ;;
    esac
}

# Parse arguments
if [[ $# -lt 1 ]]; then
    usage
fi

LANG="$1"
shift

SHOW_FIXES=false
if [[ "${1:-}" == "--fix" ]]; then
    SHOW_FIXES=true
fi

# Get language-specific settings
get_config "$LANG"

TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

FILESYSTEM_FILES="$TEMP_DIR/filesystem_files.txt"
BAZEL_FILES="$TEMP_DIR/bazel_files.txt"

echo "🔍 Checking if all $DISPLAY_NAME files are included in Bazel targets..."
echo

# Build find command with optional exclusion patterns
echo "📂 Finding all $DISPLAY_NAME files in filesystem..."
FIND_EXCLUDES=""
for pat in $EXCLUDE_PATS; do
    FIND_EXCLUDES="$FIND_EXCLUDES -not -name $pat"
done

# shellcheck disable=SC2086
find . -name "$EXT" \
    -not -path "./.git/*" \
    -not -path "./bazel-*" \
    -not -path "./.bazel-*" \
    -not -path "./vendor/*" \
    -not -path "./.cache/*" \
    $FIND_EXCLUDES \
    | sed 's|^\./||' | sort > "$FILESYSTEM_FILES"

FILESYSTEM_COUNT=$(wc -l < "$FILESYSTEM_FILES")
echo "Found $FILESYSTEM_COUNT $DISPLAY_NAME files in filesystem"

# Extract files from Bazel targets (remove duplicates)
# Use the file extension without the leading *
FILE_EXT="${EXT#\*}"
echo "🏗️  Extracting $DISPLAY_NAME files from Bazel targets..."
bazel query "kind(${RULE_PREFIX}.*, //...)" --output=streamed_jsonproto 2>/dev/null | \
    jq -r --arg ext "$FILE_EXT" 'select(.type == "RULE") | select(.rule.ruleClass | startswith("'"$RULE_PREFIX"'")) | (.rule.attribute[] | select(.name == "srcs" and has("stringListValue")) | .stringListValue[]) | select(endswith($ext))' | \
    sed 's|^//||' | \
    sed 's|:|/|' | \
    sort -u > "$BAZEL_FILES"

BAZEL_COUNT=$(wc -l < "$BAZEL_FILES")
echo "Found $BAZEL_COUNT $DISPLAY_NAME files in Bazel targets"
echo

# Compare the files
MISSING_FROM_BAZEL="$TEMP_DIR/missing_from_bazel.txt"
EXTRA_IN_BAZEL="$TEMP_DIR/extra_in_bazel.txt"

comm -23 "$FILESYSTEM_FILES" "$BAZEL_FILES" > "$MISSING_FROM_BAZEL"
comm -13 "$FILESYSTEM_FILES" "$BAZEL_FILES" > "$EXTRA_IN_BAZEL"

MISSING_COUNT=$(wc -l < "$MISSING_FROM_BAZEL")
EXTRA_COUNT=$(wc -l < "$EXTRA_IN_BAZEL")

# Report results
if [[ $MISSING_COUNT -eq 0 && $EXTRA_COUNT -eq 0 ]]; then
    echo "✅ All $DISPLAY_NAME files are properly included in Bazel targets!"
    exit 0
fi

echo "❌ Found differences between filesystem and Bazel targets:"
echo

if [[ $MISSING_COUNT -gt 0 ]]; then
    echo "🚨 $DISPLAY_NAME files NOT included in Bazel targets ($MISSING_COUNT files):"
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
                echo "  Create $build_file and add ${RULE_PREFIX}library/${RULE_PREFIX}binary rule for '$file'"
            fi
        done < "$MISSING_FROM_BAZEL"
        echo
    fi
fi

if [[ $EXTRA_COUNT -gt 0 ]]; then
    echo "⚠️  $DISPLAY_NAME files in Bazel targets but NOT found in filesystem ($EXTRA_COUNT files):"
    echo "   (These might be generated files or moved/deleted files)"
    while IFS= read -r file; do
        echo "  - $file"
    done < "$EXTRA_IN_BAZEL"
    echo
fi

# Summary
echo "📊 Summary:"
echo "  Filesystem $DISPLAY_NAME files: $FILESYSTEM_COUNT"
echo "  Bazel target $DISPLAY_NAME files: $BAZEL_COUNT"
echo "  Missing from Bazel: $MISSING_COUNT"
echo "  Extra in Bazel: $EXTRA_COUNT"

if [[ $MISSING_COUNT -gt 0 ]]; then
    exit 1
fi
