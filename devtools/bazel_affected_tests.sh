#!/bin/bash
# Find affected Bazel test targets from staged files
#
# This script identifies which Bazel test targets might be affected by changes
# in your git staging area. It works by:
# 1. Getting all staged files (Added, Copied, Modified - not Deleted)
# 2. Finding the nearest Bazel package (directory with BUILD/BUILD.bazel) for each file
# 3. Using bazel query to find test targets that depend on that package's targets
#
# Usage: ./bazel_affected_tests.sh
# Output: List of test targets, one per line
#
# Limitations:
# - Uses package-level granularity, not file-level (may be overly broad)
# - Root package changes may still return many targets
# - Doesn't distinguish between different types of file changes
# - Test data files are treated the same as source files (which is correct)

# Get staged files that are Added, Copied, or Modified (exclude Deleted)
# We use --diff-filter=ACM to avoid processing deleted files that don't exist
staged_files=$(git diff --cached --name-only --diff-filter=ACM)

if [[ -z "$staged_files" ]]; then
    exit 0
fi

# Function to find the nearest Bazel package for a given file
# A Bazel package is a directory containing a BUILD or BUILD.bazel file
find_bazel_package() {
    local file_path="$1"
    local dir=$(dirname "$file_path")

    # Walk up the directory tree to find the nearest BUILD file
    # This is necessary because files may be in subdirectories without BUILD files
    while [[ "$dir" != "." && "$dir" != "/" ]]; do
        if [[ -f "$dir/BUILD" || -f "$dir/BUILD.bazel" ]]; then
            echo "//$dir"  # Return Bazel package label format
            return 0
        fi
        dir=$(dirname "$dir")
    done

    # Check if root directory has a BUILD file
    if [[ -f "BUILD" || -f "BUILD.bazel" ]]; then
        echo "//"  # Root package
        return 0
    fi

    # No BUILD file found - this file is not part of any Bazel package
    return 1
}

# Debug function - only outputs if DEBUG environment variable is set
debug() {
    [[ -n "$DEBUG" ]] && echo "$@" >&2
}

affected_tests=""
for file in $staged_files; do
    debug "Processing file: $file"

    # Find the Bazel package containing this file
    package=$(find_bazel_package "$file")

    if [[ -n "$package" ]]; then
        debug "  Found package: $package"

        # Query for test targets that are affected by changes to this package
        # 1. Test targets within the same package: kind('.*_test rule', $package:*)
        # 2. External test targets that depend on this package: rdeps(//..., $package:*) intersect kind('.*_test rule', //...)
        # 3. Format test targets (always include, will be filtered later based on file types)
        # We use $package:* (not $package/...) to avoid recursing into subpackages
        same_package_tests=$(bazel query "kind('.*_test rule', $package:*)" 2>/dev/null || true)
        debug "    Same package tests: $same_package_tests"
        external_test_deps=$(bazel query "rdeps(//..., $package:*) intersect kind('.*_test rule', //...)" 2>/dev/null || true)
        debug "    External test deps: $external_test_deps"
        format_test_targets=$(bazel query "//tools/format:* intersect kind('.*_test rule', //...)" 2>/dev/null || true)
        debug "    Format test targets: $format_test_targets"
        test_deps="$same_package_tests $external_test_deps $format_test_targets"

        if [[ -n "$test_deps" ]]; then
            debug "  Found test dependencies: $(echo $test_deps | wc -w) targets"
            debug "  Test deps: $test_deps"
            affected_tests="$affected_tests $test_deps"
        else
            debug "  No test dependencies found"
        fi
    else
        debug "  No Bazel package found for this file"
    fi
done

# Filter out format test targets that don't have corresponding file types in staged files
filter_format_tests() {
    local tests="$1"
    local filtered=""

    # Check if we have files of each type in staged files
    local has_cpp=$(echo "$staged_files" | grep -E '\.(cpp|cc|cxx|c\+\+|h|hpp|hxx|hh)$' || true)
    local has_c=$(echo "$staged_files" | grep -E '\.(c|h)$' || true)
    local has_go=$(echo "$staged_files" | grep -E '\.go$' || true)
    local has_jsonnet=$(echo "$staged_files" | grep -E '\.(jsonnet|libsonnet)$' || true)
    local has_proto=$(echo "$staged_files" | grep -E '\.proto$' || true)
    local has_python=$(echo "$staged_files" | grep -E '\.py$' || true)
    local has_rust=$(echo "$staged_files" | grep -E '\.rs$' || true)
    local has_starlark=$(echo "$staged_files" | grep -E '\.(bzl|BUILD|BUILD\.bazel|WORKSPACE|WORKSPACE\.bazel|MODULE|MODULE\.bazel)$' || true)

    debug "  File type detection:"
    debug "    has_cpp: $has_cpp"
    debug "    has_c: $has_c"
    debug "    has_go: $has_go"
    debug "    has_jsonnet: $has_jsonnet"
    debug "    has_proto: $has_proto"
    debug "    has_python: $has_python"
    debug "    has_rust: $has_rust"
    debug "    has_starlark: $has_starlark"

    while IFS= read -r test; do
        debug "    Processing test: $test"
        case "$test" in
            "//tools/format:format_test_C++_with_clang-format")
                if [[ -n "$has_cpp" ]]; then
                    debug "      Including C++ format test (has C++ files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding C++ format test (no C++ files)"
                fi
                ;;
            "//tools/format:format_test_C_with_clang-format")
                if [[ -n "$has_c" ]]; then
                    debug "      Including C format test (has C files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding C format test (no C files)"
                fi
                ;;
            "//tools/format:format_test_Go_with_gofmt")
                if [[ -n "$has_go" ]]; then
                    debug "      Including Go format test (has Go files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding Go format test (no Go files)"
                fi
                ;;
            "//tools/format:format_test_Jsonnet_with_jsonnetfmt")
                if [[ -n "$has_jsonnet" ]]; then
                    debug "      Including Jsonnet format test (has Jsonnet files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding Jsonnet format test (no Jsonnet files)"
                fi
                ;;
            "//tools/format:format_test_Protocol_Buffer_with_buf")
                if [[ -n "$has_proto" ]]; then
                    debug "      Including Protocol Buffer format test (has proto files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding Protocol Buffer format test (no proto files)"
                fi
                ;;
            "//tools/format:format_test_Python_with_ruff")
                if [[ -n "$has_python" ]]; then
                    debug "      Including Python format test (has Python files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding Python format test (no Python files)"
                fi
                ;;
            "//tools/format:format_test_Rust_with_rustfmt")
                if [[ -n "$has_rust" ]]; then
                    debug "      Including Rust format test (has Rust files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding Rust format test (no Rust files)"
                fi
                ;;
            "//tools/format:format_test_Starlark_with_buildifier")
                if [[ -n "$has_starlark" ]]; then
                    debug "      Including Starlark format test (has Starlark files)"
                    filtered="$filtered $test"
                else
                    debug "      Excluding Starlark format test (no Starlark files)"
                fi
                ;;
            *)
                # Keep all other tests
                debug "      Including non-format test: $test"
                filtered="$filtered $test"
                ;;
        esac
    done <<< "$tests"

    echo "$filtered"
}

# Remove duplicates and empty lines
debug "Raw affected tests: $affected_tests"
unique_tests=$(echo "$affected_tests" | tr ' ' '\n' | sort -u | grep -v '^$')
debug "Unique tests before filtering: $unique_tests"

# Filter out format tests without corresponding file types
filtered_tests=$(filter_format_tests "$unique_tests")
debug "Filtered tests: $filtered_tests"

# Print final result
echo "$filtered_tests" | tr ' ' '\n' | grep -v '^$' || true
