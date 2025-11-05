#!/bin/bash

# Find all .org files
ORG_FILES=$(find . -name "*.org" -type f ! -path "./.git/*" ! -path "*/bazel-*" | sort)

for ORG_FILE in $ORG_FILES; do
    DIR=$(dirname "$ORG_FILE")
    BASENAME=$(basename "$ORG_FILE")
    TEST_NAME="${BASENAME%.org}_org_lint_test"

    BUILD_FILE="$DIR/BUILD.bazel"

    # Create the test target
    TEST_TARGET="sh_test(
    name = \"$TEST_NAME\",
    srcs = [\"//tools/org-lint:test_org_lint.sh\"],
    args = [\"$BASENAME\"],
    data = [\"$BASENAME\"],
    tags = [
        \"org-lint\",
        \"local\",
    ],
)"

    # Check if BUILD.bazel exists
    if [ -f "$BUILD_FILE" ]; then
        # Check if the test already exists
        if grep -q "name = \"$TEST_NAME\"" "$BUILD_FILE"; then
            echo "SKIP: Test already exists in $BUILD_FILE"
        else
            echo "APPEND: Adding test to $BUILD_FILE"
            echo "" >> "$BUILD_FILE"
            echo "$TEST_TARGET" >> "$BUILD_FILE"
        fi
    else
        echo "CREATE: Creating $BUILD_FILE with test"
        echo "$TEST_TARGET" > "$BUILD_FILE"
    fi
done

echo "Done! Generated org-lint tests for all .org files"
