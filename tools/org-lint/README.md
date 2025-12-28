# Org-Lint Bazel Tests

This directory contains the infrastructure for running org-lint tests via Bazel.

## Overview

Org-lint tests use a **Starlark macro** approach for maintainability. Instead of auto-generating BUILD.bazel files, developers manually add org files to the `org_lint_tests()` macro, and a check script validates that all .org files are included.

This approach:
- ✅ Prevents conflicts with other BUILD file generators (like Gazelle)
- ✅ Gives developers explicit control over BUILD files
- ✅ Validates completeness automatically (like `check-bazel-src-files.sh`)
- ✅ Uses Bazel-native Starlark macros

## Files

- `defs.bzl` - Starlark macro `org_lint_tests()` that generates test targets
- `test_org_lint.sh` - Shell wrapper script that runs org-lint
- `check-org-lint-tests.sh` - Validation script that checks all .org files are included
- `BUILD.bazel` - Exports the test script and macro

## Usage

### Running org-lint tests

```bash
# Run all org-lint tests
bazel test --test_tag_filters=org-lint //...

# Run a specific test
bazel test //:README_org_lint_test

# Run tests in a specific directory
bazel test //devtools/...:all --test_tag_filters=org-lint
```

### Adding a new .org file

When you create a new .org file, add it to the BUILD.bazel in the same directory:

```python
load("//tools/org-lint:defs.bzl", "org_lint_tests")

org_lint_tests([
    "README.org",
    "new-file.org",  # Add your new file here
])
```

### Validating coverage

Run the check script to verify all .org files have tests:

```bash
make check-org-lint-tests
# or
./tools/org-lint/check-org-lint-tests.sh
```

This is run automatically as part of `make all`.

## How it works

### The Starlark macro (defs.bzl)

The `org_lint_tests()` macro generates `sh_test` targets:

```python
# Input: org_lint_tests(["README.org", "docs.org"])

# Generates:
sh_test(
    name = "README_org_lint_test",
    srcs = ["//tools/org-lint:test_org_lint.sh"],
    args = ["README.org"],
    data = ["README.org"],
    tags = ["org-lint", "local"],
)
# ... and similar for docs.org
```

### The test wrapper (test_org_lint.sh)

1. Checks if `org-lint` is available
2. Uses `--skip-if-no-emacs` to gracefully skip in CI without emacs
3. Runs org-lint on the specified file
4. Reports results in Bazel test format

### The validation check (check-org-lint-tests.sh)

Similar to `check-bazel-src-files.sh`:
1. Finds all .org files in the repository
2. Checks each has a corresponding entry in BUILD.bazel
3. Verifies the BUILD file loads the macro
4. Exits with error if any .org files are missing

## CI Integration

These tests run automatically in CI via Bazel. The tests gracefully skip with exit code 0 when emacs is not installed (using `--skip-if-no-emacs`), allowing CI to pass without requiring emacs installation.

## Why this approach?

We considered auto-generating BUILD files (like ansible syntax tests do), but chose the validation approach because:

1. **No conflicts**: Won't interfere with Gazelle or other BUILD generators
2. **Explicit control**: Developers manually manage BUILD files
3. **Bazel-native**: Uses Starlark macros, the idiomatic Bazel way
4. **Validation**: `check-org-lint-tests.sh` ensures nothing is forgotten
5. **Simple**: Just add filenames to a list, no complex generation logic

This follows the same pattern as `check-bazel-src-files.sh` which validates Go files are in BUILD files without auto-generating them.
