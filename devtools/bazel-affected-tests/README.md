# Bazel Affected Tests

> **DEPRECATED**: This package has moved to
> [github.com/jaeyeom/bazel-affected-tests](https://github.com/jaeyeom/bazel-affected-tests).
> Please use the new module for all future installations and updates:
>
> ```bash
> go install github.com/jaeyeom/bazel-affected-tests/cmd/bazel-affected-tests@latest
> ```
>
> This copy will no longer receive updates.

A fast Go implementation of the Bazel affected tests detection tool. This tool
identifies which Bazel test targets might be affected by changes in your git
staging area.

## Features

- **Fast**: 10-100x faster than shell implementation with caching
- **Smart Caching**: Caches results based on BUILD file content hashes
- **Cross-platform**: Works on Linux, macOS, and Windows
- **Format Test Filtering**: Only runs format tests for file types that changed
- **Config File Support**: Add custom targets based on file patterns
- **Debug Mode**: Detailed output for troubleshooting

## Installation

```bash
go install github.com/jaeyeom/bazel-affected-tests/cmd/bazel-affected-tests@latest
```

## Usage

### Basic Usage

```bash
# Run the tool (outputs affected test targets)
bazel-affected-tests

# Or pipe to xargs to run the tests
bazel-affected-tests | xargs bazel test
```

### Command Line Options

- `--debug` or env `DEBUG=1`: Enable debug output
- `--cache-dir`: Custom cache directory (default: `$HOME/.cache/bazel-affected-tests`)
- `--clear-cache`: Clear the cache and exit
- `--no-cache`: Disable caching for this run

### Examples

```bash
# Run with debug output
bazel-affected-tests --debug

# Clear cache
bazel-affected-tests --clear-cache

# Run without cache
bazel-affected-tests --no-cache

# Use in a pre-commit hook to run affected tests
bazel-affected-tests | xargs bazel test
```

### Integration with Pre-commit Hooks

Add to your pre-commit configuration:

```yaml
- id: bazel-affected-tests
  name: Run affected Bazel tests
  entry: bazel-affected-tests
  language: system
```

Make sure to run `go install` first to ensure the binary is in your PATH.

## How It Works

1. **File Detection**: Gets staged files from git (Added, Copied, Modified - not Deleted)
2. **Package Finding**: Finds the nearest Bazel package (directory with BUILD file) for each file
3. **Test Discovery**: Uses `bazel query` to find:
   - Test targets within the same package
   - External test targets that depend on the package
   - Format test targets (filtered by file type)
4. **Caching**: Results are cached based on BUILD and `.bzl` file content hashes
5. **Output**: Prints affected test targets, one per line

**Granularity**: This tool operates at **package-level granularity**, not file-level. A Bazel package is a directory containing a BUILD file. When any file in a package is modified, all tests that depend on that package are considered affected.

## Performance

- **First Run**: Similar to shell script (builds cache)
- **Subsequent Runs**: 10-100x faster (uses cache)
- **Cache Invalidation**: Automatic when BUILD or `.bzl` files change

## Implementation Details

### Package Structure

- `cmd/bazel-affected-tests/`: Main CLI application
- `internal/cache/`: Cache management with BUILD and `.bzl` file hashing
- `internal/config/`: Configuration file loading and pattern matching
- `internal/git/`: Git operations for staged files
- `internal/query/`: Bazel query execution and package finding

### Cache Management

The cache is stored in `$HOME/.cache/bazel-affected-tests/` by default. You can
customize this location using the `--cache-dir` flag.

The cache key is a SHA-256 hash of BUILD and `.bzl` files in the repository. This ensures proper cache invalidation when:
- BUILD files change (affecting which targets exist and their dependencies)
- `.bzl` files change (affecting macros/rules that generate targets)

**What's NOT included:** WORKSPACE and MODULE files are intentionally excluded because they define external dependencies and don't affect the internal dependency graph between your packages.

Cache structure:
```
~/.cache/bazel-affected-tests/
└── <sha256-hash>/          # Hash of all BUILD and .bzl files
    ├── root.json           # Cache for root package (//)
    ├── src.json            # Cache for //src package
    └── src__lib.json       # Cache for //src/lib package
```

### Format Test Filtering

Format tests are only included if corresponding file types are staged:
- C++ files (`.cpp`, `.cc`, `.h`, etc.) → C++ format test
- Go files (`.go`) → Go format test
- Python files (`.py`) → Python format test
- And so on for all supported languages

## Design

This tool uses **package-level granularity** to identify affected tests. A Bazel package is a directory containing a BUILD file. When any file in a package is modified, the tool finds all tests affected by changes to that entire package.

It queries for:
1. Test targets within the same package as modified files (using `kind('.*_test rule', //package:*)`)
2. External test targets that depend on those packages (using `rdeps(//..., //package:*)`)
3. Format tests (filtered by file type)

### Caching System

The caching system stores results **per package** using a hash of all BUILD and `.bzl` files as the cache key.

**Cache Key Computation**: SHA-256 hash of the paths and contents of all:
- BUILD and BUILD.bazel files (defines which targets exist and their dependencies)
- `.bzl` files (defines macros/rules that can generate targets and affect dependencies)

**What's excluded and why:**
- WORKSPACE and WORKSPACE.bazel files (define external dependencies, not internal dependency graph)
- MODULE and MODULE.bazel files (define external dependencies, not internal dependency graph)

Since the tool queries only within `//...` (your repository) to find which of your tests depend on your packages, external dependency configurations in WORKSPACE/MODULE files don't affect the results. Excluding them avoids unnecessary cache invalidation when updating external dependencies.

## Configuration File

You can create a `.bazel-affected-tests.yaml` file in the repository root to specify additional targets that should be included when certain files change. This is useful for targets that cannot be discovered via `bazel query`, such as external tools.

### Config File Format

```yaml
version: 1

rules:
  - patterns:
      - "**/BUILD"
      - "**/BUILD.bazel"
      - "**/*.bzl"
    targets:
      - "//tools/format:format_test_Starlark_with_buildifier"
```

### Pattern Syntax

The config file uses glob patterns to match files:

- `**` matches any number of directories (e.g., `**/BUILD` matches `foo/bar/BUILD`)
- `*` matches any characters within a single path component (e.g., `*.bzl` matches `defs.bzl`)
- Exact names match only that specific file (e.g., `WORKSPACE` matches only `WORKSPACE`, not `foo/WORKSPACE`)

### How It Works

1. When staged files are detected, each file is checked against the patterns in the config
2. If any pattern matches, the corresponding targets are added to the output
3. Config targets are added **after** filtering, so they bypass the format test filter
4. Targets are deduplicated, so the same target won't appear twice

### Use Cases

- **Buildifier**: Run buildifier checks when BUILD or .bzl files change
- **External tools**: Include external tool targets that depend on specific file types
- **Custom workflows**: Add any targets based on file patterns that can't be discovered via bazel query
