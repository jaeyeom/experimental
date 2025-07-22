# Bazel Affected Tests

A fast Go implementation of the Bazel affected tests detection tool. This tool
identifies which Bazel test targets might be affected by changes in your git
staging area.

## Features

- **Fast**: 10-100x faster than shell implementation with caching
- **Smart Caching**: Caches results based on BUILD file content hashes
- **Cross-platform**: Works on Linux, macOS, and Windows
- **Format Test Filtering**: Only runs format tests for file types that changed
- **Debug Mode**: Detailed output for troubleshooting

## Installation

```bash
go install github.com/jaeyeom/experimental/devtools/bazel-affected-tests/cmd/bazel-affected-tests@latest
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

Replace the shell script in your pre-commit configuration:

```yaml
# Before
- id: bazel-affected-tests
  name: Run affected Bazel tests
  entry: devtools/bazel_affected_tests.sh

# After
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
4. **Caching**: Results are cached based on BUILD file content hashes
5. **Output**: Prints affected test targets, one per line

## Performance

- **First Run**: Similar to shell script (builds cache)
- **Subsequent Runs**: 10-100x faster (uses cache)
- **Cache Invalidation**: Automatic when BUILD files change

## Implementation Details

### Package Structure

- `cmd/bazel-affected-tests/`: Main CLI application
- `internal/cache/`: Cache management with BUILD file hashing
- `internal/git/`: Git operations for staged files
- `internal/query/`: Bazel query execution and package finding

### Cache Management

The cache is stored in `$HOME/.cache/bazel-affected-tests/` by default. You can
customize this location using the `--cache-dir` flag.

The cache key is a SHA-256 hash of all BUILD, WORKSPACE, and MODULE files in the
repository. This ensures cache invalidation when the dependency graph changes.

Cache structure:
```
~/.cache/bazel-affected-tests/
└── <sha256-hash>/          # Hash of all BUILD files
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

## Migration from Shell Script

This Go implementation is a drop-in replacement for the shell scripts with the
same behavior but better performance and maintainability.
