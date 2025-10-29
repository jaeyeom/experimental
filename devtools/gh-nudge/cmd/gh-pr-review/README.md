# gh-pr-review

A command-line utility for managing GitHub Pull Request review comments with intelligent diff tracking and atomic comment operations.

> **📖 For comprehensive documentation, see [pr-review.org](../../pr-review.org)**

## Overview

`gh-pr-review` captures PR diff hunks, manages line-specific comments locally, and submits reviews using the GitHub API. Key features:

- **Local Comment Management** - Store and manage review comments locally before submission
- **Atomic Operations** - File locking ensures thread-safe operations for parallel workflows
- **Auto-Detection** - Automatically detects repo and PR/branch from current directory
- **Line Number Auto-Adjustment** - Automatically update comment line numbers after code changes
- **Branch Support** - Review local git branches without creating PRs
- **Parallel Access** - Multiple LLM instances can review code simultaneously

## Installation

### Prerequisites

- GitHub CLI (`gh`) installed and authenticated
- Go 1.21 or later

### Install from Source

```bash
go install github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-pr-review@latest
```

## Quick Start

```bash
# Basic workflow with auto-detection
gh-pr-review capture                    # Auto-detect repo and PR
gh-pr-review comment src/main.js 15 "Use const instead of let"
gh-pr-review list
gh-pr-review submit --event APPROVE

# Explicit repo and PR
gh-pr-review capture octocat/Hello-World 42
gh-pr-review comment octocat/Hello-World 42 src/main.js 15 "Use const"
gh-pr-review submit octocat/Hello-World 42 --body "LGTM" --event APPROVE

# Branch workflow
gh-pr-review capture feature/auth-fix
gh-pr-review comment feature/auth-fix src/auth.js 45 "Add validation"
gh-pr-review list feature/auth-fix
```

## Core Commands

For detailed command documentation with all options and advanced usage, see the [Command Reference](../../pr-review.org#command-reference) in pr-review.org.

### Quick Command Reference

| Command       | Description                                 |
|---------------|---------------------------------------------|
| `capture`     | Capture PR diff hunks or branch diffs       |
| `comment`     | Add line-specific comments                  |
| `list`        | Display stored comments                     |
| `submit`      | Submit review to GitHub (PR only)           |
| `next`        | Get next unresolved comment                 |
| `resolve`     | Mark comment as resolved                    |
| `auto-adjust` | Auto-adjust line numbers after code changes |
| `pull`        | Fetch comments from GitHub to local storage |
| `delete`      | Remove specific comment                     |
| `clear`       | Remove all comments                         |

### Key Command Examples

```bash
# Capture and comment
gh-pr-review capture                              # Auto-detect repo/PR
gh-pr-review comment src/main.js 15 "Use const"   # Single line
gh-pr-review comment src/main.js 15-20 "Refactor" # Line range

# Review and submit
gh-pr-review list                                 # View all comments
gh-pr-review submit --event APPROVE               # Submit with approval

# Iterative workflow
gh-pr-review next                                 # Get next comment
gh-pr-review resolve --comment-id abc123          # Mark resolved
gh-pr-review auto-adjust --staged                 # Adjust line numbers

# Pull existing comments
gh-pr-review pull                                 # Merge with local comments
```

See [pr-review.org](../../pr-review.org) for complete command documentation with all options and examples.

## Common Workflows

### Basic Review Workflow

```bash
gh-pr-review capture                          # Capture diff hunks
gh-pr-review comment src/auth.js 45 "Fix"     # Add comments
gh-pr-review list                             # Review comments
gh-pr-review submit --event APPROVE           # Submit review
```

### Iterative Resolution Workflow

```bash
gh-pr-review next                             # Get next unresolved comment
# Fix the issue in your editor
gh-pr-review auto-adjust --staged             # Adjust line numbers
gh-pr-review resolve --comment-id a1b2c3d4    # Mark resolved
gh-pr-review next                             # Continue to next
```

### Local Branch Review

```bash
gh-pr-review capture feature/new-api                 # Capture branch diff
gh-pr-review comment feature/new-api src/api.js 25 "TODO: Add error handling"
gh-pr-review list feature/new-api                    # Review comments
```

For more workflow examples including parallel LLM review, automated scripts, and AI agent usage, see the [Workflow Examples](../../pr-review.org#workflow-examples) section in pr-review.org.

## How It Works

Comments and diff hunks are stored locally under `~/.config/gh-nudge/storage/` with:

- **File Locking** - Atomic operations using shared/exclusive locks for thread-safe parallel access
- **Diff Validation** - Comments validated against captured diff hunks (file, line, side, SHA)
- **Comment Lifecycle** - `local → submitted → resolved/archived`

The tool uses `gh` CLI for GitHub API operations and supports concurrent access from multiple LLM instances with configurable retry behavior.

### Environment Variables

```bash
GH_STORAGE_HOME          # Storage directory [default: ~/.config/gh-nudge/storage]
GH_STORAGE_LOCK_TIMEOUT  # Lock timeout in seconds [default: 30]
GH_STORAGE_DEBUG         # Enable debug logging [default: false]

# Parallel LLM support (retry configuration)
GH_STORAGE_LOCK_MAX_RETRIES=20        # Max retry attempts [default: 10]
GH_STORAGE_LOCK_INITIAL_DELAY=100ms   # Initial delay [default: 50ms]
GH_STORAGE_LOCK_MAX_DELAY=5s          # Max delay [default: 2s]
GH_STORAGE_LOCK_BACKOFF_FACTOR=1.2    # Backoff factor [default: 1.5]
```

For detailed architecture, storage structure, and implementation details, see the [Architecture](../../pr-review.org#architecture) and [Implementation Details](../../pr-review.org#implementation-details) sections in pr-review.org.

## Documentation

- **[pr-review.org](../../pr-review.org)** - Comprehensive documentation with full command reference, advanced workflows, and architecture details
- **[storage.org](../../storage.org)** - Storage system documentation and management operations
- **[GitHub Repository](https://github.com/jaeyeom/experimental)** - Source code and issue tracking

## Getting Help

```bash
gh-pr-review help           # Show help message
gh-pr-review <command> -h   # Command-specific help
gh-pr-review version        # Show version
```
