---
name: golang
description: Expert Go developer guide. Use when writing, debugging, or reviewing Go code.
allowed-tools: Bash, Read, Grep, Glob, Edit, Write
---

# Go Development Skill

## Instructions

When working with Go code, follow these rules:

1.  **Logging**: Use the `log/slog` package in the standard library.
    -   Add appropriate log messages with proper log levels.
2.  **Interfaces**: Define interfaces where they are *used*, not where they are implemented.
    -   Avoid introducing unused interfaces.
3.  **Testing**:
    -   Use table-driven tests.
    -   Clarify which case is failing in the test log.
4.  **Linting**:
    -   Run `make lint` to check for issues.
    -   Run `make fix` to auto-fix issues.
    -   If running manually: `golangci-lint run ./...`
5.  **Flags**: Only the `main` function can access command-line flags.

## Tools
-   **Lint**: `make lint`
-   **Test**: `make test`
