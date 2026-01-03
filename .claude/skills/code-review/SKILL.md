---
name: code-review
description: Comprehensive code review checklist. Use when asked to review code, PRs, pull requests, or before committing changes.
allowed-tools: Bash(git diff:*), Bash(git log:*), Bash(git show:*), Bash(gh:*), Bash(make lint:*), Read, Grep, Glob
---

# Code Review Skill

## Key Focus Areas

### Code Quality
-   **DRY (Don't Repeat Yourself)**: Extract duplicated code.
-   **Error Handling**: Ensure proper error wrapping, propagation, and logging.
    -   *Go*: Do not log and bubble up (avoids duplicate logs).
-   **Resource Cleanup**: Use `defer` (Go) or RAII patterns (C++).
-   **Clear Naming**: meaningful names for functions and variables.
-   **Simplicity**: Prioritize simple solutions over complex ones.
-   **Organization**: Keep files concise (200-300 lines).

### Language-Specific Checks
-   **Go**: Goroutine safety, error handling, interface usage.
-   **Python**: PEP 8, type hints.
-   **C++**: RAII, const correctness.
-   **Protobuf**: Backward compatibility.

## Best Practices
1.  Be constructive and specific.
2.  Focus on significant issues, not nitpicks.
3.  Acknowledge good practices.
