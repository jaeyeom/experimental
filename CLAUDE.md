# Claude Code Configuration

This file guides Claude Code's behavior.

## Core Principles
1.  **Simplicity**: Prioritize the simplest solution.
2.  **No Duplication**: Reuse existing functionality.
3.  **TDD**: Practice Test-Driven Development. Understand desired behavior -> Test -> Implement.

## Automation & Workflow
-   **ALWAYS run `make check`** before requesting a commit.
    -   This runs formatters, linters, tests, and generators.
    -   If `make check` changes files, include them in the commit.
-   **Use `make` targets**: `make test`, `make lint`, `make fix`, `make format`.
-   **Use `make format`** instead of individual formatting tools (e.g., `gofmt`, `goimports`).

## Skills & Commands
-   **Go**: See `.claude/skills/golang/SKILL.md` (Auto-discovered for Go tasks).
-   **Code Review**: See `.claude/skills/code-review/SKILL.md` (Auto-discovered for reviews).
-   **Slash Commands**:
    -   `/review`: Review code (references `code-review` skill).
