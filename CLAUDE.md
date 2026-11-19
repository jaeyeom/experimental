@AGENTS.md

# Claude Code

Claude Code-specific notes. Shared project instructions are in `AGENTS.md`.

## Sandbox $TMPDIR
-   **`$TMPDIR` differs between sandbox and unsandbox modes.** Sandbox uses
    `/tmp/claude`, unsandbox uses `/var/folders/.../T//claude`. Files created
    in one mode are invisible via `$TMPDIR` in the other.
-   Use the project-local `.tmp/` directory (see `AGENTS.md`) when a temp
    file must survive a switch between sandbox modes.
-   **If you must use `$TMPDIR`**: capture the resolved absolute path on first
    use and pass the literal path (not `$TMPDIR`) to subsequent commands.

## Skills & Commands
-   **Go**: Use the `go-dev` plugin skill (Auto-discovered for Go tasks).
-   **Code Review**: See `.claude/skills/code-review/SKILL.md` (Auto-discovered for reviews).
-   **Slash Commands**:
    -   `/review`: Review code (references `code-review` skill).
