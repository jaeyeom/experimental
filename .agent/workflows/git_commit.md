---
description: How to commit changes with a detailed log
---

1.  **Stage Changes**: Run `git add <files>` to stage the changes you want to commit.
2.  **Commit with Detailed Message**: Run `git commit` (or `git commit -m` if you can format it correctly) with a message following this format:
    ```
    <type>(<scope>): <summary>

    <detailed description of what changed and why>

    Changes include:
    - <file>: <description of change>
    - <file>: <description of change>

    Fixes #<issue-number>
    ```
    - **Header**: Use Conventional Commits (e.g., `feat`, `fix`, `docs`).
    - **Body**: Explain the *why* and *what* in detail.
    - **Changes**: List key changes by file or component.
    - **Footer**: **ALWAYS** include "Fixes #<issue-number>" (or "Closes", "Resolves") if this commit completes a GitHub issue.
3.  **Verify**: Run `git log -1` to check the commit message.
