---
name: devcheck
description: CLI that detects repository context and runs format, lint, and test tools.
---

# devcheck

`devcheck` detects project build systems and languages, runs format/lint/test tools, and formats results for humans or AI agents.

Common usage:
- `devcheck` — run all checks in current directory.
- `devcheck --filter test` — run only tests.
- `devcheck --format json` — emit JSON report.
- `devcheck --changed-only` — run only on changed files.
