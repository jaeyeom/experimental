---
name: claude-usage
description: CLI that prints Claude Code 5-hour and 7-day usage windows.
---

# claude-usage

`claude-usage` prints Claude Code's 5-hour and 7-day quota usage windows.

Common usage:
- `claude-usage` — human-readable table.
- `claude-usage --json` — stable JSON output with unrounded utilization, `fetchedAt`, and `source`.
- `claude-usage --quotapace` — JSON lines (`window`, `percent`, `resetsAt`, `source`, `at`) for `quotapace record --profile P --stdin`.
