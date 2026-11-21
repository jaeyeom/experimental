---
name: diskwatch
description: CLI that checks free disk space and runs configured alert/cleanup actions.
---

# diskwatch

`diskwatch` monitors configured mount points for low free disk space and runs configured actions when thresholds are crossed.

Common commands:
- `diskwatch check [--dry-run] [--config path]` — check mounts and run actions.
- `diskwatch status [--json] [--config path]` — show mount status.
- `diskwatch hook [--config path]` — run in hook mode.
