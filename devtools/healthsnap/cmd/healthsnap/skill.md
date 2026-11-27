---
name: healthsnap
description: CLI that runs config-driven health checks, saves snapshots, and alerts only when rules fire or clear.
---

# healthsnap

`healthsnap` runs configured checks (external commands, `file-age`, or built-in `host` load/memory/disk checks), writes one JSON (and optional Markdown) snapshot per run, and notifies only on rule state transitions (`fire`, `clear`, or `repeat`), routing `judgment = true` rules to a separate recipient.

Common commands:
- `healthsnap run [--config P] [--json] [--markdown] [--force] [--dry-run]` — run checks, save snapshot, and emit change alerts.
- `healthsnap status [--config P] [--json] [--markdown]` — inspect the latest snapshot.
- `healthsnap doctor [--offline] [--json]` — verify config, state directory, check commands, and notifier setup.
