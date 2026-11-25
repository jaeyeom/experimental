---
name: secretwatch
description: CLI that watches systemd journals for leaked secrets and alerts without exposing them.
---

# secretwatch

`secretwatch` scans the systemd user and system journals for secret-shaped strings and alerts with masked 4-character prefixes without ever printing or storing secret values.

Common commands:
- `secretwatch check [--since TIME] [--follow] [--json] [--config PATH]` — scan new entries and alert once per finding.
- `secretwatch scan --since TIME [--until TIME] [--unit UNIT] [--json]` — one-shot incident scan without changing state.
