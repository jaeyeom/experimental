---
name: orgctl
description: CLI that checks an organization spec and diffs it against live machine state.
---

# orgctl

`orgctl` validates organization specs (`SPEC.json`) and diffs them against live machine state.

Common commands:
- `orgctl check SPEC.json` — validate spec rules.
- `orgctl plan [--only KIND]... [--json] [--cron-state-dir DIR] [--cron-stale 24h] SPEC.json` — diff spec against live state.
- `orgctl cron publish --role ROLE --file FILE [--state-dir DIR]` — publish a session's live cron list (`--file -` for stdin).
- `orgctl tree SPEC.json` — print role hierarchy.
- `orgctl diff OLD.json NEW.json` — diff two specs.
