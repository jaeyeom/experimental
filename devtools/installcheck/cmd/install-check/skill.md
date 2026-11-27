---
name: install-check
description: Timer-run CLI that notifies a configured command, stdout, or agentbus target when watched GitHub issues land on main.
---

# install-check

`install-check` runs from a timer or cron job. It compares each configured
repo's `main` head with the last run and, only when it moved, checks the
watched issues with `gh` (configurable via `gh_bin` or `GH_BIN`, e.g. `gh-throttle`).
Newly closed issues produce one notification via `notify_command`, `stdout`, or
`agentbus` (`notify_name`). It never installs or builds anything.

Common commands:
- `install-check [run] [--dry-run] [--json] [--config path]` - one pass; `--dry-run` prints the notification and changes nothing.
- `install-check status [--json] [--config path]` - last run, heads, watched open issues.
- `install-check doctor [--config path] [--offline] [--json]` - validate config and external binaries (`git`, `gh`, configured notifier).
- `install-check version [--json]` - print version metadata.
