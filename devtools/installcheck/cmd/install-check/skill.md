---
name: install-check
description: Timer-run CLI that messages an agent over agentbus when watched GitHub issues land on main.
---

# install-check

`install-check` runs from a systemd user timer. It compares each configured
repo's `main` head with the last run and, only when it moved, checks the
watched issues with `gh`. Newly closed issues produce one agentbus message.
It never installs or builds anything.

Common commands:
- `install-check [run] [--dry-run] [--config path]` - one pass; `--dry-run` prints the message and changes nothing.
- `install-check status [--config path]` - last run, heads, watched open issues.
