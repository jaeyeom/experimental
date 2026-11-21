---
name: gh-nudge
description: CLI that nudges reviewers on pending GitHub pull requests via Slack.
---

# gh-nudge

`gh-nudge` fetches pending GitHub pull requests and sends Slack reminders to requested reviewers based on `~/.config/gh-nudge/config.{pkl,yaml}`.

Common flags:
- `--config <path>` — path to configuration file.
- `--dry-run` — preview notifications without sending.
- `--verbose` — enable debug logging.
