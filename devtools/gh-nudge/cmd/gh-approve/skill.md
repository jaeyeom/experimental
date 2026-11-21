---
name: gh-approve
description: CLI that approves or hands off matching GitHub pull requests based on gh-nudge rules.
---

# gh-approve

`gh-approve` evaluates pull requests requesting the logged-in `gh` user as an individual reviewer and approves or hands them off according to `~/.config/gh-nudge/config.{pkl,yaml}`.

Common commands:
- `gh-approve` — dry run.
- `gh-approve --apply` — approve or hand off matching PRs.
- `gh-approve --pr owner/repo#N --explain` — evaluate one PR.
- `gh-approve history` — list recent decisions.
