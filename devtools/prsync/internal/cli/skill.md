---
name: prsync
description: CLI that surveys open GitHub PRs and matches them to herdr agent tabs.
---

# prsync

`prsync` surveys open GitHub pull requests, checks CI/review/merge status, and matches them to `herdr` agent tabs.

Common commands:
- `prsync scan` — survey open PRs and tabs.
- `prsync tabs` — list herdr tabs.
- `prsync dispatch` — dispatch PR work to tabs.
- `prsync gate` — check readiness gates.
