---
name: gh-outbox
description: CLI that holds pull request comments and reviews until approved in decide.
---

# gh-outbox

`gh-outbox` stages pull request comments and reviews, requests approval in `decide`, and posts approved items via `gh`.

Common commands:
- `gh-outbox submit --repo owner/name --pr N --owner AGENT` — submit a draft (`--kind thread|reply|comment|request-changes|approve`, `--in-reply-to ID`, `--allow-resolved`, `--edit ID`).
- `gh-outbox ls` — list staged items.
- `gh-outbox show [--json] ID` — show one item.
- `gh-outbox poll [--watch]` — poll for decisions and post approved items.
