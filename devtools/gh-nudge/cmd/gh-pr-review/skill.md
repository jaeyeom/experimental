---
name: gh-pr-review
description: CLI for capturing, adjusting, and submitting GitHub pull request line comments.
---

# gh-pr-review

`gh-pr-review` manages local line-level PR review comments, adjusts line numbers across rebases/diffs, and submits reviews to GitHub.

Common commands:
- `gh-pr-review capture` — capture diff hunks.
- `gh-pr-review comment <file> <line> "<comment>"` — add a line comment.
- `gh-pr-review list` — list stored comments.
- `gh-pr-review submit` — submit review directly to GitHub.
- `gh-pr-review submit --via-outbox --owner AGENT [--body TEXT] [--event COMMENT|APPROVE|REQUEST_CHANGES] [--file F] [--ref REF]` — use `--via-outbox` when posting needs human approval. Hands stored comments to `gh-outbox submit` (with no `--event`, `--kind thread`; with `--event`, the matching review kind with line comments), prints the draft ID, and posts nothing itself. Local comments stay in place marked with the draft ID until `gh-outbox` reports them posted, when `--after` applies; lint refusals fail without marking comments. Can be enabled by default in `~/.config/gh-nudge/config.pkl` under `review_post { via_outbox = true; owner = "..."; outbox_command { "gh-outbox" } }`.
- `gh-pr-review rerequest [<owner>/<repo>] <pr> [--apply] [--json] [--include-bots] [--config <path>]` — re-request reviewers once the author has answered them. Skips GitHub `Bot` reviewers and logins matching `rerequest.exclude_reviewers` (for example `*-bot` or `agent-*`) in `~/.config/gh-nudge/config.pkl` (missing config is ignored; `--include-bots` disables both skips).
