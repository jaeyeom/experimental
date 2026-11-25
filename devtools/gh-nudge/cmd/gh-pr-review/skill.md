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
- `gh-pr-review submit` — submit review to GitHub.
- `gh-pr-review rerequest [<owner>/<repo>] <pr> [--apply] [--json] [--include-bots] [--config <path>]` — re-request reviewers once the author has answered them. Skips GitHub `Bot` reviewers and logins matching `rerequest.exclude_reviewers` (for example `*-bot` or `agent-*`) in `~/.config/gh-nudge/config.pkl` (missing config is ignored; `--include-bots` disables both skips).
