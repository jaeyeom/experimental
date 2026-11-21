---
name: review-and-push-loop
description: CLI that reviews local commits with Codex and pushes them one at a time.
---

# review-and-push-loop

`review-and-push-loop` fetches `origin`, finds the oldest unpushed local commit ahead of the base branch, runs a read-only Codex review pass (`PUSH` or `BLOCKED`), validates the push target, and pushes each commit sequentially.

Common flags:
- `--repo <dir>` — repository directory (default `.`).
- `--base <branch>` — override base branch detection.
- `--max-iterations <n>` — maximum loop iterations (default `100`).
- `--output human|compact` — output mode.
