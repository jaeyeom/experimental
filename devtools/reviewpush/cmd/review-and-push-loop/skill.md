---
name: review-and-push-loop
description: CLI that reviews local commits with a read-only reviewer and pushes them one at a time.
---

# review-and-push-loop

`review-and-push-loop` fetches `origin`, finds the oldest unpushed local commit ahead of the base branch, runs a read-only review pass (`PUSH` or `BLOCKED`, using Codex CLI by default or any custom reviewer via `--reviewer-cmd`), validates the verdict and push target, and pushes each commit sequentially.

Common flags:
- `--repo <dir>` — repository directory (default `.`).
- `--base <branch>` — override base branch detection.
- `--max-iterations <n>` — maximum loop iterations (default `100`).
- `--output human|compact|json` — output mode.
- `--json` — emit a single structured JSON summary on stdout (shorthand for `--output json`).
- `--reviewer-cmd <cmd>` — custom reviewer command (reads prompt on stdin, writes verdict JSON on stdout).
