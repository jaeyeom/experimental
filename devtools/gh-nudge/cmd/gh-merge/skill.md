---
name: gh-merge
description: CLI for listing, merging, and managing GitHub pull requests on merge queues.
---

# gh-merge

`gh-merge` lists and merges eligible pull requests, enqueues PRs, watches merge queue progress, and records ejection RCAs.

Common commands:
- `gh-merge list [-v] [--log-level debug] [--pr <ref> ...] [--timeout <dur>] [pr ...]` — stream mergeable PR verdicts (with progress and debug timings on stderr).
- `gh-merge merge [--dry-run] [-d]` — merge eligible PRs.
- `gh-merge queue [--when-green] <pr>` — enqueue a PR.
- `gh-merge watch <pr>` — watch a PR until merged or ejected.
