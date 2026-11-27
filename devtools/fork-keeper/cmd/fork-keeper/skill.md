---
name: fork-keeper
description: Standalone scheduler CLI that rebases configured personal forks on upstream and opens review pull requests.
---

# fork-keeper

`fork-keeper` runs the standalone fork-syncing scheduler:

```sh
fork-keeper [flags] [<fork-name>...]
```

For each configured fork in `forks.json` (default `~/.config/fork-keeper/forks.json`), it:
1. Fetches upstream and fork refs into a bare mirror cache.
2. Computes the review range, fork patch stack, and upstream tag deltas in Go (`ComputeRange`).
3. Runs either the configured external reviewer/agent command inside a credential-scrubbed OS sandbox (`seatbelt` or `bwrap`) or the built-in deterministic `StandaloneReviewer` (which rebases clean commits and emits `needs-review`).
4. Strictly validates `verdict.json` against the computed range and candidate head, runs deterministic diff/history tripwires, opens or updates a `sync/<date>` pull request via `gh` (or `gh_bin` / `GH_BIN` such as `gh-throttle`), and promotes the default branch atomically only when all checks pass or the PR carries a current `fork-keeper:approved` label.
