---
name: fork-keeper
description: Scheduler CLI that rebases configured personal forks on upstream and opens review pull requests.
---

# fork-keeper

`fork-keeper` runs the scheduler side of fork syncing:

```sh
fork-keeper [<fork-name>...]
```

For each configured fork in `forks.json` (default `~/.config/fork-keeper/forks.json`), it fetches upstream and fork refs, computes the review range, invokes the reviewing agent in a credential-scrubbed throwaway clone, validates the verdict and deterministic tripwires, opens or updates a `sync/<date>` pull request, and promotes the default branch only when all checks pass.
