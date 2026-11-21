---
name: docsync
description: CLI that reports documentation implicated by a set of changed files using docsync.yml.
---

# docsync

`docsync` matches changed files against `docsync.yml` rules to report which documentation sections may be stale.

Common commands:
- `docsync check --base main` — check docs implicated since `main`.
- `docsync check --files <path>` — check docs implicated by explicit files.
- `docsync validate` — lint `docsync.yml`.
