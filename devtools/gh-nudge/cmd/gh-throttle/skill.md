---
name: gh-throttle
description: Drop-in wrapper for the GitHub CLI (gh) that enforces per-user rate limits and caching.
---

# gh-throttle

`gh-throttle` wraps `gh` with a per-user concurrency limiter, mutation gap, backoff, and optional read cache.

When invoked via a `gh` symlink, all arguments (including `gh version`) pass through to the real `gh`. When invoked as `gh-throttle`, it also exposes `version`, `update`, `doctor`, `skill`, and `--stats`.
