---
name: gh-throttle
description: Drop-in wrapper for the GitHub CLI (gh) that enforces per-user rate limits and caching.
---

# gh-throttle

`gh-throttle` wraps `gh` with a per-user concurrency limiter, mutation gap, account-wide rate-limit cooldown, per-minute point/search/GraphQL budgets, `--paginate` page counting and timeout, and optional read cache.

During an active cooldown, preflight checks (`gh auth status`, GET `gh api rate_limit`) return quickly with exit code `75` and `GH_THROTTLE_RATE_LIMITED` on stderr instead of retrying or looking like an auth failure.

When invoked via a `gh` symlink, all arguments (including `gh version`) pass through to the real `gh`. When invoked as `gh-throttle`, it also exposes `version`, `update`, `doctor`, `skill`, and `--stats`.
