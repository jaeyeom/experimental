# Agent Instructions

These instructions apply to every coding agent working in this repository.

## Core Principles
1.  **Simplicity**: Prioritize the simplest solution.
2.  **No Duplication**: Reuse existing functionality.
3.  **TDD**: Practice Test-Driven Development. Understand desired behavior -> Test -> Implement.

## Automation & Workflow
-   **ALWAYS run `make check`** before requesting a commit.
    -   This runs formatters, linters, tests, and generators.
    -   If `make check` changes files, include them in the commit.
-   **Use `make` targets**: `make test`, `make lint`, `make fix`, `make format`.
-   **Use `make format`** instead of individual formatting tools (e.g., `gofmt`, `goimports`).

## Git Workflow
-   **Keep main branch history flat** (no merge commits).
-   **Use rebase** to integrate upstream changes (`git pull --rebase`).
-   **Use fast-forward merges** to main (`git merge --ff-only`).
-   **Reuse fixed worktree paths** (e.g., `../<repo>-wt-1`, `../<repo>-wt-2`) instead of per-issue paths (`../<repo>-issue-<N>`) so Bazel reuses output bases.

## Versioning
-   **Single module, single tag**: This repository uses a single root `go.mod`
    and a single monorepo-wide Semantic Versioning tag (`vMAJOR.MINOR.PATCH`) at
    the repository root. Do not create per-tool tags or split `devtools/` into
    separate Go submodules.
-   **Bump rules**:
    -   `MAJOR`: Any breaking change in a released tool or shared package (`devtools/pkg/...`).
    -   `MINOR`: Backwards-compatible new features or tools.
    -   `PATCH`: Backwards-compatible fixes or internal/documentation updates.
-   **Shared version**: Unchanged tools sharing a bumped version (and being
    byte-identical across consecutive releases) is expected and acceptable.
-   **Binary version reporting**: Go binaries under `devtools/` should report
    their version via `github.com/jaeyeom/experimental/devtools/pkg/version`
    (`-ldflags "-X github.com/jaeyeom/experimental/devtools/pkg/version.Version=..."`
    with `runtime/debug.ReadBuildInfo()` fallback).
-   **Release workflow**: `release-please` maintains a Release PR on `main`
    updating `CHANGELOG.md` and `.release-please-manifest.json` from
    Conventional Commits (`bump-minor-pre-major: true` during `0.x`; override
    with a `Release-As: X.Y.Z` commit footer). Cutting `vX.Y.Z` requires
    squash-merging the Release PR; see `devtools/README.org` for the checklist.

## Pull requests
This repository squash-merges with the pull request title and description
as the commit message. HTML comments (`<!-- ... -->`) in the PR body are
copied into `git log` even though GitHub hides them in the rendered
description.

When creating or updating a PR:
- Do not copy `<!-- ... -->` hints from `.github/pull_request_template.md`
  into the submitted body. They are compose-time hints only.
- Delete unused optional sections instead of leaving them empty.

A workflow strips leftover comments from the stored body as a backstop.
Other repositories can call the same workflow (pin the SHA):

```yaml
jobs:
  strip:
    permissions:
      contents: read
      pull-requests: write
    uses: jaeyeom/experimental/.github/workflows/strip-pr-html-comments.yml@<sha>
```

## Temp File Handling
-   **For multi-step workflows**: use the project-local `.tmp/` directory
    (gitignored) instead of `$TMPDIR` when a temp file must be referenced
    across several commands.
    ```sh
    mkdir -p .tmp
    echo "data" > .tmp/myfile
    # Safe to reference .tmp/myfile in any subsequent command
    ```
