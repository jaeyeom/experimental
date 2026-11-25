# gh-pr-review

gh-pr-review lets you write line-by-line review comments on a pull request, or on a local branch, from the terminal, keep them locally while you work, and post them to GitHub as one review.

Full command options are in [pr-review.org](../../pr-review.org).

## Install

Requires `gh` authenticated (`gh auth login`) and Go 1.26.0, the version in the repo `go.mod`.

```bash
go install github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-pr-review@latest
```

## Quick start

```bash
gh-pr-review capture
gh-pr-review comment src/main.js 15 "Use const instead of let"
gh-pr-review list
gh-pr-review next
gh-pr-review resolve --comment-id a1b2c3d4
gh-pr-review submit --body "First pass" --event COMMENT
```

`list` prints a table. The `ID` column is the first 8 characters of the comment ID, and that value is what `resolve` and `delete` take as `--comment-id`. At the default width of 120 columns the header is shorter than the dashed rule. Example, not a live capture:

```text
ID       | File                 | Line     | Side     | Comment                                    | Created
------------------------------------------------------------------------------------------------------------------------
a1b2c3d4 | src/main.js          | 15       | RIGHT    | Use const instead of let                   | 2026-11-14 22:13
------------------------------------------------------------------------------------------------------------------------

Total: 1 items
```

`next` prints the next unresolved comment:

```text
File: src/main.js
Line: 15
Side: RIGHT
ID: a1b2c3d4
Created: 2026-11-14 22:13

Comment:
Use const instead of let
```

Repo and pull request are detected from the current checkout when you omit them. A number is a pull request. Any other identifier is a branch name.

## Commands

| Command | Does |
|---|---|
| `capture` | Capture diff hunks for a PR or a branch |
| `comment` | Add a line comment locally |
| `pull` | Fetch GitHub line comments into local storage |
| `list` | List stored comments |
| `clear` | Delete the local comments |
| `delete` | Delete one comment by `--comment-id` |
| `submit` | Post the local comments as one GitHub review |
| `adjust` | Move comment lines after the diff changes |
| `next` | Print the next unresolved comment |
| `resolve` | Mark a comment resolved, by `--comment-id` |
| `archive` | List, show, or clean archived submissions |
| `auto-adjust` | Move comment lines from the git diff |
| `rerequest` | Re-request reviewers once the author has answered them |
| `version` | Print the version |
| `help` | Print the command list |

`gh-pr-review help` and `gh-pr-review <command> -h` print the flags.

## Using from scripts or agents

File locks make the local store safe for several processes. A second process waits and retries. The retry knobs are under Advanced. Pass `--json` on `list`, `next`, and `submit` when a script reads the result.

## Advanced

Storage environment variables. Unset, the defaults are:

| Variable | Default |
|---|---|
| `GH_STORAGE_HOME` | `~/.config/gh-nudge/storage` |
| `GH_STORAGE_BACKUP_DIR` | `~/.config/gh-nudge/backups` |
| `GH_STORAGE_LOCK_TIMEOUT` | `30` seconds |
| `GH_STORAGE_DEBUG` | `false` |
| `GH_STORAGE_LOCK_MAX_RETRIES` | `10` |
| `GH_STORAGE_LOCK_INITIAL_DELAY` | `50ms` |
| `GH_STORAGE_LOCK_MAX_DELAY` | `2s` |
| `GH_STORAGE_LOCK_BACKOFF_FACTOR` | `1.5` |

`GH_STORAGE_BACKUP_DIR` is read by `gh-storage`.
