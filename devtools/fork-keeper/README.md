# fork-keeper

`fork-keeper` is a standalone scheduler CLI that rebases configured personal forks onto upstream, checks deterministic tripwires, runs an optional sandboxed reviewer or agent, and opens or promotes `sync/<date>` pull requests on GitHub.

It has no dependency on `jaeyeom/claude-toolbox`, `jq`, or any external shell scripts.

## Install

```sh
make install
# or:
go install github.com/jaeyeom/experimental/devtools/fork-keeper/cmd/fork-keeper@latest
```

The Bazel target is `//devtools/fork-keeper/cmd/fork-keeper`.

## Requirements

- `git` on `PATH`
- `gh` on `PATH` (or a rate-limiting wrapper such as `gh-throttle` configured via `gh_bin`, `--gh-bin`, or `GH_BIN`)
- Platform sandbox tool when running an external reviewer command:
  - macOS: `sandbox-exec` (built in)
  - Linux: `bwrap` (bubblewrap), unless `FORK_KEEPER_SANDBOX=none` or `--sandbox none`

## Usage

```sh
fork-keeper [flags] [<fork-name>...]
fork-keeper version [--json]
fork-keeper update [--check] [--ref <ref>] [--json]
fork-keeper doctor [--offline] [--json] [--config <path>]
fork-keeper skill [install|uninstall|status ...]
```

### Flags

| Flag | Default | Description |
| --- | --- | --- |
| `--config` | `FORK_KEEPER_CONFIG` or `~/.config/fork-keeper/forks.json` | Path to `forks.json` |
| `--cache-dir` | `FORK_KEEPER_CACHE` or `~/.cache/fork-keeper` | Scratch bare mirrors, worktrees, and logs |
| `--gh-bin` | `gh_bin` in config, `GH_BIN`, or `gh` | GitHub CLI executable or wrapper (`gh-throttle`) |
| `--reviewer-cmd` | per-fork `reviewer_cmd`/`agent`, config `reviewer_cmd`, or `FORK_KEEPER_REVIEWER_CMD` | External reviewer command (parsed without a shell) |
| `--sandbox` | `FORK_KEEPER_SANDBOX` or `auto` | Agent confinement mode: `auto`, `seatbelt`, `bwrap`, or `none` |
| `--date` | `FORK_KEEPER_DATE` or UTC `YYYY-MM-DD` | Date used for `sync/<date>` branches and `pre-sync/<date>` tags |

## Configuration (`forks.json`)

```json
{
  "gh_bin": "gh-throttle",
  "reviewer_cmd": ["my-reviewer", "--read-only"],
  "forks": [
    {
      "name": "my-fork",
      "repo": "me/my-fork",
      "upstream_url": "https://github.com/upstream/repo.git",
      "branch": "main"
    }
  ]
}
```

- Top-level keys:
  - `gh_bin` (optional): `gh` executable or wrapper path (`~/` expands to `HOME`). Overrides `GH_BIN`.
  - `reviewer_cmd` (optional): default external reviewer command array for forks without their own `reviewer_cmd` or `agent`.
  - `forks`: list of fork entries.
- Per-fork keys:
  - `name`: unique short identifier (used as positional filter `fork-keeper <name>`).
  - `repo`: GitHub `owner/repo` slug used for `gh` calls.
  - `fork_url` (optional): git remote URL for the fork (defaults to `https://github.com/<repo>.git`).
  - `upstream_url`: git remote URL for upstream.
  - `branch` (optional): target default branch (defaults to `main`).
  - `reviewer_cmd` or `agent` (optional): per-fork command array (cannot set both). Supports `{out}`, `{branch}`, `{repo}`, `{workdir}`, `{range}`, `{diff}`, `{prompt}`, `{schema}`, `{candidate}`, `{upstream_old}`, `{upstream_new}`, and `{review_base}` placeholders.

## Standalone vs External Reviewer Mode

### Standalone default (no reviewer configured)

When neither `reviewer_cmd`, `agent`, `--reviewer-cmd`, nor `FORK_KEEPER_REVIEWER_CMD` is set, `fork-keeper` uses its built-in deterministic `StandaloneReviewer`:

1. Computes the review range, fork patch stack, and upstream tag changes directly in Go.
2. Cherry-picks the fork's user commits onto `upstream_new` to form `fork-keeper/candidate`.
3. Emits a `needs-review` verdict and a markdown report containing the upstream commit log and diffstat.
4. Runs deterministic tripwires and opens or refreshes the `sync/<date>` pull request with label `needs-review`.
5. Promotes the default branch only after a human adds `fork-keeper:approved` on the current PR head.

### External reviewer / agent boundary

When an external reviewer command is configured, `fork-keeper` runs it inside a credential-scrubbed throwaway worktree with OS sandboxing (`seatbelt` on macOS, `bwrap` on Linux) that denies access to `~/.ssh`, `~/.config/gh` (plus `$XDG_CONFIG_HOME/gh` / `$GH_CONFIG_DIR`), `~/.gnupg`, `~/.aws`, `~/.netrc`, `~/.git-credentials`, `~/.config/git/credentials`, and the bare mirror git dir.

It supplies:
- The threat-review prompt on `stdin` and at `FORK_KEEPER_PROMPT_FILE`
- `FORK_KEEPER_OUT` (directory for `verdict.json` and `report.md`)
- `FORK_KEEPER_RANGE_FILE` (`range.json`)
- `FORK_KEEPER_DIFF_FILE` (`upstream.diff`)
- `FORK_KEEPER_SCHEMA_FILE` (`verdict-schema.json`)
- `FORK_KEEPER_BRANCH`, `FORK_KEEPER_REPO`, `FORK_KEEPER_WORK_DIR`, `FORK_KEEPER_UPSTREAM_OLD`, `FORK_KEEPER_UPSTREAM_NEW`, `FORK_KEEPER_REVIEW_BASE`, `FORK_KEEPER_CANDIDATE_HEAD`

Two integration styles are supported behind the `Reviewer` interface:
- **Agent style**: creates `refs/heads/fork-keeper/candidate` in the worktree (resolving any conflicts) and writes `{out}/verdict.json` + `{out}/report.md`.
- **Read-only JSON-over-stdio style**: inspects the pre-rebased `FORK_KEEPER_CANDIDATE_HEAD` (when clean) and prints a single `verdict.json` object on `stdout`.

`fork-keeper` validates `verdict.json` strictly in Go (`schema == 1`, `verdict` in `{"clean", "needs-review"}`, matching `candidate_head` and independently computed `range`, valid finding categories, confident conflicts, and no `first_run` / `rewritten` / `fork_has_merges` / `fork_behind_recorded_head` / moved or deleted tags on `clean`) and runs deterministic diff/history tripwires before any auto-promotion.

## Environment Variables

- `FORK_KEEPER_CONFIG`: path to `forks.json` (default `~/.config/fork-keeper/forks.json`)
- `FORK_KEEPER_CACHE`: scratch repos and agent output (default `~/.cache/fork-keeper`)
- `GH_BIN`: GitHub CLI executable or wrapper when `gh_bin` and `--gh-bin` are unset (default `gh`)
- `FORK_KEEPER_REVIEWER_CMD`: external reviewer command when not set via flag or `forks.json`
- `FORK_KEEPER_DATE`: date used in branch and tag names (default UTC `YYYY-MM-DD`)
- `FORK_KEEPER_SANDBOX`: agent confinement mode: `auto` (default: `seatbelt` on macOS, `bwrap` on Linux), `seatbelt`, `bwrap`, or `none`
- `FORK_KEEPER_SCRIPTS` *(optional legacy)*: directory holding `review-range.sh` / `validate-verdict.sh`; ignored if unset or missing
