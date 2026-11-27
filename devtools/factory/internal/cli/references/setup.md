# Setting up `factory` for a Repository

Follow these steps in order when configuring `factory` for a new repository or troubleshooting an unconfigured checkout.

## 1. Check prerequisites

Verify the installed CLI and local dependencies:

1. Check version and local diagnostics:
   ```sh
   factory version
   factory doctor --offline
   ```
   `factory doctor --offline` verifies `binary-on-path`, `config`, `external-commands` (`herdr`, `gh`, `git`), `skills`, and the local SQLite `store`.
2. Check source credentials and optional companion tools:
   - **GitHub sources**: run `gh auth status` (or `<gh_bin> auth status` if `gh_bin` / `GH_BIN` is set, such as `gh-throttle`). `factory doctor` also checks that `gh` meets the minimum supported version and probes native GitHub issue statuses and fields.
   - **Jira sources**: create a personal access token file with mode `0600` (for example `~/.config/factory/jira-token` via `chmod 600`) or export the environment variable referenced by `token_env`. Set **exactly one** of `token_file` or `token_env`.
   - **Spawners & leases**:
     - Default `[worker] spawner = "agentbus"` and `[lease]` / `wake_before_prompt` use `agentbus` and `herdr`.
     - `[worker] spawner = "herdr"` uses `herdr` without `agentbus`.
     - `[worker] spawner = "subagent"` claims items and prints/returns the worker brief for a coordinator without spawning `herdr` tabs.

## 2. Choose repo-local vs. global config and scaffold

When `--config PATH` is omitted, `factory` searches in order:
1. `./factory.toml`
2. `<git-root>/factory.toml` (when inside a git repository)
3. `$XDG_CONFIG_HOME/factory/factory.toml` (`~/.config/factory/factory.toml` when `XDG_CONFIG_HOME` is unset; an empty `XDG_CONFIG_HOME` is an error)

Choose the location that fits the workflow:
- **Repo-local (`<git-root>/factory.toml`)**: best for a single repository or when each repo carries its own independent config.
- **Global (`$XDG_CONFIG_HOME/factory/factory.toml`)**: best when coordinating multiple repositories from one config, keeping `factory.toml` out of the repo tree, or sharing top-level `[defaults]`, `[gate]`, `[[lane]]`, and `[[agent_rule]]` settings across sources.

Scaffold the initial config from inside the target git repository:

1. Preview with `--dry-run` first:
   ```sh
   factory init --dry-run
   ```
   Useful flags:
   - `--kind github` or `--kind jira`: override auto-detection from `origin` (required for Jira or non-`github.com` remotes).
   - `--base <branch>`: override `origin/HEAD` (fallback is `main`).
   - `--cwd <dir>`: inspect another directory inside the git repository.
   - `--global`: target `$XDG_CONFIG_HOME/factory/factory.toml` (creates it or appends a new `[[source]]` block if neither source `name` nor covering `cwd` already exists).
   - `--force`: overwrite an existing `<git-root>/factory.toml` (mutually exclusive with `--global`).
2. Write the file once the preview looks right:
   ```sh
   factory init            # repo-local <git-root>/factory.toml
   # or:
   factory init --global   # append to $XDG_CONFIG_HOME/factory/factory.toml
   ```

## 3. Configure key sections in `factory.toml`

Review and adjust the generated TOML sections (see `factory.example.toml` and `examples/` in the `factory` package for full examples).

### `[[source]]`
- **GitHub (`kind = "github"`)**:
  - `name`: short source name (used in `github:<name>`).
  - `repo`: `"owner/repo"`.
  - `cwd`: local checkout path (`~/` is expanded); `base`: base branch (e.g. `"main"`).
  - `require_labels` (e.g. `["agent-ready"]`) and `exclude_labels` (e.g. `["needs-human", "blocked-external"]`): filter which issues are eligible.
  - Native GitHub issue statuses and starter fields are enabled by default (`ready_statuses = ["Todo", "Backlog", "OPEN"]`, `claim_status = "In progress"`, `priority_field = "Priority"`, `defer_field = "Start date"`, `due_field = "Target date"`). Only set `claim_label`, `priority_labels`, or `defer_label_prefix` on legacy repositories without native statuses/fields.
  - `start_prompt`: prompt template sent to workers (for example `"/gh-issue-resolver {url}"`).
  - `[source.trust]` (explicitly classify **every** source; `factory doctor` warns when `trust.environment` is unset):
    - `environment = "trusted"`: for corporate Jira or private single-team GitHub repositories where only trusted users can create and edit issues. External-author gating, tamper guards, and prompt hardening stay off.
    - `environment = "untrusted"`: required for public GitHub repositories or any tracker open to external authors. Configure `trusted_author_associations = ["OWNER"]`, `untrusted_ready_statuses = ["Todo"]` (avoid `"OPEN"` or `"Backlog"`), and `guards = ["locked"]` or `["approved_hash"]` (or both for OR semantics; avoid `guards = []`). With `"approved_hash"`, run `factory approve <id>` to inspect the live title/body/SHA-256 hash, `factory approve <id> --apply` to record the approval hash and move the issue to the first `untrusted_ready_statuses` entry, and `factory approve --revoke <id>` to clear a stored approval.
- **Jira (`kind = "jira"`)**:
  - `name`, `site`, `email`, and exactly one of `token_file` or `token_env`.
  - `jql` (e.g. `"project = PRJ"`), `ready_statuses` (e.g. `["To Do", "Backlog"]`), `claim_status` (e.g. `"In Progress"`), optional `claim_transition`.
  - `require_labels`, `exclude_labels`, `blocks_link_type = "Blocks"`, `cwd`, `base`, `workspace`, `start_prompt`.
  - `[source.trust]`: set `environment = "trusted"` for internal/corporate Jira instances restricted to trusted employees, or `"untrusted"` if external users can create/edit issues.
  - Optional ranking & filing fields: `parent_priority`, `rank_order`, `defer_field`, `default_parent`, `create_project`, `[[source.workspace_rule]]`.

### `[defaults]` and `[sync]`
- Keep `dry_run = true` in `[defaults]` until end-to-end checks pass (`start` and `tick` then require `--apply`).
- Common `[defaults]` keys: `max_in_flight`, `min_start_interval`, `full_sync_every`, `exclude_labels`, `human_labels`, `skip_types`, `caller`, `wake_before_prompt` (`"auto"` or `"off"`), `start_prompt_append`.
- Optional `[sync] max_age = "15m"` enables auto-refreshing stale sources on `next`, `show`, `doctor`, `priorities`, and `close` (default `0` never auto-syncs).

### `[claim]` (and per-source `[source.claim]`)
- Safety checks: `require_leaf = true`, `require_unblocked = true`, `source_containment = true`.
- Ownership: `assign = true`, `ownership_rule = "unassigned_or_me"` (`"unassigned_or_me"`, `"unassigned"`, or `"any"`).
- In-progress marking: `mark_in_progress = true`, `in_progress_rule = "block"` (`"block"` or `"allow"`), `create_label = false` (GitHub), plus per-source `status`, `transition` (Jira), `label`.
- Claim comments: `comment = true`, `nickname = ""` (`[a-z0-9][a-z0-9-]{0,31}`), `comment_template = ""`.

### `[worker]` (and `[source.worker]` / `[lane.worker]`)
- `spawner`: `"agentbus"` (default), `"subagent"`, or `"herdr"`.
- `agent`: coding agent (`"agy"`, `"claude"`, `"codex"`, `"grok"`). Do not set alongside legacy `defaults.agent_kind`.
- `agent_args`: list of extra CLI args for the agent (e.g. `["--model", "opus"]`).
- `max_workers`: parallel running workers cap (default `1`).
- `worktree`: `false` (default) or `true` to isolate each worker in its own git worktree.

### `[output]` (and `[source.output]` / `[lane.output]`)
- `mode`: `"pr"` (default: create pull request and retain worker for review) or `"merge"` (rebase onto `base` and push linear history).
- `push_cmd`: optional merge-mode command template (e.g. `"review-and-push-loop --base {base} --json"`, supporting `{base}` and `{id}`) instructed to workers instead of raw `git push` (clear inherited values with `push_cmd = ""` on `[source.output]` or `[lane.output]`).

### `[gate]`
- `ignore_panes`, `ignore_tabs`: pane IDs or tab labels (such as a coordinator tab) skipped when counting working panes.
- `base_branch` and `require` (`"on_base"` or `"clean_against_base"`): enforce clean base-branch state before starting work.
- `hold_files`, `hold_tabs`, `hold_ttl`: local hold controls (`~/.local/state/factory/hold` is the default hold file when `hold_files` is empty).

### `[lease]`
- `gate`: `"auto"` (default), `"off"`, `"required"`, `"queue"` (allows `held` and `releasing` leases held by another agent so the start proceeds, skips checkout git checks with a note, and queues the started agent behind the holder, while still running checkout checks for `free`/self-held leases and refusing `paused` and `needs-adopt`; unreadable leases fall back with a note), or `"queue-required"` (same as `"queue"`, but refuses when the lease cannot be read) (top-level only; checks `agentbus` workdir lease).
- `mode`: `"off"` (default), `"prompt"`, or `"prompt+check"`.
- `claim_for` (default `"45m"`) and `restore` (`false` default). (`mode`, `claim_for`, and `restore` can also be set in `[source.lease]` or `[lane.lease]`.)

### Optional `[[lane]]`, `[lanes]`, and `[[agent_rule]]`
- `[[lane]]` + `[lanes] default = [...]`: route labeled items or default work across local or remote `herdr` machines (`name`, `machine`, `checkout`, `max_workers`, optional `max_active`, `labels`, `exec`, `[lane.worker]`, `[lane.lease]`, `[lane.output]`).
- `[[agent_rule]]`: match item `labels` (optional `name`) to replace `agent_args` for matching items (first match wins).

## 4. Verify end-to-end before enabling writes

Run the read/dry-run verification sequence from inside the repository:

1. Sync the local mirror:
   ```sh
   factory sync
   ```
2. Run full diagnostics (online):
   ```sh
   factory doctor
   ```
   Verify that trust classification and threat audits pass cleanly:
   - Fix any `trust-environment` warning by explicitly setting `[source.trust] environment = "trusted"` or `"untrusted"` (online `factory doctor` checks GitHub `repository.isPrivate` and flags unclassified public repositories).
   - Fix any `trust-config` warning (for example: public GitHub repo marked `"trusted"`, `"OPEN"` or `"Backlog"` in `untrusted_ready_statuses`, empty `guards`, or broad non-maintainer `trusted_author_associations`).
   - Review any `github-permissions` warning (non-owner users/teams with triage/write access on untrusted/public repos; gracefully skipped if the `gh` token lacks permission) and any `github-automation` warning (`.github/ISSUE_TEMPLATE/*` or `.github/workflows/*` rules in `cwd` that auto-apply ready labels or set `Todo`/status on newly opened issues; also checked under `--offline`).
   - Fix any other `fail` or unexpected `warn` items (such as missing GitHub issue statuses/fields, bad Jira tokens, or missing commands).
3. Check the start gate:
   ```sh
   factory gate
   ```
   Confirm `OK: ...` (or inspect which working tabs, branch state, lease, or hold is blocking).
4. Inspect ranked ready items:
   ```sh
   factory next
   factory next --explain <id>
   ```
5. Preview a start in dry-run mode (with `dry_run = true` and no `--apply`):
   ```sh
   factory start <id>
   ```
   Verify the rendered plan, target source/lane, claim comment preview, and `start_prompt`.
6. Once the dry-run plan is verified and the user wants to start work, run `factory start <id> --apply` (or `factory tick --apply`), or set `dry_run = false` in `[defaults]` if automated ticks should apply directly.
