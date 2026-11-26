# factory

`factory` keeps a local mirror of Jira and GitHub issues, chooses the next item that is ready to work, and starts one coding agent on it in [herdr](https://herdr.dev).

You point it at one or more sources. On each pass it refreshes the mirror and, when you ask it to, claims the best ready item, opens a herdr tab, and sends that source's start prompt. The checked-in examples send `/implement-jira-ticket` or `/gh-issue-resolver`. The agent then drafts a pull request, and [prsync](https://github.com/jaeyeom/experimental/tree/main/devtools/prsync) takes it from there. `factory` returns once the agent is working.

One agent starts at a time. A dirty checkout, an agent already working, too much work in flight, or a start inside the last few minutes holds the next start. The claim written back to Jira or GitHub is the lock between machines. Until that lock has proved itself, run the loop on one machine. `[[lane]]` config lets `factory start` put workers on other herdr machines, each with its own checkout and worker cap. With lanes, `tick` and `run` start at most one item per lane per tick, and `settle` closes each tab on its claim's lane; see [docs/usage.md](docs/usage.md#lanes).

`factory verify` on a remote lane runs its checkout checks on that machine through the lane's `exec` prefix, for example `exec = ["ssh", "mac1"]`, and prints `checked on MACHINE:CHECKOUT`; an unreachable machine is an error, never a pass. A lane has two caps. `max_workers` counts the workers running there now: a claim whose tab is still open on that machine, one that holds the lane checkout's lease, or a start still under way. A claim handed off with its pull request waiting for review, its tab closed, does not take a slot. The optional `max_active` counts every open claim on the lane, handed off or not, for a cap on review backlog; unset, only the global `max_in_flight` bounds those. `factory gate` prints both, for example `linux: OK workers 1/2 (running), active 3`.

## Requirements

- Go 1.26 or newer, to build
- `herdr` 0.9 or newer, `gh`, and `git` on `PATH`
- `gh auth status` succeeding before a GitHub source can sync

## Install

From a clone of `github.com/jaeyeom/experimental`:

```sh
git clone https://github.com/jaeyeom/experimental.git
cd experimental
make install                               # installs all devtools, including factory
# or build/install factory alone:
go install ./devtools/factory/cmd/factory
bazel build //devtools/factory/cmd/factory
```

`make install` installs this tree with `go install` and stamps the version via `-ldflags` (`github.com/jaeyeom/experimental/devtools/pkg/version.Version`). Once installed, `factory update` runs `go install` into the directory of the running executable with `-ldflags` stamping (or falls back to Go build info when built with plain `go install`).

From the module directly:

```sh
go install github.com/jaeyeom/experimental/devtools/factory/cmd/factory@main
```

## Configure

`factory` looks for a config file in this order:

1. The file you pass with `--config`.
2. `./factory.toml` in the current directory.
3. `<git-root>/factory.toml` when inside a git repository.
4. `$XDG_CONFIG_HOME/factory/factory.toml`, which is `~/.config/factory/factory.toml` when `XDG_CONFIG_HOME` is unset.

Run `factory init` inside a git repository to scaffold `<git-root>/factory.toml` from `origin` and the default branch (`--global` appends to `$XDG_CONFIG_HOME/factory/factory.toml`, `--kind jira` scaffolds a Jira source, `--dry-run` previews without writing, and `--force` overwrites an existing `<git-root>/factory.toml`). Or copy a starting file and replace the placeholders:

| Starting point | Use it when |
| --- | --- |
| [`examples/github-repo.toml`](examples/github-repo.toml) | One GitHub repo |
| [`examples/jira-monorepo.toml`](examples/jira-monorepo.toml) | One Jira project in a shared checkout |
| [`factory.example.toml`](factory.example.toml) | Jira and GitHub together, with the shared defaults written out |

A Jira source needs `name`, `kind = "jira"`, `site`, `email`, `token_file` or `token_env`, `jql`, `claim_status`, `cwd`, `base`, and `start_prompt`. Put the API token alone in `token_file` and run `chmod 600` on that file. Create the token at <https://id.atlassian.com/manage-profile/security/api-tokens>.

`factory file` files only into the source of the item the caller is working on: `start` sets `FACTORY_ITEM_SOURCE` in each worker's tab, and any other `--source` is refused, with no override. A coordinator without that variable must pass `--source`. Filing into a public repository happens only when it is the item's own source, with a warning. Workers on remote lanes never run `file`, `link`, or `release`; they report back to the coordinator. See [docs/usage.md](docs/usage.md#where-factory-file-files).

Every `gh` call runs one binary: the top-level `gh_bin` key, else the `GH_BIN` environment variable, else `gh`. Point it at a wrapper such as `gh-throttle` when several tools on one account share GitHub's rate limits. `gh_bin` is a top-level key, so put it above the first `[table]`. An empty `gh_bin` or a set but empty `GH_BIN` is an error. One `gate`, `start`, or `tick` lists your open pull requests once and reuses that list.

A GitHub source needs `name`, `kind = "github"`, `repo` (`owner/name`), `cwd`, `base`, and `start_prompt`. To have the worker report back, put `{report_back}` in `start_prompt`: it becomes `agentbus send <caller> "<message>"`, which reaches the caller from another machine, or `herdr agent prompt <caller pane> "<message>"` without agentbus. `{caller}` alone is a pane id that only works on the caller's machine. `gh` holds the credentials. By default, GitHub sources use native GitHub issue statuses (`claim_status = "In progress"`, `ready_statuses = ["Todo", "Backlog", "OPEN"]`, release moves back to `"Todo"`) and native issue fields (`priority_field = "Priority"` mapping `Urgent`/`High`/`Medium`/`Low`, `defer_field = "Start date"`, and `due_field = "Target date"`). Explicit `claim_label`, `priority_labels`, and `defer_label_prefix` remain supported for legacy repos, and `factory doctor` checks `gh` version, repo statuses, and issue fields (nudging when legacy label keys can be removed).

`dry_run` defaults to true. `start`, `tick`, and `run` then print a plan. Pass `--apply` when you want a claim and a herdr tab. `link`, `unlink`, `file`, and `release` write as soon as you run them.

An item id is a Jira key (`PRJ-1234`) or a GitHub issue (`owner/repo#57`). A source key is `kind:name`, for example `jira:monorepo` or `github:issues`.

The mirror is a SQLite file at `$XDG_STATE_HOME/factory/factory.db`, or `~/.local/state/factory/factory.db`. `factory logs --path` prints the path it opened. It also keeps history for reports: pull request times factory already read, finished items, the agent rule and model each start used, and an hourly backlog count after a sync.

An optional `[worker]` table configures how workers are spawned (`spawner = "agentbus"` by default, `"subagent"`, or `"herdr"`), the coding agent (`agent`, such as `"agy"`, `"claude"`, `"codex"`, or `"grok"`), `agent_args`, `max_workers` (default `1`, orthogonal to `spawner` and `max_in_flight`), and `worktree` (`false` by default; isolates each worker in its own git worktree, inspected by `verify`, and removed by `settle` or `release` only when clean so dirty worktrees are preserved with a warning). An optional `[output]` table chooses `mode = "pr"` (the default: create a pull request and keep the worker alive for review feedback, `keep_worker: true`) or `"merge"` (rebase onto `base` for linear history, push, and exit the worker on completion, `keep_worker: false`; local tabs run `factory settle`, remote tabs close themselves, and subagents return immediately for the coordinator to settle and discard; `start` passes `FACTORY_OUTPUT_MODE` in the worker environment; `verify` skips PR/template checks and verifies a clean checkout, matching stash count, and a linear commit referencing the item on `origin/<base>`, while `settle`, `tick`, `show`, and `doctor` inspect `origin/<base>` for the landed commit instead of pull requests). Both tables may be overridden in `[source.worker]` / `[source.output]` and `[lane.worker]` / `[lane.output]` (`max_workers` on a lane stays on `[[lane]]`). See [docs/usage.md](docs/usage.md#worker-lease-and-output-tables).

An optional `[claim]` table configures start safety checks (`require_leaf`, `require_unblocked`, `source_containment`), ownership (`assign`, default `true` on Jira and GitHub; `ownership_rule`: `"unassigned_or_me"`, `"unassigned"`, or `"any"`), in-progress marking (`mark_in_progress`, `in_progress_rule`: `"block"` or `"allow"`, `create_label`), and claim comments (`comment`, `nickname`, `comment_template`), with per-source overrides in `[source.claim]` (which may also set `status`, `transition`, and `label`; setting both a legacy `[[source]]` key and its `[source.claim]` counterpart on the same source is an error, and `mark_in_progress = false` allows empty `claim_status`/`claim_label`). See [docs/usage.md](docs/usage.md#worker-lease-and-output-tables).

An optional `[lease]` table puts the [agentbus](https://github.com/jaeyeom/experimental/tree/main/devtools/agentbus) checkout-lease rule into the start prompt, so a worker claims the shared checkout before it changes it. `mode` is `"off"` (the default: the prompt is unchanged and `agentbus` is never run at start), `"prompt"` (the rule goes at the top of the prompt, or where `start_prompt` has `{{lease_rule}}`), or `"prompt+check"` (the same, and an applied start also prints and records the lease state; it never blocks). `claim_for` (default `"45m"`) is the estimate in the rule, and `restore` (`false` by default) toggles `agentbus lease restore` alongside `claim` when `mode` is `"prompt"` or `"prompt+check"`. `mode`, `claim_for`, and `restore` may also be set per source (`[source.lease]`) and per lane (`[lane.lease]`); a lane overrides its source, which overrides the top level. `gate` in the same table controls the gate's lease check and is top level only. See [docs/usage.md](docs/usage.md#checkout-lease).

Before it types the start prompt, an applied `start` runs `agentbus wake --if-hibernated --wait` on the new pane, so a hibernated agent is relaunched instead of the prompt landing in a shell. `[defaults] wake_before_prompt` is `"auto"` (the default: wake when `agentbus` is installed) or `"off"`. A failed wake fails the start without prompting. `factory logs` shows `(woke hibernated agent)` when a wake happened. See [docs/usage.md](docs/usage.md#waking-a-hibernated-agent).

Field-by-field rules, prompt placeholders, and the exact help text are in [docs/usage.md](docs/usage.md).

## A first session

Stay in dry-run until `next` and `show` match what you expect.

```sh
factory sync --full
factory next
factory next --explain PRJ-1234
factory show PRJ-1234
factory doctor
factory gate
```

`next` lists ready items, best first. `--explain` prints each readiness check as `PASS` or `FAIL`. An item is ready when it is open, has the labels you require, has none of the excluded labels, is in a ready status, is unassigned or assigned to you, and every blocker is already done. A blocks link from an item to its own descendant is ignored by default (`parent_blocks_child`). Nothing that is claimed, already has a herdr tab, or already has your open pull request is ready. Ranking uses `rank_order`. Unset, it prefers the item's own priority, then the item that unblocks the most other work, then the nearest listed ancestor in `parent_priority`, then age.

`doctor` names graph and claim problems, including a blocker the mirror has never seen. On Jira, `sync` fetches such a blocker by key (`external_blocker_lookups`, default 100), and `doctor` stays quiet when that blocker is Done. It does not report a parent in progress as a dropped claim, and it checks the tabs on remote lane machines. A blocker outside the mirror that the last sync found Done does not block, so an item blocked only by another team's finished work becomes ready. One that is open, missing, or not fetched keeps the item out of `next`, and `next --explain` says which. It also names a priority inversion: a less urgent open item blocking a more urgent one. `factory priorities` prints those inversions and does not write. `factory priorities --apply` raises each blocker on the source. `tick`, `start`, and `run` do not change priority.

`gate` says whether another agent can start. It checks that no herdr agent is working inside a configured source `cwd`, except the pane running factory (`HERDR_PANE_ID`) and any pane or tab listed in `gate.ignore_panes` or `gate.ignore_tabs`. An agent whose `cwd` and `foreground_cwd` are both outside every configured checkout does not count. `--log-level=debug` names each working agent ignored for that reason. It also checks a clean source checkout, in-flight work under `max_in_flight` (default 3), and a last start at least `min_start_interval` ago (default 10 minutes). In-flight work is the union of active claims (`claimed`, `started`, and `handed_off`) and open pull requests you authored. A claim and a pull request that are the same work count once: by the pull request URL recorded on the claim, then by the claim's recorded head branch, then by the item id in the title (`pr_title_pattern`) or in the head branch name. An open pull request that matches no claim still counts. At the limit, the gate prints `in flight: N (claims C, PRs P, overlap O) of max M`. When it passes, it prints `OK: N in flight (max M)`. When `gate.base_branch` is set, it also refuses a checkout that is not on that branch (`require = "on_base"`) or that has commits not on `origin/<base_branch>` (`require = "clean_against_base"`). Unset, the branch is not checked. A hold file (`gate.hold_files`, or `~/.local/state/factory/hold` when that list is empty) and a live `gate.hold_tabs` label also refuse the start. `factory hold` creates the hold file. `factory release-hold` removes it. A file older than `gate.hold_ttl` is called stale and still refuses. When the checkout is guarded by an [agentbus](https://github.com/jaeyeom/experimental/tree/main/devtools/agentbus) workdir lease, the gate also reads it: the checkout is usable when the lease is `free`, or held by the agent being started, and otherwise the reason names the holder, the deadline, and the queue length. `[lease] gate` is `auto` (the default: a missing `agentbus` keeps the old checks and adds a `note:` line), `off`, or `required` (a lease that cannot be read refuses); see [docs/usage.md](docs/usage.md#checkout-lease). When every check passes, `gate` prints `OK: <n> in flight (max <limit>)`, for example `OK: 3 in flight (max 5)`, so you can confirm a `max_in_flight` change took effect. Otherwise it prints one failing reason per line. It exits 0 either way.

When the plan looks right:

```sh
factory start PRJ-1234          # plan only, while dry_run is true
factory start PRJ-1234 --apply  # claim, create the tab, send the prompt
```

`start` accepts a canonical item id, shorthand `<number>` / `#<number>` (for the scoped GitHub source), or a GitHub/Jira issue URL. By default (`[claim]` defaults), `start` scopes to the current git repository's configured source (refusing unconfigured git repositories unless `--source` or `--config` is passed) and enforces `FACTORY_ITEM_SOURCE` containment. It refuses closed/done items (never bypassed), non-leaf items (`skip_types` or open children when `require_leaf = true`), blocked items (`require_unblocked = true`), foreign ownership (`ownership_rule`), already in-progress items (`in_progress_rule = "block"`, including foreign `nickname` claim comments), a closed gate, an existing active claim, or a tab already open in live herdr. When the gate refuses, a dry run still prints the plan (including the claim comment preview when `claim.comment = true`) and marks the first line `blocked  yes`. `--force` is not required to see it. `start` then exits `3`. `--force` skips queue checks and the start gate while keeping those safety checks and live tab checks; `--force-taking-ownership` takes over foreign assignees, in-progress state, and foreign machine nickname claims (re-assigning to you and removing prior assignees on GitHub). On `--apply`, `start` posts the claim comment (`Claimed by <agent> [on <nickname>] at <timestamp>.`) matching `claims.claimed_at` unless `claim.comment = false` or re-running on this machine's own claim.

`start` ends with one unambiguous line on stdout (or one JSON object with `--json`, which for `spawner = "subagent"` includes `spawner`, `source`, `cwd`, `agent`, `agent_args`, `env`, and the full rendered `brief` so a coordinator can spawn its subagent tool without `herdr`) and a distinct exit code:

- `0` — `factory start <id>: started (tab <tab>, claim <time>)` (or dry-run plan succeeded)
- `3` — `factory start <id>: refused, nothing changed: <reason>`
- `4` — `factory start <id>: failed after claim; claim kept: <next step>` (for example when `herdr` or `prompt` fails after claiming, or when `start` did not change the tracker)
- `5` — `factory start <id>: failed after claim; claim released: <next step>` (when agent launch failed and `start` closed its tab and released the tracker claim it made)

## Letting it pick

```sh
factory tick            # sync, settle merged work, print what it would start
factory tick --apply    # do that start, at most one item
factory run --interval 15m --apply
```

`tick` exits 0 when the gate refuses or nothing is ready, and prints the reason. A dry run also prints the blocked plan before that reason. `run` repeats `tick` until you interrupt it. The interval has to be at least one minute. Keep this loop on a single machine.

## Commands

`next`, `stats`, `doctor`, `priorities`, `close`, `logs`, `sync`, `gate`, `tick`, and `run` scope to the source whose `cwd` holds the current directory (the deepest one when `cwd`s nest), and say so on stderr: `factory: showing github:name (current directory); --all for every source` (or `scoping to` for `sync`, `gate`, `tick`, and `run`). `--all` includes every source, and `--source KEY` picks one. Inside an unconfigured git repository, they fail and point to `factory init`, `--all`, or `--source`; outside any git repository, they cover every source as before. See [docs/usage.md](docs/usage.md#which-sources-commands-scope-to).

| Command | What it does |
| --- | --- |
| `approve ID` | Inspect an item's live title, body, and SHA-256 content hash (`dry_run` aware). `--apply` records the local approval and transitions status to the first `untrusted_ready_statuses` entry; `--revoke` clears the stored approval. |
| `init` | Scaffold `<git-root>/factory.toml` (or append to global XDG `factory.toml` with `--global`) from the current git repository. Supports `--cwd`, `--kind github\|jira`, `--base`, `--force`, and `--dry-run`. |
| `sync` | Refresh the local mirror. Items that left the source query are removed. `--full` rereads every item. `--source kind:name` limits the refresh. |
| `next` | List ready items. `--explain ID` shows why one item is or is not ready. `--sync` refreshes first. A stale mirror also refreshes when `sync.max_age` is set. `--no-sync` skips that. |
| `next --deferred` | List items deferred by a not-before date (`defer_field` on Jira, `defer_label_prefix` labels on GitHub), soonest first. |
| `show ID` | Print one item, its blockers, claim, tab, and pull requests. |
| `doctor` | Run CLI diagnostics (`binary-on-path`, `revision`, `config`, `store`, `external-commands`, `sources`, `github-status` and `github-fields` when GitHub sources are configured, `skills`) plus dependency-graph and claim checks. `--offline` skips network checks; `--json` outputs JSON. Exits `0` (ok), `1` (warnings), or `2` (failures). |
| `update` | Install the latest `factory` from the default branch (or `--ref`) into the running binary's directory. `--check` reports without installing; `--json` outputs JSON. Exits `0` (current/updated), `1` (update available with `--check`), or `2` (failure). |
| `priorities` | Report a lower-priority item blocking a higher-priority one. `--apply` writes the raises. |
| `close [ID...]` | Detect open parent issues whose children are all done in the local cache (dry-run by default). `--apply` closes eligible parents (with optional `--comment` or `--comment-file`), strictly refusing any item with open children, no known children, already done, missing from cache, or outside `FACTORY_ITEM_SOURCE` (no `--force`). |
| `gate` | Report whether another agent can start. `--json` prints the counts. |
| `claim-pr ID URL` | Record a pull request on an active claim. |
| `start ID` | Claim that item and start an agent. Add `--apply` while dry-run is on. |
| `tick` | Sync, settle merged work, and start at most one ready item. `--enqueue` prints a `start-item` job per ready item (JSON lines, with lane or owner affinity) instead. |
| `owner` | Print the machine and tab of an item's latest claim (`--json`, `null` when never claimed). |
| `settle` | Finish merged work without starting an agent. |
| `run` | Repeat `tick` on an interval. |
| `release ID` | Undo a claim. |
| `link` / `unlink` | Record or remove a "blocker blocks blocked" link. |
| `file` | File a follow-up into the item's own source (`FACTORY_ITEM_SOURCE`, or `--source` for a coordinator) and print its new id. A Jira source takes `--project KEY` when no parent, `create_project`, or linked key gives the project. |
| `logs` | Show recent runs. `--path` prints the database path. |
| `stats` | Report throughput, backlog, per-epic progress, flow, and a burndown from the local store, with a short bar column in the throughput, backlog, and epics tables (`--bar-width N`, default 12, `0` for none), sparklines, and a braille burndown with a legend and a spelled-out projection (`--color auto\|always\|never`, `--ascii`, `--burndown bars`, `--burndown-pace` for a dotted reference line to zero). Times and day buckets are local (`--utc` for UTC). `--since`, `--by`, `--split`, `--epic`, and `--json` (UTC, unchanged apart from `window.time_zone`). No network calls. |
| `skill` | Print the agent skill, or manage coding-agent skill installs with `skill install`, `skill uninstall`, and `skill status [--json]`. |
| `version` | Print version, commit, module path, and build time (`--json` for JSON). `make build`, `make install`, and `factory update` stamp them via `-ldflags`; `go install` falls back to Go build info. |

`factory --help` and `factory <command> --help` list flags. [docs/usage.md](docs/usage.md) records that help text and the exit codes: `0` finished (including a tick that started nothing), `1` the tool failed, `2` bad usage, `3` `start` refused before any change, `4` `start` failed after claim (claim kept), `5` `start` failed after claim (claim released).

## Develop

From the repository root:

```sh
bazel build //devtools/factory/...
bazel test //devtools/factory/...
make check
```

Tests stay offline. They do not call herdr, `gh`, or git. The manual pass against a real project is [docs/e2e-checklist.md](docs/e2e-checklist.md).

## Read next

- [docs/usage.md](docs/usage.md) — install, config, commands, ranking, exit codes
- [docs/design.md](docs/design.md) — sources, store, readiness, claim protocol
- [docs/e2e-checklist.md](docs/e2e-checklist.md) — click-through against a live project
