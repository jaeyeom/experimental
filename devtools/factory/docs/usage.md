# Usage

`factory` mirrors Jira and GitHub issues, picks the next ready item, and starts an agent on it in herdr. The rules are in [design.md](design.md). This page is how to install it, where the config goes, and what the commands print. The manual proof against a real project is [e2e-checklist.md](e2e-checklist.md).

## Install

Go 1.26 or newer:

```sh
go install github.com/jaeyeom/experimental/devtools/factory/cmd/factory@main
```

`herdr`, `gh`, and `git` must be on `PATH`. A GitHub source uses `gh`, so `gh auth status` has to succeed before `sync`. [design.md](design.md) expects herdr >= 0.9. This binary does not check that version itself.

`factory version` prints version, commit, module path, and build time (or JSON with `--json`). `make install` from the `jaeyeom/experimental` root stamps `github.com/jaeyeom/experimental/devtools/pkg/version.Version` via `-ldflags -X`. A plain `go install github.com/jaeyeom/experimental/devtools/factory/cmd/factory@main` falls back to Go build info (`Main.Version`, `vcs.revision`, appending `-dirty` when `vcs.modified` is true, and `vcs.time`). A build with none of those stays `(devel)` and `unknown`.

## Config file

Search order, and only these paths:

1. `--config PATH`, when the flag is set. That file is the only one tried. A missing file is `config file not found: PATH`.
2. `./factory.toml` in the current directory.
3. `<git-root>/factory.toml` when inside a git repository (and `<git-root>` differs from the current directory).
4. `$XDG_CONFIG_HOME/factory/factory.toml`. An unset `XDG_CONFIG_HOME` means `~/.config`, so the path is `~/.config/factory/factory.toml`. An empty `XDG_CONFIG_HOME` is an error, not a fallback.

When none of those files exist, the error is `error: no config found; looked in: ...`, listing each distinct path checked in order (for example `<cwd>/factory.toml, <git-root>/factory.toml, <config home>/factory/factory.toml` when inside a subdirectory of a git repository).

Copy [factory.example.toml](../factory.example.toml), [examples/jira-monorepo.toml](../examples/jira-monorepo.toml), or [examples/github-repo.toml](../examples/github-repo.toml) and replace the placeholders, or run `factory init` inside a git repository to scaffold `<git-root>/factory.toml` (pass `--global` to append to `$XDG_CONFIG_HOME/factory/factory.toml`, `--kind jira` for a Jira source, `--dry-run` to preview, or `--force` to overwrite an existing `<git-root>/factory.toml`). A leading `~/` on `cwd`, `token_file`, `gate.hold_files`, `gh_bin`, `GH_BIN`, or a start-prompt `@` path expands to the home directory. A `~/` anywhere else is left as written.

The top-level `gh_bin` key chooses the `gh` binary for every factory call: syncing a GitHub source, the open-pull-request search, and the merged-pull-request lookups. Unset, factory reads `GH_BIN`, then uses `gh`. A leading `~/` expands to the home directory. An empty key, or a set but empty `GH_BIN`, is an error. Put the key above the first `[table]`, or TOML reads it as part of that table and the config fails with `unknown key`. `gate`, `start`, and `tick` each search your open pull requests once per run.

The database is `$XDG_STATE_HOME/factory/factory.db`, or `~/.local/state/factory/factory.db` when `XDG_STATE_HOME` is unset. `factory logs --path` prints the path actually opened. An empty `XDG_STATE_HOME` is an error.

`dry_run` defaults to true. `start`, `tick`, and `run` then print a plan and do not claim or start an agent unless you pass `--apply` (or set `dry_run = false`). When the gate refuses, that dry run still prints the plan and marks the first line `blocked  yes`. `--force` is not required. `start` then exits 1. `tick` and `run` exit 0 and also print `gate: ...`. A failed ready check with a passing gate prints no plan. `--apply` does not print the blocked plan. `link`, `unlink`, `file`, `release`, `hold`, `release-hold`, and `settle` ignore `dry_run`: asking for them is the write. `settle` syncs, finishes merged work, and does not start an agent. With no ids it uses the same merged-pull-request window as `tick` and every active claim. With ids it searches merged pull requests by the item key in the title, with no merged-after date, and an id with no merged pull request is an error. For each match it moves the tracker item to done when that item is still open and `finish_on_merge` is set, marks the claim done, removes clean worktrees, prints unblocked dependents, refreshes the mirror, and closes the item's tab last. Under `output.mode = "pr"`, a working agent leaves the tab open, and that is printed. Under `output.mode = "merge"`, self-settle from inside the worker's tab (`HERDR_PANE_ID`) closes that tab even while `working`, and external `settle` or `tick` waits briefly for a `working` agent to reach `idle` or `done` before closing the tab (leaving it open with a warning note if the wait times out or fails). A close result of `ok`, or any other success result from herdr, counts as a closed tab, and that result type is printed on its own line. An unexpected close result still moves the tracker item to done. It also prints each dependent whose blockers are now all done. An active claim whose item is already done is released even when no merged pull request is in the window. That release does not move the tracker item back to a ready state. `tick` still settles before it starts. `settle` stops after settling.

Built-in defaults are `max_in_flight` 3, `min_start_interval` 10m, `full_sync_every` 24h, `stall_after` 2h, `sync.max_age` `0`, `exclude_labels` `needs-human` and `blocked-external`, `human_labels` `needs-human`, `skip_types` `Epic`, `agent_kind` `claude`, `agent_args` empty, and `dry_run` true. `sync.max_age` is how old a source may be before `next`, `show`, `priorities`, and `doctor` sync it. `0` means never. A source `max_age` overrides the top-level value for that source only. `--no-sync` skips the refresh. `next` and `doctor` print the mirror age, with `(stale)` when a source is older than its TTL. A failed refresh warns on stderr, including that age, and still answers from the mirror. This is separate from `full_sync_every`. A mirror that has not recorded a successful sync yet prints `mirror age unknown`. In-flight work is the union of active claims (`claimed`, `started`, and `handed_off`) and open pull requests you authored (`is:pr is:open author:@me archived:false`). A claim and a pull request that are the same work count once: by the pull request URL recorded on the claim, then by the claim's recorded head branch, then by `ClosingIssues` on GitHub issues, then by the item id in the title (`pr_title_pattern`, when set) or in the head branch name. An open pull request that matches no claim still counts. The count must be below `max_in_flight`. At the limit, the gate prints `in flight: N (claims C, PRs P, overlap O) of max M`. When the gate passes, it prints `OK: N in flight (max M)` with that same count. A source that omits `exclude_labels`, `human_labels`, or `skip_types` inherits those defaults. Setting any of those lists, including to `[]`, replaces the default. The lists are not merged. Unset `human_labels` means `needs-human`, not every `exclude_labels` entry.

A sync is full when the source has never had one, when the last one is at least `full_sync_every` old, or with `--full`. A full sync rereads every item and deletes mirrored items the source no longer returns. An incremental sync reads items updated since the last one, then asks the source for the IDs that match its query now (Jira: the same JQL, key field only; GitHub: every issue number) and deletes mirrored items of that source that are not in the set. Its line is `kind:name  incremental  N items (M removed) since TIMESTAMP`. When that ID query fails, or returns no IDs while the mirror has rows for that source, nothing is deleted and `sync` prints `warning: sync kind:name: ...` on stderr. That is a warning, not a failure: the upserts still apply and the exit code is 0. After `release`, and after `settle` or `tick` finishes an item, that item's mirror row is reread at once. When the item no longer matches the source query, for example an `assignee = currentUser()` query after `release` clears the assignee, the row is removed, so `next` stops listing it before any sync.

`agent_args` is appended after `--` when `herdr agent start` runs. For example, `["--model", "opus"]`. Leave it unset, or set `[]`, and nothing is appended. `[[agent_rule]]` replaces that list for an item that has any of the rule's `labels`. The first match wins. A rule does not append, so two `--model` flags are not combined. A rule with neither `labels` nor another matcher is an error. An empty label or an empty argument is an error. `factory start ID --agent-arg=ARG` (repeatable) or `--model MODEL` replaces both the matching rule and the default for that one start. Passing both flags is an error. The dry-run plan and `next --explain` print the final args and whether a rule, the default, or the flag chose them. `tick` and `run` use the rules and the default. They have no flag.

An `[[agent_rule]]` may set `name`. A started claim records the rule that chose its args: that `name`, else the rule's first label, or `default` or `flag`. It also records the `--model` value in those args (`--model M` or `--model=M`). `--model` with no value, or twice, is an error at start. Claims from before this was recorded have neither.

The database also keeps history for reports. It records no new gh calls. `tick`, `settle`, and `verify` store each pull request they already read for a claimed item in a `prs` table, by repo and number: when it opened, merged, and closed, whether it is a draft, and when it was last seen. A time that is set never moves back or clears. When a claim reaches `done`, or a sync sees a mirrored item change to done, the item goes in a `finished` table with its title, parent, labels, finish time, and claim time. The first finish is kept. A sync that later deletes the item from the mirror leaves that row. After each successful sync, at most once an hour per source, a `backlog_snapshots` row stores how many open items are ready, blocked, in flight (an active claim or an open pull request in `prs`), and needs-human, and the count by status. Those counts use the readiness rules `next` uses, from the mirror only, without herdr tabs or a live gh search. Snapshots older than 90 days are deleted.

`factory stats` reports from that history and the mirror. It does not sync and makes no network calls. The first lines give the window and how fresh the data is: each source's last sync and the last time pull request state was seen. `--since` is the window, `7d` by default (`Nd`, `Nw`, or a duration such as `36h`). Both ends are inclusive. `--by day|week` sets the throughput bucket. Buckets start at local midnight (weeks on Monday), computed with the local calendar so a DST day is one 23- or 25-hour bucket; `--utc` buckets and prints in UTC instead. Text times are local and short, such as `2026-11-14 20:54 PST`, and durations are compact (`3d 2h`, `1d 12h`, `4m 49s`; seconds only under an hour). `--json` keeps every time in UTC RFC 3339 and adds `window.time_zone`, the zone the buckets used: an IANA name, `UTC`, or the local offset such as `-08:00`. The default is day for a window up to 14 days and week otherwise. The sections are:

The text report draws charts when the output is at least 40 columns wide: the terminal's width, or 80 when stdout is not a terminal. Narrower terminals get the tables only. Every number in the tables stays. `--color auto` (the default) colors only a terminal, and never when `NO_COLOR` is set; `always` and `never` force it. Color is only in charts and status cells. `--ascii` draws with `#`, `=`, `.` and `*` for terminals without block or braille glyphs; without color, stacked bars tell their parts apart by shade (`█▓▒░`).

Bars sit in their table as a short column, so each bar is on the row it draws. The column is `--bar-width` cells, 12 by default, and `--bar-width 0` leaves it out. Bars in one table are sized against that table's largest value, with eighth-block partial cells (`▏▎▍▌▋▊▉`) where color allows, and a value above zero always gets at least one cell. A legend line follows each table with bars.

- **Throughput:** a sparkline per series (finished, started, merged) above the table. Each day or week row ends with a stacked bar, finished (green, `█`) then started (blue, `▒`), sized against the busiest row, and one magenta `◆` per merged pull request, or `◆×N` when N is more than `--bar-width`. The total row and an all-zero row have no bar.
- **Burndown:** remaining items at the end of each bucket, for the whole scope or `--epic`. An item counts from its create time until its finish time; leaf items only, without `skip_types`. A done item with no recorded finish is left out. A braille line, 8 rows tall (`--burndown bars` draws vertical bars instead), with a legend, a projection, and a note on items with no create time. See [Reading the burndown](#reading-the-burndown). The burndown is not in `--json`.
- **Backlog:** the `now` row ends with a stacked bar of ready, blocked, in flight, and needs-human, with the legend under the table. In the change row, green is a move the right way (more ready, fewer blocked, fewer needs-human) and red the other way.
- **Epics:** a `PROGRESS` column before `TITLE`, so a long title does not push it out of line: done (green, `█`), in progress (yellow, `▓`), remaining (gray, `░`), sized against the largest epic in that table. Each epic is listed once. The `--epic` tree colors each node's state the same way.
- **Flow:** an open draft's age is yellow over 1 day and red over 3 days.

- **Throughput** per bucket: rows in `finished`, claims started (a failed start does not count), and pull requests opened and merged from `prs`.
- **Backlog**: the current ready, blocked, in-flight, and needs-human counts and the counts by status, as in the snapshots, and the change since the snapshot nearest the window start. The change is `n/a` when a source has no snapshot, and under `--epic`, because snapshots are not kept per subtree.
- **Epics**: for each direct parent, its children; for each top-level parent, its leaf descendants. Each row has done, in progress (an active claim or an open pull request), remaining, % complete, and items created in the window. Rows sort by remaining, descending. `*` marks a `parent_priority` id. Counts cover only items in the source queries, plus finished history.
- **Flow**, for pull requests merged in the window: the median and p90 (nearest rank) of claim to the item's first pull request opened, and of pull request opened to merged. Then the open draft pull requests with their age, oldest first.
- **Split**, with `--split model` or `--split rule`: throughput totals and flow medians per model or agent rule. A claim from before those were recorded, or an event with no claim, is `unknown`. A recorded claim whose args named no model is `default`.

`--epic ID` limits every section to that item and its descendants in the mirror and finished history, and prints the tree with each node's state. `--json` prints one object per section (`throughput`, `backlog`, `epics`, `flow`, `split`, plus `window`, `freshness`, and the `--epic` `tree`), with durations in seconds and times in RFC 3339.

The optional `[gate]` table lists `ignore_panes` (pane ids) and `ignore_tabs` (tab labels). The start gate skips those. It also skips the pane named by `HERDR_PANE_ID`, which herdr exports for the pane running factory. Factory reads that variable once, when it loads the config. A working agent counts only when its `cwd` or `foreground_cwd` is a configured source `cwd` or a subdirectory of one. An agent outside every configured checkout does not count. `--log-level=debug` names each working agent ignored for that reason. A refusal names every working tab that is still counted. When that tab still contains the caller's pane, the line ends with `(this pane)`.

`base_branch` is optional and is not the source `base` field. Leave it unset and the gate does not check the branch, and it runs no git command beyond `git status --porcelain`. Set it with `require = "on_base"` to refuse unless HEAD is that branch, or `require = "clean_against_base"` to refuse when HEAD has commits that are not on `origin/<base_branch>`. A checkout that is only behind that ref still passes. The refusal names the current branch and the expected one. The check reads the local remote-tracking ref and does not fetch. Any other `require`, or setting only one of the two keys, is an error.

`hold_files` is a list of paths. While any path exists, `gate`, `start`, `tick`, and `run` refuse and name it. An empty file still holds. A line `owner: NAME` is the owner. Every other non-empty line is the reason. Both are included in the refusal. When `hold_files` is empty, the gate and `factory hold` use `~/.local/state/factory/hold`. That default does not follow `XDG_STATE_HOME`.

`hold_ttl` is a duration. Leave it unset and the age is not checked. When it is set, a file older than that duration is stale: the gate still refuses, and the refusal says the hold is stale and names its age. A file of exactly that age is not stale.

`hold_tabs` is a list of tab labels. A matching tab that exists and is not `done` refuses the start. The refusal names the tab and its status (`hold tab LABEL (ID) is STATUS`). A missing tab, or a tab whose status is `done`, does not hold. `ignore_tabs` and the caller's pane do not exempt a hold tab.

`factory hold` creates that one hold file and prints the path. `--owner` and `--reason` are optional. `factory release-hold` removes the file and prints the path. If `hold_files` has more than one path, both commands fail and name the count; they do not guess. Neither command calls herdr, gh, or git. `start --force` still skips the whole gate, including holds.

A value factory does not understand is an error. The message names the field, what it expected, and what it found. It does not fill in a quiet default for that field.

### Reading the burndown

```text
Burndown
465 │⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠔⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉
    │
  0 │
    └────────────────────────────────────────────────────────────
     11-02                                                  11-09
─ remaining items (end of each day)
14 items have no create time. They count as remaining from 2026-11-01, so the start of the line may be up to 14 too high.
projection: 465 remaining ÷ 10.6 finished/day (last 7 days) ≈ 44 days → done around 2026-12-23
assumes the same finish rate and no new items; not drawn on the chart
```

- **The line** is the only series by default: remaining items at the end of each day or week. `--burndown bars` draws the same counts as vertical bars, with `█ remaining items (end of each <bucket>)` as the legend.
- **`--burndown-pace`** adds a dotted line straight from the first bucket's remaining count down to 0 at the last bucket, and the legend line `┄ pace to reach 0 by <last date> from <first value> (reference only)`. It is not a target anyone set, and not a forecast; with a growing backlog it only shows how far off a straight burn would be. Bars mode ignores it. With `--ascii` both lines are `*`, and the legend names the pace line `pace (the straight line)`.
- **The projection** is `remaining now ÷ (items finished in the last 7 days / 7)` days from now. It assumes the same finish rate and no new items, and it is not drawn on the chart. With no finish in the last 7 days it says `no projection: nothing finished in the last 7 days`; with nothing remaining, `no projection: nothing remains`. `--ascii` writes `/`, `~`, and `->` for `÷`, `≈`, and `→`.
- **Items with no create time** count as remaining from the window start, so the start of the line may be up to that many too high, and the line never steps up when they were really created. The note says how many.

### Which sources commands scope to

`next`, `stats`, `doctor`, `priorities`, `close`, `logs`, `sync`, `gate`, `tick`, and `run` default to the source of the current directory. With no `--source` or `--all`, when the current directory is a configured source's `cwd` or below it, they scope to that source and print one line on stderr (`showing` for read-only commands, `scoping to` for `sync`, `gate`, `tick`, and `run`):

```text
factory: showing github:agentic-factory (current directory); --all for every source
```

Paths are compared after `~` expansion and symbolic links are resolved. When `cwd`s are nested, the deepest one wins. When two sources share that `cwd`, neither is picked.

When neither `--all`, `--source`, nor `--config` is passed and no configured source matches:

- Inside a git repository (`<git-root>` found), the command fails (exit 1) naming the repository root and advising `factory init`, `--all`, or `--source KEY`.
- Outside any git repository, every source in the config is included and no scoping line is printed.

- `--all` includes every source. `--all` with `--source` is a usage error (exit 2).
- `--source KEY` (kind:name) selects that source wherever you are.
- `stats --json` adds a top-level `"source"` when the report covers one source. `next --json` and `doctor --json` are lists, so only the stderr line says which source was picked.
- `logs` keeps a run whose detail names a source (`sync`, `start`) only when it names this one. A run that names no source, such as `tick` or `file`, is always shown. `logs --source` and `logs --all` read no config; the current-directory default needs one, and without it `logs` shows every run.
- `next --explain ID` names one item and is not narrowed.
- On `gate` with `[[lane]]` configured, `--all` keeps the existing lane checkout rule, and `--source` is still refused because each lane has its own checkout.
- `start`, `settle`, `file`, `adopt`, `release`, `show`, `owner`, `verify`, `link`, `unlink`, `hold`, and `release-hold` are unchanged.

### Worker, lease, and output tables

Three optional tables configure orthogonal worker axes at the top level, with per-source (`[source.worker]`, `[source.lease]`, `[source.output]`) and per-lane (`[lane.worker]`, `[lane.lease]`, `[lane.output]`) overrides. A lane key overrides its source, and a source key overrides the top level.

```toml
[worker]
spawner     = "agentbus" # "agentbus" (default), "subagent", or "herdr"
agent       = "claude"   # coding agent (for example "agy", "claude", "codex", "grok")
agent_args  = []         # arguments passed to the coding agent
max_workers = 1          # parallel running workers cap, orthogonal to spawner and max_in_flight
worktree    = false      # isolate each worker in its own git worktree

[output]
mode = "pr"              # "pr" (default) or "merge" (rebase onto base and push linear history)

[claim]
require_leaf       = true               # refuse skip_types and items with open children
require_unblocked  = true               # refuse items with open BlockedBy blockers
source_containment = true               # enforce FACTORY_ITEM_SOURCE and current git repo scope
assign             = true               # take assignee ownership on claim (default true on Jira & GitHub)
ownership_rule     = "unassigned_or_me" # "unassigned_or_me" (default), "unassigned", or "any"
mark_in_progress   = true               # mark issue in-progress on claim (false allows empty status/label)
in_progress_rule   = "block"            # "block" (default) or "allow"
create_label       = false              # GitHub: create claim label if missing
comment            = true               # post claim comment on start
nickname           = ""                 # optional machine nickname: [a-z0-9][a-z0-9-]{0,31}
comment_template   = ""                 # placeholders: {agent}, {nickname}, {timestamp}, {id}, {source}
```

- **`[claim]`**: Configures start safety invariants (`require_leaf`, `require_unblocked`, `source_containment`, all `true` by default), ownership (`assign`, default `true` on both Jira and GitHub; `ownership_rule`: `"unassigned_or_me"` default, `"unassigned"`, or `"any"`), in-progress marking (`mark_in_progress`, default `true`; `in_progress_rule`: `"block"` default or `"allow"`; `create_label`, default `false` on GitHub), and claim comments (`comment`, default `true`; optional `nickname` matching `^[a-z0-9][a-z0-9-]{0,31}$`; optional `comment_template` supporting `{agent}`, `{nickname}`, `{timestamp}`, `{id}`, and `{source}`). Per-source `[source.claim]` overrides the top-level `[claim]` and may also set `status`, `transition` (Jira), and `label` (GitHub/Jira). Legacy `[[source]]` keys (`claim_assign`, `claim_status`, `claim_transition`, `claim_label`, `assignee_ok`) remain supported and reconcile with `[source.claim]`, but setting both a legacy key and its `[source.claim]` counterpart on the same source is an error. When `mark_in_progress = false`, `claim_status` (Jira) and `claim_label` (GitHub) are not required to be non-empty.

- **`[worker]`**: `spawner` is `"agentbus"` (the default: spawn a new worker through agentbus with `agent`), `"subagent"` (claim the item and emit the brief for the coordinator's subagent tool), or `"herdr"` (open a herdr tab directly). `agent` names the coding agent (default `"claude"`, or `[defaults] agent_kind` when set; setting both `worker.agent` and `defaults.agent_kind` at the top level is an error, and the same holds for `worker.agent_args` and `defaults.agent_args`). `max_workers` (default `1`, at least `1`) caps concurrent running workers orthogonally from `spawner` and `max_in_flight`; it may be overridden in `[source.worker]`, while a `[[lane]]` sets `max_workers` on the lane itself. `worktree` (`false` by default) toggles per-worker git worktree isolation: a dry-run `start` prints the planned worktree path and branch, an applied `start` creates the worktree, records it on the claim, passes it as `cwd` to `herdr`/`subagent`, and cleans it up if pre-run agent start fails. With `worktree = true`, uncommitted changes inside another claim's isolated worktree do not fail `gate` (base-branch checks still apply to the main checkout), `verify` runs its status, merge-base, and stash checks against the claim's recorded worktree, and `settle` and `release` check `git status --porcelain` before running `git worktree remove`—if the worktree has uncommitted changes, it is left on disk with a warning naming the path.

When `spawner = "subagent"`, `factory start <id> --apply` claims the item, records `spawner = "subagent"` (and `worktree` when enabled) on the claim, skips creating a `herdr` tab or starting a `herdr` agent, and emits the worker brief (`id`, `source`, `output`, `FACTORY_ITEM_SOURCE`, `FACTORY_OUTPUT_MODE`, `cwd`, `agent`, `agent_args`, and the full rendered `start_prompt`). Pass `--json` on `factory start` (supported for both dry-run and `--apply`) to receive a single JSON outcome object with `id`, `status`, `spawner`, `output_mode`, `keep_worker`, `source`, `cwd`, `agent`, `agent_args`, `env`, `worktree`, `brief`, `claim_time`, and `exit_code` for programmatic coordinator consumption (`keep_worker: true` under `mode = "pr"`, `keep_worker: false` under `mode = "merge"`). `factory show` displays `spawner: subagent` without looking up a tab, `factory doctor` does not flag active `subagent` claims as `dropped-claim` or `stalled`, and `factory settle` marks finished `subagent` claims `done` without closing any `herdr` tab.
- **`[output]`**: `mode` is `"pr"` (the default: create a pull request) or `"merge"` (rebase onto `base` to keep history straight, fast-forward merge, and push to `origin/<base>`). `start` passes `FACTORY_OUTPUT_MODE=<pr|merge>` alongside `FACTORY_ITEM_SOURCE` in the worker environment and reports `output_mode` and `keep_worker` in `--json`. Under `mode = "pr"`, workers stay alive for review feedback until settled (`keep_worker: true`). Under `mode = "merge"` (`keep_worker: false`), the dry-run plan marks `output   merge (exit worker on completion)` and the prompt instructs workers to exit on completion: local tab workers run `factory settle <id>` as their final command, remote-lane tab workers close their own tab (`herdr tab close "$HERDR_TAB_ID"`) after reporting back, and subagent workers return immediately so the coordinator can run `factory settle <id>` and discard them. Under `mode = "merge"`, `verify` skips pull-request and `pr_template` checks and instead verifies that the checkout (or claim worktree) is clean, `git stash list` matches the recorded stash count, and `origin/<base>` contains a linear commit (no merge commits) referencing the item. `settle` and `tick` settle `mode = "merge"` claims once a matching linear commit lands on `origin/<base>` (transitioning Jira when `finish_on_merge = true`, closing idle tabs, cleaning up clean worktrees, and recording `finished` history), `settle <id>` errors if no linear commit has landed yet, `show` displays `output: merge` and the landed commit on `origin/<base>`, and `doctor` checks `origin/<base>` before flagging `dropped-claim` or `stalled` (suggesting `factory settle <id>` when a landed commit exists).

### Checkout lease

The gate also reads the [agentbus](https://github.com/jaeyeom/experimental/tree/main/devtools/agentbus) workdir lease of the checkout it would start work in. The optional `[lease]` table sets how:

```toml
[lease]
gate = "auto"      # or "off" or "required"; top level only
mode = "off"       # or "prompt" or "prompt+check"
claim_for = "45m"  # the --for estimate written into the rule
restore = false    # include agentbus lease restore alongside claim
```

`gate` (below) is how the gate reads the lease. `mode`, `claim_for`, and `restore` are how `start` tells the worker about it. They are separate because the gate guards a checkout, which several sources may share, while the rule is part of one source's start prompt. `gate` is allowed in the top-level `[lease]` only. `mode`, `claim_for`, and `restore` are also allowed per source, as `[source.lease]` after that `[[source]]`, and per lane, as `[lane.lease]` after that `[[lane]]`. A lane key overrides the source's, and a source key overrides the top level. A key a lane leaves out keeps the source's value. `restore = true` requires `mode` to be `"prompt"` or `"prompt+check"`; overriding `mode = "off"` on a source or lane clears an inherited `restore = true` unless `restore = true` is also set there (which is an error).

`mode` is `off` by default: the start prompt is exactly what `start_prompt` renders, and `start` never runs `agentbus`. Any other value than `off`, `prompt`, or `prompt+check` is an error. `claim_for` defaults to `45m` and must be a positive duration.

With `mode = "prompt"`, `start`, `tick`, and `run` put this rule at the top of the start prompt, followed by a blank line and the `start_prompt` text, so the worker reads it before it can act:

```
Before changing the shared checkout (<checkout>), run:
  agentbus lease claim --for <claim_for> --why '<id> <title>'
If it says queued, end your turn and wait for the "[lease] granted" message.
Run `agentbus lease release` when the tree is clean and pushed. Reading needs no claim.
```

`<checkout>` is the lane's `checkout` when a lane was picked, and otherwise the source `cwd`. `'<id> <title>'` is quoted for a POSIX shell, so a title with quotes or `$` is passed as written. A `{{lease_rule}}` placeholder in `start_prompt` puts the rule there instead of at the top. With `mode = "off"` the placeholder expands to nothing. The dry-run plan prints the rule after the `prompt` line, under `lease    MODE, rule at the top of the prompt:` or `rule at {{lease_rule}}:`.

`mode = "prompt+check"` is `prompt`, and an applied start also runs `agentbus lease status --path <checkout> --json` just before it claims and launches. It prints one line, such as `lease: free` or `lease: held by pm/foo until 14:20 (1 queued)`, and records the state, holder, deadline, and queue length under `lease` in the start run's detail (`factory logs`). It never refuses the start; refusing is the gate's job. When `agentbus` is missing or fails, when the checkout is not guarded, or when the checkout is on a remote lane, the line is `lease: unknown (<why>)` and the start goes on. A dry run does not read the lease.

`gate` is `auto` by default. Any other value than `auto`, `off`, or `required` is an error. Under `auto` and `required`, `gate`, `start`, and `tick` run `agentbus lease status --guarded --json --path CHECKOUT`, which answers from the local agentbus config without contacting the daemon, and then, for a guarded checkout, `agentbus lease status --path CHECKOUT --json`. The checkout is the source `cwd`, or a local lane's `checkout`. The lease adds to the herdr and git checks; it does not replace them.

The checkout is usable when the lease is `free`, or when it is `held` by the agent `start` is starting. That agent is recognised by its lease `--why`, whose first word is the item id (the start-prompt lease rule writes it that way), or by a registered agentbus name equal to the herdr agent name. `factory gate` and `tick` start no particular item, so for them only `free` is usable. Any other state refuses, and the reason names the holder, the deadline as local `HH:MM`, and the queue length, for example `/src/repo: lease held by pm/foo until 14:20 (1 queued)`, `/src/repo: lease paused by user (0 queued)`, or `/src/repo: lease needs-adopt (no holder, 2 queued)`.

When the lease cannot be read, `auto` keeps the herdr and git checks and prints a note, and `required` refuses. Missing `agentbus`, an unreachable hub, a failed call, output that is not the documented status shape (an unknown `state`, for one), and a checkout that is not in agentbus `lease_checkouts` all count as a lease that cannot be read. `gate` prints the note after its result as `note: CHECKOUT: lease not read (WHY); used the herdr and git checks only`. `start` prints it after the plan as a `note` line. `tick` logs it. `off` never runs `agentbus`.

agentbus runs on this machine and cannot resolve a checkout on another herdr machine, so a remote lane's lease is not read under any setting. Its gate line ends `(checkout and lease not checked on remote machine MACHINE)`, and that is not a refusal.

### Starting the agent and sending the prompt

An applied start creates the tab with `--env FACTORY_ITEM_SOURCE=KIND:NAME`, then runs `herdr agent start`. While the new pane's shell is still busy, for example loading a slow `.envrc`, herdr answers `agent_pane_busy`; factory retries with backoff (1s, 2s, 4s, ...) for up to `[defaults] agent_busy_timeout` (default `20s`). When the agent start still fails, the agent never ran, so factory closes the tab it opened, releases the claim (the tracker item goes back and the claim row is `released`), and prints `...; closed tab TAB and released ID; re-run: factory start ID --apply`. A plain re-run works, with no `--force` and no manual tab close.

Next it runs `herdr agent wait PANE --until idle --timeout MS` (`agent_ready_timeout`, default `60s`) and pauses `agent_ready_settle` (default `1s`, `0s` for none) so the agent's input box accepts the prompt. Then it sends the prompt with `--wait --until working`.

herdr reports `agent_prompt_stalled` (or `timeout`) when it saw no `working` state in time. That does not prove nothing was typed, so factory reads the agent before any retry, and tries at most once:

- the agent is `working` or `blocked`: the prompt went through; nothing more is done;
- the agent is `idle` and the prompt's first and last lines are on its screen: the text sits unsubmitted, so factory sends Enter (`herdr agent send-keys PANE enter`) and waits up to 15s for `working` or `blocked`;
- the agent is `idle` and the prompt is not on screen: factory sends the prompt once more;
- anything else: no retry; the error quotes the end of the screen.

The prompt is never sent a third time. A start is recorded `started` only after `working` or `blocked` was seen. Otherwise the claim is marked `failed` and the tab is left open for inspection. The start run's detail records `ready_wait_ms`, and `stalled` and `recovery` (`enter`, `resent`, or `none`) when the prompt stalled. On a remote lane every one of these herdr calls runs with `--machine MACHINE`.

### Waking a hibernated agent

A prompt typed into a pane whose agent agentbus has hibernated lands in the shell, not the agent. So before every `herdr agent prompt`, an applied `start` (and `tick`) runs `agentbus wake --if-hibernated --wait --machine MACHINE PANE`. `MACHINE` is `local`, or the lane's herdr machine. agentbus prints nothing and exits 0 when the agent is awake. When it relaunched one, the start run's summary in `factory logs` reads `started ID (woke hibernated agent)` and its detail has `"woke": true`.

`[defaults] wake_before_prompt` is `"auto"` (the default) or `"off"`. Any other value is an error. Under `auto`, a missing `agentbus` skips the wake and the prompt is sent as before. A wake that fails, or times out, fails the start at step 6 with `wake before prompt: ...; prompt not sent`; the item stays claimed, as for any failed launch step. `off` never runs `agentbus wake`.

### Lanes

A lane is a herdr machine with its own checkout, its own worker cap, and the item labels routed to it. Leave `[[lane]]` out and nothing changes: there is one implicit lane, the local herdr server and each source `cwd`, and every command prints what it printed before lanes existed.

```toml
[[lane]]
name = "linux"
machine = ""            # empty = local herdr server
checkout = "/path/to/checkout"
max_workers = 4
labels = ["needs-linux", "high-resource"]

[[lane]]
name = "mac"
machine = "mac1"        # herdr saved machine label or id
checkout = "/path/on/mac/checkout"
max_workers = 2
# max_active = 4        # optional cap on every open claim, handed off or not
# exec = ["ssh", "mac1"] # runs verify's checkout checks on that machine
labels = ["needs-mac"]

[lanes]
default = ["linux", "mac"]   # preference order for unlabeled items
```

Every lane sets `name` (unique, no spaces or colons), `checkout`, and `max_workers` (at least 1). `machine`, `labels`, `max_active` (at least 1 when set), and `exec` are optional. `exec` is for a remote lane only: a command prefix that runs a plain command on that machine, such as `["ssh", "mac1"]` or `["ssh", "-o", "BatchMode=yes", "user@mac1"]`. herdr cannot run a plain command, so `factory verify` uses it: it runs `PREFIX 'true'` as a reachability probe, then `PREFIX 'git' '-C' 'CHECKOUT' ...` for the branch, clean tree, stash count, merge-base, and commit time, and `PREFIX 'cat' '--' 'CHECKOUT/TEMPLATE'` for the pull request template. Each argument after the prefix is single-quoted, because ssh hands the command to the remote shell; the prefix must run its arguments through a POSIX shell, as ssh does. Output starts with `checked on MACHINE:CHECKOUT`. When the probe fails, times out, or exits non-zero (ssh exits 255 when it cannot connect), verify exits 1 with `verify ID: machine MACHINE unreachable: ...` and never prints `ok`. A claim on a remote lane without `exec` is an error that says to set it. A local lane's `checkout` must be an existing directory, and a leading `~/` expands. A remote lane's `checkout` is a path on that machine and is not checked or expanded. One label may belong to one lane only. `[lanes] default` is required with lanes, and lists each lane at most once. `[lanes]` without a `[[lane]]` is an error.

Routing: an item with a lane's label goes to that lane only. An item whose labels match more than one lane is a config error. `next` and `start` report it and name each lane and label. An item with no lane label tries `[lanes] default` in order and takes the first lane that passes its gate. `next --explain ID` prints `lanes  <names>`, the lanes the item may use in that order.

`factory gate` checks the global conditions once: hold files, hold tabs (against the local herdr server's tabs), `max_in_flight`, and `min_start_interval`. It prints that result as it does without lanes, either `OK: <n> in flight (max <limit>)` or one reason per line. Then it prints one line per lane, in config order:

```text
OK: 2 in flight (max 5)
linux: OK workers 2/4 (running), active 3
mac: unreachable (skipped): host unreachable: ssh: connect to host mac1 port 22: Operation timed out
```

The lane checks are the working agents on that machine whose `cwd` or `foreground_cwd` is inside the lane checkout, the checkout state, `max_workers` against the lane's running workers, and `max_active`, when set, against all of its active claims. A claim recorded before lanes were configured counts toward `max_in_flight` but toward no lane. Every line shows both counts, `workers R/MAX (running), active A` (`active A/MAX` with `max_active`). A failing lane prints `NAME: blocked workers R/MAX (running), active A: reason; reason`.

An active claim on the lane is a running worker when its start is still under way (`claimed`), when its tab is still open on the lane machine whatever the agent's status, when it holds the lane checkout's agentbus lease, or when it is `started` and the lane's tabs could not be read. A `handed_off` claim whose tab is closed and that holds no lease is not running: its pull request is waiting for review, and it no longer takes a worker slot. The lease is the local lane's `agentbus lease status` (read once for both the lease check and the count), or, for a remote lane, the `MACHINE:CHECKOUT` entry of the hub's `agentbus lease status --all --json`. `[lease] gate = "off"` reads neither. When the remote list cannot be read, the count leaves the lease out and the line's note says `lease list not read (WHY)`. An unreachable lane counts its `claimed` and `started` claims as running. git runs locally, so a remote lane's checkout is not read: the line ends `(checkout not checked on remote machine MACHINE)`, and that is not a refusal. The calling pane in `HERDR_PANE_ID` is skipped on a local lane only. `gate --source` is a usage error with lanes, because each lane has its own checkout.

A remote lane's herdr list calls time out after 15 seconds. A lane whose machine does not answer, or whose herdr call fails, is skipped, not an error. factory then runs `herdr machine status --json MACHINE` and says which case it is: `host unreachable: ...` when that status reports an error, or `authentication locked (unlock your SSH/gpg agent): ...` when the host answers but the call hung or failed. A local herdr failure is still an error.

`factory start ID` uses the first routed lane that passes. `--machine VALUE` takes a lane name, or a herdr machine that exactly one lane uses, and is refused when that lane is not one the item's labels route to. Every herdr call for the worker runs as `herdr --machine MACHINE ...`, the tab and workspace are created with that lane's checkout as `--cwd`, and the plan adds `lane     NAME (herdr --machine MACHINE)` or `lane     NAME (local herdr)`. The claim records the lane. A remote lane records no stash count, because git cannot read that checkout from here. The ready check for an existing tab also reads the remote lane's tabs. When no lane passes, the gate refuses with the global reasons and one `lane NAME: ...` part per lane, and a dry run still prints the blocked plan for the first lane that answered. `--force` skips every gate check and takes the first routed lane that answers. `--machine` without lanes is a usage error.

`factory adopt ID` records the lane too. It reads tabs and panes on the lane's machine. The lane is the open claim's lane when there is one. Otherwise `--machine` picks one of the item's routed lanes, or the item must route to exactly one lane. `show` reads the item's tab on the claim's lane and prints `lane: NAME` in the claim section. `verify` checks the checkout of the claim's lane. On a remote lane it runs those checks on that machine through the lane's `exec` prefix (see below); the GitHub checks (the pull request, draft, labels, reviewers, and body sections) still run locally.

`factory tick` (and each tick of `factory run`) with lanes syncs and settles as without them, then checks the global gate once: hold files, hold tabs, `max_in_flight`, and `min_start_interval`. A global refusal prints one `gate: ...` line and starts nothing. Otherwise it walks the lanes in order, the `[lanes] default` names first and then the other lanes in config order, and for each lane prints one line and records one run row:

- `started ID in WORKSPACE/TAB (lane NAME)`, or `dry-run ...` without `--apply`, for the top ready item routed to that lane (a lane label routes an item to that lane only; an unlabeled item may go to any `[lanes] default` lane). An item one lane took is not offered to the next.
- `lane NAME: gate: REASONS` when the lane's own check refuses, for example `max_workers` reached or a dirty local checkout.
- `lane NAME: skipped: WHY` when the lane's machine does not answer, with the same `host unreachable` or `authentication locked` explanation as `factory gate`.
- `lane NAME: no ready items`.
- `lane NAME: gate: max_in_flight M reached by this tick's starts` once this tick's starts have used the room under `max_in_flight`.

So a tick starts at most one item per lane. The start uses that lane's machine and checkout as `factory start` does, and the start run's detail and summary name the lane. Without `[[lane]]` config, `tick` starts at most one item, as before.

`factory settle` (and the settle step of `tick`) closes each item's tab on the lane recorded on its claim, running `herdr --machine MACHINE` for a remote lane. The tab lines end `on lane NAME`, and each claim settled on a lane also writes a `settle` run row, `settled ID (lane NAME)`. When a remote lane's machine does not answer, the claim is still marked done and settle prints `lane NAME: tab for ID not checked: WHY` instead of closing the tab. A claim whose recorded lane is no longer configured is an error.

### Jobs: `tick --enqueue` and `owner`

`factory tick --enqueue` is a producer for the jobs layer (jaeyeom/experimental#876). It syncs and settles like `tick`, then, instead of the start gate and a start, prints one `start-item` job per ready item, in rank order, as one JSON line each on stdout. The summary line (`enqueued N start-item jobs`, or `no ready items`) goes to stderr, so stdout can be piped to a queue. It claims nothing, opens no tab, and runs no git check; the job runner's `factory start --apply` applies the gate when it runs. `--enqueue` with `--apply` is a usage error. Without `--enqueue`, `tick` is unchanged.

```json
{"kind":"start-item","subject":"PRJ-1","source":"jira:acme","title":"Ship PRJ-1","dedupe":"start-item:jira:acme:PRJ-1","priority":1,"needs":{"machine":"mac1","affinity":"lane","checkout":"/path/on/mac/checkout","executor":"new-tab"},"command":["factory","start","--apply","--machine","mac","PRJ-1"]}
```

`dedupe` is the kind, source, and id, so the queue keeps one live job per item. `priority` is the rank in this tick, 1 first. A job has no gate: gates come from the jobs policy only. `needs.machine` and `needs.affinity` follow the jobs affinity rules, most specific first:

1. The item has a lane label: `affinity` is `lane` and `machine` is the lane's herdr machine, or this host for a local lane. `checkout` is the lane's checkout, and `command` passes `--machine LANE`.
2. The item was claimed before (any state, the latest claim): `affinity` is `owner` and `machine` is that claim's owner, as `factory owner` prints it.
3. Otherwise `machine` is `any` and there is no `affinity`.

`checkout` is the source `cwd` unless a lane label set it, or `none` when that is empty.

`factory owner ID` prints the machine and tab of the item's latest claim, from the local store only, so it answers for an item that has left the mirror: `jira:acme PRJ-1: machine desk, lane -, tab t7, workspace w2, done since 2026-11-01T10:00:00Z`. The machine is the lane's herdr machine for a claim on a remote lane that is still configured, and otherwise the host that made the claim. An item never claimed prints `no owner: ID was never claimed`. `--json` prints `{"source", "id", "machine", "lane", "tab", "workspace", "state", "claimed_at"}`, with empty `lane`, `tab`, and `workspace` left out, or `null` for an item never claimed. Both exit 0. An id claimed in two sources needs `--source`.

### Where `factory file` files

`factory file` files only into the source of the item the caller is working on. `factory start` opens every worker's tab with `FACTORY_ITEM_SOURCE=KIND:NAME`, the item's source. In a worker, `file` files into that source. A `--source` naming any other source is refused outright, `file: this item is from SRC; factory never files into OTHER`, and no flag overrides it. When `FACTORY_ITEM_SOURCE` names a source this machine's config does not have, `file` refuses with `file: source SRC is not configured on this machine; report back to your coordinator instead`. Without `FACTORY_ITEM_SOURCE` (a coordinator running `file` itself), `--source` is required; factory never picks a source, even when only one is configured. A follow-up for a public GitHub repository is filed only when that repository is the item's own source, and `file` prints `factory: warning: OWNER/REPO is a public repository; this follow-up will be public` first. There is no public opt-in. Tool issues for public tool repositories are filed by a person, never by factory.

Workers on remote lanes do not run `file`, `link`, or `release`. They report follow-ups back to the coordinator (`{report_back}`), who files them on the machine that has the item's source.

### Jira API token

Jira requests use HTTP basic auth: the `email` field and an API token ([design.md](design.md) §3.1).

1. Open <https://id.atlassian.com/manage-profile/security/api-tokens> and create an API token. Copy it on that screen. Atlassian does not show it again. An unscoped token is what this basic auth sends. A scoped token has to be allowed to read and update Jira issues.
2. Write the token by itself in the file named by `token_file`. Leading and trailing whitespace is stripped. Do not wrap it in quotes.
3. `chmod 600` that file. If the mode lets group or others read it, the command that contacts the source fails. The error names the path and says `chmod 600`. Loading the config does not read the file.

Set exactly one of `token_file` and `token_env`. `token_env` is the name of an environment variable that holds the token. An unset variable is an error when a command contacts the source, not when the config loads. `chmod` applies to `token_file` only. `next`, `show`, `doctor`, `gate`, `logs`, and `priorities` read the local mirror and do not need the token when `sync.max_age` is `0`. A positive `max_age` makes `next`, `show`, `doctor`, and `priorities` contact a stale source before they answer. `sync` also fetches blockers that are not in the mirror (`external_blocker_lookups`); `doctor` and readiness read what the last sync stored and fetch nothing themselves. `priorities --apply` contacts the source.

### Start prompt

The placeholders are `{id}`, `{title}`, `{url}`, `{source}`, `{base}`, `{parent}`, `{caller}`, `{caller_bus}`, and `{report_back}`, plus `{{lease_rule}}` (which places the [checkout lease](#checkout-lease) rule), `{{trust_rule}}` (which places the untrusted-input rule when `trust.environment = "untrusted"`, and expands to nothing otherwise), and `{{start_prompt_append}}` (which places `start_prompt_append`). Any other `{word}` is an error. A brace that is not one of those placeholders is an error. A prompt that starts with `@` is a file path; factory reads the file and uses its bytes as the prompt.

`start_prompt_append` adds standing lines to every start prompt without duplicating them across sources. It is either a list of strings (joined with newlines) or `@path` to a file, and can be set in `[defaults]`, per `[[source]]`, or per `[[lane]]` (a lane overrides its source, which overrides `[defaults]`; `[]` clears an inherited value). The lines are placed directly after the built-in lease rule as their own paragraph before `start_prompt`, at the top when lease mode is `off`, or where `start_prompt` has `{{start_prompt_append}}`. The same placeholders as `start_prompt` are expanded. When unset or empty, the prompt is unchanged. The dry-run plan prints the lines under `append`.

```toml
[defaults]
start_prompt_append = [
  "If you have a question or hit a blocker, send it to your coordinator ({report_back}) instead of asking the user directly.",
]
```

`{caller}` is who started the run, so the agent can report back. `start`, `tick`, and `run` take `--caller NAME`. Otherwise factory uses `[defaults] caller`. Otherwise it uses the pane id read from `HERDR_PANE_ID` when the config was loaded. It does not read that variable again. An omitted or blank `[defaults] caller` is not used. When the flag, that config value, and the pane id are all empty, `{caller}` expands to an empty string and factory logs a warning that names the placeholder. It does not substitute a hostname or the word `unknown`. An empty `--caller` is an error.

`{caller}` is a herdr pane id, which only means something on the caller's machine. A worker on a remote lane cannot reach it. `{caller_bus}` is the caller's agentbus name, or its handle `herdr:MACHINE/SESSION/PANE` when it has no registered name, as printed by `agentbus whoami`, run in the caller's environment. `{report_back}` is a ready-made instruction: `agentbus send CALLER_BUS "<message>"` when `{caller_bus}` is set (agentbus delivers across machines and wakes a hibernated caller), else `herdr agent prompt CALLER "<message>"`. factory runs `agentbus whoami` only when some `start_prompt` uses `{caller_bus}` or `{report_back}`. Without agentbus both fall back as above, `{caller_bus}` expanding to empty. Another agentbus failure does the same and logs a warning that names it. When a start goes to a remote lane and the prompt uses `{caller}` without `{caller_bus}` or `{report_back}`, the plan ends with `warning: start_prompt reports back with {caller}, ...`.

[examples/jira-monorepo.toml](../examples/jira-monorepo.toml) tells the agent to branch from `origin/{base}`, then run `/implement-jira-ticket {id}`. It also sets `require_labels` to `agent-ready` and two `workspace_rule` entries (one `parent`, one `label`). [examples/github-repo.toml](../examples/github-repo.toml) sets `start_prompt` to `/gh-issue-resolver {url}` and does not set `workspace`, so the workspace label is the repo name (`yourrepo` for `you/yourrepo`).

### Fields a source must set

Jira: `name`, `kind = "jira"`, `site` (an `https` URL with a host), `email`, `token_file` or `token_env`, `jql`, `claim_status`, `cwd` (an existing directory), `base`, and `start_prompt`.

GitHub: `name`, `kind = "github"`, `repo` (`owner/name`), `cwd`, `base`, and `start_prompt`. Leave `workspace` empty to use the repo name. By default (when neither `claim_status` nor `claim_label` is set), GitHub sources use native GitHub issue statuses via `gh api graphql`: `claim_status` defaults to `"In progress"`, `ready_statuses` defaults to `["Todo", "Backlog", "OPEN"]` (`OPEN` covers open issues whose native status is unset/null; `"In progress"` and `"In review"` are excluded), and `factory release` restores the first non-`OPEN` entry in `ready_statuses` (`"Todo"` by default). GitHub sources also read native issue fields by default: `priority_field` (`"Priority"`, mapping `Urgent`=0, `High`=1, `Medium`=2, `Low`=3, overridable via `priority_ranks`, and written by `factory priorities --apply` via `setIssueFieldValue` when `priority_labels` is empty), `defer_field` (`"Start date"`, read into `NotBefore`), and `due_field` (`"Target date"`, read into `Due` ahead of `milestone.dueOn`). Set any field key to `""` to disable it. Legacy `claim_label`, `priority_labels`, and `defer_label_prefix` remain supported as fallbacks, and `factory doctor` verifies `gh` version (>= `2.0.0`), repository `planningStatuses`, and `issueFields`, warning when legacy label keys can be removed.

Each `workspace_rule` sets `workspace` and exactly one of `parent` or `label`. The first match wins. A Jira item with no match uses the source `workspace`, then `factory`. A GitHub item uses `workspace` or the repo name. The Jira tab label is the issue key. The GitHub tab label is `#<number>`, in the workspace whose label is that repo name.

Jira also defaults `claim_assign` to true, `blocks_link_type` to `Blocks`, `pr_title_pattern` to `\[{id}\]`, and `create_issue_type` to `Task`. A Jira `factory file` picks the new issue's project in this order: `--project KEY`, then `create_project`, then the project prefix of the parent (`--parent` or `default_parent`), then the prefix of the first `--blocks` key, or else the first `--blocked-by` key. With none of these, `file` fails with `jira create: no project for the new item; pass --project KEY or --parent KEY, or set create_project for source jira:<name>`. `--project` with a parent from another project files in `--project` under that parent; when Jira rejects that, `file` shows Jira's error. A GitHub source has no projects: it ignores `--project` and prints `factory: warning: --project KEY is ignored for github:<name>, which has no projects`. `factory file` with a parent (`--parent` or `default_parent`) reads that parent's issue type first. Under an epic it uses `create_issue_type`. Under a standard issue such as a Task or Story it uses `create_subtask_issue_type`, or the project's only subtask issue type when that key is unset. More than one subtask type without the key is an error. Under a subtask, or in a project with no subtask type, `file` fails before creating anything, for example `cannot file under ABC-12 (Sub-task): Jira does not allow children of a subtask`. `assignee_ok` defaults to `me` (`me`, `unassigned`, or `any`). `create_assignee` defaults to `me` (`me` or `unassigned`) on both kinds: `factory file` assigns the new item to the authenticated user, so `next` with `assignee_ok = "me"` can pick it. Jira sends `fields.assignee` with your account id in the create request. If Jira rejects only the `assignee` field (for example when it is not on the create screen), `file` creates the issue without it and then assigns it with `PUT /assignee`. GitHub passes `--assignee @me` to `gh issue create`. `unassigned` sends no assignee. A Jira `file` body is Markdown and is converted to Jira's document format: headings, bullet and numbered lists, fenced code with its language, inline code, bold, italic, links, and a single newline as a line break. Backslash escapes such as `\_` are unescaped. A table or raw HTML stays as plain text. GitHub gets the body unchanged. When a body has the two characters `\n` and no real newline, as with `--body "a\nb"` in a shell, `file` prints `factory: warning: body contains a literal \n and no newline; use --body-file or $'...' quoting` on stderr and files the text unchanged. `parent_blocks_child` defaults to `ignore` (`ignore` or `honor`).

`claim_transition` is unset by default. It is a transition id or a transition name. When more than one available transition lands in `claim_status`, factory posts the one whose own name equals `claim_status` if exactly one has that name. Otherwise it posts `claim_transition` when that value is one of those transitions. Otherwise it posts the smallest id and warns with the chosen id and the ids and names it did not use. Ids compare as integers when both are integers, and lexicographically otherwise. A missing or mismatched `claim_transition` is an error and posts nothing. Release still requires exactly one transition to a ready status.

`factory start` enforces the `[claim]` safety invariants on both the local snapshot and the live `Get` before claiming: closed/done items are always refused (never bypassed by `--force` or `--force-taking-ownership`); untrusted-author gating and tamper guards from `[source.trust]` (when `environment = "untrusted"`) are enforced on both the snapshot and live `Get` (even under `--force`); non-leaf items (`skip_types` or open children) and blocked items are refused when `claim.require_leaf` / `claim.require_unblocked` are `true` (the default), including under `--force`; and `claim.source_containment` (default `true`) refuses starting items outside `FACTORY_ITEM_SOURCE`, scopes `start` to the current git repository's configured source by default (refusing unconfigured git repositories unless `--source` or `--config` is passed), and resolves shorthand `<number>`, `#<number>`, and GitHub/Jira issue URLs against the scoped source. Ownership (`claim.ownership_rule`, default `"unassigned_or_me"`) and in-progress state (`claim.in_progress_rule`, default `"block"` for `claim_status` on Jira and `claim_label` on GitHub/Jira, plus foreign machine `claim.nickname` in the latest `"Claimed by ..."` comment) also block by default unless owned by this machine's own claim or overridden with `--force-taking-ownership` (which replaces prior assignees with `@me` / `Me`). `--force` skips queue checks (`require_labels`, `exclude_labels`, `not_before`, `parent`, non-in-progress `status`, `tab`, `pr`, `claimed`) and the start gate, naming each skipped check in its warning, while still enforcing the safety checks, `[source.trust]` gating, and a live existing tab. When `claim.comment` is `true` (the default), dry-run previews the claim comment (`Claimed by <agent> [on <nickname>] at <timestamp>.`, or `claim.comment_template`) and `--apply` posts it using the exact UTC `claimed_at` timestamp recorded in SQLite unless re-running on this machine's own claim. When `claim.assign = false`, claim confirmation verifies in-progress state without requiring `Assignee == me`.

Each `[[source]]` may also configure `[source.trust]`:

```toml
  [source.trust]
  environment = "untrusted"               # "trusted" or "untrusted"; unset leaves gating off
  trusted_author_associations = ["OWNER"] # default under "untrusted"; authenticated me is always trusted
  trusted_authors = []                    # optional explicit author login allowlist
  untrusted_ready_statuses = ["Todo"]     # default under "untrusted"; [] blocks all external authors
  guards = ["locked"]                     # "locked", "approved_hash", or [] to disable tamper guards
```

When `environment = "trusted"` or unset, external-author gating, tamper guards, and prompt hardening stay off. When `environment = "untrusted"`, issues from trusted authors (`me`, `trusted_author_associations`, or `trusted_authors`) stay eligible under `ready_statuses`, while issues from untrusted authors (including deleted users) are eligible only when `state` is in `untrusted_ready_statuses` and at least one enabled tamper guard in `guards` is satisfied (`locked == true` for `"locked"`, or a matching local SHA-256 `(title, body)` approval recorded by `factory approve` for `"approved_hash"`), and `start` injects the untrusted-input rule at `{{trust_rule}}` (or prepended before `start_prompt`). When `"approved_hash"` is enabled and `"locked"` is not satisfied, `factory start` re-fetches the live `(title, body)` before claiming and refuses with `not approved` or `edited since approval` if no matching approval exists.

Use `factory approve <id>` to inspect an untrusted-author issue's live `title`, `body`, canonical SHA-256 hash (`sha256:<hex>`), and planned transition to the first `untrusted_ready_statuses` entry (default `Todo`). Respecting `dry_run`, pass `--apply` (or set `dry_run = false`) to record the hash in SQLite (`approvals`) and transition the issue's native status when not already in an allowed status; pass `--revoke` to clear a stored local approval.

`factory doctor` audits `[source.trust]` classification and GitHub threat settings: it warns when `trust.environment` is unset (noting online via `repository.isPrivate` when an unclassified GitHub repo is public), warns if a public GitHub repo is marked `"trusted"` or if an `"untrusted"`/public source enables `"OPEN"`/`"Backlog"` in `untrusted_ready_statuses`, sets `guards = []`, or allows broad non-maintainer `trusted_author_associations`, scans local `.github/ISSUE_TEMPLATE/*` and `.github/workflows/*` in `cwd` for auto-triage rules that add `require_labels` or set native status/`Todo` on new issues (offline-capable), and (when online) inspects collaborator/team permissions via `gh api` (gracefully noting and skipping on 403/insufficient scope).

`factory adopt ID` records a claim for work that already has a tab or a pull request. It does not create a workspace or a tab, and it does not start an agent or send a prompt. `--pane` is a pane id and records the tab that contains that pane. Otherwise `--tab` is a tab id or a tab label. Otherwise the tab label is the one `start` would use. Repeatable `--pr` is `owner/repo#N` or a pull request URL. Open pull requests whose head branch contains the item id are included, and so are the pull requests `show` already matches by title or closing issue. At least one tab or pull request is required. If neither resolves, the item and the claim store stay unchanged. A second `adopt` of an open claim (`claimed`, `started`, or `handed_off`) does not transition the item and does not insert another row. It fills in a tab or pull request the first call did not record. The recorded pull request includes the head branch when the open pull request has one. On Jira, the transition is the one `start` uses, and an item already in `claim_status` is not transitioned. On GitHub, `claim_label` is added only when it is missing. `show` lists a `--pr` even when its title does not match `pr_title_pattern`. When `show` finds no open pull request for the item, it searches every state with `in:title` for the item id and keeps titles that match `pr_title_pattern`.

`factory claim-pr ID URL` records that pull request on the active claim (`claimed`, `started`, or `handed_off`). The argument is `owner/repo#N` or a pull request URL. When `gh` lists that open pull request, the recorded row keeps its URL, title, and head branch. A missing active claim is an error and writes nothing. `gh` failing writes nothing. The command ignores `dry_run` and does not start an agent.

`pr_template` is the path of the pull request template. `verify` requires it. A relative path is inside the source `cwd`. `verify ID` reads that file and the checkout, and it does not fetch. It records each pull request it sees on the active claim. The pull request must be a draft with no labels and no reviewers. Every section in the template must appear in the pull request body with content. A section starts at a heading: an ATX heading (`#` to `######` then a space) or a setext heading (a title line underlined by a line of only `=` or only `-`, at least two). Sections match by heading text, whatever the style or level, so `Changelog` underlined by `=====` in the body satisfies `## Changelog` in the template, and the other way round. Inside a fenced code block nothing is a heading, a `---` after a blank line is a thematic break, and a list item is never a heading title. A heading text used twice is an error (`expected unique headings`). A line that is empty or only an HTML comment is not content. The merge-base of `HEAD` and `origin/<base>` must be an ancestor of `origin/<base>` and no older than 7 days. `git status --porcelain` must be empty. `start` records the number of `git stash list` lines on the claim, and `verify` fails when that count is missing or different.

`orphan_ignore` is a list of item ids. `orphan_ignore_labels` is a list of labels. `doctor` does not report `has no parent` for an open item whose id is in `orphan_ignore`, or whose label matches `orphan_ignore_labels` without regard to case. Other checks still report that item. An omitted list and an empty list both keep every no-parent warning. These lists are per source and are not inherited from `[defaults]`.

Three more per-source keys cut `doctor` warnings that are not actionable:

- `claim_parent_types` (default `["Epic", "Story"]`, case-insensitive). `dropped-claim` skips an item in `claim_status` whose type is in this list, or that has an open child in the mirror (an item whose parent is its id and that is not Done). Such a parent is in progress because its children are, and has no tab or pull request of its own. A leaf with no tab and no open pull request is still reported.
- `external_blocker_lookups` (default `100`; `0` looks up none; the old name `doctor_blocker_lookups` is still accepted, but not both). The mirror query is often narrow (for example `assignee = currentUser()`), so a blocker assigned to someone else or in another project is never mirrored. On a Jira source, every `sync` fetches each blocker id of a mirrored item that is not itself in the mirror by key, once per id, up to this many in all, and stores the result as an *external blocker* record: status, done, assignee, or not found. These records are never work items: they are not candidates, not counted in `stats`, and never orphans. A blocker over the bound keeps the record from an earlier sync, or is recorded as not checked. A record no longer referenced by any item is dropped. `sync` prints `outside blockers: N fetched, D done, S skipped` after the source line, and `sync --json` has the same counts under `external_blockers`. Readiness, `show`, and `doctor` all use these records (see the blockers rule below). In `doctor`, a Done blocker is no finding. An open one stays an `unknown-blocker` warning, and the message adds its status and assignee, such as `PRJ-2 is blocked by OPS-7 (In Review, assignee bob), which is not in the mirror`. A key Jira does not return (missing, or not visible to you) warns `which was not found or is not visible`. A blocker over the bound, or whose fetch failed, is `info` that says it was not checked and why. A blocker the last sync has not looked at yet, and every GitHub blocker outside the mirror, keeps the plain `which is not in the mirror` warning.
- `orphan_epic_child_types` (default `["Story", "Task", "Bug"]`, case-insensitive; `[]` turns this rule off). An orphan whose type is not in the list, such as a Sub-task, cannot sit under an epic, so `orphan` lists the options that work instead of `set its parent epic`: link it to its umbrella item (relates or blocks), move it under that item's epic, or add it to `orphan_ignore`. The same options, naming the item, apply when the orphan blocks or is blocked by an open mirror item whose type is in the list: that task-level item is its likely umbrella. `doctor --json` adds those options as a `suggestions` array; `suggestion` joins them with `; `.

With [lanes](#lanes), `doctor` also lists each remote lane machine's workspaces and tabs, as `tick` does. A claim recorded on a lane is matched against that lane's tabs. A remote lane that does not answer is not an error: an in-progress item claimed there is `info` (`lane <name> unreachable; tab not checked`), not a dropped claim.

## Command reference

The blocks below are the output of `factory --help` and `factory <command> --help`. They are not a paraphrase. `--db` is hidden and is not listed.

### `factory --help`

```text
Pick the next ready work item and start an agent on it

Usage:
  factory [command]

Available Commands:
  adopt        Record a claim for an existing item without starting an agent
  approve      Inspect, approve, or revoke the content-hash approval for an item
  claim-pr     Record a pull request on an active claim
  close        Detect and safely close open parent issues whose children are all done
  completion   Generate the autocompletion script for the specified shell
  doctor       Run CLI diagnostics and report dependency-graph and claim problems
  file         File a follow-up and print only its new id
  gate         Report whether another agent can start now
  help         Help about any command
  hold         Create a hold file that blocks new starts
  init         Scaffold factory.toml for the current git repository
  link         Record that blocker blocks blocked
  logs         Show the run history
  next         List the next ready work items
  owner        Print the machine and tab whose agent last worked an item
  priorities   Report a lower-priority item blocking a higher-priority one
  release      Undo a claim and mark the latest active claim released
  release-hold Remove the hold file so new starts can proceed
  run          Run factory tick on an interval until interrupted
  settle       Finish merged work without starting an agent
  show         Show one work item, its blockers, claim, tab, and pull requests
  skill        Print or install the agent skill for this CLI
  start        Claim an item and start an agent in herdr
  stats        Report throughput, backlog, epic progress, and flow from the local store
  sync         Refresh the local mirror from each source
  tick         Sync, settle merged work, and start at most one ready item
  unlink       Remove the blocks link from blocker to blocked
  update       Update factory to the latest version
  verify       Check a worker pull request and checkout
  version      Print version information

Flags:
      --config string      path to config file
  -h, --help               help for factory
      --log-level string   log level: debug, info, warn, or error (default "info")

Use "factory [command] --help" for more information about a command.
```

### `factory adopt --help`

```text
Record a claim for an existing item without starting an agent

Usage:
  factory adopt <id> [flags]

Flags:
  -h, --help             help for adopt
      --machine string   lane name or lane herdr machine the tab is on (needs [[lane]] config)
      --pane string      pane id; record the tab that contains it
      --pr stringArray   pull request to record, as owner/repo#N or a URL (repeatable)
      --tab string       tab id or tab label to record

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory approve --help`

```text
Inspect, approve, or revoke the content-hash approval for an item

Usage:
  factory approve <id> [flags]

Flags:
      --apply           record the approval hash and transition status (implied when dry_run is false)
  -h, --help            help for approve
      --revoke          clear the stored local approval for this item
      --source string   limit to this source key (kind:name, for example github:repo)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory claim-pr --help`

```text
Record a pull request on an active claim

Usage:
  factory claim-pr <id> <url> [flags]

Flags:
  -h, --help   help for claim-pr

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory close --help`

```text
Detect and safely close open parent issues whose children are all done

Usage:
  factory close [id...] [flags]

Flags:
      --all                   show every source, not only the one whose cwd holds the current directory
      --apply                 close eligible parent issues in the source
      --comment string        comment text to post when closing
      --comment-file string   path of a file to use as the closing comment
  -h, --help                  help for close
      --json                  print the result as JSON
      --no-sync               do not refresh a stale mirror
      --source string         limit to this source key (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory completion --help`

```text
Generate the autocompletion script for factory for the specified shell.
See each sub-command's help for details on how to use the generated script.

Usage:
  factory completion [command]

Available Commands:
  bash        Generate the autocompletion script for bash
  fish        Generate the autocompletion script for fish
  powershell  Generate the autocompletion script for powershell
  zsh         Generate the autocompletion script for zsh

Flags:
  -h, --help   help for completion

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")

Use "factory completion [command] --help" for more information about a command.
```

### `factory completion bash --help`

```text
Generate the autocompletion script for the bash shell.

This script depends on the 'bash-completion' package.
If it is not installed already, you can install it via your OS's package manager.

To load completions in your current shell session:

	source <(factory completion bash)

To load completions for every new session, execute once:

#### Linux:

	factory completion bash > /etc/bash_completion.d/factory

#### macOS:

	factory completion bash > $(brew --prefix)/etc/bash_completion.d/factory

You will need to start a new shell for this setup to take effect.

Usage:
  factory completion bash

Flags:
  -h, --help              help for bash
      --no-descriptions   disable completion descriptions

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory completion fish --help`

```text
Generate the autocompletion script for the fish shell.

To load completions in your current shell session:

	factory completion fish | source

To load completions for every new session, execute once:

	factory completion fish > ~/.config/fish/completions/factory.fish

You will need to start a new shell for this setup to take effect.

Usage:
  factory completion fish [flags]

Flags:
  -h, --help              help for fish
      --no-descriptions   disable completion descriptions

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory completion powershell --help`

```text
Generate the autocompletion script for powershell.

To load completions in your current shell session:

	factory completion powershell | Out-String | Invoke-Expression

To load completions for every new session, add the output of the above command
to your powershell profile.

Usage:
  factory completion powershell [flags]

Flags:
  -h, --help              help for powershell
      --no-descriptions   disable completion descriptions

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory completion zsh --help`

```text
Generate the autocompletion script for the zsh shell.

If shell completion is not already enabled in your environment you will need
to enable it.  You can execute the following once:

	echo "autoload -U compinit; compinit" >> ~/.zshrc

To load completions in your current shell session:

	source <(factory completion zsh)

To load completions for every new session, execute once:

#### Linux:

	factory completion zsh > "${fpath[1]}/_factory"

#### macOS:

	factory completion zsh > $(brew --prefix)/share/zsh/site-functions/_factory

You will need to start a new shell for this setup to take effect.

Usage:
  factory completion zsh [flags]

Flags:
  -h, --help              help for zsh
      --no-descriptions   disable completion descriptions

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory doctor --help`

```text
Report dependency-graph and claim problems

Usage:
  factory doctor [flags]

Flags:
      --all             show every source, not only the one whose cwd holds the current directory
  -h, --help            help for doctor
      --json            print the findings as JSON
      --no-sync         do not refresh a stale mirror
      --source string   limit to this source key (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory file --help`

```text
File a follow-up and print only its new id

Usage:
  factory file [flags]

Flags:
      --blocked-by stringArray   id that blocks this item (repeatable)
      --blocks stringArray       id this item blocks (repeatable)
      --body string              body text
      --body-file string         path of a file to use as the body
  -h, --help                     help for file
      --label stringArray        label to set (repeatable)
      --parent string            parent id
      --project string           Jira project key to file in (default: create_project, then the parent's or first linked key's project)
      --source string            source key (kind:name) to file in
      --title string             title of the new item

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory gate --help`

```text
Report whether another agent can start now

Usage:
  factory gate [flags]

Flags:
      --all             show every source, not only the one whose cwd holds the current directory
  -h, --help            help for gate
      --json            print the gate result as JSON
      --source string   checkout to inspect (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory help --help`

```text
Help provides help for any command in the application.
Simply type factory help [path to command] for full details.

Usage:
  factory help [command] [flags]

Flags:
  -h, --help   help for help

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory hold --help`

```text
Create a hold file that blocks new starts

Usage:
  factory hold [flags]

Flags:
  -h, --help            help for hold
      --owner string    name recorded as the hold owner
      --reason string   reason recorded in the hold file

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory init --help`

```text
Scaffold factory.toml for the current git repository

Usage:
  factory init [flags]

Flags:
      --base string   default branch name (overrides origin/HEAD; fallback is main)
      --cwd string    directory inside the git repository to inspect (default: current directory)
      --dry-run       print the target path and generated TOML without writing
      --force         overwrite an existing <git-root>/factory.toml
      --global        append to $XDG_CONFIG_HOME/factory/factory.toml instead of writing <git-root>/factory.toml
  -h, --help          help for init
      --kind string   source kind to scaffold: github or jira (default: inferred from origin)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory link --help`

```text
Record that blocker blocks blocked

Usage:
  factory link <blocker> <blocked> [flags]

Flags:
  -h, --help   help for link

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory logs --help`

```text
Show the run history

Usage:
  factory logs [flags]

Flags:
      --all             show every source, not only the one whose cwd holds the current directory
  -h, --help            help for logs
      --json            print the runs as JSON
  -n, --n int           maximum runs to print (default 20)
      --path            print the database path and exit
      --source string   limit to this source key (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory next --help`

```text
List the next ready work items

Usage:
  factory next [flags]

Flags:
      --all                      show every source, not only the one whose cwd holds the current directory
      --deferred                 list deferred items (not_before date ahead), soonest first
      --explain string           print readiness checks and the score for this item ID
  -h, --help                     help for next
      --human                    list ready items that carry a human_labels label
      --json                     print the ready items as JSON
      --mine                     with --human or --only-label, keep items assigned to you
  -n, --n int                    maximum ready items to print (default 10)
      --no-sync                  do not refresh a stale mirror
      --only-label stringArray   list ready items with this label (repeatable; replaces human_labels)
      --source string            limit to this source key (kind:name, for example jira:acme)
      --sync                     refresh the mirror before ranking

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory owner --help`

```text
Print the machine and tab whose agent last worked an item

Usage:
  factory owner <id> [flags]

Flags:
  -h, --help            help for owner
      --json            print the owner as JSON, or null when the item was never claimed
      --source string   source key (kind:name) when the id is in more than one source

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory priorities --help`

```text
Report a lower-priority item blocking a higher-priority one

Usage:
  factory priorities [flags]

Flags:
      --all             show every source, not only the one whose cwd holds the current directory
      --apply           write suggested priority raises to the source
  -h, --help            help for priorities
      --no-sync         do not refresh a stale mirror
      --source string   limit to this source key (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory release --help`

```text
Undo a claim and mark the latest active claim released

Usage:
  factory release <id> [flags]

Flags:
  -h, --help   help for release

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory release-hold --help`

```text
Remove the hold file so new starts can proceed

Usage:
  factory release-hold [flags]

Flags:
  -h, --help   help for release-hold

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory run --help`

```text
Run factory tick on an interval until interrupted

Usage:
  factory run [flags]

Flags:
      --all                 show every source, not only the one whose cwd holds the current directory
      --apply               claim the top item and start the agent (implied when dry_run is false)
      --caller string       identifier substituted for {caller} (else config caller, else the herdr pane id)
  -h, --help                help for run
      --interval duration   time between ticks (minimum 1m)
      --source string       limit to this source key (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory settle --help`

```text
Finish merged work without starting an agent

Usage:
  factory settle [id...] [flags]

Flags:
  -h, --help   help for settle

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory show --help`

```text
Show one work item, its blockers, claim, tab, and pull requests

Usage:
  factory show <id> [flags]

Flags:
  -h, --help      help for show
      --no-sync   do not refresh a stale mirror

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory skill --help`

```text
Print the agent skill for this CLI

Usage:
  factory skill [flags]

Flags:
  -h, --help   help for skill

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory start --help`

```text
Claim an item and start an agent in herdr

Usage:
  factory start <id> [flags]

Flags:
      --agent-arg stringArray    agent argument after -- (repeatable; overrides rules)
      --apply                    claim the item and start the agent (implied when dry_run is false)
      --caller string            identifier substituted for {caller} (else config caller, else the herdr pane id)
      --force                    skip queue checks and the start gate; still refuse closed/done, untrusted, non-leaf, blocked, foreign ownership, in-progress, and existing tab
      --force-taking-ownership   take over an item assigned to someone else, already in progress, or claimed on another machine
  -h, --help                     help for start
      --json                     print the start outcome as JSON
      --machine string           lane name or lane herdr machine to start on (needs [[lane]] config)
      --model string             shorthand for --agent-arg=--model --agent-arg=MODEL
      --source string            limit to this source key (kind:name, for example github:repo)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory stats --help`

```text
Report throughput, backlog, epic progress, and flow from the local store

Usage:
  factory stats [flags]

Flags:
      --all               show every source, not only the one whose cwd holds the current directory
      --ascii             draw charts with #, =, . and * instead of blocks and braille
      --bar-width int     cells in the bar column of the throughput, backlog, and epics tables; 0 for none (default 12)
      --burndown string   burndown chart: braille or bars (default "braille")
      --burndown-pace     add a dotted pace line to zero by the window end to the braille burndown (reference only)
      --by string         throughput bucket: day or week (default day up to 14d, else week)
      --color string      color charts and status cells: auto (a terminal without NO_COLOR), always, or never (default "auto")
      --epic string       limit every section to this item's subtree and print the tree
  -h, --help              help for stats
      --json              print the report as JSON
      --since string      window length: Nd, Nw, or a duration such as 36h (default "7d")
      --source string     limit to this source key (kind:name, for example jira:acme)
      --split string      add throughput and flow per model or rule
      --utc               bucket and print times in UTC instead of the local zone

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory sync --help`

```text
Refresh the local mirror from each source

Usage:
  factory sync [flags]

Flags:
      --all             show every source, not only the one whose cwd holds the current directory
      --full            resync every item and delete items the source no longer returns
  -h, --help            help for sync
      --json            print one JSON array with a row per source
      --source string   limit to this source key (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory tick --help`

```text
Sync, settle merged work, and start at most one ready item

Usage:
  factory tick [flags]

Flags:
      --all             show every source, not only the one whose cwd holds the current directory
      --apply           claim the top item and start the agent (implied when dry_run is false)
      --caller string   identifier substituted for {caller} (else config caller, else the herdr pane id)
      --enqueue         print a start-item job (JSON line) per ready item to stdout instead of starting one; summary goes to stderr
  -h, --help            help for tick
      --source string   limit to this source key (kind:name, for example jira:acme)

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory unlink --help`

```text
Remove the blocks link from blocker to blocked

Usage:
  factory unlink <blocker> <blocked> [flags]

Flags:
  -h, --help   help for unlink

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory verify --help`

```text
Check a worker pull request and checkout

Usage:
  factory verify <id> [flags]

Flags:
  -h, --help      help for verify
      --no-sync   do not refresh a stale mirror

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```

### `factory version --help`

```text
Print version information as JSON

Usage:
  factory version [flags]

Flags:
  -h, --help   help for version

Global Flags:
      --config string      path to config file
      --log-level string   log level: debug, info, warn, or error (default "info")
```


## How an item gets picked

`factory next` prints ready items, best first. The same rules are in [design.md](design.md) §5. An item is ready only when every check passes. `factory next --explain ID` prints them in this order, `PASS` or `FAIL`. A failure puts the detail after two spaces (`FAIL blockers  blocker PRJ-1 is not done`).

1. **done.** The item is not done. Jira done means the status category is `done`. GitHub done means the issue is closed.
2. **type.** `Type` is not in `skip_types` (default `Epic`). The comparison is case-insensitive.
3. **labels.** It has every `require_labels` entry and none of `exclude_labels` (default `needs-human`, `blocked-external`).
4. **assignee.** `assignee_ok = me` (the default) allows an empty assignee or you. `unassigned` allows only an empty assignee. `any` allows anyone.
5. **status.** `State` is one of `ready_statuses`. On GitHub, `ready_statuses` defaults to `["Todo", "Backlog", "OPEN"]` (so `OPEN` issues with no native status remain eligible while `"In progress"` and `"In review"` are excluded). An explicit empty list (`ready_statuses = []`) passes any state (still respecting `claim.in_progress_rule`).
6. **blockers.** Every blocker is done. A blocker in the mirror that is not done fails with `blocker <id> is not done`. A blocker outside the mirror is judged by the record the last `sync` stored for it (`external_blocker_lookups`): Done passes; open fails with `blocker <id> is <status> (outside mirror, assigned to <name>)` (or `unassigned`); a key Jira does not return fails with `blocker <id> not found or no access`; one over the bound, or whose fetch failed, fails with `blocker <id> not in mirror (not checked: <why>)`; and one no sync has looked at yet fails with `blocker <id> not in mirror (not fetched yet)`. A failed fetch never lets an item through. A GitHub source, or a Jira source with `external_blocker_lookups = 0`, fails with plain `blocker <id> not in mirror`. `show` prints the same status next to each outside blocker. A blocker that is the item's ancestor is different. `parent_blocks_child` defaults to `ignore`: the detail is `ignored: PRJ-100 is the parent of PRJ-105; a parent blocking its own child is usually a linking mistake`, and that link does not fail the check. A grandparent says `an ancestor` instead of `the parent`. `honor` keeps the failure and starts the same detail with `honored:`. The walk is the `parent_rank` walk. A child that blocks its own ancestor still fails this check. `doctor` reports that direction as `child-blocks-parent`.
7. **parent.** If `parent_allowlist` is set, `Parent` is in it. A parent that is in the mirror and done fails. A parent that is not in the mirror does not fail this check by itself (`PASS parent  parent not mirrored`).
8. **children.** No other mirrored item whose `Parent` is this item is still open. Each open child (such as a GitHub sub-issue or Jira sub-task) fails with `has open child <id>`, sorted by id. Once every mirrored child is done, this check passes.
9. **claimed.** There is no claim in `claimed`, `started`, or `handed_off`.
10. **tab.** No herdr tab already uses this item's tab label. Jira looks in every workspace. GitHub looks in the workspace named above.
11. **pr.** No open pull request you authored references it. The search is `author:@me`. Jira matches `pr_title_pattern` (default `\[{id}\]`) against the title. GitHub matches an id in the pull request's closing issues.
12. **not_before.** The item is not deferred past today. A deferral date after today's local date fails with `deferred until YYYY-MM-DD`. On and after that date the check passes by itself, with no write to the source (`PASS not_before  not before YYYY-MM-DD (passed)`). An item with no date passes. `--force` skips this check like the others, and its warning names the deferral.

The deferral date comes from the source. On Jira, `defer_field` names a field id, such as `customfield_12345` or a built-in like `duedate`. Sync requests that field and reads a `YYYY-MM-DD` date, or a Jira datetime, whose calendar date in its own offset is used. Null, empty, and missing are no deferral. Any other value is a sync error that names the issue and the field. On GitHub, `defer_field` defaults to the native `"Start date"` date issue field (set `defer_field = ""` to disable), and `defer_label_prefix` (for example `not-before:`) also reads labels `not-before:2026-12-01` (taking the later date if both are present). A label whose date does not parse defers nothing, and `factory doctor` reports it as `bad-defer-label`. `defer_label_prefix`, `priority_field`, and `due_field` are for GitHub sources only.

`factory next` ends with one line when any items are deferred, for example `3 deferred (earliest 2026-11-20); factory next --deferred lists them`. `factory next --deferred` lists them soonest first, with their date (`#  ID  NOT_BEFORE  SOURCE  TITLE`). `--json` gives each item a `notBefore` field, `YYYY-MM-DD` or `null`.

Ready items then sort by `rank_order`. When `rank_order` is unset, the order is:

1. `priority` ascending. A smaller number comes first: Highest, High, Medium, Low, then Lowest. Jira maps those names to 0, 1, 2, 3, and 4. `priority_ranks` overrides those names. `known_priorities` stay at 2 and do not warn. A name in both uses the rank. A missing priority is 4, with Lowest and after Low, and does not warn. Any other name is 2, and factory logs `unknown jira priority` once per name for that source. GitHub reads the native single-select `priority_field` (default `"Priority"`, mapping `Urgent`=0, `High`=1, `Medium`=2, `Low`=3, overridable via `priority_ranks`), falling back to the smallest matching `priority_labels` value, or 4 when none match.
2. `unblocks` descending. This is how many distinct not-done items are reachable through blocks edges, including through a done item. The item itself is not counted. An item at the head of a chain comes first. When `parent_blocks_child` is `ignore`, an edge from an ancestor to its own descendant is left out of this count.
3. `parent_rank` ascending: the index of the nearest ancestor in `parent_priority`. The walk starts at `Parent` and follows `Parent` through the mirror. The item itself is not a match. The direct parent matches when it is listed, even if that parent is not mirrored. A later ancestor matches only when each step between it and the item is mirrored. A parent cycle stops the walk. An item with no listed ancestor sorts after every listed parent, the same as an unlisted parent.
4. `created` ascending, oldest first.

`id` ascending is the final tie-break in every order. It is not a `rank_order` key, and the score line does not print it.

Set `rank_order` to a permutation of `parent_rank`, `unblocks`, `priority`, and `created` to change which key wins. Directions stay as above: `unblocks` descending, the other three ascending. An unknown key, a duplicate key, or a missing key is a config error. Sources merged by one `next` or `tick` must share one order. Different orders are an error that names both.

`factory next --explain` prints `ancestor  <id>` when a listed ancestor matched, or `ancestor  none` when none did. The next line names the active key order, for example `rank_order  priority, unblocks, parent_rank, created`.

The score line lists those keys in that same order. Unset, it is `score  priority=N  unblocks=N  parent_rank=N  created=<UTC timestamp>`. The `next` table columns for the score follow that order. Unset, they are `PRI`, `UNBLOCKS`, `PARENT`, and `CREATED`, and `TITLE` stays last. `PARENT` is the parent id.

`factory next --explain ID` adds an `inversion` line when that item is part of one. A blocker line names the more urgent item, both priorities, and `via` plus the chain when the link is transitive. A blocked line does the same for each less urgent item whose reported inversion ends on this one. No inversion adds no line.

`factory next --human` lists ready items that carry a `human_labels` label. The command only prints. It does not claim an item or start an agent. `next` has no `--apply` flag, so passing `--apply` is a usage error.

`--only-label LABEL` is repeatable. It turns this listing on without `--human`, and those labels replace `human_labels` for that run. When both are set, `--only-label` wins. An item matches when it carries at least one selected label. Those selected labels are not exclude labels for this run. Any other `exclude_labels` entry still excludes the item. Every other readiness check stays, including `require_labels`, `assignee_ok`, and blockers that are in the mirror and done.

`--mine` then keeps items whose assignee equals the account stored for that source at sync. An empty assignee does not match. `--mine` without `--human` and without `--only-label` is an error. `--explain` together with `--human` or `--only-label` is an error.

This listing does not use `rank_order`. It sorts by priority ascending, then due date ascending with a missing date last, then how many items it unblocks descending, then id ascending. Unblocks is the same transitive not-done set the score counts. The row lists those ids, sorted by id. Done items are left out of the list, and an edge through a done item still reaches the items behind it. The item itself is not included. When `parent_blocks_child` is `ignore`, an ancestor-to-descendant edge is left out of that set.

The columns are `KEY` (the item id), `PRI`, `DUE`, `TITLE`, `URL`, and `UNBLOCKS`. `DUE` is empty when the mirror has no due date, and `YYYY-MM-DD` when it does. Jira stores `fields.duedate` (`YYYY-MM-DD`; null or empty stays missing). GitHub stores the UTC calendar date of the issue milestone `dueOn` when a milestone is present, and leaves the date missing otherwise. `TITLE` is the full title. `--json` prints `key`, `priority`, `due` (`null` when missing), `title`, `url`, and `unblocks` (an array of those ids). `-n`, `--source`, `--json`, `--sync`, and `--no-sync` still apply, and they do not claim or start. Without `--human` and without `--only-label`, `next` keeps its current columns and `rank_order`.

## Priority inversions

`factory priorities` reads the mirror and prints each not-done item that blocks a more urgent not-done item. A smaller priority number is more urgent. The line names the blocker, its priority, the most urgent item it blocks, the chain when that item is not a direct block, and the suggested priority.

```text
PRJ-10  priority Medium (2)  blocks PRJ-20 (Highest, 0) via PRJ-10 → PRJ-11 → PRJ-20  suggest Highest
PRJ-11  priority Medium (2)  blocks PRJ-20 (Highest, 0)  suggest Highest
```

Done and closed items are absent. A `blocks` link between an item and its own ancestor or descendant is absent too. `parent_blocks_child = ignore` (the default) leaves an ancestor→descendant edge out of the walk. `honor` can walk through that edge, but the descendant is still not the inversion. A child→ancestor edge is always left out.

The default does not write. `factory doctor` prints the same lines under `priority-inversion` and does not write. `factory priorities --apply` is the only way to raise a priority. It sets each blocker to the suggested rank on the source. `next`, `tick`, `start`, and `run` never change priority. Jira writes the priority name (`Highest` for rank 0, unless `priority_ranks` says otherwise). GitHub adds the single `priority_labels` label for that rank. A rank with no single name is an error and is not written.

## Rollout

These are the steps in [design.md](design.md) §14. Do them in order. The GitHub source in this repo is step 6's adapter; the prsync matching change in §13 is still a separate change in prsync.

1. `sync`, `next`, `show`, and `doctor`. These only read and are useful on their own. Check them against the PRJ backlog. The click-by-click version is [e2e-checklist.md](e2e-checklist.md).
2. Tag a small first batch as `agent-ready` (the low-risk, well-specified items).
3. `start ID --apply` by hand for a few items, to prove the herdr start sequence works.
4. `tick --apply` run by hand.
5. `run --interval` or cron, on **one** machine only, until the claim protocol has proved itself. An interval below `1m` exits 2.
6. The GitHub adapter, plus the prsync matching change (§13).

## Exit codes

- `0` means the command finished. `tick` and `run` still exit `0` when the gate refuses (`gate: ...` on stdout, after a dry-run plan marked `blocked  yes`) or nothing is ready (`no ready items`). `doctor` and `gate` exit `0` whenever they run. `gate` prints `OK: <n> in flight (max <limit>)` when it passes, for example `OK: 3 in flight (max 5)`, or one failing reason per line. With [lanes](#lanes) it then prints one line per lane, and an unreachable lane is skipped, not a failure. `start` exits `0` on a dry-run plan or a started agent (`factory start <id>: started (tab <tab>, claim <time>)`).
- `1` means the tool failed. stderr is `factory: ` plus the error.
- `2` means a usage error: an unknown command, a bad flag, or a rejected argument. stderr is the error with no `factory: ` prefix.
- `3` means `start` refused before changing anything (`factory start <id>: refused, nothing changed: <reason>`).
- `4` means `start` failed after claiming and kept the claim (`factory start <id>: failed after claim; claim kept: <next step>`).
- `5` means `start` failed after claiming and released the claim (`factory start <id>: failed after claim; claim released: <next step>`).
