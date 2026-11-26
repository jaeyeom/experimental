# Manual end-to-end checklist

Run this against a real Jira project or GitHub repo, with real `herdr`, `gh`, and `git`. It is not a unit test. Setup is [usage.md](usage.md). If a step fails because factory is wrong, file a new issue. Do not patch factory from this checklist.

Assume `dry_run = true` (the default). With `dry_run = false`, `factory start A` claims and starts, which is step 4, not the dry run in step 3.

`A` and `B` below are the ids `factory file` prints. A Jira id is a key such as `PRJ-123`. A GitHub id is `owner/repo#N`. Commands that take an id want that whole string.

`factory file` prints only the new id on stdout. With one configured source, `--source` is optional. With more than one, and no `--blocks` or `--blocked-by` id to copy a source from, it exits 2 and stderr says `--source required`. Pass `--source kind:name` (for example `jira:monorepo`).

Jira `file` needs a project. Pass `--project KEY` or `--parent PARENT`, set `default_parent` or `create_project`, or link the new issue with `--blocks` or `--blocked-by` so the first linked key gives the project. If none of these gives one, stdout is empty, the exit code is 1, and stderr is `factory: jira create: no project for the new item; pass --project KEY or --parent KEY, or set create_project for source jira:<name>`. A Jira `PARENT` that is a standard issue (a Task or Story) makes A and B subtasks, so pick an epic as `PARENT` when A and B should be standard issues. A subtask `PARENT` fails with `cannot file under PARENT (Sub-task): Jira does not allow children of a subtask` and creates nothing.

The new items have to pass every readiness check except B's blocker. Add every `require_labels` entry (`--label` is repeatable; the examples use `agent-ready`). Do not add an `exclude_labels` entry. Leave the assignee empty when `assignee_ok` is `me`. On Jira, the new issue's status has to be in `ready_statuses`. If `factory next --explain A` says `FAIL status`, move A and B to one of those statuses in the web UI and run `factory sync` before expecting step 2 to pass. A Jira issue with no parent still shows up in `next` unless `parent_allowlist` is set; `doctor` reports it as `orphan`. Put `--parent` in the allowlist when that list is set.

GitHub `start --apply` sets native issue status to `claim_status` (default `"In progress"`) after assigning `@me`. If you explicitly configure legacy `claim_label`, create that label in the repo first (or set `claim.create_label = true`); if it is missing, stderr contains `create the "<label>" label in <repo> first`.

## 1. Sync, next, and doctor

```sh
factory sync --full
factory next
factory doctor
```

`sync --full` prints one line per source and exits 0:

```text
jira:monorepo  full  N items (M removed)
```

The source key is `kind:name`. `N` and `M` are the counts from that source. A GitHub source uses the same shape with a `github:` key. A source that fails prints `kind:name  error: ...` and the command exits 1 with `factory: sync: ...` on stderr.

`next` exits 0. Either there are no ready items:

```text
no ready items (O open, B blocked, E not eligible)
```

or a table whose columns follow `rank_order`. Unset, they are `#`, `ID`, `PRI`, `UNBLOCKS`, `PARENT`, `CREATED`, and `TITLE`. The header is those words. Rows are padded so the columns line up. The default limit is 10 (`-n`). An explicit `rank_order` keeps that key order in the columns, with `TITLE` last.

`doctor` exits 0 whether or not it finds anything. No findings means empty stdout. Findings are grouped by check. `almost-ready` and `child-blocks-parent` are `info`, and so are an `unknown-blocker` that was not looked up and a `dropped-claim` on an unreachable lane. The other findings are `warn`. On Jira, `doctor` fetches each blocker that is not in the mirror, and a Done one is no finding. Without lookups, or on GitHub, an unknown blocker reads:

```text
unknown-blocker (1)
  warn  PRJ-2 is blocked by PRJ-NOPE, which is not in the mirror
        widen jql, or unlink
```

The check names are `cycle`, `unknown-blocker`, `parent-blocks-child`, `child-blocks-parent`, `orphan`, `dropped-claim`, `stale-tab`, `stalled`, `almost-ready`, and `priority-inversion`. A started claim older than `stall_after` with no open pull request whose item is already done says `done: release claim` instead of a stuck start. `doctor` does not release it. A Done blocker is not a finding, except `parent-blocks-child`, which still lists that edge. `parent-blocks-child` suggests `factory unlink <ancestor> <descendant>`. `priority-inversion` is a less urgent open item blocking a more urgent one, and it suggests `factory priorities --apply`. `doctor` does not write that raise.

## 2. A blocks B

Create two throwaway issues, with A blocking B. Repeat `--label` for every required label.

Jira needs a project. Pass `--parent` or `--project`, or rely on `default_parent` or `create_project`:

```sh
factory file --title "factory e2e A" --label agent-ready --parent PARENT
factory file --title "factory e2e B" --label agent-ready --parent PARENT --blocked-by A
```

GitHub does not need a parent. Omit `--parent` unless B should be a sub-issue:

```sh
factory file --title "factory e2e A" --label agent-ready
factory file --title "factory e2e B" --label agent-ready --blocked-by A
```

Each command's stdout is only the new id. A link failure after the issue exists still prints that id, then exits 1. On success, `file` writes the link and refreshes the mirror, so a separate sync is not required for these two ids. On Jira, fix the status first if it is not in `ready_statuses`, then `factory sync`.

```sh
factory next -n 100
factory next --explain B
```

`next` includes A and does not include B. If the table has exactly 100 rows, raise `-n`; a truncated table can hide A.

`--explain B` includes this line (the id is A's):

```text
FAIL blockers  blocker A is not done
```

The other checks are `PASS` or `FAIL` with a detail. `ancestor  ID` is printed when a listed ancestor matched, or `ancestor  none` when none did. The next line names the active key order. Unset, it is `rank_order  priority, unblocks, parent_rank, created`. The score line lists those keys in that order:

```text
score  priority=N  unblocks=N  parent_rank=N  created=TIMESTAMP
```

`TIMESTAMP` is UTC. When B is otherwise eligible, `blockers` is the only `FAIL`.

## 3. Dry-run start

```sh
factory start A
```

This prints a plan when A is ready and the start gate passes: the checkout is clean, no other herdr agent is `working` inside a configured source `cwd` (the pane running factory, `gate.ignore_panes` / `gate.ignore_tabs`, and an agent whose `cwd` and `foreground_cwd` are both outside every configured checkout do not count), no hold file is present (`gate.hold_files`, or `~/.local/state/factory/hold` when that list is empty) and no `gate.hold_tabs` label is a tab that is not `done`, in-flight work is under `max_in_flight`, and the latest non-released claim is at least `min_start_interval` old (default 10m). In-flight work is the union of active claims (`claimed`, `started`, and `handed_off`) and open pull requests you authored. A claim and a pull request that are the same work count once: by the pull request URL recorded on the claim, then by the claim's recorded head branch, then by the item id in the title (`pr_title_pattern`) or in the head branch name. An open pull request that matches no claim still counts.

When the gate refuses, stdout is still that plan with a first line `blocked  yes`. `--force` is not required to see it. The exit code is 1, and stderr is `factory: start A: gate: ...`. `--apply` does not print the plan on a refusal. A ready check that fails while the gate passes prints nothing on stdout, exits 1, and stderr is `factory: start A: not ready: ...`.

On success, stdout is five lines and the exit code is 0. Nothing is claimed, and no tab is created. Jira, with `claim_assign` true and `claim_status` `In Progress`:

```text
claim    A → In Progress (assign to me)
tab      workspace "WORKSPACE" / tab "A" (cwd CWD)
agent    claude
args     (default)
prompt   FIRST…
```

GitHub, with the default claim label:

```text
claim    A → in-progress (assign to me)
tab      workspace "REPO" / tab "#N" (cwd CWD)
agent    claude
args     (default)
prompt   /gh-issue-resolver URL…
```

`WORKSPACE` is the first matching `workspace_rule`, otherwise the source `workspace`, otherwise `factory` for Jira. For GitHub it is `workspace` or the repo name. The tab label is the issue key on Jira and `#N` on GitHub. `agent` is `agent_kind` (default `claude`). `args` is the agent argv after `--`, and `(default)`, `(flag)`, or `(rule N labels ...)` says which one chose it. With no `agent_args` and no matching rule the line is `args     (default)`. `prompt` is the first line of the rendered start prompt plus `…`. Placeholders are already replaced. `CWD` is the source checkout.

## 4. Start for real

```sh
factory start A --apply
```

Stdout is the same five lines as step 3. The exit code is 0. After that:

- herdr has a tab with that label in that workspace, on `CWD`.
- The agent kind from the plan is running in the tab.
- The rendered start prompt was submitted. Factory returns once the agent is `working`, not when it finishes.
- Jira: the issue is in `claim_status` (`In Progress` in the example) and assigned to you when `claim_assign` is true.
- GitHub: the issue has `claim_label` and is assigned to you.

A failure after the claim is in place exits 1. stderr includes `item is claimed; fix and re-run with --force, or factory release A`. Do not expect the claim to disappear on its own.

## 5. Gate while A is working

Run this while that agent is still `working`:

```sh
factory gate
```

Stdout includes:

```text
agent TAB (TABID) is working
```

`TAB` is the label of a counted working tab, and `TABID` is that tab's id. The caller's pane and the ignore lists are not counted. Every counted tab is its own line. Right after step 4, that is A's tab (`A` on Jira, `#N` on GitHub) unless some other tab is already `working`. The exit code is 0. If the counted tab still contains the pane running factory, the line ends with ` (this pane)`. There is no `OK` line. Once A is no longer `working` and every other check passes, stdout is `OK: <n> in flight (max <limit>)`, where `<n>` is the in-flight count, a claim and its pull request counted once, and `<limit>` is `max_in_flight`.

A start newer than `min_start_interval` (default 10m) also prints:

```text
last start ELAPSED ago (min 10m0s)
```

`ELAPSED` is a Go duration (`1.5s`, `2m3s`). A dirty checkout adds `<cwd> has uncommitted changes (N files)`. At least `max_in_flight` items in flight (default 3) adds `in flight: N (claims C, PRs P, overlap O) of max M`. That count is the union of active claims (`claimed`, `started`, and `handed_off`) and open pull requests you authored. A claim and a pull request that are the same work count once: by the pull request URL recorded on the claim, then by the claim's recorded head branch, then by the item id in the title (`pr_title_pattern`) or in the head branch name. An open pull request that matches no claim still counts. When `gate.base_branch` is set, a checkout on another branch adds `<cwd>: expected branch BASE, found CURRENT`. `require = "clean_against_base"` adds `<cwd>: expected no commits beyond origin/BASE, found N on CURRENT`. Unset, neither line appears. A hold file adds `hold <path>`, and `is stale (age <duration>)` when the file is older than `gate.hold_ttl`. A live hold tab adds `hold tab LABEL (TABID) is STATUS`.

## 6. Close A, then B is next

Mark A done in the source. On Jira, move it to a status in the Done category (the name is often `Done`). On GitHub, close the issue. Then:

```sh
factory sync
factory next -n 100
```

`sync` (not `--full`) exits 0. After the full sync in step 1, stdout is one line per source:

```text
kind:name  incremental  N items (M removed) since TIMESTAMP
```

If this source has never had a full sync, or the last one is at least `full_sync_every` old (default 24h), that line is the `full` line from step 1 instead.

`TIMESTAMP` is UTC RFC3339, such as `2026-11-01T20:15:04Z`. `next` includes B and does not include A. A is done, and B's blocker is done, so the blockers check passes.

## 7. Unlink and link

```sh
factory unlink A B
factory link A B
```

Both commands print nothing and exit 0. They ignore `dry_run`. Confirm in the source's web UI, not only in the terminal:

- After `unlink`, A no longer blocks B.
- After `link`, A blocks B again.

`factory link` is `link BLOCKER BLOCKED`, so `link A B` means A blocks B. The mirror is refreshed immediately. `factory show B` prints the whole item; its `Blocked by:` section matches the UI. After unlink, when A was the only blocker:

```text
Blocked by:
  none
```

After link:

```text
Blocked by:
  A  STATE  done
```

`STATE` is the Jira status name, or `CLOSED` for a closed GitHub issue. A not-done blocker would say `not done`.

## 8. Clean up

```sh
factory release A
```

Stdout is empty. On success the exit code is 0.

- Jira: the issue moves to the first `ready_statuses` entry that has one transition, and the assignee is cleared.
- GitHub: `claim_label` is removed and the assignee is cleared. Closing the issue in step 6 does not block this.
- The latest active claim becomes `released`. `factory show A` includes `state: released`.
- If A still matches the source `jql`, its mirror row is refreshed. If it no longer matches, for example under `assignee = currentUser()`, the row is removed and `factory next` no longer lists A.

If Jira has no transition from A's current status to a ready status, the exit code is 1 and stderr is `factory: jira release A: expected a transition to one of "To Do", "Backlog", found ...` (the quoted names are the configured ready statuses and the transitions that exist). The claim stays active.

Close the herdr tab from step 4 if it is still open. `factory settle` closes that tab when a merged pull request finishes the item and the agent is not working. `doctor` tells you to close a stale tab with `prsync tabs`. Then delete A and B in the Jira or GitHub web UI. factory has no delete command.
