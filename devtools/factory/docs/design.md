# `factory` — pick the next work item and start an agent on it in herdr

Status: design, 2026-10-29 (migrated from `github.com/jaeyeom/agentic-factory` into `github.com/jaeyeom/experimental/devtools/factory`).
Home: `github.com/jaeyeom/experimental/devtools/factory`. Go 1.26, built with
Bazel (`//devtools/factory/cmd/factory`) or `go build`. External deps on PATH: `herdr` (>= 0.9),
`gh`, `git`. Jira is reached over REST directly (no MCP).
Prior art in the same style: prsync, at
`devtools/prsync` (https://github.com/jaeyeom/experimental/tree/main/devtools/prsync).

## 1. Goal

A single command that, when run, does this:

1. Mirrors work items from one or more **sources** (Jira projects and GitHub repos)
   into a local SQLite file, fetching only what changed.
2. Computes which items are **ready**: open, eligible, and with every blocker
   finished.
3. If it is safe to start, **claims** the best one, creates a herdr **tab**
   named after it, starts `claude`, and sends that source's **start prompt**.

It does not review, push, merge, or mark PRs ready. Once the tab exists, the
existing chain takes over: `/implement-jira-ticket` or `/gh-issue-resolver` →
draft PR → `/pr-watch` + `prsync dispatch`.

### Non-goals
- No repo-specific knowledge in code. Everything project-specific is in config (§9).
- No running several agents at once. Only **one agent starts at a time** (§6).
- No handling of review comments (`prsync dispatch`). `settle`, and the
  settle step of `tick`, close an item's tab when its agent is not working.
  A working agent keeps the tab.

## 2. Concepts

| Term | Meaning |
|---|---|
| Source | A configured backend instance: `jira:<name>` or `github:<owner>/<repo>`. |
| Item | One issue. Its ID is unique within its source: `PRJ-1234`, or `owner/repo#57`. |
| Ready | Open, eligible, no unfinished blockers, and not already in flight (§5). |
| In flight | Claimed, or has a herdr tab with its label, or has an open PR that references it. |
| Claim | The action in the source that marks the item as taken (§7). The source itself acts as the lock between machines. |
| Handed off | An agent tab exists and a PR for the item is open. From here, pr-watch owns the item. |

## 3. Source adapter interface

```go
type Source interface {
    Name() string                                   // "jira:acme", "github:jaeyeom/experimental"
    Sync(ctx context.Context, since time.Time) ([]Item, error)   // incremental
    IDs(ctx context.Context) ([]string, error)                   // IDs the query matches now
    Get(ctx context.Context, id string) (Item, error)            // live read, used by claim
    Claim(ctx context.Context, id string) error
    Release(ctx context.Context, id string) error   // undo claim (manual `factory release`)
    Finish(ctx context.Context, id string) error    // optional, see §8
    Link(ctx context.Context, blocker, blocked string) error
    Unlink(ctx context.Context, blocker, blocked string) error
    Create(ctx context.Context, req CreateRequest) (string, error) // follow-up filing
}

type Item struct {
    Source, ID, Title, URL string
    Type        string    // "Task", "Bug", "Epic", "issue", ...
    State       string    // raw status name
    Done        bool      // statusCategory == Done / issue closed
    Parent      string    // epic key / parent issue ID ("" if none)
    Labels      []string
    Assignee    string    // account id / login
    Priority    int       // normalized: 0 = highest
    BlockedBy   []string  // item IDs, may be in another source
    Updated     time.Time
    Created     time.Time
}
```

### 3.1 Jira adapter
- Auth: email plus API token. The token is read from `token_file` (mode 0600) or
  from the env var named in `token_env`. Load checks that exactly one of those
  fields is set and does not read the file or the variable. The first command
  that contacts the source reads it once. A missing or unreadable token fails
  that command and names the source and the field.
- Sync: `POST /rest/api/3/search/jql` (the new search endpoint; the old
  `/search` is deprecated), with
  `jql = <source.jql> AND updated >= "<since - 2m>" ORDER BY updated ASC`. Fetch
  only these fields: `summary,status,issuetype,parent,labels,assignee,priority,issuelinks,created,updated`.
  Page through with `nextPageToken`. The 2-minute overlap absorbs clock skew;
  upserts are idempotent.
- `BlockedBy`: the `issuelinks` entries whose type name matches
  `blocks_link_type` (default `Blocks`) and that have an `inwardIssue`, i.e.
  "is blocked by". The link IDs are kept so that `Unlink` can call
  `DELETE /rest/api/3/issueLink/{id}`. This deletion is the one thing the MCP
  connector cannot do.
- Claim: `POST /rest/api/3/issue/{id}/transitions`, using the transition whose
  *target status name* equals `claim_status` (looked up with GET transitions;
  fail loudly if there's no such transition). Then `PUT .../assignee` to self,
  if `claim_assign = true`. Create sets `fields.assignee` to self when
  `create_assignee = "me"` (the default), and falls back to `PUT .../assignee`
  after the create when Jira rejects only that field.
- Finish: transition to `finish_status`. It is off by default (§8).
- Link/Create: `POST /rest/api/3/issueLink`, `POST /rest/api/3/issue`
  (with `parent` from `CreateRequest.Parent`, falling back to `default_parent`).
  With a parent, Create first reads the parent's issue type. An epic-level
  parent gets `create_issue_type`. A standard parent gets
  `create_subtask_issue_type`, or the project's single subtask type from
  `GET /rest/api/3/project/{key}`. A subtask parent, or a project with no
  subtask type, fails before the create call.

### 3.2 GitHub adapter
- Auth: `gh` (`gh api graphql`). No token handling in factory.
- Sync: GraphQL `repository.issues(filterBy:{since:$since}, states:[OPEN,CLOSED], orderBy:UPDATED_AT)`.
  The query asks for `number,title,url,state,labels,assignees,parent{number},
  blockedBy(first:50){nodes{number repository{nameWithOwner} state}},
  createdAt,updatedAt`.
  Field names were checked against the live schema on 2026-10-29: `Issue.blockedBy`,
  `Issue.blocking`, `Issue.parent`, `Issue.subIssues`, `Issue.closedByPullRequestsReferences`.
- `Done` = `state == CLOSED`. `State` reads `Issue.status.name` when present (`Backlog`, `Todo`, `In progress`, `In review`, `Done`, `Not planned`, `Duplicate`), falling back to `state` (`OPEN`/`CLOSED`) when `status` is `null`.
- Native issue fields: `Sync` and `Get` read `Issue.issueFieldValues` for `priority_field` (default `"Priority"`, mapping `Urgent`=0, `High`=1, `Medium`=2, `Low`=3, falling back to `priority_labels`), `defer_field` (default `"Start date"`, merged with `defer_label_prefix`), and `due_field` (default `"Target date"`, overriding `milestone.dueOn`). `SetPriority` writes via GraphQL `setIssueFieldValue` when `priority_labels` is empty, or adds the label when `priority_labels` is configured.
- Claim: assign `@me` (when enabled), add any configured `claim_label`, and then set native status (`claim_status`, default `"In progress"` when neither `claim_status` nor `claim_label` is set) via GraphQL `setIssueStatus` (after assignment so GitHub's auto-transition on assign does not overwrite `"In progress"`). Release unassigns `@me`, removes any configured `claim_label`, and moves native status back to the first non-`OPEN` `ready_statuses` entry (default `"Todo"`).
- Link/Unlink: the GraphQL mutations `addBlockedBy` / `removeBlockedBy`, which
  were checked to exist. The REST equivalent is
  `POST/DELETE /repos/{o}/{r}/issues/{n}/dependencies/blocked_by`.
- Finish: none. `Fixes #N` in the PR closes the issue on merge.

## 4. Local store

SQLite via `modernc.org/sqlite` (pure Go, so the binary stays single).
Path: `$XDG_STATE_HOME/factory/factory.db`, falling back to
`~/.local/state/factory/factory.db`.

```sql
items(source, id, title, url, type, state, done, parent, labels_json,
      assignee, priority, created, updated, raw_json,
      PRIMARY KEY(source, id))
links(source, blocker_id, blocked_id, link_id,          -- link_id: Jira link id / GH node id
      PRIMARY KEY(source, blocker_id, blocked_id))
sync_state(source PRIMARY KEY, last_updated_seen, last_full_sync, last_sync)
claims(source, id, claimed_at, machine, tab_id, workspace_id, state)
      -- state: claimed | started | handed_off | released | failed
runs(started_at, command, outcome, detail_json)          -- audit log, like prsync logs
```

- A sync replaces all `links` rows of each item it refetched. That is how links
  deleted in the source disappear from the mirror.
- `factory sync --full` resyncs everything once a day (`full_sync_every`, default
  24h), so deletes and permission changes the incremental query misses still
  get picked up.
- An incremental sync also asks the source for the IDs its query matches now
  (`IDs`, the same query with no `since`, IDs only) and deletes mirrored items
  of that source not in the set. A failed ID query, or an empty set while the
  mirror has rows, deletes nothing and is reported as a warning, so an auth or
  query glitch cannot wipe the mirror.
- `factory release`, and finishing an item on merge, reread that item with
  `Get` and check it against `IDs`. The row is updated when the item still
  matches and removed when it does not.
- `last_sync` is the wall time of the last successful sync, separate from
  `full_sync_every`. `sync.max_age` (default `0`) is how old a source may be
  before `next`, `show`, `priorities`, and `doctor` sync it. `0` means never.
  A source `max_age` overrides the top-level value. `--no-sync` skips the
  refresh. A failed refresh warns and still answers from the mirror.
- If a blocker ID is not in the mirror (another project, or outside the
  `jql`), the item is **not ready**, and `doctor` names the unknown blocker.
  Fail closed: an unknown blocker never counts as finished.

## 5. Readiness

An item is **ready** when all of these hold:

1. `!Done`, and `Type` is not in `skip_types` (default `Epic`).
2. It is eligible: it has every label in `require_labels` (for example: `agent-ready`)
   and none in `exclude_labels` (default `needs-human`, `blocked-external`).
3. `Assignee` is empty or yourself (`assignee_ok = me|unassigned|any`).
4. `State` is in `ready_statuses` (Jira: e.g. `To Do`, `Backlog`), or the source
   has none (GitHub: any open issue).
5. Every `BlockedBy` item is in the mirror and `Done`. A blocker that is an
   ancestor of the item is different when `parent_blocks_child` is `ignore`
   (the default): the link does not gate the item, and `next --explain` says
   it was ignored. `honor` keeps the gate and still warns. The walk is the
   `parent_rank` walk, so a grandparent counts, a direct parent counts even
   when it is not mirrored, and a parent cycle stops the walk. A descendant
   that blocks its own ancestor still gates.
6. If `parent_allowlist` is set, `Parent` is in it. The parent epic itself is
   not `Done`.
7. **Not in flight:**
   - no `claims` row in `claimed|started|handed_off`;
   - no herdr tab whose label equals the item's tab label (§6.1), searched in
     every workspace. This also catches work you started by hand;
   - no open PR by you that references it. Jira: title matches
     `pr_title_pattern` (default `\[{id}\]`). GitHub: the PR's
     `closingIssuesReferences` contain the issue. To find these, run one
     `gh search prs --author @me --state open --json ...` per tick.

### Ranking (deterministic; `factory next` prints the score parts)
Sort by `rank_order`. When it is unset, the order is:
1. `priority` asc. A smaller rank comes first: Highest, High, Medium, Low, then
   Lowest. Jira maps those names to 0, 1, 2, 3, and 4. `priority_ranks` overrides
   those names. `known_priorities` stay at 2 and do not warn. A name in both uses
   the rank. A missing priority is 4, with Lowest and after Low, and does not
   warn. Any other name is 2, and factory logs `unknown jira priority` once per
   name for that source. GitHub uses the smallest `priority_labels` value among
   the issue's labels, or 4 when none match.
2. `unblocks` desc. This is the number of not-Done items that become ready, or
   come one step closer to ready, once this item finishes. It is the transitive
   count over the `links` graph, so an item at the head of a long chain goes
   first. When `parent_blocks_child` is `ignore`, a blocks edge from an ancestor
   to its own descendant is left out, including items reached only through it.
3. `parent_rank` asc: index of the nearest ancestor in `parent_priority`.
   The walk starts at `Parent` and follows `Parent` through the mirror.
   The item itself is not a match. A direct parent matches when it is listed,
   even if that parent is not mirrored. A later ancestor matches only through
   mirrored steps. A parent cycle stops the walk. No listed ancestor sorts
   after every listed parent.
4. `Created` asc (oldest first).

`id` ascending is the final tie-break and is not a `rank_order` key.
`rank_order` is a permutation of `parent_rank`, `unblocks`, `priority`, and
`created`. Directions stay as above. An unknown key, a duplicate, or a missing
key is a config error. Sources merged by `next` or `tick` must share one order.

`factory next --explain PRJ-1234` prints why an item is or isn't ready, rule by
rule, then `ancestor  <id>` or `ancestor  none`, then `rank_order` and the score.
The score keys, and the `next` table columns for them, follow that order.
`TITLE` stays last. Unset, the order line is
`rank_order  priority, unblocks, parent_rank, created`. This is the main
debugging tool.

## 6. Starting an agent (one at a time)

### 6.1 Tab and workspace naming
- **Jira:** the tab label is the issue key (`PRJ-1234`). Keys are unique across
  every workspace. The workspace comes from the first matching `[[workspace_rule]]`
  (match by `parent` or `label`), falling back to the source's `workspace`.
  Example: `PRJ-2131` children → `smoke`; default → `factory`.
- **GitHub:** the tab label is `#<number>`, in the workspace **whose label is the
  repo name** (`experimental`). The source can override this with `workspace = ...`.
  Tab identity is the pair (workspace label, tab label), so `#57` can exist in
  several repo workspaces.
- If the workspace is missing, create it with
  `herdr workspace create --label <ws> --cwd <cwd> --no-focus`.
  If more than one workspace has that label, fail loudly and do not guess. This
  is the same rule prsync follows.

### 6.2 Start gate: all must pass, otherwise `tick` exits 0 with a reason
1. **No other herdr agent is `working` inside a configured checkout.** The pane
   in `HERDR_PANE_ID` does not count, nor do panes listed in `gate.ignore_panes`
   or tabs whose label is in `gate.ignore_tabs`. A working agent counts only
   when its `cwd` or `foreground_cwd` is a source `cwd` or a subdirectory of
   one. An agent outside every configured checkout does not count.
   `--log-level=debug` names each working agent ignored for that reason.
   Every other working tab is named. If the caller's pane is still in a
   counted tab, that line ends with `(this pane)`.
2. **The target checkout is clean**: `git -C <cwd> status --porcelain` is empty.
   With one shared checkout, this is what really keeps runs serial. An agent
   that is `blocked` mid-edit leaves the tree dirty, so nothing new starts
   until it's resolved. When `gate.base_branch` is unset, that status command
   is the only git command. When it is set, `gate.require` is `on_base` or
   `clean_against_base`. `on_base` refuses unless `HEAD` is that branch.
   `clean_against_base` refuses when `HEAD` has commits that are not on
   `origin/<base_branch>` (a checkout merely behind that ref still passes).
   A refusal names the current branch and the expected one. The check does
   not fetch. An unknown `require`, or only one of the two keys, is a config
   error. This is not the source `base` field.
3. **There's room for more work in progress**: count the union of active
   claims (`claimed`, `started`, and `handed_off`) and open pull requests
   from the gh client's OpenByMe query
   (`is:pr is:open author:@me archived:false`). A claim and a pull request
   that are the same work count once: by the pull request URL recorded on
   the claim, then by the claim's recorded head branch, then by the item id
   in the title (`pr_title_pattern`) or in the head branch name. An open
   pull request that matches no claim still counts. The count must be below
   `max_in_flight` (default 3). At the limit, the gate prints
   `in flight: N (claims C, PRs P, overlap O) of max M`. Your review time is
   the real limit, not the number of agents.
4. **No start in the last `min_start_interval`** (default 10m), so a quick
   failure can't cause a burst of starts.
5. **No hold is active.** While any `gate.hold_files` path exists, the gate
   refuses and names that path. An empty file still holds. A line `owner: NAME`
   is the owner. Every other non-empty line is the reason. Both go into the
   refusal. `gate.hold_ttl`, when set, marks a file older than that duration as
   stale. The refusal says so and names the age. The hold is not ignored.
   Unset means no age check. When `hold_files` is empty, the path is
   `~/.local/state/factory/hold`. `XDG_STATE_HOME` is not used.
   `factory hold` creates that file. `factory release-hold` removes it. More
   than one configured path is an error for those two commands. `gate.hold_tabs`
   is a list of tab labels. A matching tab that exists and is not `done`
   refuses the start and names the tab and its status. A missing tab or a
   `done` tab does not hold. `ignore_tabs` does not exempt a hold tab.

### 6.3 Start sequence (`tick --apply`, or `start <id> --apply`)
1. Pick the top ready item. A `start` call names it instead, but still goes
   through the ready checks and the start gate (§6.2) unless `--force`.
   `--force` skips those checks on the dry run and on the live re-check, and
   the warning names each one. It still requires the item, refuses another
   assignee and an existing tab, and confirms the claim.
2. Claim it (§7).
3. `herdr tab create --workspace <ws> --cwd <cwd> --label <tab> --no-focus`. The JSON
   response is `{"result":{"type":"tab_created","tab":TabInfo,"root_pane":PaneInfo}}`,
   and the pane id comes from `root_pane`. Check the shape with `herdr api schema --json`.
4. `herdr agent start <tab> --kind claude --pane <pane> [-- <agent args>]`, then wait until it's ready.
   Agent args come from `factory start --agent-arg` or `--model`, else the first matching `[[agent_rule]]`, else `[defaults] agent_args`.
5. `herdr agent prompt <pane> "<rendered start_prompt>" --wait --until working --timeout 30000`.
   Don't wait for it to finish: factory returns once the agent is working.
6. Write `claims.state = started` with the tab and workspace IDs; log the run.

A dry run (`start` without `--apply`, or `tick` / `run` while `dry_run` is
true) still prints the plan when the gate refuses. The first line is
`blocked  yes`, then the claim, tab, agent, args, and prompt lines. `--force` is
not required. `start` still exits 1. `tick` and `run` exit 0 and also print
`gate: ...`. A ready-check failure with a passing gate prints no plan.
`--apply` does not print the blocked plan.

If step 3 or later fails, the claim stays in place, the claim row is set to
`failed`, and the error is printed in full. **Never silently release it**: a
half-started item needs a human to look. `factory release <id>` undoes it.

### 6.4 Start prompt template
Variables: `{id} {title} {url} {source} {base} {parent} {caller}`.
`{caller}` is `--caller` on `start`, `tick`, and `run`, else `[defaults] caller`,
else the pane id `Load` already stored from `HERDR_PANE_ID`. It does not read
that variable again. When all three are empty, `{caller}` expands to an empty
string and factory logs a warning that names the placeholder. It is never a
hostname or the word `unknown`. Per source:

```
start_prompt = """
Before anything else: run `git fetch origin` and create a fresh branch from
origin/{base} for this work. Other agents share this checkout, and their
branches must be left as they are.
/implement-jira-ticket {id}
"""
```

## 7. Claim protocol (race-safe across machines)
1. `Get(id)`, a live read that skips the mirror. Check the readiness rules again
   against this live data (blockers, assignee, status, labels). If any fails,
   abort this item and try the next one.
2. `Claim(id)`.
3. `Get(id)` again and confirm the assignee is you and the status is
   `claim_status`. If it isn't, someone else won the race: log it, skip the item,
   and don't release it.
4. Insert a `claims` row with `machine = hostname`.

## 8. After the PR
- `factory tick` and `factory settle` notice merged PRs. With no ids, the
  window is the same one tick already uses: merged on or after the last tick,
  minus one hour, or the last 7 days when there has been no tick. `settle ID`
  searches merged pull requests by the item key in the title, with no merged-after date. An id with no
  merged pull request is an error. A title match (`pr_title_pattern`, or a
  closing issue when the id contains `#`) marks the active claim `done`.
- When `finish_on_merge = true` and the item isn't already done, it calls
  `Finish`. Default `false` for Jira, because many projects already have a
  workflow rule that does this. Turn it on only where none does.
- It closes the item's tab when the agent is not working. A working agent
  leaves the tab open, and that is printed. A result type of `ok`, or any
  other success result from herdr, is a successful close. That result type
  is printed on its own line. An unexpected close result still moves the
  tracker item to done.
- It prints each dependent whose blockers are now all done. Those items also
  show up as ready at the next tick.
- An active claim whose item is already done is released even when no merged
  pull request is in the window. That release does not call `Release`, so the
  tracker item is not moved back to a ready state.
- `settle` stops there and does not start an agent. `tick` still settles
  before it may start one item.

## 9. Config: `~/.config/factory/factory.toml`
TOML rather than prsync's KEY=VALUE format, because it has to hold several
sources and rule lists. Search order: `--config`, then `./factory.toml`, then
`$XDG_CONFIG_HOME/factory/factory.toml`.

```toml
[defaults]
max_in_flight      = 3
min_start_interval = "10m"
full_sync_every    = "24h"
exclude_labels     = ["needs-human", "blocked-external"]
skip_types         = ["Epic"]
agent_kind         = "claude"
dry_run            = true          # tick/start need --apply unless false
# caller           = "coordinator" # {caller}; else the herdr pane id from startup

# [gate]
# ignore_panes = [] # pane ids the start gate skips
# ignore_tabs  = [] # tab labels the start gate skips
# base_branch = "main" # unset = no branch check
# require = "on_base"  # or "clean_against_base"
# hold_files = ["~/.local/state/factory/hold"] # empty = that default path
# hold_tabs = [] # tab labels that hold until missing or done
# hold_ttl = "12h" # unset = no age check; a stale hold still refuses

[[source]]
name            = "acme"
kind            = "jira"
site            = "https://example.atlassian.net"
email           = "you@example.com"
token_file      = "~/.config/factory/jira-token"
jql             = "project = PRJ"
ready_statuses  = ["To Do", "Backlog"]
claim_status    = "In Progress"
claim_assign    = true
require_labels  = ["agent-ready"]
blocks_link_type = "Blocks"
parent_priority = ["PRJ-2131", "PRJ-538", "PRJ-2435"]
# parent_blocks_child is "ignore" (default) or "honor".
# ignore does not let an ancestor's blocks link gate its descendant
# or count toward that ancestor's unblock score. Both modes warn.
# parent_blocks_child = "ignore"
# rank_order permutes parent_rank, unblocks, priority, and created.
# Unset keeps priority, unblocks, parent_rank, created.
# rank_order = ["parent_rank", "unblocks", "priority", "created"]
# priority_ranks overrides Highest=0, High=1, Medium=2, Low=3, Lowest=4.
# priority_ranks = { Highest = 0, High = 1 }
# known_priorities stay at rank 2 and do not warn.
# known_priorities = ["Untriaged"]
pr_title_pattern = '\[{id}\]'
cwd             = "~/src/monorepo"
base            = "main"
workspace       = "factory"
start_prompt    = "@~/.config/factory/monorepo-start.txt"
default_parent  = "PRJ-2436"        # `factory file` without --parent

  [[source.workspace_rule]]
  parent    = "PRJ-2131"
  workspace = "smoke"

  [[source.workspace_rule]]
  label     = "clang-tidy"
  workspace = "clang-tidy"

[[source]]
name           = "experimental"
kind           = "github"
repo           = "jaeyeom/experimental"
cwd            = "~/go/src/github.com/jaeyeom/experimental"
base           = "main"
require_labels = ["agent-ready"]
claim_label    = "in-progress"
start_prompt   = "/gh-issue-resolver {url}"
# workspace defaults to the repo name: "experimental"
```

## 10. CLI

| Command | Effect | Writes? |
|---|---|---|
| `factory sync [--full] [--source S]` | Refresh the mirror | local DB only |
| `factory next [-n 10] [--explain ID] [--json]` | Ranked ready list, or rule-by-rule reasons for one item | no |
| `factory show ID` | Item, blockers, what it blocks, claim/tab/PR | no |
| `factory doctor [--json]` | Graph health (§11) | no |
| `factory priorities [--apply]` | Priority inversions in the blocks graph. `--apply` raises them | source, only with `--apply` |
| `factory tick [--apply]` | sync → settle merged work → start gate → start the top item, at most one | source + herdr |
| `factory settle [ID...]` | Finish merged work and stop. Does not start an agent | source + herdr |
| `factory run --interval 10m [--apply]` | `tick` in a loop; stops on SIGINT | source + herdr |
| `factory start ID [--apply] [--force]` | Start a specific item | source + herdr |
| `factory adopt ID [--pane P] [--tab T] [--pr PR]` | Record a claim for an existing tab or pull request. No agent, tab, or workspace is created | source + local claim |
| `factory release ID` | Undo the claim, mark the claim row `released` | source |
| `factory link BLOCKER BLOCKED` / `unlink ...` | Manage blocks edges | source |
| `factory file --title T [--blocks ID] [--blocked-by ID] [--parent P] [--label L]` | File a follow-up with its links set in one step, for agents to call | source |
| `factory logs` | Run log (same idea as `prsync logs`) | no |

Exit codes: 0 when the run worked, including a gate refusal that just means
"nothing to do"; 1 when the tool itself errors; 2 for usage errors. A refusal
prints one line explaining why, e.g. `gate: agent PRJ-2366 working`.

## 11. `doctor` checks
- A cycle in the blocks graph (print the cycle).
- A Done blocker is normal for the other checks. Readiness already treats that
  link as satisfied, so `doctor` does not report it there and does not suggest
  `unlink` for that reason.
- `parent-blocks-child`: an ancestor blocks its own descendant (a child, or
  deeper through `Parent`). Jira sub-tasks, Jira epic children, and GitHub
  sub-issues are children through `Parent`. The suggestion is
  `factory unlink <ancestor> <descendant>`. Both `ignore` and `honor` report
  it, and a Done ancestor is still listed.
- `child-blocks-parent` (info): a descendant blocks its own ancestor. Readiness
  does not change. The suggestion is `factory unlink <descendant> <ancestor>`.
- A blocker that isn't in the mirror (outside the `jql` or another project).
- An open non-epic item with no parent (the orphan sweep from 2026-10-29).
- An item in `claim_status` with no tab and no open PR: a claim that was
  dropped somewhere.
- A tab whose label looks like an item ID while the item is `Done`, and the tab
  has been idle for over a day (overlaps with `prsync tabs`; report only).
- A claim in `started` whose agent has been `idle` for over `stall_after`
  (default 2h) with no PR: it stalled. When that item is already done, the
  finding is `done: release claim` instead. `doctor` does not release it.
- A ready item missing `require_labels` but otherwise ready. These are the
  candidates you might mark `agent-ready`.
- `priority-inversion`: a not-done item blocks a more urgent not-done item.
  Done items, and a blocks link between an item and its own ancestor or
  descendant, are left out. The suggestion is `factory priorities --apply`.
  `doctor` does not write the raise.
- `closable-parent` (info): an open parent whose known children (across the
  mirror and finished history) are all done. The suggestion is
  `factory close <ID> --apply`. `doctor` does not close it.

## 12. Code sharing with prsync
prsync's herdr client lives in a Go `internal/` package in another repo, so it
can't be imported. Port it instead: `internal/herdr` in this repo should copy
the approach of `devtools/prsync/internal/herdr/client.go`. That means running
commands through `github.com/jaeyeom/go-cmdexec`'s `Executor` so tests can
inject a fake, decoding the `{"result":{...}}` envelope, and treating a missing
field as an error (`ErrDegraded`), never as an empty value.

## 13. Required prsync change (lives in jaeyeom/experimental, tracked separately)
For GitHub-issue tabs (§6.1), prsync's tab matching today parses `[ID]` from
the PR title. Extend it: if the title has no ID and the PR has
`closingIssuesReferences` → (workspace label = repo name, tab label = `#N`).
Without this, pr-watch can't dispatch review comments to factory-started
GitHub tabs.

## 14. Rollout
1. `sync`, `next`, `show`, `doctor`. These only read and are useful on their
   own. Check them against the PRJ backlog.
2. Tag a small first batch as `agent-ready` (the low-risk, well-specified items).
3. `start ID --apply` by hand for a few items, to prove the herdr start sequence works.
4. `tick --apply` run by hand.
5. `run --interval` or cron, on **one** machine only, until the claim protocol
   has proved itself.
6. The GitHub adapter, plus the prsync matching change (§13).

## 15. Open questions
- `ready_statuses` for PRJ: which statuses can a picked ticket be in? (Check the
  PRJ workflow; your earlier note says "Queue" shares a status category with
  "To Do".)
- Should `factory` start the agent with `--permission-mode` from config, or
  leave it at your claude default?
