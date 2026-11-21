# review-and-push-loop

`review-and-push-loop` pushes your unpushed local commits one at a
time, but only after OpenAI's Codex CLI reviews each one. Codex runs
read-only and can only answer push or blocked; the tool validates the
answer and runs `git push`. Use it when another agent (such as Claude
Code) has written several local commits and you want a second model to
gate them before they reach `origin`.

It pushes to `origin` without asking. For a first try, pass
`--max-iterations 1`.

## Prerequisites

- The `codex` command is on `PATH` and already authenticated. Set
  `CODEX_BIN` when the binary has another name.
- The repository has a remote named `origin`.
- You have push rights on that remote.

## Quick start

Install:

```bash
go install github.com/jaeyeom/experimental/devtools/reviewpush/cmd/review-and-push-loop@latest
```

Review and push a single commit:

```bash
review-and-push-loop --max-iterations 1
```

The Bazel target is `//devtools/reviewpush/cmd/review-and-push-loop`.

Human mode is the default. It fetches, names the commit under review,
prints that commit with `git log --stat --format=fuller -1`, and on a
push prints `Pushed through <commit>.` A finished run ends with
`DONE: HEAD is not ahead of <remote>.`:

```text
Fetching origin/main before iteration 1...
Iteration 1: reviewing and pushing a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2 (1 commit(s) ahead of origin/main).
========================================
TARGET COMMIT
========================================
commit a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2
Author:     Ada Lovelace <ada@example.com>
AuthorDate: Sat Nov 14 09:00:00 2026 -0800
Commit:     Ada Lovelace <ada@example.com>
CommitDate: Sat Nov 14 09:00:00 2026 -0800

    Fix the startup path

 main.go | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)
========================================
Pushed through a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2.
DONE: HEAD is not ahead of origin/main.
```

The text between the `========` lines is the `git log` output. The
other lines are what the command prints. `--output compact` skips the
banner and the `Pushed through` line, and it does not stream Codex
stderr when the pass succeeds. One iteration is a single line:

```text
ITERATION 1 target=a1b2c3d4e5f6 ahead=2 status=PUSH push_target=c3d4e5f6a7b8
DONE base=origin/main
```

A blocked review in human mode stops like this:

```text
BLOCKED: review-and-push stopped on a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2.
Reason: hard blocker
```

> "I finally found the proper usage of codex! Review but don't work on the code. It shows its super power when I tell it that the code is written by Claude Code!"

## Options

| Name | Kind | Default | Meaning |
| --- | --- | --- | --- |
| `--repo` | flag | `.` | Repository directory. |
| `--base` | flag | detected | Override base detection (`origin/HEAD`, then `main`, `master`, or `trunk`). |
| `--max-iterations` | flag | `100` | Maximum review and push cycles. Must be greater than zero. |
| `--output` | flag | `human` | `human` or `compact`. |
| `GIT_BIN` | environment | `git` | Git executable. |
| `CODEX_BIN` | environment | `codex` | Codex executable. |
| `CODEX_TIMEOUT_SECONDS` | environment | `300` | Seconds for each Codex pass. A non-negative integer. `0` waits without a deadline. |

```bash
CODEX_TIMEOUT_SECONDS=300 review-and-push-loop --repo . --base main --max-iterations 1 --output human
```

`--max-iterations` below 1, an unknown `--output` value, a
non-integer `CODEX_TIMEOUT_SECONDS`, and a base branch that cannot be
detected are configuration errors.

## Exit codes

- `0`: success. `HEAD` is no longer ahead of the remote base branch.
- `1`: configuration error.
- `2`: blocked, push failed, Codex failed, fetch failed, or the loop hit a guardrail.

## What it does

![reviewpush overview](overview.jpg)

`review-and-push-loop` repeats the following until `HEAD` is not ahead
of the remote base, or a pass stops the loop:

1. Fetch `origin`.
2. Detect the base branch from `origin/HEAD`, then `main`, `master`,
   or `trunk`. If none of those remote refs exist, use the current
   local branch and treat the remote branch as not created yet, so the
   first push can create it. A detached `HEAD` in that situation is an
   error unless you pass `--base`.
3. Find the oldest local commit ahead of that base. When the remote
   branch does not exist yet, that is the root commit reachable from
   `HEAD`.
4. Start a fresh Codex process to review that commit. Codex is told to
   stay read-only, to inspect Git objects directly, and to ignore a
   dirty work tree unless those local changes affect the review.
5. Read Codex's answer: push, or stop because a hard blocker was found.
6. Check the commit that would be pushed, then run
   `git push origin <commit>:refs/heads/<base>`.
7. Repeat.

```text
Claude Code writes commits locally

  main
    |
    o  origin/main
    |
    o  C1  "Claude change 1"
    |
    o  C2  "Claude change 2"
    |
    o  C3  "Claude change 3"   <- HEAD

Queue seen by review-and-push-loop:

  oldest ----------------------------------------------> newest
  [ C1 ] -> [ C2 ] -> [ C3 ]

Loop:

  +------------------------+
  | review-and-push-loop   |
  +------------------------+
            |
            v
    pick oldest ahead commit
            |
            v
  +------------------------+
  | Codex review pass      |
  | read-only              |
  | no code edits          |
  | no git push            |
  +------------------------+
        |            |
   PUSH |            | BLOCKED
        v            v
  validate target    stop the loop
  git push origin
  origin/main moves forward
        |
        v
  queue shrinks:
  [ C2 ] -> [ C3 ]
        |
        v
  repeat until empty
```

With non-blocking findings, the intended human workflow is often:

```text
Codex review
   |
   +--> hard blocker found
   |      -> stop push
   |
   +--> non-blocking concern
          -> push commit anyway
          -> optionally spin off a GitHub issue for follow-up
```

Filing that GitHub issue is not automated. The tool only decides
whether to push or stop, then performs the validated push.

## Review policy

The prompt treats only hard blockers as push-stopping issues:

- security bugs
- leaked secrets
- obvious destructive data loss or corruption
- clearly broken startup or primary-path behavior

Other feedback stays non-blocking. This tool lands the reviewed
commits. It does not ask Codex to rewrite the branch.

## Safety properties

Before `git push`, the wrapper checks that the commit it is about to
push:

- resolves to a real commit
- is a descendant of the remote base ref, when that ref already exists
- is the oldest commit under review, or a descendant of it
- is an ancestor of `HEAD`

Those checks keep the push inside the contiguous range from the oldest
unpushed commit through `HEAD`. A push of several commits is allowed
only when Codex explicitly recommends that, and only through a commit
that passes the same checks. The field names for that recommendation
are in Internals.

## Internals

The Go package behind the command is `reviewpush`.

Codex does not push. Each pass is a new process. The wrapper validates
the JSON below and is the only thing that runs `git push`.

### Codex response contract

Each pass must return JSON of this shape and nothing else:

```json
{
  "status": "PUSH",
  "summary": "brief explanation",
  "multi_commits": false,
  "push_target": "abc123",
  "blocked_reason": null
}
```

When the review stops:

```json
{
  "status": "BLOCKED",
  "summary": "hard blocker found",
  "multi_commits": false,
  "push_target": null,
  "blocked_reason": "security bug"
}
```

`status` is `PUSH` or `BLOCKED`. On `PUSH`, `blocked_reason` is null.
On `BLOCKED`, `push_target` is null and the wrapper does not push.

`multi_commits` is the cross-check for how far to push:

- `false`: push only the commit under review, even when `push_target`
  names a later commit.
- `true`: push through `push_target` after the safety checks. That
  commit must be a descendant of the commit under review.

The wrapper asks Codex for this object with `codex exec`. It runs
`codex exec --help` and passes only flags that this Codex binary
advertises: `--ask-for-approval` (`never`, or `--full-auto` when that
flag is absent), `--sandbox` (`workspace-write`, or
`danger-full-access` when `TERMUX_VERSION` is set), `--ephemeral`,
`--color` (`never`), `--output-schema`, and `--output-last-message`.

## Standard CLI protocol

`review-and-push-loop` also implements `devtools/pkg/toolcli` subcommands:

- `review-and-push-loop version [--json]`
- `review-and-push-loop update [--check] [--ref <ref>] [--json]`
- `review-and-push-loop doctor [--json] [--offline]`
- `review-and-push-loop skill [install|uninstall|status ...]`
