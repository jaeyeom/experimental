# pkg/outbox

`devtools/pkg/outbox` is the shared, channel-agnostic confirm-before-doing gate for outbound actions across developer tools.

## Design Boundaries

Following [`devtools/AGENTS.md`](../../AGENTS.md):

1. **Core is agnostic**: zero references to GitHub, Slack, terminal pane managers (`herdr`), or specific LLM harnesses.
2. **Human UI approval only**: [`EvaluateApproval`](./decide.go) requires human UI provenance (`author == "human"`, empty `by`, `via == "ui"`) via [`devtools/decide`](../../decide/README.org). Agent replies and CLI answers cannot approve outbound execution.
3. **Narrow integration boundaries**:
   - **Store**: `Store` interface + `TypedFileStore[T]` / `FileStore` with `flock` directory locking (`LockDir`) and atomic writes (`WriteFileAtomic`).
   - **Preflight**: `PreflightHook` and agnostic deny-rule linter (`BuildLintRules`, `LintFields`, `LintText`), plus inbox message shape helpers (`ShapeMessage`, `ShapeBullet`, `ShapeLinkLine`).
   - **Approval**: `Approver` interface + `DecideCLI` (with optional `EnvFunc` hook for boundary environment customization).
   - **Execution**: `Executor` interface + crash-safe `ExecuteSteps` (`InFlight` tracking prevents double execution after a restart) + `CommandExecutor` for direct-argv script execution without an LLM in the loop.
   - **Owner notification**: `Transport` interface (`PullTransport` no-op default, `CommandTransport` for direct-argv notification hooks, `TransportFunc`).

## Per-Channel Adapters

- **GitHub PR comments/reviews**: [`devtools/gh-outbox`](../../gh-outbox/README.org) layers GitHub PR draft validation, head SHA/thread safety checks (`BeforeExecuteHook` behavior), `gh api` execution steps, and owner delivery on top of `pkg/outbox`. [`devtools/gh-nudge/cmd/gh-pr-review`](../../gh-nudge/pr-review.org) routes `--via-outbox` submissions through `gh-outbox`.
- **Slack outbound messages**: [`devtools/slackwatch`](../../slackwatch/docs/posting.org) routes outbound Slack approval, UI provenance evaluation, digest checks, message shaping, `decide` CLI interaction, and delivery orchestration through `pkg/outbox` while keeping SQLite persistence, identity pinning, and thread watching in `slackwatch`.

## Using from Non-LLM CLI Tools and Scripts

Any CLI tool or daemon without an LLM in the loop can drive `Engine` directly:

- Use `NewFileStore(stateDir)` + `DecideCLI{Run: Exec([]string{"decide"})}`
- Configure `Executor: CommandExecutor{Command: []string{"/path/to/executor"}}` (receives JSON `Record` on stdin and `OUTBOX_*` env vars, prints URL or `{"receipts":[...]}` JSON on stdout)
- Configure `Transport: PullTransport{}` (pull `Store.Get` status) or `Transport: CommandTransport{Command: []string{"/path/to/notify"}}` (receives notice text on stdin and `OUTBOX_*` env vars).
