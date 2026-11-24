---
name: agent-pool
description: Dispatch read-only coding agent skills to pooled volunteer workers and manage consumer worker daemons.
---

# agent-pool

`agent-pool` is the CLI for both **Producers** (dispatching read-only skills to pooled capacity) and **Consumers** (volunteering idle Claude Code capacity via a sandboxed worker daemon).

## Producer Commands (CLI or MCP)

Producers can connect over MCP (`POST /mcp` on `agent-poold`) or use the first-class CLI directly. Configure connection via `AGENT_POOL_BROKER_URL` + `AGENT_POOL_TOKEN`, `--broker` / `--token-file`, or `~/.config/agent-pool/client.toml`.

- `agent-pool status [--json]` — inspect active workers, pool health, queue depth, and available skills.
- `agent-pool dispatch <repo> <skill> --args-json '{...}' [--idempotency-key <k>] [--wait] [--timeout 5m] [--json]` — enqueue a read-only skill job (and optionally wait for completion).
- `agent-pool result <job_id> [--wait] [--timeout 5m] [--json]` — fetch job state, progress, usage report, and structured deliverable.
- `agent-pool cancel <job_id> [--json]` — cancel a queued or running job.

## Catalog Validation

- `agent-pool catalog validate [path-to-repo-or-catalog.json]` — lint `.agent-pool/catalog.json` (and verify referenced `SKILL.md` files when given a repo directory).

## Consumer Worker Commands

- `agent-pool init-auth [--config worker.toml]` — initialize isolated `$HOME` and run `claude login` for the donated account.
- `agent-pool worker run [--config worker.toml] [--dry-run]` — run the outbound-only consumer worker daemon.
- `agent-pool worker pause [--config worker.toml] [--reason "..."]` — pause claiming new jobs while letting in-flight jobs finish.
- `agent-pool worker resume [--config worker.toml]` — resume claiming jobs.
- `agent-pool worker stats [--config worker.toml] [--since 7d] [--json]` — show local donation ledger stats from `usage.db`.
- `agent-pool worker install [--config worker.toml] [--dry-run]` — render or install the `systemd --user` (Linux) or `launchd` (macOS) worker service.

## Diagnostics & Maintenance

- `agent-pool doctor [--json] [--offline]` — verify `cplt`, platform sandbox support (`bwrap` or `keychain_substitute`), `claude`, `claude-usage`, `gh`, and broker `/healthz`.
- `agent-pool version [--json]` — show binary version and VCS revision.
- `agent-pool update [--check] [--ref <ref>] [--json]` — update `agent-pool` and companion `agent-poold`.
- `agent-pool skill [install|uninstall|status]` — manage this embedded skill file.
