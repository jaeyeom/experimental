# agent-pool Security & Threat Model

This document describes the security boundaries, threat mitigations, platform isolation guarantees, residual risks, and operational verification steps for `agent-pool` and `agent-poold`.

## 1. Trust Boundaries

`agent-pool` separates three principals:

1. **Producer (Requester)**: Calls `agent-poold` via MCP (`POST /mcp`) or REST/CLI (`/v1/*`) using a scoped producer bearer token. Producers never connect directly to volunteer machines.
2. **Central Broker (`agent-poold`)**: Authenticates tokens, enforces repository allowlists and rate limits, syncs default-branch `.agent-pool/catalog.json` snapshots, validates job arguments and outputs against strict JSON Schemas, runs Stage-2 DLP, and brokers leases.
3. **Consumer Worker (`agent-pool worker`)**: Outbound-only daemon on a volunteer host. Pulls leases over HTTPS, checks out a detached git worktree pinned to a verified default-branch `catalogSha`, executes `claude` inside a strict `cplt` sandbox under an isolated `$HOME`, enforces mid-flight turn/token/time budgets, and runs Stage-1 DLP before uploading deliverables.

## 2. Threat Model & Mitigations

### 2.1 Confused Deputy Repository Access

**Threat**: A Producer with access to the broker attempts to dispatch a skill against a private repository they cannot access, tricking a volunteer worker (whose host has read access to that repo) into reading and returning private code.

**Mitigations**:
- **Per-token `allowedRepos` allowlist**: Producer and worker tokens are scoped at mint time (`agent-poold token create --repos ...`). A producer token cannot dispatch jobs for repos outside its `allowedRepos`.
- **Optional broker-side requester GitHub PAT verification**: When `[github_acl] require_repo_access_check = true` is enabled in `broker.toml`, the producer sends `X-GitHub-Token` to the broker. `agent-poold` calls GitHub's API (`GET /repos/{owner}/{repo}`) with the requester's token, caches positive read access briefly (`cache_ttl`), and rejects unauthorized requests with `FORBIDDEN`.
- **Requester PAT never reaches volunteers**: `X-GitHub-Token` is validated exclusively on `agent-poold` and is never stored in the job payload, SQLite database, or forwarded to workers.

### 2.2 Indirect Prompt Injection & Arbitrary Code Execution

**Threat**: Malicious text in repository files, PRs, issues, or skill arguments attempts to hijack the coding agent to exfiltrate host secrets, modify repositories, or invoke external tools/MCP servers.

**Mitigations**:
- **Read-only skills only (v1)**: Both the broker and worker reject any catalog skill where `mode != "readonly"`.
- **Default-branch ancestry pinning**: Workers fetch the repository's default branch into a bare mirror and verify via `git merge-base --is-ancestor <catalogSha> refs/remotes/origin/<defaultBranch>` before checking out a detached worktree. Unmerged branches or arbitrary commits cannot supply untrusted `SKILL.md` prompts.
- **Strict argument & output schemas**: Arguments and deliverables are validated against strict JSON Schema subsets (`additionalProperties: false`, string patterns/lengths).
- **Isolated Claude execution**:
  - Dedicated isolated `$HOME` (`~/.local/share/agent-pool/isolated-home`) with `0700` permissions and a neutral `.gitconfig` (`gpgsign = false`, empty `credential.helper`).
  - `--strict-mcp-config --mcp-config <isolatedHome>/empty-mcp.json` and `--setting-sources project` prevent loading the volunteer's personal MCP servers or user-level Claude settings.
  - Cloud connectors (Gmail, Google Drive, Slack, Calendar) must be disabled on the donated `claude.ai` account (reminded by `agent-pool init-auth`).
- **Hard execution caps**: Effective limits are the minimum of catalog limits and local `worker.toml` caps (`max_turns`, `max_effective_tokens`, `timeout_seconds`, `max_deliverable_bytes`). Exceeding any cap terminates the sandboxed process group (`SIGTERM` then `SIGKILL`).

### 2.3 Two-Stage Data Loss Prevention (DLP)

Deliverables pass through two independent DLP scans before a Producer can read them:

- **Stage 1 (Consumer Worker)**: Before calling `POST /workers/jobs/{id}/complete`, the worker normalizes ephemeral worktree paths to `<repo>` and scans for:
  - High-confidence secret patterns (GitHub PATs/OAuth/App tokens, Anthropic/OpenAI keys, AWS access keys, Slack tokens, PEM private keys, Bearer/JWT tokens).
  - High-entropy values in secret-like assignments.
  - Host metadata leaks (real `$HOME`, OS username, hostname, personal email, generic `/Users/<name>/` or `/home/<name>/` paths).
  - Blocked payloads are quarantined locally (`0600`) and reported as `DLP_BLOCKED` with rule summaries only (raw secret values are never transmitted).
- **Stage 2 (Central Broker)**: `agent-poold` independently runs the DLP scanner on incoming deliverables before persisting or returning them, defending against buggy or misconfigured workers.

## 3. Linux vs. macOS `cplt` Isolation Matrix

`agent-pool` generates an isolated `cplt` `config.toml` (`preset = "strict"`, forced proxy with default allowlist, `gh_guard` read-only with `block_auth_token = true`, `git_guard` blocking push/force-push, clipboard denied, Docker/browser/GPG/tmp-exec/env-files disabled) and passes `--deny-path` for the volunteer's real `~/.claude`, `~/.claude.json`, `~/.ssh`, `~/.gitconfig`, `~/.config/gh`, `~/.aws`, and `~/.gnupg` (plus `~/Library/Keychains` on macOS).

| Control | Linux (`cplt` + Bubblewrap) | macOS (`cplt` + `sandbox-exec`) |
| --- | --- | --- |
| Filesystem isolation | `use_bubblewrap = true`, `refuse_unenforced = true`; only worktree and isolated home mounted | Seatbelt (`sandbox-exec`) profile with `--deny-path` on real home secret dirs |
| Network egress enforcement | `proxy.netns = true` + `proxy.forced = true` (kernel network namespace blocks direct bypass) | `proxy.forced = true` + Seatbelt network restrictions (no kernel netns; relies on Seatbelt + proxy) |
| Keychain / credential store | N/A (isolated `~/.claude/.credentials.json`) | `keychain_substitute = true` + `--deny-path ~/Library/Keychains` |
| Process cleanup | Process group (`Setpgid`) termination (`SIGTERM` -> grace -> `SIGKILL`) | Process group (`Setpgid`) termination (`SIGTERM` -> grace -> `SIGKILL`) |
| Recommended posture | Preferred for unattended or multi-repo workers | Suitable for trusted teammates; keep `supported_repos` minimal |

## 4. Volunteer `gh` Credential (`cplt-gh`) Residual Risks

When `github_auth = "cplt-gh"` is configured in `worker.toml`:
- `cplt` reads the volunteer's `gh` token from the real `GH_CONFIG_DIR` outside the sandbox and injects it only in the proxy while denying `GH_CONFIG_DIR` inside the sandbox and enabling `gh_guard` (`scope_check = true`, `block_auth_token = true`, `allow_api_write = false`, `allow_pr_merge = false`).
- **Residual risk**: The proxy still holds a credential scoped to the volunteer's GitHub account. A bug in `cplt`'s `gh_guard` or proxy domain allowlist could allow read access to other repositories the volunteer can read, or unintended API calls if an endpoint is misclassified as read-only.
- **Mitigation**: For public repositories or skills that only need the local git worktree, set `github_auth = "none"` in `worker.toml` to disable token injection completely.

## 5. Live `cplt` + `claude` Smoke-Test Verification Checklist

Before enabling a volunteer worker for unattended team use, verify on the target host:

1. Run `agent-pool doctor` and confirm `cplt`, `platform-sandbox`, `claude`, `quotapace`, `gh-auth` (if enabled), and `broker-healthz` are `ok`.
2. Verify `agent-pool init-auth` created `~/.local/share/agent-pool/isolated-home` (`0700`) and that cloud connectors are disabled in `claude.ai` settings.
3. Run `agent-pool worker run --dry-run` and verify heartbeats appear on `agent-poold` without claiming jobs.
4. Dispatch a test read-only skill against a test repo and verify:
   - The sandboxed process cannot read `~/.ssh`, `~/.aws`, or real `~/.claude`.
   - Direct outbound connections to non-allowlisted domains are blocked by the forced proxy.
   - `git push` and `gh auth token` fail inside the sandbox.
   - A synthetic secret in output triggers Stage-1 `DLP_BLOCKED` and local quarantine without reaching the broker.
   - `agent-pool worker stats --since 7d --json` records the completed run in `usage.db`.

## 6. v2 Upgrade Path: `broker-app` GitHub Auth

In v2, `github_auth = "broker-app"` will replace volunteer `gh` credentials with short-lived, single-repository, read-only GitHub App installation tokens minted by `agent-poold` per leased job:

- Eliminates volunteer personal GitHub tokens from worker hosts (`github_auth = "cplt-gh"` becomes unnecessary).
- Enforces single-repo read-only scope cryptographically at GitHub's API boundary rather than relying solely on `gh_guard` heuristics.
- Automatically expires tokens shortly after the job's `timeout_seconds` window.
