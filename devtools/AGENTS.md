# Devtools Architecture & Boundary Rules

These rules apply to all connectable tools under `devtools/` (such as `decide`,
`agentbus`, `slackwatch`, `gh-outbox`, `gh-ask`, `installcheck`, `orgctl`, and
`prsync`). They supplement the root `AGENTS.md`.

## Core Principles

### 1. Keep the Core Agnostic

- Core domain logic (state, storage, schema, protocol) must remain agnostic to
  whatever connects to it.
- Do not hardcode `herdr`, terminal pane layouts (`HERDR_PANE_ID`), a specific
  LLM harness, or a sibling tool into core packages.
- Every tool must remain usable standalone without requiring companion tools to
  be installed or running.

### 2. Isolate Peer Integrations at the Boundary

- Put peer integrations (`herdr`, `agentbus`, `tmux`, chat bridges, etc.) behind
  a narrow core boundary—such as a `Transport` interface or an executable
  JSON-over-stdio plugin—never woven into core logic.
- Keep convenience profiles, presets, and config defaults for common pairings at
  the configuration/adapter edge only. They must never become core assumptions
  or mandatory dependencies.

### 3. Treat Non-LLM and Traditional CLI Tools as First-Class

- Coordination tools like `decide` and `agentbus` are general-purpose
  primitives, not LLM-only tools.
- Traditional CLI tools, daemons, and scripts (`slackwatch`, `gh-outbox`, cron
  jobs) must be able to invoke them and consume structured input/answers without
  an LLM in the loop.
- **Concrete example (`decide` + `slackwatch`)**: `slackwatch` (a plain daemon)
  can post a decision request to `decide` when human judgment is needed—for
  example, confirming whether to reply or escalate a thread—and read back the
  structured answer (JSON / exit status / pull target) to act on it directly,
  with no LLM harness or `herdr` pane involved.

### 4. Keep Interfaces Conservative

- Prefer small, general boundaries (transport/adapter interfaces, structured
  JSON or environment contracts) over adding special-case subcommands, flags, or
  branches for individual callers or missing backends.
- When a backend is absent or not configured, degrade cleanly through the same
  interface (e.g., a pull/no-op transport) instead of scattering caller-specific
  checks across core logic.
