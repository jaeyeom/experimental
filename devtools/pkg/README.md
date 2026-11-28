# pkg

Shared Go packages used by the repository’s developer tools.

This directory contains incubator packages (`baseline`, `ghauth`, `ghtest`,
`notify`, `outbox`). Graduated shared packages (`genfiles`, `ghcli`, `ghlimit`,
`herdrtest`, `hubcall`, `procinfo`, `remotecall`, `sqlitedb`, `toolcli`,
`version`) now live in [`chorong-sh/chorong`](https://github.com/chorong-sh/chorong/tree/main/pkg).

## Packages

- `baseline` — Shared JSON baseline loading and transition checks for repository linters.
- `ghauth` — GitHub authentication token source chain.
- `ghtest` — GitHub CLI mock executor and test helpers.
- `notify` — Shared direct-argv notification command builder and runner used by `authwatch` and `secretwatch`.
- `outbox` — Shared, channel-agnostic confirm-before-doing gate (draft storage, preflight linting, inbox shaping, human-UI approval evaluation via `decide`, crash-safe step execution, and owner notification transports).
