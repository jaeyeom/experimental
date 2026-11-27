# pkg

Shared Go packages used by the repository’s developer tools.

This directory contains internal packages, not a standalone command. Package documentation and usage are maintained alongside each package’s source.

## Packages

- `notify` — Shared direct-argv notification command builder and runner used by `authwatch` and `secretwatch`.
- `outbox` — Shared, channel-agnostic confirm-before-doing gate (draft storage, preflight linting, inbox shaping, human-UI approval evaluation via `decide`, crash-safe step execution, and owner notification transports).
