---
name: agentbus
description: Local and hub-mode message bus CLI for coding agents running in herdr panes.
---

# agentbus

`agentbus` lets coding agents in separate `herdr` panes register names, send and reply to messages, manage checkout leases, and coordinate jobs via `agentbusd`.

Common commands:

- `agentbus register <name> --when "<work>"` — claim a bus name.
- `agentbus send <name|handle> "<body>"` — send a message.
- `agentbus reply <id> "<body>"` — reply to a message.
- `agentbus list [--json]` — list registered agents.
- `agentbus doctor [--json] [--offline]` — run diagnostics.
