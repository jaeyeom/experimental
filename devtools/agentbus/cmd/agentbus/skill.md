---
name: agentbus
description: Local and hub-mode message bus CLI for coding agents running in herdr panes.
---

# agentbus

`agentbus` lets coding agents in separate `herdr` panes register names, send and reply to messages, manage checkout leases, and coordinate jobs via `agentbusd`.

Common commands:

- `agentbus register <name> --when "<work>"` — claim a bus name.
- `agentbus send <name|handle> "<body>" [--expect-reply]` — send a message (`--expect-reply` marks it for `asks`).
- `agentbus reply <id> "<body>"` — reply to a message.
- `agentbus list [--json]` — list registered agents.
- `agentbus held|asks|dialogs [--json]` — health checks for stuck holds, unanswered asks, and pending dialogs (exit 0 none, 1 matched, 2 error).
- `agentbus doctor [--json] [--offline]` — run diagnostics.
