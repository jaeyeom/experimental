---
name: gh-ask
description: Loopback server CLI that answers questions about pull request line ranges.
---

# gh-ask

`gh-ask` runs a loopback HTTP server that hands questions about pull request line ranges to a configured command.

Common commands:
- `gh-ask token [--config PATH]` — create the shared secret if missing and print it.
- `gh-ask serve [--config PATH]` — serve `POST /ask`, `GET /status/<id>`, and `GET /thread`.
