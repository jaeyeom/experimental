---
name: authwatch
description: CLI that monitors agent CLI OAuth token expiries and notifies on level changes.
---

# authwatch

`authwatch` monitors OAuth token expiries for agent CLIs and notifies when status levels change.

Common commands:
- `authwatch check [--config PATH]` — run one check pass.
- `authwatch run [--every 5m] [--config PATH]` — run continuously.
- `authwatch status [--profile P] [--json]` — print current levels and expiries.
- `authwatch report-failure --profile P` — record an auth failure.
