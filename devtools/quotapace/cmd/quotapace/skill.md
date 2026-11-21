---
name: quotapace
description: CLI that records quota readings and projects them against a pacing plan.
---

# quotapace

`quotapace` records usage readings of rolling quota windows and projects them against a pacing plan.

Common commands:
- `quotapace record --profile P --window 7d --percent 27 --resets-in 6d5h`
- `quotapace plan start --profile P --window 7d`
- `quotapace status [--profile P] [--json]`
- `quotapace report [--profile P] [--window W] [--json]`
- `quotapace check [--profile P]`
