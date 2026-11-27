---
name: quotapace
description: CLI that records quota readings and projects them against a pacing plan.
---

# quotapace

`quotapace` records usage readings of rolling quota windows and projects them against a pacing plan.

Common commands:
- `quotapace` (or `quotapace dashboard [--profile P] [--recent D] [--stale D] [--watch D]`)
- `quotapace collect claude [--json | --jsonl] [--profile P [--auto-start]]`
- `quotapace record --profile P --window 7d --percent 27 --resets-in 6d5h`
- `quotapace record --profile remote --stdin`
- `quotapace plan start --profile P --window 7d`
- `quotapace plan set --profile P --window 7d --reset "Thu 15:00" --tz America/Los_Angeles --rate 0.55 --offset 8`
- `quotapace status [--profile P] [--json]`
- `quotapace report [--profile P] [--window W] [--json]`
- `quotapace check [--profile P]`
- `quotapace gate --profile P --window 7d --min-gap 5 [--stale 6h] [--json]`
- `quotapace gate --profile P --window 7d --min-gap 5 --watch --every 1m --notify-cmd CMD`
