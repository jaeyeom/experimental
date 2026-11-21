---
name: gh-storage
description: Unified storage management CLI for gh-nudge state, backups, and locks.
---

# gh-storage

`gh-storage` manages the local `gh-nudge` storage directory (`$GH_STORAGE_HOME` or `~/.config/gh-nudge/storage`).

Common commands:
- `gh-storage init` — initialize storage.
- `gh-storage info [path]` — show storage info.
- `gh-storage ls [path]` — list storage contents.
- `gh-storage backup` / `restore` / `verify` — manage backups and integrity.
