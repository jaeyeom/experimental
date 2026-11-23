---
name: memstore
description: Compartmentalized personal memory store CLI for coding agents.
---

# memstore

`memstore` stores entities, structured facts, markdown note blocks, and episodic events across compartmentalized SQLite vaults with default-deny profile policies and FTS5 BM25 search.

Common commands:
- `memstore init --vault <name>` — initialize a configured vault.
- `memstore profile check <name> [--json]` — inspect a profile's access policy.
- `memstore resolve <query> [--json]` — resolve an entity by ID, alias, slug, or prefix.
- `memstore entity get <ref> [--json]` — inspect an entity and its permitted facts/notes/links.
- `memstore search <query> [--kind K] [--limit N] [--json]` — scoped FTS5 search.
- `memstore event add --vault V --channel CH --summary S [--entity E]` — record an episodic event.
- `memstore import-aside --src <dir> --vault V [--rules <file>] [--dry-run] [--json]` — import Aside Markdown directory.
- `memstore sync-check --profile P --dir <dir> [--stage] [--json]` — detect external Markdown edits and optionally stage diffs to triage.
- `memstore triage list|approve|reject ...` — review and resolve staged triage items.
