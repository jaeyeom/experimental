# Ansible Development Setup

This directory contains a Go-based generator for cross-platform Ansible playbooks.

## Skill Reference

When working with this codebase, use the **ansible-dev-setup** skill (`/ansible-dev-setup`) which provides:
- How to add new packages and platform-specific tools
- Installation method types (brew, apt, cargo, go install, uv, etc.)
- Playbook regeneration workflow
- Testing procedures

## Quick Commands

```bash
# Regenerate playbooks after editing packages_data.go
make generate-ansible   # from repo root
# or
make                    # from this directory

# Test playbook syntax
bazel test //devtools/setup-dev/ansible:ansible_syntax_tests
```

## Key Files

- `packages_data.go` - Package definitions (edit this to add tools)
- `generate_packages.go` - Main generator
- `README.org` - Full documentation

## Shell Installer Verification

Any `ShellInstallMethod` whose `InstallCommand` downloads and executes an
arbitrary remote script (e.g. `curl ... | bash`, `wget ... | sh`, or
piping any HTTP(S) URL into a shell) **must** route the download through
the local `verified-run` helper:

```go
InstallCommand: "{{ playbook_dir }}/verified-run exec https://example.com/install.sh -- <args>",
```

`verified-run` pins the script by SHA-256 and refuses to execute if the
upstream content changes, forcing a manual review before re-approval.
This guards against supply-chain compromise of third-party install
scripts.

- Use `verified-run exec <url>` for scripts with no args.
- Use `verified-run exec <url> -- <args>` to pass arguments to the
  script (everything after `--` is forwarded).
- Reference examples: `starship`, `githooks-cli`, `grok`.
- Raw `curl ... | bash` without `verified-run` is **not** acceptable for
  new entries, even if older entries (e.g. `claude`) still use it.

## Claude Code Permission Rules

When editing the `permissions.allow` / `permissions.deny` lists in
`setup-claude-sandbox.yml` (and any `.claude/settings*.json`), every Bash
entry must be wrapped in `Bash(...)`. The unwrapped colon-only form (e.g.
`git log:*` without the `Bash(...)` wrapper) is the legacy syntax and is no
longer accepted.

Inside `Bash(...)`, the official docs (code.claude.com/docs/en/permissions)
specify:

| Pattern | Example | Notes |
|---------|---------|-------|
| Exact match | `Bash(go version)` | Matches that exact command only |
| Prefix with word boundary | `Bash(npm run *)` | Preferred — what `/permissions` writes |
| Prefix alias | `Bash(npm run:*)` | Equivalent to `Bash(npm run *)`, trailing only |
| Middle wildcard | `Bash(git * main)` | Supported anywhere in the pattern |
| Suffix wildcard | `Bash(* --help *)` | Supported |
| Match-all | `Bash` or `Bash(*)` | Matches every Bash command |

Notes:
- **Prefer the space form `Bash(<prefix> *)`** for prefix matching — this is
  the canonical form the permission dialog writes when you accept "Yes,
  don't ask again". `Bash(<prefix>:*)` is an equivalent alias accepted only
  at the end of a pattern.
- The space before `*` enforces a word boundary: `Bash(ls *)` matches
  `ls -la` but not `lsof`. Without the space (`Bash(ls*)`), both match.
- A single `*` matches across spaces, so it can span multiple arguments.
- The `:*` form is recognized only as a trailing suffix. In the middle of a
  pattern (e.g. `Bash(git:* push)`), the colon is treated as a literal.
- Non-Bash tools keep their own wrappers: `Read(...)`, `Edit(...)`,
  `WebFetch(domain:...)`, `WebSearch`, `mcp__<server>__<tool>`,
  `Agent(<name>)`.

Both styles co-exist in this repo's playbook; new entries should default to
the space form to match the dialog output.
