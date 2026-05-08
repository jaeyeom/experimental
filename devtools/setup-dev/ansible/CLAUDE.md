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

## Claude Code Permission Rules

When editing the `permissions.allow` / `permissions.deny` lists in
`setup-claude-sandbox.yml` (and any `.claude/settings*.json`), use the
tool-wrapped syntax that Claude Code's `/doctor` validator accepts. Two forms
are valid; everything else is rejected.

| Pattern | Example | Status |
|---------|---------|--------|
| Prefix match (colon-star) | `Bash(go test:*)`, `Bash(gh issue view:*)` | Use this |
| Exact match | `Bash(go version)`, `Bash(npm install express)` | Use this |
| Wildcard-only `*` | `Bash(go test*)`, `Bash(go test *)` | Invalid |
| Middle / trailing `*` | `Bash(gh issue edit * --add-assignee @me)` | Invalid |
| Unwrapped (legacy) | `git log:*`, `gh issue:*` | Invalid |

Rules:
- Every Bash entry must be wrapped: `Bash(<command pattern>)`.
- Use `:*` (colon then star) for prefix matching after a subcommand
  (`Bash(go test:*)`). Plain `*` is not a valid wildcard.
- For finer constraints than a prefix allows, list exact commands instead of
  using mid-pattern wildcards.
- Non-Bash tools keep their own wrappers: `Read(...)`, `WebFetch(domain:...)`,
  `WebSearch`, `mcp__<server>__<tool>`.

Verify a settings file with `claude doctor` (or `/doctor` inside a session)
after edits — it flags any unsupported pattern.
