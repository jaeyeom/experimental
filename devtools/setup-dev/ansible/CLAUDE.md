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
`setup-claude-sandbox.yml` (and any `.claude/settings*.json`), always use the
modern tool-wrapped syntax. The legacy colon-only form is no longer accepted.

| Form | Example | Status |
|------|---------|--------|
| Modern | `Bash(git log:*)`, `Bash(gh issue edit * --add-assignee @me)` | Use this |
| Legacy | `git log:*`, `gh issue edit:*` | Do not use |

Rules:
- Every Bash entry must be wrapped: `Bash(<command pattern>)`.
- Use `:*` after a subcommand to allow any trailing arguments
  (`Bash(go test:*)`).
- Use shell-glob `*` inside the parentheses to constrain specific argument
  positions (`Bash(gh issue edit * --add-assignee @me)`).
- Non-Bash tools keep their own wrappers: `Read(...)`, `WebFetch(domain:...)`,
  `WebSearch`, `mcp__<server>__<tool>`.
