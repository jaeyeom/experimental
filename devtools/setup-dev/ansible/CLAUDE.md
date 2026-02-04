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
