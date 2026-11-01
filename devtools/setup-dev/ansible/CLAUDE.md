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

Skill text written into an agent skill path is pinned the same way.
`review-text`, `apply-text`, and `check-text` hash a command's stdout and
store the approved bytes under
`~/.cache/setup-dev/verified-scripts/texts/`. `apply-text` writes the
destination only when the hash matches. A mismatch on a terminal shows the
diff and asks on `/dev/tty`. Without a terminal it exits non-zero and
prints the `review-text` command. `setup-herdr.yml` installs
`herdr --skill` with `apply-text herdr-skill`.

The Ansible local command module has no controlling terminal, so that
prompt cannot run inside the playbook task. When the selected playbooks
import `setup-herdr.yml`, `ensure.sh` runs `herdr.yml` first, then runs
`review-text herdr-skill -- herdr --skill` in this shell if the text is
non-empty and not the stored approval. A no stops `ensure.sh` before any
skill file is written. `apply-text` then writes the approved bytes. The
later playbook imports `herdr.yml` again; that second run is idempotent
when the installed version already matches. `./ensure.sh --check` (and
`-C`) still runs that preliminary `herdr.yml` in check mode and skips the
skill review, so a dry run does not prompt or store an approval. Missing
`herdr`, a failing `herdr --skill`, or empty output warns and continues,
so an older binary does not fail `./ensure.sh setup-vibe-coding`. This is
separate from
`pre_approve_urls`, which stays URL-only and runs before the binary is
upgraded. A direct `ansible-playbook setup-herdr.yml` fails the skill task
and prints the `review-text` command. The installer URL stays on
`verified-run exec`.

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
- Do not put `*` before a git subcommand in allow rules (e.g.
  `Bash(git * add *)`). Git global options such as `-c` and `--exec-path`
  can run arbitrary commands, and Claude Code warns on those patterns at
  startup. `git -C <path> ...` will prompt; `cd <path> && git add ...`
  still matches `Bash(git add *)`.
- The `:*` form is recognized only as a trailing suffix. In the middle of a
  pattern (e.g. `Bash(git:* push)`), the colon is treated as a literal.
- Non-Bash tools keep their own wrappers: `Read(...)`, `Edit(...)`,
  `WebFetch(domain:...)`, `WebSearch`, `mcp__<server>__<tool>`,
  `Agent(<name>)`.

Both styles co-exist in this repo's playbook; new entries should default to
the space form to match the dialog output.
