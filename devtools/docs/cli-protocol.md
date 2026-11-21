# Tool CLI Protocol

This document defines the standard CLI protocol for agent-workflow tools in `devtools/`. Every tool that adopts this protocol exposes the same subcommands, flags, output formats, and exit codes via the shared `devtools/pkg/toolcli` package.

## Common Rules

1. **Human output vs. errors**: Human-readable output goes to `stdout`; error messages go to `stderr`.
2. **Stable `--json` output**: Every subcommand that supports `--json` emits a single JSON object on `stdout` with camelCase field names.
3. **No interactive prompts**: None of the protocol subcommands ever prompt for input on `stdin`.
4. **Network access**: Only `update` and the `revision` check in `doctor` (skipped with `--offline`) access the network. All other subcommands and checks are offline.

---

## Subcommands

### 1. `<tool> version [--json]`

Prints module path, version, VCS revision, and build time from the binary's embedded build info (`runtime/debug.ReadBuildInfo`).

**Human output**:
```text
<tool> <version> (<vcsRevision>)
module: <modulePath>
built: <buildTime>
```

**JSON output (`--json`)**:
```json
{
  "tool": "decide",
  "modulePath": "github.com/jaeyeom/experimental",
  "version": "v0.1.0",
  "vcsRevision": "abc1234",
  "vcsModified": false,
  "buildTime": "2026-11-20T21:00:00Z"
}
```

**Exit codes**:
- `0`: Success.
- `2`: Invalid arguments or flags.

---

### 2. `<tool> update [--check] [--ref <ref>] [--json]`

Installs the latest version of the tool and its declared companion binaries from the default branch (or `--ref`, defaulting to `latest`). Binaries are installed into the directory of the running executable (`GOBIN=<exe-dir>`).

- `--check`: Only checks whether a newer revision exists; does not install.
- `--ref <ref>`: Target branch, tag, or module version (default `latest`).
- Reports `old -> new` revision for each binary.
- **Never restarts** daemons or background services; instead, prints which services must be restarted to take effect.
- If skills were previously installed via `skill install`, reports which installed skills are now stale and prints the command to refresh them.

**Human output**:
```text
decide: abc1234 -> def5678 (updated)
restart needed: decide follow
stale skills:
  claude (user): ~/.claude/skills/decide/SKILL.md
refresh with: decide skill install --agent claude --scope user
```

**JSON output (`--json`)**:
```json
{
  "tool": "decide",
  "ref": "latest",
  "checkOnly": false,
  "updateAvailable": true,
  "updated": true,
  "binaries": [
    {
      "name": "decide",
      "package": "github.com/jaeyeom/experimental/devtools/decide/cmd/decide",
      "oldRevision": "abc1234",
      "newRevision": "def5678",
      "updated": true
    }
  ],
  "restartNeeded": ["decide follow"],
  "staleSkills": [
    {
      "agent": "claude",
      "scope": "user",
      "path": "/home/user/.claude/skills/decide/SKILL.md",
      "installedHash": "sha256:...",
      "currentHash": "sha256:..."
    }
  ],
  "refreshCommand": "decide skill install --agent claude --scope user"
}
```

**Exit codes**:
- `0`: Updated successfully, or already at the latest revision.
- `1`: A newer revision exists (only returned when `--check` is set).
- `2`: Failure (network error, install failure, invalid flags).

---

### 3. `<tool> doctor [--json] [--offline]`

Runs standard diagnostics plus tool-specific checks and prints one line per check.

- `--offline`: Skips network-dependent checks (such as `revision` and remote hub reachability).

**Standard checks** (provided by `devtools/pkg/toolcli`):
1. `binary-on-path`: Verifies the binary resolved on `PATH` matches the running executable.
2. `revision`: Compares the running revision against the latest available revision (skipped with `--offline`).
3. `config`: Verifies the tool's config file is found and parses cleanly (if configured).
4. `external-commands`: Verifies required external commands are present on `PATH`.
5. `skills`: Verifies any installed agent skills match the embedded skill content hash.

**Human output**:
One line per check: `<status>  <name>  <detail>` (with `hint: <fix>` for non-`ok` results):
```text
ok    binary-on-path     /Users/user/go/bin/decide
warn  revision           skipping check (--offline)
ok    config             /Users/user/.config/decide/config.json
ok    external-commands  herdr, ssh
fail  skills             claude (user) is stale; hint: run `decide skill install --agent claude --scope user`
```

**JSON output (`--json`)**:
```json
{
  "tool": "decide",
  "status": "fail",
  "checks": [
    {
      "name": "binary-on-path",
      "status": "ok",
      "detail": "/Users/user/go/bin/decide"
    },
    {
      "name": "skills",
      "status": "fail",
      "detail": "claude (user) is stale",
      "hint": "run `decide skill install --agent claude --scope user`"
    }
  ]
}
```

**Exit codes**:
- `0`: All checks are `ok`.
- `1`: One or more `warn` results, no `fail` results.
- `2`: One or more `fail` results (or invalid flags).

---

### 4. `<tool> skill` and Subcommands

#### `<tool> skill`

With no arguments, prints the raw embedded skill markdown to `stdout` unchanged.

#### `<tool> skill install`

```text
<tool> skill install --agent <claude|codex|antigravity|grok|all> [--scope user|project] [--dir D] [--dry-run] [--force]
```

Writes the rendered skill to the target agent's skill location.

- `--agent`: Target agent (`claude`, `codex`, `antigravity`, `grok`), repeatable (`--agent claude --agent codex`), or `all` for every agent detected on the machine.
- `--scope`: `user` (default) or `project`.
- `--dir D`: Override target directory (writes `<D>/SKILL.md` or `<D>/<tool>/SKILL.md`? Writes `<D>/<tool>/SKILL.md` when `--dir` is a skills root, or `<D>/SKILL.md` if `--dir` ends with `/<tool>` — specifically `<D>/SKILL.md` when `--dir` is given as the skill directory). Wait: let's be explicit: `--dir D` writes `<D>/SKILL.md` directly.
- `--dry-run`: Reports what would be written without modifying disk.
- `--force`: Allows overwriting an existing file that was not installed by this tool. Without `--force`, `skill install` refuses to overwrite foreign files.
- Never edits agent settings, hooks, or permissions; only writes skill files.

**Front-matter stamp**:
Each installed `SKILL.md` records the tool, version, and SHA-256 hash of the raw embedded skill in its YAML front matter:
```yaml
---
name: decide
description: "..."
toolcli-tool: decide
toolcli-version: v0.1.0
toolcli-hash: sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
---
```

#### `<tool> skill uninstall`

```text
<tool> skill uninstall --agent <claude|codex|antigravity|grok|all> [--scope user|project] [--dir D] [--dry-run]
```

Removes only skill files that have a matching `toolcli-tool: <tool>` front-matter stamp. Foreign files are never removed.

#### `<tool> skill status [--json]`

Lists each agent and scope with its installation path, installed version, and whether it is current, stale, missing, or foreign.

**JSON output (`--json`)**:
```json
{
  "tool": "decide",
  "currentHash": "sha256:...",
  "entries": [
    {
      "agent": "claude",
      "scope": "user",
      "path": "/home/user/.claude/skills/decide/SKILL.md",
      "status": "current",
      "installedVersion": "v0.1.0",
      "installedHash": "sha256:..."
    }
  ]
}
```
Status values: `missing`, `current`, `stale`, `foreign`.

#### Agent Skill Location Table

| Agent | User Scope (`--scope user`) | Project Scope (`--scope project`) |
| --- | --- | --- |
| `claude` | `~/.claude/skills/<tool>/SKILL.md` | `.claude/skills/<tool>/SKILL.md` |
| `codex` | `~/.agents/skills/<tool>/SKILL.md` | `.agents/skills/<tool>/SKILL.md` |
| `antigravity` | `~/.gemini/config/skills/<tool>/SKILL.md` | `.agents/skills/<tool>/SKILL.md` |
| `grok` | `~/.grok/skills/<tool>/SKILL.md` | `.grok/skills/<tool>/SKILL.md` |

Use `--dir <D>` to override the skill directory (writes `<D>/SKILL.md`) for any other agent or custom path.
