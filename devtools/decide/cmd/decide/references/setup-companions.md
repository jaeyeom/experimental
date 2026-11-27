# Setting up Companion Tools and Agent Skills

## 1. `orgctl` Contacts org tree

With `orgctl` installed and a rendered org spec JSON file, Contacts (`g` then `c` in `decide inbox`) can toggle between a flat list and an org tree (`t`).

In `${XDG_CONFIG_HOME:-$HOME/.config}/decide/config.json` (on the hub in hub mode; clients sync `ui.orgTree` from the hub and can override it locally to `false`):

```json
{
  "ui": {
    "orgTree": true,
    "orgSpec": "/path/to/org-spec.json"
  }
}
```

Verify that `orgctl tree --json <ui.orgSpec>` succeeds. If `orgctl` is missing or fails, Contacts falls back to the flat list with a status message.

## 2. `owners` allow-list & `contactAliases`

In multi-agent setups where only lead agents should open new inbox items (`decide ask`, `decide send --to`), configure `owners` (enforced on the hub in hub mode; replies and `--via ui` actions stay allowed):

```json
{
  "owners": {
    "mode": "warn",
    "allowed": ["lead/*"],
    "managers": {
      "team/*": "lead/planner"
    }
  }
}
```

- `mode`: `"off"` (default), `"warn"` (prints a warning to stderr and tags the item `off-line`; inspect with `decide list --off-line`), or `"enforce"` (refuses and exits `3`). `allowed` uses `path.Match` globs and must be non-empty when `mode` is `"warn"` or `"enforce"`.
- If an agent files under multiple names from the same pane, map them with `decide contacts alias <alias> <canonical>` or `"contactAliases": {"<alias>": "<canonical>"}` in `config.json` (`decide doctor` checks `agent-aliases`).

## 3. `slackwatch`, `gh-outbox`, and `prsync`

- **`slackwatch`**: when `post.mode` is set in `slackwatch` config, it files `decide` approval items (`post.decideOwner`, default `"slackwatch"`, optional `post.decideNotifyPane`, and `decideCommand` if `decide` is not on `PATH` or uses SSH). If wiring callback delivery from `decide` to a command target, add a `targets[]` `"command"` entry in `decide`'s `config.json` and pass `--notify-pane target:<name>`.
- **`gh-outbox`**: submits GitHub PR comment/review drafts as `decide` approval items (`--kind relay`, `--owner` set to the agent's `agentbus` name). Ensure `decide`, `agentbus`, and `gh` are on `PATH` (or set `decide_command`, `agentbus_command`, `gh_command`, and optional `notify_pane` in `$XDG_CONFIG_HOME/gh-outbox/config.toml`) and run `gh-outbox poll --watch` (or its systemd/launchd service).
- **`prsync`**: automatically reads open `decide` items as a busy signal in its checkout gate whenever `decide` is on `PATH` (no extra `decide` config needed).

## 4. Installing agent skills (`decide skill install`)

Install `SKILL.md` and `references/*.md` for all detected coding agents (`claude`, `codex`, `antigravity`, `grok`):

```sh
decide skill install --agent all --dry-run   # preview target paths
decide skill install --agent all             # add --force to overwrite foreign SKILL.md
decide skill status --json
```

Pass `--scope project` to install into the current repository instead of user home directories. For automated setups, `devtools/setup-dev/ansible/setup-decide.yml` installs `decide` and runs `decide skill install --agent all --force` idempotently.
