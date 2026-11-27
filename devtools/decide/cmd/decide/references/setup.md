# Setting up `decide`

Follow these steps in order when installing, configuring, or troubleshooting `decide`.

## 1. Probe environment first (read-only)

Inspect what is already present before asking questions or changing files:

```sh
command -v decide && decide version
ls -la "${XDG_CONFIG_HOME:-$HOME/.config}/decide/config.json"
```

If `decide` is installed, run diagnostics:

```sh
decide doctor --json        # or: decide doctor --offline --json
```

Check which optional ecosystem tools exist on `PATH`:

```sh
for cmd in herdr agentbus orgctl emacs tailscale ssh slackwatch gh-outbox prsync; do
  command -v "$cmd" || true
done
```

## 2. Install or update the `decide` binary

Do not proceed to configuration until `decide` is installed and runnable.

If `decide` is missing, offer the user one of these install paths:

- **Go install (latest or pinned tag)**:
  ```sh
  go install github.com/jaeyeom/experimental/devtools/decide/cmd/decide@latest
  # or pin a release tag: @vX.Y.Z
  ```
- **From a local `experimental` checkout**:
  ```sh
  go install ./devtools/decide/cmd/decide
  # or: make install
  ```
- **Ansible playbook** (also runs `decide skill install --agent all --force`):
  `devtools/setup-dev/ansible/setup-decide.yml`

If `decide` was installed into `$(go env GOBIN)` or `$(go env GOPATH)/bin` and is not on `PATH`, help the user add `$(go env GOPATH)/bin` to `PATH` in their shell profile, then verify:

```sh
decide version
```

If an older `decide` binary is already installed and the user wants to upgrade, check with `decide update --check` and install with `decide update`.

## 3. Ecosystem interview

Ask only what is not already obvious from the probe (`config.json` and detected tools):

1. **Topology**:
   - **Standalone** (single machine holding the ledger), or
   - **SSH hub + client machines** (one hub holds the ledger; clients connect over SSH).
2. **Human UI(s)** (one or more):
   - **Terminal TUI** (`decide inbox`)
   - **Web / Mobile PWA** (`decide web`)
   - **Emacs** (`devtools/decide/emacs/decide.el`)
   - **CLI only** (`decide list` / `decide show` / `decide answer`)
3. **Answer delivery**:
   - **`herdr` panes** (plus optional `decide follow` service)
   - **`agentbus`** (`agentbus:<name>` delivery, live compose recipients, health banner, checkout leases)
   - **`pull`** (standalone scripts / CI polling `decide show --json` / `decide list --json`)
   - **Custom `targets[]` or `transports.<scheme>` plugins**
4. **Optional companions**:
   - **`orgctl`** (Contacts org-tree view)
   - **`slackwatch` / `gh-outbox` / `prsync`**
   - **`owners` allow-list** (multi-agent hierarchy)
   - **Agent skills** (`decide skill install --agent all`)

## 4. Load only the matching branch guides

Read **only** the `references/setup-*.md` file(s) next to this file (or print via `decide skill ref <path>`) that match the user's choices:

- **SSH hub + clients** -> [setup-hub.md](setup-hub.md) (`references/setup-hub.md`)
- **Terminal TUI or Emacs** -> [setup-tui-emacs.md](setup-tui-emacs.md) (`references/setup-tui-emacs.md`)
- **Web / Mobile PWA** -> [setup-web-mobile.md](setup-web-mobile.md) (`references/setup-web-mobile.md`)
- **Answer delivery (`herdr`, `decide follow`, `agentbus`, `pull`, `targets[]`, `transports`)** -> [setup-delivery.md](setup-delivery.md) (`references/setup-delivery.md`)
- **Companions (`orgctl`, `slackwatch`, `gh-outbox`, `prsync`, `owners`, agent skills)** -> [setup-companions.md](setup-companions.md) (`references/setup-companions.md`)

## 5. Confirm, apply, verify, and smoke-test

1. **Confirm before writing**: merge the chosen keys into `${XDG_CONFIG_HOME:-$HOME/.config}/decide/config.json`, show the exact diff, and wait for explicit user confirmation before writing `config.json` or enabling background services.
2. **Verify diagnostics**: run `decide doctor` (or `decide doctor --offline` when offline) until every check relevant to the chosen setup passes (`ok`).
3. **Install agent skills** (if desired):
   ```sh
   decide skill install --agent all
   decide skill status
   ```
4. **Optional smoke test**: offer to file a harmless note and view/archive it in the chosen UI:
   ```sh
   decide ask --class note --owner setup-test "Setup test note"
   ```
   Open it in `decide inbox`, `decide web`, Emacs (`M-x decide-list`), or verify via `decide list` / `decide show <id>`, then archive it (`e` / `y` in UI, or `decide done --via ui <id> archived`).
