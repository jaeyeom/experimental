# Setting up SSH Hub Mode

Use hub mode when one machine (the **hub**) holds the ledger (`$XDG_STATE_HOME/decide/ledger.json`, default `~/.local/state/decide/ledger.json`) and one or more **client** machines read and write it over SSH.

## 1. Hub machine setup

On the hub machine:

1. Ensure `decide` is installed on a `PATH` visible to non-interactive POSIX login shells (`ssh -T -o BatchMode=yes user@hub decide version` must succeed without prompting for a password or passphrase).
2. No background daemon is required for `decide serve --stdio` itself: client `decide inbox` processes spawn `decide serve --stdio` over SSH on demand.
3. If using `decide web` or `decide follow`, run those services **on the hub** (where the ledger lives).

Verify from a client machine:

```sh
ssh -T -o BatchMode=yes me@hub decide version
```

## 2. Client machine configuration

On each client machine, configure `${XDG_CONFIG_HOME:-$HOME/.config}/decide/config.json`:

```json
{
  "hub": "me@hub",
  "hubTimeout": "5s",
  "machine": "laptop",
  "hubMachine": "hub"
}
```

- `hub`: SSH target (`user@host` or SSH config host alias). `DECIDE_HUB` overrides `hub`; `--remote user@host` overrides for a single command.
- `hubTimeout`: SSH connect timeout as a positive Go duration (default `"5s"`).
- `machine`: this client's `herdr` machine label as the hub knows it (`DECIDE_MACHINE` overrides it). When an agent runs `decide ask` inside a `herdr` pane on the client, `decide` sends `DECIDE_AS_HANDLE=herdr:<machine>/<session>/<pane>` over SSH so the hub records `notifyPane` and `notifyMachine` and delivers answers back via `herdr --machine`.
- `hubMachine`: the hub's `herdr` machine label on this client so pressing `o` in `decide inbox` focuses an owner pane on the hub via `herdr --machine <hubMachine>`.

Client machines never create a local `ledger.json`. Commands other than `help`, `version`, `update`, `doctor`, `skill`, `avatars`, `keys`, and `inbox` run on the hub via `ssh -T -o BatchMode=yes`.

## 3. Local replica & outbox behavior

On a client, `decide inbox` opens one SSH session to `decide serve --stdio` on the hub:

- Maintains a local replica under `$XDG_CACHE_HOME/decide/replicas` synced by monotonic sequence number (`watch`). Reads render immediately from the replica even while offline.
- Queues UI writes (`answer`, `reply`, `done`, `archive`, `snooze`, `unsnooze`, `send`, settings) into `$XDG_CACHE_HOME/decide/outbox` so the UI never blocks. View pending/held/failed entries in the Outbox folder (`g` then `o`); `ui.outboxStaleAfter` (default `"5m"`) controls stale warnings.

## 4. Config precedence & verification

Display keys (`avatars`, `avatarDefault`, `avatarMono`, `recipients`, `recipientsTimeout`, `targets`, `hyperlinks`, `hiddenContacts`, `favoriteContacts`, `contactAliases`, `ui.orgTree`) are inherited from the hub unless overridden locally. Local-only keys (`hub`, `hubTimeout`, `machine`, `hubMachine`, `openCommand`, `checkouts`, `notifyTemplate`, `sendTemplate`, `web`, and `ui` keys other than `ui.orgTree`) stay local. `owners` is enforced on the hub.

Verify merged settings and connectivity:

```sh
decide config show --effective
decide doctor
```
