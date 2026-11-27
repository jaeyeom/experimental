# Setting up Answer Delivery

`decide` routes answers and compose messages based on the item's `notifyPane` (precedence: `--notify-pane` flag -> `HERDR_PANE_ID` when inside `herdr` -> `notifyPane` in `config.json` -> `"pull"`).

## 1. `herdr` panes + `decide follow`

Inside `herdr`, `--owner` defaults to the current tab label and `notifyPane` is captured automatically.

- **`notifyTemplate`** (machine-local, runs via `sh -c` for local relay answers; empty runs nothing; no `{...}` substitution — reads `DECIDE_*` env vars such as `DECIDE_ID`, `DECIDE_ANSWER`, `DECIDE_ANSWER_LABEL`, `DECIDE_OWNER`, `DECIDE_WORKER`, `DECIDE_PANE`, `DECIDE_KIND`, `DECIDE_QUESTION`, `DECIDE_ANSWERED_AT`, `DECIDE_ANSWERED_BY`, `DECIDE_THREAD_HEADER`, `DECIDE_REPLY_HINT`, `DECIDE_MACHINE`, `DECIDE_DELIVERY_KEY`):
  ```json
  {
    "notifyTemplate": "herdr agent prompt \"$DECIDE_PANE\" \"$DECIDE_THREAD_HEADER\n$DECIDE_ANSWER\n$DECIDE_REPLY_HINT\""
  }
  ```
- **`sendTemplate`** (compose / remote `herdr` delivery, default `herdr agent prompt {pane}`, note passed as its own argument without shell interpolation).
- **`decide follow` service** (run on the hub/ledger machine **unless** `decide web` is already running without `--no-follow`):
  - Retries failed deliveries (5s, 20s, max 3 attempts) and sends one stale follow-up after `ui.staleAnswerAfter` (default `"30m"`).
  - Run manually or from cron with `decide follow [--every 1m] [--once]`.
  - **Linux**: install `~/.config/systemd/user/decide-follow.service` (`ExecStart=%h/go/bin/decide follow`, `Restart=on-failure`, `WantedBy=default.target`), then `systemctl --user daemon-reload`, `systemctl --user enable --now decide-follow`, and optionally `sudo loginctl enable-linger $USER`.
  - **macOS**: install `~/Library/LaunchAgents/com.github.jaeyeom.decide-follow.plist` running `decide follow` (`RunAtLoad`, `KeepAlive`) and load with `launchctl bootstrap gui/$(id -u) ...`.
  - Ensure `decide`, the notify command, and `herdr` are on `PATH` in the service environment (`Environment=PATH=...` or `EnvironmentVariables`).

## 2. `agentbus` integration

Configure in `config.json`:

```json
{
  "recipients": {"agentbus": true},
  "recipientsTimeout": "3s",
  "ui": {"busHealthInterval": "15s", "lateDeliveryAfter": "5m"},
  "checkouts": [
    {"name": "repo", "path": "/abs/path/to/repo"},
    {"name": "laptop-repo", "machine": "laptop", "path": "/abs/path/to/repo"}
  ]
}
```

- `recipients.agentbus`: adds `agentbus list --json` names to compose recipients, enables the bus-health banner (`ui.busHealthInterval`), and polls delivery marks (`waiting`, `delivered HH:MM`, `failed`).
- `agentbus:<name>` target (via `notifyPane`, `--notify-pane`, or `targets[].pane`): delivers with `agentbus send --client-id <id>:<answeredAt>`.
- `checkouts`: maps status resource names to git checkout paths (add `"machine"` on the hub for checkouts on remote machines) so `decide status list` and the Status view show `agentbus lease status`.
- `decide doctor` verifies `agentbus` on `PATH` (fails if `recipients.agentbus`, `checkouts`, or `agentbus:` targets are configured without `agentbus`).

## 3. `pull` mode (standalone / scripts / CI)

Outside `herdr` with no `notifyPane` configured (or with `--notify-pane pull`), `decide` stores `pull` and performs no push delivery. Poll from scripts or CI:

```sh
decide show --json <id>              # check state == "answered" and answer.answeredBy == "human"
decide list --state answered --json
```

## 4. Custom `targets[]` and `transports.<scheme>` plugins

- **`targets[]`** (for non-LLM tools or named shortcuts): each entry has `"name"` and either `"pane"` or `"command"` (mutually exclusive):
  ```json
  {
    "targets": [
      {"name": "slackwatch", "command": ["slackwatch", "decide-callback"]},
      {"name": "ops", "pane": "agentbus:ops"}
    ]
  }
  ```
  File asks with `--notify-pane target:slackwatch`. For `command` targets, `decide` executes `command[0]` directly (no shell) with `DECIDE_*` env vars and item JSON on stdin on both answers and human replies.
- **`transports.<scheme>`** (custom delivery plugins, machine-local): map lowercase `[a-z0-9][a-z0-9-]*` scheme names (cannot shadow `herdr`, `agentbus`, `pull`, `target`) to a non-empty argv slice:
  ```json
  {
    "transports": {
      "tmux": ["/usr/local/bin/decide-tmux-plugin"]
    }
  }
  ```
  Use `--notify-pane tmux:%1`. `decide` invokes the argv with a v1 JSON request on stdin (`op: "deliver"` or `"status"`, 10s timeout) and reads a JSON response from stdout (`outcome`: `"delivered"`, `"waiting"`, or `"failed"`).
