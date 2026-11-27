# Setting up Terminal TUI and Emacs

## Terminal TUI (`decide inbox`)

Launch the terminal inbox with:

```sh
decide inbox              # optional: --snoozed, --utc, --print-keys, --remote <user@host>
```

### Useful `ui` and display options in `config.json`

All `ui.*` options (except `ui.orgTree`) are machine-local in `${XDG_CONFIG_HOME:-$HOME/.config}/decide/config.json`:

```json
{
  "ui": {
    "utc": false,
    "answerAction": "stay",
    "openAt": "new",
    "fillColumn": 72,
    "clipboard": "auto",
    "asciiKeys": false,
    "archiveGrace": "2s"
  },
  "hyperlinks": true
}
```

- `ui.utc` (`false`): show inbox timestamps in UTC instead of local time.
- `ui.answerAction` (`"stay"`): `"stay"` (keep thread open), `"list"` (return to list after picking an option), or `"edit"` (copy option text into the reply box so you can add text before sending).
- `ui.openAt` (`"new"`): `"new"` (scroll to first unread message), `"last"`, or `"top"`.
- `ui.fillColumn` (`72`): wrap width for `Alt+q` in compose boxes.
- `ui.clipboard` (`"auto"`): `"auto"` (OSC 52 + `pbcopy`/`wl-copy`/`xclip`), `"osc52"`, `"command"`, or `"off"`.
- `ui.asciiKeys` (`false`): show ASCII labels (`C-Enter`) on buttons instead of Unicode (`^⏎`).
- `ui.archiveGrace` (`"2s"`): delay before newly arrived messages can be archived.
- `hyperlinks` (`true`): OSC 8 clickable links (`false` disables); `openCommand` overrides link opener argv (default `xdg-open` on Linux, `open` on macOS).

### Keybindings (`ui.keys`) & terminal keys

Inspect the active keymap or export JSON to customize:

```sh
decide keys             # or: decide inbox --print-keys
decide keys --json
```

Override actions per view under `ui.keys` (an empty list unbinds an action):

```json
{
  "ui": {
    "keys": {
      "detail.reply": ["r", "R"],
      "edit.external": ["ctrl+x ctrl+e", "ctrl+g"],
      "list.snooze": []
    }
  }
}
```

- `decide doctor` (`keys` check) flags removed/deprecated action names.
- `Ctrl+Enter` works when the terminal sends `ESC [27;5;13~` (xterm modifyOtherKeys) or `ESC [13;5u` (kitty); `Alt+Enter` and `C-s` always send. Debug key input with `DECIDE_KEYDEBUG=/tmp/decide-keys.log decide inbox`.

### Avatars (optional)

Preview built-in 10x6 figures and configure rules:

```sh
decide avatars list
decide avatars list --ids
decide avatars set <name> <id|default|none>   # add --local in hub mode
```

In `config.json`: `"avatarDefault": true` assigns deterministic built-in figures to unmatched names, `"avatarMono": "hide"` hides avatars on monochrome terminals, and `"messageAvatars": false` hides per-message avatars.

---

## Emacs (`devtools/decide/emacs/decide.el`)

Requires **Emacs 27.1+** and `decide` on `PATH` (or `decide-program` configured).

Add to `init.el`:

```elisp
(add-to-list 'load-path "/path/to/experimental/devtools/decide/emacs")
(require 'decide)
```

Commands:

- `M-x decide-list` — open the inbox (`*decide*`)
- `M-x decide-compose` — compose a note to a recipient
- `M-x decide-status` — open the status board (`*decide status*`)

Customizable variables (`M-x customize-group RET decide RET`):

- `decide-program` (default `"decide"`)
- `decide-herdr-program` (default `"herdr"`, used by `O` to focus an owner's tab)
- `decide-display-buffer-action` (default `'(display-buffer-same-window)`)

`evil` is supported automatically (`j`/`k`, `g r`, and `g i`/`a`/`d`/`w`/`s` in list/item/status buffers; insert/hybrid state in answer/reply/compose buffers).

For hub mode only inside Emacs (without setting `hub` globally), wrap `decide --remote me@hub "$@"` in an executable script and set `(setq decide-program "/path/to/decide-hub")`.
