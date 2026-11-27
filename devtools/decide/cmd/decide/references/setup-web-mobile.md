# Setting up Web and Mobile PWA (`decide web`)

`decide web` serves the inbox as a web app and installable Progressive Web App (PWA) using the same ledger, drafts, and status as `decide inbox`. Run it **on the hub** (the machine holding the ledger); it always reads the local ledger even if `hub` is set in `config.json`.

## 1. Run the interactive setup wizard

Delegate setup to the built-in wizard on the ledger/hub machine:

```sh
decide web setup --dry-run   # preview without making changes
decide web setup             # interactive setup (or --yes for non-sudo steps)
```

`decide web setup` handles all of the following idempotently, showing each plan and asking before making changes:

1. **Token auth**: creates `~/.config/decide/web-token` (or `$XDG_CONFIG_HOME/decide/web-token`, mode `0600`, 64-character random token) and configures `web.auth.mode = "token"` and `web.auth.tokenFile`. (`DECIDE_WEB_TOKEN` overrides the file; mode `none` is refused on non-loopback binds unless `--allow-insecure-unauth` is passed.)
2. **Background service**:
   - **Linux**: writes `~/.config/systemd/user/decide-web.service`, runs `systemctl --user daemon-reload` and `enable --now decide-web.service`, and offers `sudo loginctl enable-linger $USER`.
   - **macOS**: writes `~/Library/LaunchAgents/com.github.jaeyeom.decide-web.plist` and loads it with `launchctl bootstrap`.
   - Note: unless `--no-follow` is passed, `decide web` runs the `decide follow` loop every minute — do not run `decide-follow` as a separate service at the same time.
3. **HTTPS ingress**:
   - If `tailscale` is on `PATH`: checks MagicDNS/HTTPS certs, optionally grants operator permission (`sudo tailscale set --operator=$USER`) and runs `tailscale serve --bg http://127.0.0.1:8787`, and sets `web.publicUrl`.
   - Otherwise: prints Caddy (`flush_interval -1`) and Cloudflare Tunnel (`cloudflared tunnel --url http://127.0.0.1:8787`) recipes and prompts for `web.publicUrl` (needed for same-origin CSRF checks behind reverse proxies and for PWA/Web Push).
4. **Config update**: backs up `config.json` to `config.json.bak` and writes `web.*` keys (`web.addr`, `web.publicUrl`, `web.auth.*`).
5. **Web Push (optional)**: sets `web.push.enabled = true` and prompts for a real `mailto:` or `https:` `web.push.subject` (Apple Push rejects `mailto:decide@localhost`; VAPID keys & subscriptions are stored in `$XDG_STATE_HOME/decide/webpush.json` mode `0600`). Pushes notify actionable items (never `note`s) after `web.push.grace` (default `"5s"`), skipping items read in the terminal or while a web tab is open.
6. **Health check**: verifies `/api/v1/health` on loopback and `web.publicUrl` (checking `X-Forwarded-Proto: https`).

## 2. Install on a phone

- **iPhone / iPad**: open `web.publicUrl` in Safari -> **Share** -> **Add to Home Screen**, open the installed app, sign in with the token, and enable notifications in Settings (Safari tabs cannot receive Web Push).
- **Android**: open `web.publicUrl` in Chrome -> menu -> **Install app** (or **Add to Home screen**). If using Tailscale Serve, keep Tailscale connected on the phone to open the app or tap "Snooze 1h".

## 3. Verify after setup

```sh
decide doctor
```

When `web` is configured or the service unit exists, `decide doctor` automatically runs the `web-config`, `web-token`, `web-service`, `web-health`, `web-ingress`, and `web-push` checks.
