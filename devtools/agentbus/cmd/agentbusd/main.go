// Command agentbusd is the local agentbus daemon.
package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"golang.org/x/sys/unix"
)

const usage = `Usage: agentbusd

agentbusd is the local agentbus daemon. agentbus starts it on demand, and
'agentbus install' runs it under launchd or a systemd user unit.

Flags:
  -h, --help  Print this help.

Environment:
  XDG_RUNTIME_DIR              required; the socket is $XDG_RUNTIME_DIR/agentbus/daemon.sock,
                               and callers forwarded from other machines use
                               $XDG_RUNTIME_DIR/agentbus/hub.sock
  XDG_STATE_HOME               state directory parent (default: $HOME/.local/state)
  XDG_CONFIG_HOME              config directory parent (default: $HOME/.config)
  HOME                         required when XDG_STATE_HOME or XDG_CONFIG_HOME is unset
  AGENTBUS_LOG_RETENTION_DAYS  daily log files to keep (default: 30)
  AGENTBUS_INLINE_BYTES        largest body pasted in full (default: 4096)

Config file ($XDG_CONFIG_HOME/agentbus/config.toml, optional):
  machines = ["linux2"]   saved herdr machines whose agents search lists;
                          herdr:<machine>/<session>/<pane> handles reach
                          any saved machine
  remote_timeout = "5s"   bound on each call to another machine (default: 5s)
  hub = "me@hub"          this machine uses the daemon on this ssh target,
                          so agentbusd refuses to start (AGENTBUS_HUB
                          overrides it)
  lease_check_interval = "2m"
                          how often a holder past its lease bid is looked
                          at, and how far one still working is extended
                          (default: 2m)
  lease_checkin_repeat = "15m"
                          least time between check-ins to a holder that
                          keeps working (default: 15m)
  lease_idle_grace = "5m" how long a checked-in holder may stay idle and
                          silent, with a waiter queued, before takeover
                          (default: 5m)

Example:
  agentbusd
`

func main() {
	os.Exit(run(os.Args[1:], os.Environ(), os.Stdout, os.Stderr))
}

func run(args, env []string, stdout, stderr io.Writer) int {
	if len(args) > 0 {
		if args[0] == "-h" || args[0] == "--help" {
			fmt.Fprint(stdout, usage)
			return 0
		}
		fmt.Fprintf(stderr, "agentbusd: unexpected argument %q\nrun 'agentbusd --help' for usage\n", args[0])
		return 2
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	days, err := retentionDays(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	inline, err := inlineBytes(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	cfg, err := config.Load(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	if hub := hubTarget(env, cfg); hub != "" {
		fmt.Fprintf(stderr, "agentbusd: this machine uses the hub %s; run the daemon there\n", hub)
		return 1
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	if err := daemon.Serve(ctx, dirs, daemon.Config{
		RetentionDays: days,
		Adapters:      []adapter.Adapter{&herdr.Adapter{Machines: cfg.Machines, Timeout: cfg.RemoteTimeout}},
		InlineBytes:   inline,
		HubCaller:     herdr.RemoteCaller,
		LeaseTiming: lease.Timing{
			Interval: cfg.LeaseCheckInterval,
			Repeat:   cfg.LeaseCheckinRepeat,
			Grace:    cfg.LeaseIdleGrace,
		},
	}); err != nil && !errors.Is(err, context.Canceled) {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	return 0
}

// hubTarget is AGENTBUS_HUB, or the config's hub. Non-empty means the
// daemon belongs on the hub, not here.
func hubTarget(env []string, cfg config.Config) string {
	for _, entry := range env {
		if key, val, ok := strings.Cut(entry, "="); ok && key == "AGENTBUS_HUB" && strings.TrimSpace(val) != "" {
			return strings.TrimSpace(val)
		}
	}
	return cfg.Hub
}

// retentionDays reads AGENTBUS_LOG_RETENTION_DAYS. Unset means the default.
func retentionDays(env []string) (int, error) {
	days := 0
	for _, entry := range env {
		key, val, ok := strings.Cut(entry, "=")
		if !ok || key != "AGENTBUS_LOG_RETENTION_DAYS" {
			continue
		}
		if val == "" {
			days = 0
			continue
		}
		n, err := strconv.Atoi(val)
		if err != nil || n <= 0 {
			return 0, errors.New("AGENTBUS_LOG_RETENTION_DAYS must be a positive number of days")
		}
		days = n
	}
	return days, nil
}

// inlineBytes reads AGENTBUS_INLINE_BYTES. Unset means the default.
func inlineBytes(env []string) (int, error) {
	for _, entry := range env {
		key, val, ok := strings.Cut(entry, "=")
		if !ok || key != "AGENTBUS_INLINE_BYTES" {
			continue
		}
		if val == "" {
			return 0, nil
		}
		n, err := strconv.Atoi(val)
		if err != nil || n <= 0 {
			return 0, errors.New("AGENTBUS_INLINE_BYTES must be a positive number of bytes")
		}
		return n, nil
	}
	return 0, nil
}
