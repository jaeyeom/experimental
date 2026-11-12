// Command agentbusd is the local agentbus daemon.
package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lastturn"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/memreport"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
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
  list_liveness_max_age = "15s"
                          how old list's cached liveness may get before a
                          list refreshes it in the background (default: 15s)
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
  lease_takeover = "log"  "on" carries out takeovers; "log" only logs a
                          lease_would_take_over event, once per holder,
                          and keeps check-ins, auto-extend and adopt.
                          Missing or any other value is "log", with a
                          warning for an invalid one (default: log)
  wake_timeout = "60s"    how long a hibernated agent relaunched on
                          delivery or by 'agentbus wake' has to become idle
                          or done in its pane (default: 60s)
  hibernate_policy = "off"
                          "on" hibernates idle agents on this machine while
                          its available memory is low; "dry-run" only logs
                          policy_would_hibernate (default: off)
  hibernate_below = "1.5GiB"
                          available memory under which the policy acts;
                          required for the policy to run
  hibernate_target = "3GiB"
                          available memory at which it stops (default:
                          hibernate_below)
  hibernate_min_idle = "30m"
                          least time an agent must have been idle (default: 30m)
  hibernate_interval = "1m"
                          how often the available memory is read (default: 1m)
  hibernate_exempt = ["ops/*"]
                          names, handles, or globs never hibernated

At start agentbusd prints the effective lease_takeover=<mode> to stderr
and records it in the event log's start event. It also prints the
effective hibernate_policy=<mode> and its thresholds.

When decide is on PATH, a checkout that needs 'agentbus lease adopt' after a
takeover, and a queued 'agentbus lease pause' that is granted, are put in the
user's decide inbox (decide ask --owner agentbus).

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
	ad := &herdr.Adapter{Machines: cfg.Machines, Timeout: cfg.RemoteTimeout}
	if err := daemon.Serve(ctx, dirs, daemon.Config{
		RetentionDays:      days,
		Adapters:           []adapter.Adapter{ad},
		Waker:              &hibernate.Waker{Panes: ad, Clock: hibernate.RealClock{}, Timeout: cfg.WakeTimeout},
		InlineBytes:        inline,
		ListLivenessMaxAge: cfg.ListLivenessMaxAge,
		HubCaller:          herdr.RemoteCaller,
		LeaseTiming:        leaseTiming(cfg, stderr),
		Notify:             decideNotifier(exec.LookPath, runQuiet, stderr),
		// The policy runs only in this daemon, so only on a machine that
		// is not in hub mode: it measures and stops this machine's agents.
		HibernatePolicy: hibernatePolicy(cfg, stderr),
		PolicyInterval:  cfg.Policy().Interval,
		LastTurn:        lastturn.Claude(lastturn.ClaudeProjectsDir(env)),
	}); err != nil && !errors.Is(err, context.Canceled) {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	return 0
}

// leaseTiming is the lease timing and takeover mode from cfg. It prints
// the config's warnings and the effective lease_takeover to stderr.
func leaseTiming(cfg config.Config, stderr io.Writer) lease.Timing {
	for _, w := range cfg.Warnings {
		fmt.Fprintf(stderr, "agentbusd: config: %s\n", w)
	}
	mode := cfg.TakeoverMode()
	fmt.Fprintf(stderr, "agentbusd: lease_takeover=%s\n", mode)
	return lease.Timing{
		Interval: cfg.LeaseCheckInterval,
		Repeat:   cfg.LeaseCheckinRepeat,
		Grace:    cfg.LeaseIdleGrace,
		Takeover: lease.TakeoverMode(mode),
	}
}

// hibernatePolicy makes the memory-pressure policy, or is nil when
// hibernate_policy is off. It prints the effective mode to stderr. The
// policy reads this machine's memory and stops only this machine's
// agents, with the same checks as agentbus hibernate.
func hibernatePolicy(cfg config.Config, stderr io.Writer) func(hibernate.Bus) *hibernate.Policy {
	p := cfg.Policy()
	if p.Mode == hibernate.PolicyOff {
		fmt.Fprintf(stderr, "agentbusd: hibernate_policy=off\n")
		return nil
	}
	fmt.Fprintf(stderr, "agentbusd: hibernate_policy=%s below=%s target=%s min_idle=%s interval=%s\n",
		p.Mode, memreport.HumanBytes(p.Below), memreport.HumanBytes(p.Target), p.MinIdle, p.Interval)
	return func(bus hibernate.Bus) *hibernate.Policy {
		return &hibernate.Policy{
			Mode: p.Mode, Below: p.Below, Target: p.Target, MinIdle: p.MinIdle,
			Memory: procinfo.LocalMemory(),
			Bus:    bus,
			Hibernator: &hibernate.Hibernator{
				Panes: &herdr.Adapter{},
				Bus:   bus,
				Procs: func(ctx context.Context) ([]procinfo.Process, error) {
					return procinfo.LocalTable().List(ctx) //nolint:wrapcheck // the hibernator names the process table
				},
				Cwd:    procinfo.LocalCwd,
				Clock:  hibernate.RealClock{},
				Exempt: cfg.HibernateExempt,
				Self:   os.Getpid(),
			},
		}
	}
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
