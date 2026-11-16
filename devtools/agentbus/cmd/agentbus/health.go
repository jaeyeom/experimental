package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// healthTimeout bounds the one round trip health makes.
const healthTimeout = 2 * time.Second

// Exit codes of health.
const (
	healthDaemonDown    = 1
	healthTransportDown = 4
)

// healthReport is the health --json object.
type healthReport struct {
	Daemon    string   `json:"daemon"`
	Transport string   `json:"transport"`
	Problems  []string `json:"problems"`
}

func parseHealth(args []string, stderr io.Writer) (bool, bool) {
	fs := flag.NewFlagSet("health", flag.ContinueOnError)
	fs.SetOutput(stderr)
	asJSON := fs.Bool("json", false, "print one JSON object")
	if err := fs.Parse(args); err != nil {
		return false, false
	}
	if len(fs.Args()) != 0 {
		fmt.Fprintln(stderr, "usage: agentbus health [--json]")
		return false, false
	}
	return *asJSON, true
}

// cmdHealth asks the daemon for op transport-status once. It never
// starts the daemon.
func cmdHealth(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	asJSON, ok := parseHealth(args, stderr)
	if !ok {
		return 2
	}
	ctx, cancel := context.WithTimeout(ctx, healthTimeout)
	defer cancel()
	resp, err := dialBus(ctx, env, daemon.Request{Op: "transport-status"})
	switch {
	case err != nil:
		return printHealth(stdout, stderr, asJSON, daemonDown("daemon unreachable: "+err.Error()))
	case !resp.OK:
		return printHealth(stdout, stderr, asJSON, healthReport{Daemon: "up", Transport: "unknown", Problems: []string{"transport status: " + resp.Error}})
	case len(resp.Transport) > 0:
		return printHealth(stdout, stderr, asJSON, healthReport{Daemon: "up", Transport: "down", Problems: resp.Transport})
	}
	return printHealth(stdout, stderr, asJSON, healthReport{Daemon: "up", Transport: "ok", Problems: []string{}})
}

func daemonDown(problem string) healthReport {
	return healthReport{Daemon: "down", Transport: "unknown", Problems: []string{problem}}
}

// printHealth writes r as JSON or as text lines and returns its exit code.
func printHealth(stdout, stderr io.Writer, asJSON bool, r healthReport) int {
	if asJSON {
		if err := json.NewEncoder(stdout).Encode(r); err != nil {
			fmt.Fprintf(stderr, "agentbus health: %s\n", err)
			return 1
		}
	} else {
		if len(r.Problems) == 0 {
			fmt.Fprintln(stdout, "ok")
		}
		for _, p := range r.Problems {
			fmt.Fprintln(stdout, p)
		}
	}
	switch {
	case r.Daemon != "up" || r.Transport == "unknown":
		return healthDaemonDown
	case r.Transport == "down":
		return healthTransportDown
	}
	return 0
}

// hubHealth asks the hub's daemon over the tunnel, or runs health on the
// hub over ssh within healthTimeout. A hub that cannot be reached is
// reported as a down daemon.
func hubHealth(ctx context.Context, cfg config.Config, args, env []string, stdout, stderr io.Writer) int {
	asJSON, ok := parseHealth(args[1:], stderr)
	if !ok {
		return 2
	}
	tunnelled, ok, err := openTunnel(ctx, cfg, "health", env, stderr)
	if err != nil && !errors.Is(err, errProtocolMismatch) {
		return printHealth(stdout, stderr, asJSON, daemonDown(err.Error()))
	}
	if ok {
		return cmdHealth(tunnelled, args[1:], env, stdout, stderr)
	}
	// The hub's shell starts within healthTimeout, and its own health
	// then takes at most healthTimeout.
	hub := hookHub(agentHub(cfg), healthTimeout)
	ctx, cancel := context.WithTimeout(ctx, 2*healthTimeout)
	defer cancel()
	code, err := hub.Run(ctx, hubcall.Call{Args: args, Stdout: stdout, Stderr: stderr})
	switch {
	case err != nil:
		return printHealth(stdout, stderr, asJSON, daemonDown(sshHint(env, cfg, err).Error()))
	case code < 0:
		return printHealth(stdout, stderr, asJSON, daemonDown(fmt.Sprintf("hub %s: health did not finish in %s", cfg.Hub, 2*healthTimeout)))
	}
	return code
}
