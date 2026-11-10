package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"golang.org/x/sys/unix"
)

// localCommands run on this machine even in hub mode.
var localCommands = map[string]bool{
	"help":   true,
	"-h":     true,
	"--help": true,
	"skill":  true,
}

// hubConfig returns the hub from DECIDE_HUB or the config. An empty
// Target means hub mode is off.
func hubConfig() (hubcall.Hub, error) {
	cfg, err := decide.LoadConfig()
	if err != nil {
		return hubcall.Hub{}, fmt.Errorf("decide hub: %w", err)
	}
	hub := hubcall.Hub{Target: strings.TrimSpace(cfg.Hub), Tool: "decide"}
	if env := strings.TrimSpace(os.Getenv("DECIDE_HUB")); env != "" {
		hub.Target = env
	}
	if strings.HasPrefix(hub.Target, "-") {
		return hubcall.Hub{}, fmt.Errorf("decide hub %q must not start with -", hub.Target)
	}
	if raw := strings.TrimSpace(cfg.HubTimeout); raw != "" {
		d, err := time.ParseDuration(raw)
		if err != nil || d <= 0 {
			return hubcall.Hub{}, fmt.Errorf("decide config hubTimeout must be a positive duration, such as \"5s\"")
		}
		hub.ConnectTimeout = d
	}
	return hub, nil
}

// maybeRunOnHub runs args on the hub when hub mode is on and reports
// whether it did.
func maybeRunOnHub(args []string, stdout, stderr io.Writer) (int, bool) {
	if len(args) == 0 || localCommands[args[0]] {
		return 0, false
	}
	hub, err := hubConfig()
	if err != nil {
		return fail(stderr, err, 1), true
	}
	if hub.Target == "" {
		return 0, false
	}
	return runOnHub(hub, args, stdout, stderr), true
}

// runOnHub runs one decide command on the hub's ledger over ssh. It
// never reads or creates the local ledger.
func runOnHub(hub hubcall.Hub, args []string, stdout, stderr io.Writer) int {
	if args[0] == "inbox" {
		fmt.Fprintf(stderr, "decide inbox: the ledger is on the hub %s; run decide inbox there\n", hub.Target)
		return 1
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	code, err := hub.Run(ctx, hubcall.Call{Args: args, Stdout: stdout, Stderr: stderr})
	if err != nil {
		fmt.Fprintf(stderr, "decide: %s\n", err)
	}
	return code
}
