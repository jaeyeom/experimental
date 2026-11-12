package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

// wakeUsage is the usage line of agentbus wake.
const wakeUsage = "agentbus wake [--if-hibernated] [--wait] [--machine <m>] <name|handle|pane>"

// wakeArgs are the parsed arguments of agentbus wake.
type wakeArgs struct {
	target string
	// machine makes target a herdr pane id on that machine.
	machine      string
	ifHibernated bool
}

// cmdWake asks the daemon to relaunch a hibernated agent now and waits
// until it is ready. In hub mode it runs on the hub, whose daemon holds
// the record and reaches the agent's pane with `herdr --machine`.
//
// It exits 0 when the agent is awake, or with --if-hibernated when it
// was not hibernated; 1 when the wake failed; 2 for an unknown name or
// handle, or bad usage.
func cmdWake(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	w, err := parseWake(args, stderr)
	if err != nil {
		fmt.Fprintln(stderr, "agentbus wake: usage: "+wakeUsage)
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "wake", Name: w.target, Machine: w.machine}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		if w.ifHibernated && notHibernated(resp) {
			return 0
		}
		fmt.Fprintf(stderr, "agentbus wake: %s\n", resp.Error)
		if resp.State == daemon.WakeUnknown {
			return 2
		}
		return 1
	}
	fmt.Fprintf(stdout, "woke %s (%s)\n", w.target, resp.Handle)
	return 0
}

// notHibernated reports whether a failed wake found its agent running.
// A daemon that predates the wake States says so only in its error.
func notHibernated(resp daemon.Response) bool {
	if resp.State != "" {
		return resp.State == daemon.WakeNotHibernated
	}
	return strings.HasSuffix(resp.Error, " is not hibernated")
}

// parseWake is the one name, handle, or pane in args and the flags. A
// flag may follow the target, and "--" lets a name start with "-".
// --wait is accepted for callers that spell it out: a wake always waits
// for the agent to be ready.
func parseWake(args []string, stderr io.Writer) (wakeArgs, error) {
	var w wakeArgs
	fs := flag.NewFlagSet("wake", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() {}
	fs.BoolVar(&w.ifHibernated, "if-hibernated", false, "exit 0 quietly when the target is not hibernated")
	wait := fs.Bool("wait", true, "return once the agent is ready")
	fs.StringVar(&w.machine, "machine", "", "the target is a herdr pane id on this machine")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return wakeArgs{}, err
	}
	if !*wait {
		return wakeArgs{}, errors.New("a wake always waits for the agent")
	}
	if len(pos) != 1 {
		return wakeArgs{}, errors.New("want one name, handle, or pane")
	}
	w.target = pos[0]
	return w, nil
}
