package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

// cmdWake asks the daemon to relaunch a hibernated agent now. In hub
// mode it runs on the hub, whose daemon holds the record and reaches the
// agent's pane with `herdr --machine`.
func cmdWake(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	name, err := parseWake(args, stderr)
	if err != nil {
		fmt.Fprintln(stderr, "agentbus wake: usage: agentbus wake <name|handle>")
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "wake", Name: name}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus wake: %s\n", resp.Error)
		return 1
	}
	fmt.Fprintf(stdout, "woke %s (%s)\n", name, resp.Handle)
	return 0
}

// parseWake is the one name or handle in args. wake has no flags, but
// one after the name is still a flag, and "--" lets a name start with
// "-".
func parseWake(args []string, stderr io.Writer) (string, error) {
	fs := flag.NewFlagSet("wake", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() {}
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return "", err
	}
	if len(pos) != 1 {
		return "", errors.New("want one name or handle")
	}
	return pos[0], nil
}
