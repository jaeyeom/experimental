package main

import (
	"context"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

// cmdWake asks the daemon to relaunch a hibernated agent now. In hub
// mode it runs on the hub, whose daemon holds the record and reaches the
// agent's pane with `herdr --machine`.
func cmdWake(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) != 1 || strings.HasPrefix(args[0], "-") {
		fmt.Fprintln(stderr, "agentbus wake: usage: agentbus wake <name|handle>")
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "wake", Name: args[0]}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus wake: %s\n", resp.Error)
		return 1
	}
	fmt.Fprintf(stdout, "woke %s (%s)\n", args[0], resp.Handle)
	return 0
}
