package main

import (
	"context"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// leaseRelease runs release's git steps here, on the checkout's machine,
// between telling the daemon the release began and that it is done. A
// dirty or mid-operation tree is refused before the daemon hears of it;
// a failed switch puts the lease back to held; a failed pull is reported
// and the release goes ahead.
func leaseRelease(ctx context.Context, f leaseFlags, largs lease.Args, env []string, stdout, stderr io.Writer) int {
	root := strings.TrimPrefix(largs.Checkout, localMachine+":")
	tree, err := checkout.Inspect(root, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease release: %s\n", err)
		return 1
	}
	if tree.Operation != "" {
		fmt.Fprintf(stderr, "agentbus lease release: %s in progress in %s; finish or abort it first\n", tree.Operation, root)
		return 1
	}
	if len(tree.Changed) > 0 {
		fmt.Fprintf(stderr, "agentbus lease release: %s is not clean; commit or remove these first:\n", root)
		for _, line := range tree.Changed {
			fmt.Fprintf(stderr, "  %s\n", line)
		}
		return 1
	}
	call := func(op string) (*lease.Reply, bool) {
		resp, code := callDaemon(ctx, env, daemon.Request{Op: op, Env: env, Lease: &largs}, stderr)
		if code != 0 {
			return nil, false
		}
		if !resp.OK || resp.Lease == nil {
			fmt.Fprintf(stderr, "agentbus lease release: %s\n", resp.Error)
			return nil, false
		}
		return resp.Lease, true
	}
	if _, ok := call("lease-release-begin"); !ok {
		return 1
	}
	branch, err := checkout.SwitchDefault(root, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease release: %s\nthe lease is still yours\n", err)
		call("lease-release-abort")
		return 1
	}
	if err := checkout.Pull(root, env); err != nil {
		largs.PullError = err.Error()
		fmt.Fprintf(stderr, "agentbus lease release: pull failed (released anyway): %s\n", err)
	}
	r, ok := call("lease-release")
	if !ok {
		return 1
	}
	if f.jsonOut {
		writeJSON(stdout, r)
		return 0
	}
	fmt.Fprintf(stdout, "released %s on %s\n", largs.Checkout, branch)
	for _, h := range r.Skipped {
		fmt.Fprintf(stdout, "skipped %s (pane gone)\n", h)
	}
	if r.Granted != "" {
		fmt.Fprintf(stdout, "granted to %s\n", who(r.Granted, r.Lease.Holder.Name))
	} else {
		fmt.Fprintln(stdout, "the checkout is free")
	}
	return 0
}
