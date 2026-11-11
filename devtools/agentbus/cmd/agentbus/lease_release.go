package main

import (
	"context"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// leaseRelease runs release's git steps here, on the checkout's machine,
// between telling the daemon the release began and that it is done. A
// dirty or mid-operation tree is refused before the daemon hears of it;
// a failed switch puts the lease back to held; a failed pull is reported
// and the release goes ahead.
func leaseRelease(ctx context.Context, f leaseFlags, largs lease.Args, env []string, stdout, stderr io.Writer) int {
	root := strings.TrimPrefix(largs.Checkout, localMachine+":")
	if !cleanTree("release", root, env, stderr) {
		return 1
	}
	call := leaseCaller(ctx, "release", &largs, env, stderr)
	if _, ok := call("lease-release-begin"); !ok {
		return 1
	}
	return handBack(handOff{sub: "release", verb: "released", abortOp: "lease-release-abort", doneOp: "lease-release"}, call, f, &largs, env, stdout, stderr)
}

// leaseResume is release for the user: the daemon checks the caller is
// the user first, then the same clean check and git steps run here.
// --as-is skips the git steps but still refuses a dirty tree. A user
// still queued withdraws the pause and the tree is not looked at.
func leaseResume(ctx context.Context, f leaseFlags, largs lease.Args, env []string, stdout, stderr io.Writer) int {
	root := strings.TrimPrefix(largs.Checkout, localMachine+":")
	largs.AsIs = f.asIs
	call := leaseCaller(ctx, "resume", &largs, env, stderr)
	begun, ok := call("lease-resume-begin")
	if !ok {
		return 1
	}
	if begun.Result == lease.ResultLeft {
		fmt.Fprintf(stdout, "withdrew your queued pause of %s\n", largs.Checkout)
		return 0
	}
	if !cleanTree("resume", root, env, stderr) {
		call("lease-resume-abort")
		return 1
	}
	return handBack(handOff{sub: "resume", verb: "resumed", abortOp: "lease-resume-abort", doneOp: "lease-resume", asIs: f.asIs}, call, f, &largs, env, stdout, stderr)
}

// cleanTree refuses, listing why, a dirty tree or one with a rebase,
// merge, cherry-pick or revert in progress.
func cleanTree(sub, root string, env []string, stderr io.Writer) bool {
	tree, err := checkout.Inspect(root, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease %s: %s\n", sub, err)
		return false
	}
	if tree.Operation != "" {
		fmt.Fprintf(stderr, "agentbus lease %s: %s in progress in %s; finish or abort it first\n", sub, tree.Operation, root)
		return false
	}
	if len(tree.Changed) > 0 {
		fmt.Fprintf(stderr, "agentbus lease %s: %s is not clean; commit or remove these first:\n", sub, root)
		for _, line := range tree.Changed {
			fmt.Fprintf(stderr, "  %s\n", line)
		}
		return false
	}
	return true
}

// handOff names one way of giving the checkout back.
type handOff struct {
	sub, verb       string
	abortOp, doneOp string
	// asIs skips the switch and pull.
	asIs bool
}

// handBack switches to the default branch and pulls (unless as-is),
// tells the daemon, and prints who got the checkout. A failed switch
// aborts and the hold stays; a failed pull is reported and the hand-off
// goes ahead.
func handBack(h handOff, call func(op string) (*lease.Reply, bool), f leaseFlags, largs *lease.Args, env []string, stdout, stderr io.Writer) int {
	root := strings.TrimPrefix(largs.Checkout, localMachine+":")
	var branch string
	var err error
	if h.asIs {
		branch, err = checkout.Branch(root, env)
	} else {
		branch, err = checkout.SwitchDefault(root, env)
	}
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease %s: %s\nthe lease is still yours\n", h.sub, err)
		call(h.abortOp)
		return 1
	}
	if !h.asIs {
		if err := checkout.Pull(root, env); err != nil {
			largs.PullError = err.Error()
			fmt.Fprintf(stderr, "agentbus lease %s: pull failed (%s anyway): %s\n", h.sub, h.verb, err)
		}
	}
	r, ok := call(h.doneOp)
	if !ok {
		return 1
	}
	if f.jsonOut {
		writeJSON(stdout, r)
		return 0
	}
	fmt.Fprintf(stdout, "%s %s on %s\n", h.verb, largs.Checkout, branch)
	for _, handle := range r.Skipped {
		fmt.Fprintf(stdout, "skipped %s (pane gone)\n", handle)
	}
	if r.Granted != "" {
		fmt.Fprintf(stdout, "granted to %s\n", who(r.Granted, r.Lease.Holder.Name))
	} else {
		fmt.Fprintln(stdout, "the checkout is free")
	}
	return 0
}
