package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// listProcs lists processes for adopt's process check. Tests swap it.
var listProcs = checkout.ListProcs

// leaseAdopt runs adopt on the checkout's machine: the daemon confirms
// the caller is the adopter (or the user) and names the backup ref; a
// process still working in the checkout refuses adopt unless --force;
// the leftovers are saved to the backup ref before anything is cleaned;
// then the checkout is cleaned, switched to the default branch and
// pulled, and the daemon marks it held. Any failure before the end
// leaves the lease needs-adopt with the caller.
func leaseAdopt(ctx context.Context, f leaseFlags, largs lease.Args, env []string, stdout, stderr io.Writer) int {
	const stays = "the lease stays needs-adopt with you"
	root := strings.TrimPrefix(largs.Checkout, localMachine+":")
	call := leaseCaller(ctx, "adopt", &largs, env, stderr)
	begun, ok := call("lease-adopt-begin")
	if !ok {
		return 1
	}
	cfg, err := hubSettings(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease adopt: %s\n", err)
		return 1
	}
	forced, ok := checkProcs(root, f.force, cfg.LeaseIgnoreProcs, stderr)
	if !ok {
		fmt.Fprintf(stderr, "%s; stop them, or rerun with --force\n", stays)
		return 1
	}
	header := "lease backup of " + largs.Checkout + " before adopt by " + begun.Caller
	saved, err := checkout.SaveLeftovers(root, begun.Lease.BackupRef, header, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease adopt: %s\nnothing was cleaned; %s\n", err, stays)
		for _, ref := range saved {
			fmt.Fprintf(stderr, "saved so far: %s\n", ref)
		}
		return 1
	}
	if err := checkout.Clean(root, env); err != nil {
		fmt.Fprintf(stderr, "agentbus lease adopt: %s\n%s\n", err, stays)
		return 1
	}
	branch, err := checkout.SwitchDefault(root, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease adopt: %s\n%s\n", err, stays)
		return 1
	}
	if err := checkout.Pull(root, env); err != nil {
		fmt.Fprintf(stderr, "agentbus lease adopt: pull failed (adopted anyway): %s\n", err)
	}
	largs.Backups, largs.Forced = saved, forced
	r, ok := call("lease-adopt")
	if !ok {
		return 1
	}
	if f.jsonOut {
		writeJSON(stdout, r)
		return 0
	}
	fmt.Fprintf(stdout, "adopted %s on %s\n", largs.Checkout, branch)
	for _, ref := range saved {
		fmt.Fprintf(stdout, "saved leftovers to %s\n", ref)
	}
	if len(saved) == 0 {
		fmt.Fprintln(stdout, "nothing to save; the tree was clean")
	}
	return 0
}

// checkProcs refuses when a process other than an ignored program or
// the caller's own chain has its working directory inside root. With
// force it prints them and goes on, returning their labels.
func checkProcs(root string, force bool, ignore []string, stderr io.Writer) ([]string, bool) {
	if ignore == nil {
		ignore = checkout.DefaultIgnoreProcs
	}
	procs, err := listProcs()
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease adopt: cannot check for processes in %s: %s\n", root, err)
		return nil, force
	}
	blockers := checkout.Blockers(root, procs, ignore, os.Getpid())
	if len(blockers) == 0 {
		return nil, true
	}
	fmt.Fprintf(stderr, "agentbus lease adopt: these processes still have their working directory in %s:\n", root)
	labels := make([]string, len(blockers))
	for i, p := range blockers {
		labels[i] = p.Label()
		fmt.Fprintf(stderr, "  %s\n", labels[i])
	}
	if force {
		fmt.Fprintln(stderr, "going on anyway (--force)")
	}
	return labels, force
}

// leaseCaller returns a function that sends op with *largs, as it is at
// the call, to the daemon and returns its lease reply, printing any
// error as agentbus lease sub.
func leaseCaller(ctx context.Context, sub string, largs *lease.Args, env []string, stderr io.Writer) func(op string) (*lease.Reply, bool) {
	return func(op string) (*lease.Reply, bool) {
		resp, code := callDaemon(ctx, env, daemon.Request{Op: op, Env: env, Lease: largs}, stderr)
		if code != 0 {
			return nil, false
		}
		if !resp.OK || resp.Lease == nil {
			fmt.Fprintf(stderr, "agentbus lease %s: %s\n", sub, resp.Error)
			return nil, false
		}
		return resp.Lease, true
	}
}
