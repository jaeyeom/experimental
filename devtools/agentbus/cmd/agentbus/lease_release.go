package main

import (
	"context"
	"fmt"
	"io"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// leaseRelease runs release's git steps here, on the checkout's machine,
// between telling the daemon the release began and that it is done. A
// dirty or mid-operation tree is refused before the daemon hears of it;
// a failed switch puts the lease back to held; a failed pull is reported
// and the release goes ahead.
func leaseRelease(ctx context.Context, f leaseFlags, largs lease.Args, env []string, stdout, stderr io.Writer) int {
	root := checkoutRoot(largs.Checkout)
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
	root := checkoutRoot(largs.Checkout)
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
	root := checkoutRoot(largs.Checkout)
	var steps gitSteps
	var err error
	if h.asIs {
		steps, err = stayAsIs(root, env)
	} else {
		steps, err = switchAndPull(root, env)
	}
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease %s: %s\nthe lease is still yours\n", h.sub, err)
		call(h.abortOp)
		return 1
	}
	largs.PullError = steps.PullErr
	r, ok := call(h.doneOp)
	if !ok {
		return 1
	}
	if f.jsonOut {
		writeJSON(stdout, struct {
			*lease.Reply
			gitSteps
			GrantedTo string `json:"granted_to"` //nolint:tagliatelle // matches the step fields
		}{r, steps, r.Granted})
		return 0
	}
	steps.print(stdout)
	for _, handle := range r.Skipped {
		fmt.Fprintf(stdout, "skipped %s (pane gone)\n", handle)
	}
	if r.Granted != "" {
		fmt.Fprintf(stdout, "%s; granted to %s\n", h.verb, who(r.Granted, r.Lease.Holder.Name))
	} else {
		fmt.Fprintf(stdout, "%s; checkout free\n", h.verb)
	}
	return 0
}

// gitSteps is what the switch to the default branch and the pull did,
// for the step lines and --json. The pulled SHAs are short and equal
// when the pull found nothing new; both are empty when it failed.
//
//nolint:tagliatelle // snake_case like the stable status shape
type gitSteps struct {
	FromBranch string `json:"from_branch"`
	ToBranch   string `json:"to_branch"`
	PulledFrom string `json:"pulled_from,omitempty"`
	PulledTo   string `json:"pulled_to,omitempty"`
	PullErr    string `json:"pull_error,omitempty"`
	// asIs: resume --as-is skipped the switch and the pull.
	asIs bool
	// rebased: the pull replayed local commits, so it was no fast-forward.
	rebased bool
}

// switchAndPull switches root to the default branch and pulls. A failed
// switch is an error; a failed pull is recorded in the steps.
func switchAndPull(root string, env []string) (gitSteps, error) {
	from, err := checkout.Branch(root, env)
	if err != nil {
		return gitSteps{}, fmt.Errorf("read the current branch: %w", err)
	}
	to, err := checkout.SwitchDefault(root, env)
	if err != nil {
		return gitSteps{}, fmt.Errorf("switch to the default branch: %w", err)
	}
	s := gitSteps{FromBranch: from, ToBranch: to}
	before, err := checkout.Head(root, env)
	if err == nil {
		err = checkout.Pull(root, env)
	}
	var after string
	if err == nil {
		after, err = checkout.Head(root, env)
	}
	if err != nil {
		s.PullErr = err.Error()
		return s, nil
	}
	s.PulledFrom, s.PulledTo = before, after
	s.rebased = before != after && !checkout.IsAncestor(root, before, after, env)
	return s, nil
}

// stayAsIs records the branch resume --as-is leaves checked out.
func stayAsIs(root string, env []string) (gitSteps, error) {
	branch, err := checkout.Branch(root, env)
	if err != nil {
		return gitSteps{}, fmt.Errorf("read the current branch: %w", err)
	}
	return gitSteps{FromBranch: branch, ToBranch: branch, asIs: true}, nil
}

// print writes one line for the switch and one for the pull.
func (s gitSteps) print(w io.Writer) {
	switch {
	case s.asIs:
		fmt.Fprintf(w, "kept %s as is; no switch or pull\n", s.FromBranch)
		return
	case s.FromBranch == s.ToBranch:
		fmt.Fprintf(w, "already on %s\n", s.ToBranch)
	default:
		fmt.Fprintf(w, "switched %s -> %s\n", s.FromBranch, s.ToBranch)
	}
	switch {
	case s.PullErr != "":
		fmt.Fprintf(w, "pull failed: %s\n", s.PullErr)
	case s.PulledFrom == s.PulledTo:
		fmt.Fprintln(w, "pulled: already up to date")
	case s.rebased:
		fmt.Fprintf(w, "pulled: rebased %s -> %s\n", s.PulledFrom, s.PulledTo)
	default:
		fmt.Fprintf(w, "pulled: fast-forward %s..%s\n", s.PulledFrom, s.PulledTo)
	}
}
