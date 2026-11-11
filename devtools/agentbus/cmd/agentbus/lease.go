package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/checkout"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// localMachine is the machine part of a checkout key on the hub.
const localMachine = "local"

// lease check exit codes. The hook tells "not the holder" from "could
// not decide".
const (
	checkAllowed = 0
	checkDenied  = 1
	checkUsage   = 2
	checkError   = 3
)

// leaseFlagsHelp is the lease command's help: its subcommands and flags.
const leaseFlagsHelp = `claim --for <duration> [--urgent] [--why <text>]
         take the checkout if it is free, else join the queue; never
         blocks. Prints "granted until HH:MM" or "queued, position N
         (holder X, bid ends HH:MM)". Claiming again updates --for and
         --why. --urgent queues ahead of normal claims (default: false).
         A free checkout with a dirty or mid-rebase/merge tree is
         granted as needs-adopt.
extend --for <duration>
         holder only: move the soft deadline to now plus --for. Also
         the answer to a check-in: when the bid ends the daemon sends
         "[lease] your bid on <checkout> ended". With a waiter queued,
         a holder still working (or releasing) is extended silently;
         one idle and silent for lease_idle_grace (5m) after the
         check-in, or whose pane is gone, is taken over. Nobody queued:
         one check-in only. Takeover tells the old holder "[lease] taken
         over from you: <checkout>; your uncommitted work will be saved
         to <backup ref>" and grants the next waiter as needs-adopt.
         Takeover runs only with lease_takeover = "on"; with "log" (the
         default) the daemon only logs lease_would_take_over.
leave    leave the queue
status [--all]
         holder, deadline, state, and queue; --all lists every checkout
         the daemon has seen (default: false, this checkout). Ends with
         "takeover: log" or "takeover: on"; --json has a "takeover"
         field
check    exit 0 when you hold the lease for --path or --path is not
         guarded, 1 with a one-line reason when you do not, 3 when it
         cannot tell; an unguarded path never contacts the daemon
release  holder only: refuse a dirty tree (modified, staged, or
         untracked and not ignored; listed) or a rebase, merge or
         cherry-pick in progress; unpushed commits are fine. Then
         switch to the default branch (origin/HEAD) and run git
         pull --rebase --prune; a failed pull is reported and the
         release goes ahead. The first waiter whose pane is not gone
         is granted and woken with an agentbus message; gone waiters
         are dropped. With nobody waiting the checkout is free.
adopt [--force]
         the needs-adopt holder, or the user from a pane with no agent
         or outside herdr: refuse while a process other than a shell,
         agent CLI, editor, pager or agentbus (lease_ignore_procs) has
         its working directory in the checkout, naming them; --force
         goes on anyway (default: false). Then save any uncommitted
         work, untracked files included, as a commit on
         refs/lease-backup/<UTC time>-<old holder> (never a branch or
         git stash), abort a rebase/merge/cherry-pick/revert after
         recording it, clean, switch to the default branch and pull.
         The lease becomes held by you. status lists the backup refs;
         they are never deleted.
pause [--with <pane or name>]...
         the user only (a pane with no agent, or outside herdr; refused
         from an agent's pane): take the checkout. Free: it is paused,
         held by "user" with no deadline, never checked in on and never
         taken over. Held by an agent: you go to the front of the queue
         and the holder is told "[lease] the user asked for <checkout>;
         please finish up and release"; the usual check-in rules
         follow. When it is granted later, decide (if installed) gets
         a note. --with (repeatable) lets that agent pane (pane id,
         registered name, or handle) edit under the pause: lease check
         passes for it, but it cannot release, extend or resume. Pause
         again to add panes.
unshare <pane or name>...
         the user only: take panes off the pause's --with list
resume [--as-is]
         the user only: release for the user, with the same clean
         check, switch and pull; the --with list clears and the next
         waiter is granted. --as-is skips the switch and pull (a dirty
         tree is still refused) (default: false). While still queued,
         resume withdraws the pause.

Every subcommand:
--path <path>  the checkout (default: the current directory), resolved to
               its git top-level, which must be in lease_checkouts
--json         print JSON (default: false, text)`

// leaseFlags is every lease subcommand's flags.
type leaseFlags struct {
	path    string
	jsonOut bool
	bid     string
	urgent  bool
	why     string
	all     bool
	force   bool
	asIs    bool
	// with is pause's --with list, or unshare's arguments.
	with stringList
}

// stringList is a repeatable string flag.
type stringList []string

func (l *stringList) String() string { return strings.Join(*l, ",") }

func (l *stringList) Set(v string) error {
	*l = append(*l, v)
	return nil
}

func parseLeaseFlags(sub string, args []string, stderr io.Writer) (leaseFlags, error) {
	var f leaseFlags
	fs := flag.NewFlagSet("lease "+sub, flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.StringVar(&f.path, "path", "", "the checkout (default: the current directory)")
	fs.BoolVar(&f.jsonOut, "json", false, "print JSON")
	switch sub {
	case "claim":
		fs.StringVar(&f.bid, "for", "", "how long you expect to hold it, such as 40m (required)")
		fs.BoolVar(&f.urgent, "urgent", false, "queue ahead of normal claims")
		fs.StringVar(&f.why, "why", "", "what the lease is for")
	case "extend":
		fs.StringVar(&f.bid, "for", "", "new bid from now, such as 20m (required)")
	case "status":
		fs.BoolVar(&f.all, "all", false, "every checkout the daemon has seen")
	case "adopt":
		fs.BoolVar(&f.force, "force", false, "adopt even when other processes work in the checkout")
	case "pause":
		fs.Var(&f.with, "with", "an agent pane or name that may edit under the pause (repeatable)")
	case "resume":
		fs.BoolVar(&f.asIs, "as-is", false, "skip the switch to the default branch and the pull")
	}
	if err := fs.Parse(args); err != nil {
		return f, fmt.Errorf("parse flags: %w", err)
	}
	if sub == "unshare" {
		f.with = fs.Args()
		if len(f.with) == 0 {
			return f, errors.New("name a pane or agent to unshare")
		}
	} else if fs.NArg() != 0 {
		return f, errors.New("unexpected arguments")
	}
	if (sub == "claim" || sub == "extend") && strings.TrimSpace(f.bid) == "" {
		return f, errors.New("--for is required, such as --for 40m")
	}
	return f, nil
}

func cmdLease(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) == 0 || isHelpFlag(args[0]) {
		cmd, _ := findCommand("lease")
		printCommandHelp(stdout, cmd)
		return 0
	}
	sub := args[0]
	switch sub {
	case "claim", "extend", "leave", "status", "check", "release", "adopt", "pause", "unshare", "resume":
	default:
		fmt.Fprintf(stderr, "agentbus lease: unknown subcommand %q\nrun 'agentbus lease --help' for usage\n", sub)
		return 2
	}
	usage, failed := 2, 1
	if sub == "check" {
		usage, failed = checkUsage, checkError
	}
	f, err := parseLeaseFlags(sub, args[1:], stderr)
	if err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return 0
		}
		fmt.Fprintf(stderr, "agentbus lease %s: %s\n", sub, err)
		return usage
	}
	largs, code, ok := leaseArgs(sub, f, env, stdout, stderr)
	if !ok {
		return code
	}
	switch sub {
	case "release":
		return leaseRelease(ctx, f, largs, env, stdout, stderr)
	case "adopt":
		return leaseAdopt(ctx, f, largs, env, stdout, stderr)
	case "resume":
		return leaseResume(ctx, f, largs, env, stdout, stderr)
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "lease-" + sub, Env: env, Lease: &largs}, stderr)
	if code != 0 {
		return failed
	}
	if !resp.OK || resp.Lease == nil {
		fmt.Fprintf(stderr, "agentbus lease %s: %s\n", sub, resp.Error)
		return failed
	}
	return printLease(sub, f, *resp.Lease, env, stdout, stderr)
}

// printStatusText prints each checkout's status and, for a checkout on
// this machine, its lease backup refs.
func printStatusText(r lease.Reply, env []string, stdout io.Writer) {
	for i, s := range r.Leases {
		if i > 0 {
			fmt.Fprintln(stdout)
		}
		printStatus(stdout, s)
		root, local := strings.CutPrefix(s.Checkout, localMachine+":")
		if !local {
			continue
		}
		// A checkout that is gone or not a repository lists no backups.
		if refs, err := checkout.Backups(root, env); err == nil && len(refs) > 0 {
			fmt.Fprintln(stdout, "backups:")
			for _, ref := range refs {
				fmt.Fprintf(stdout, "  %s\n", ref)
			}
		}
	}
}

// leaseArgs does the local part of a lease command: read the config,
// resolve the checkout, and inspect the tree for a claim. When it is not
// ok, the command exits with the code it returns; an unguarded check
// passes here without a daemon call.
func leaseArgs(sub string, f leaseFlags, env []string, stdout, stderr io.Writer) (lease.Args, int, bool) {
	failed := 1
	if sub == "check" {
		failed = checkError
	}
	cfg, err := hubSettings(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease %s: %s\n", sub, err)
		return lease.Args{}, failed, false
	}
	largs := lease.Args{For: f.bid, Urgent: f.urgent, Why: f.why, All: f.all, With: f.with}
	if sub != "status" || !f.all {
		key, code := leaseKey(sub, f, cfg, env, stdout, stderr)
		if key == "" {
			return lease.Args{}, code, false
		}
		largs.Checkout = key
	}
	if cfg.Hub != "" {
		fmt.Fprintf(stderr, "agentbus lease %s: hub mode is not supported yet\n", sub)
		return lease.Args{}, failed, false
	}
	if sub == "claim" {
		tree, err := checkout.Inspect(strings.TrimPrefix(largs.Checkout, localMachine+":"), env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus lease claim: %s\n", err)
			return lease.Args{}, failed, false
		}
		largs.Tree = tree.Reason()
	}
	return largs, 0, true
}

// leaseKey resolves --path to its checkout key. An empty key comes with
// the exit code: check passes an unguarded path, the rest refuse it.
func leaseKey(sub string, f leaseFlags, cfg config.Config, env []string, stdout, stderr io.Writer) (string, int) {
	path := f.path
	if path == "" {
		wd, err := os.Getwd()
		if err != nil {
			fmt.Fprintf(stderr, "agentbus lease %s: %s\n", sub, err)
			return "", 1
		}
		path = wd
	}
	root, err := checkout.Resolve(path, cfg.LeaseCheckouts, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus lease %s: %s\n", sub, err)
		if sub == "check" {
			return "", checkError
		}
		return "", 1
	}
	if root != "" {
		return localMachine + ":" + root, 0
	}
	if sub == "check" {
		if f.jsonOut {
			writeJSON(stdout, checkJSON{Allowed: true, Reason: "not a guarded checkout"})
		}
		return "", checkAllowed
	}
	cfgPath, _ := config.Path(env)
	fmt.Fprintf(stderr, "agentbus lease %s: %s is not in a guarded checkout; list the checkout in lease_checkouts in %s\n", sub, path, cfgPath)
	return "", 1
}

type checkJSON struct {
	Allowed  bool   `json:"allowed"`
	Guarded  bool   `json:"guarded"`
	Checkout string `json:"checkout,omitempty"`
	Reason   string `json:"reason,omitempty"`
}

func printLease(sub string, f leaseFlags, r lease.Reply, env []string, stdout, stderr io.Writer) int {
	switch {
	case sub == "check":
		return printCheck(f.jsonOut, r, stdout, stderr)
	case sub == "status":
		printStatusReply(f, r, env, stdout)
		return 0
	case f.jsonOut:
		writeJSON(stdout, r)
		return 0
	}
	s := r.Lease
	switch r.Result {
	case lease.ResultGranted:
		line := "granted until " + clock(s.Holder.Deadline)
		if s.State == lease.StateNeedsAdopt {
			line += " (needs-adopt: the tree is not clean; run agentbus lease adopt before changing it)"
		}
		fmt.Fprintln(stdout, line)
	case lease.ResultQueued:
		line := fmt.Sprintf("queued, position %d (holder %s)", r.Position, holderLabel(s.Holder))
		if r.Asked != "" {
			line += "; asked " + r.Asked + " to finish up and release"
		}
		fmt.Fprintln(stdout, line)
		printWith(stdout, s.With)
	case lease.ResultPaused:
		fmt.Fprintf(stdout, "paused %s\n", s.Checkout)
		printWith(stdout, s.With)
	case lease.ResultUnshared:
		fmt.Fprintf(stdout, "unshared %s\n", strings.Join(f.with, ", "))
		printWith(stdout, s.With)
	case lease.ResultExtended:
		fmt.Fprintf(stdout, "extended until %s\n", clock(s.Holder.Deadline))
	case lease.ResultLeft:
		fmt.Fprintf(stdout, "left the queue for %s\n", s.Checkout)
	}
	return 0
}

// statusJSON is the status --json shape: the stable lease.Status plus
// the daemon's effective lease_takeover, "log" or "on".
type statusJSON struct {
	lease.Status
	Takeover lease.TakeoverMode `json:"takeover,omitempty"`
}

// printStatusReply prints status: one object for a checkout, an array
// for --all, or text. Each object carries the takeover mode; text ends
// with it.
func printStatusReply(f leaseFlags, r lease.Reply, env []string, stdout io.Writer) {
	switch {
	case f.jsonOut && f.all:
		out := make([]statusJSON, 0, len(r.Leases))
		for _, s := range r.Leases {
			out = append(out, statusJSON{Status: s, Takeover: r.Takeover})
		}
		writeJSON(stdout, out)
	case f.jsonOut:
		writeJSON(stdout, statusJSON{Status: r.Leases[0], Takeover: r.Takeover})
	default:
		printStatusText(r, env, stdout)
		if r.Takeover != "" {
			fmt.Fprintf(stdout, "takeover: %s\n", r.Takeover)
		}
	}
}

func printCheck(jsonOut bool, r lease.Reply, stdout, stderr io.Writer) int {
	s := r.Lease
	out := checkJSON{Allowed: r.Result == lease.ResultAllowed, Guarded: true, Checkout: s.Checkout}
	if !out.Allowed {
		out.Reason = denyReason(*s, r.Caller)
	}
	if jsonOut {
		writeJSON(stdout, out)
	} else if !out.Allowed {
		fmt.Fprintf(stderr, "agentbus lease check: %s\n", out.Reason)
	}
	if out.Allowed {
		return checkAllowed
	}
	return checkDenied
}

// denyReason is the one-line reason lease check gives.
func denyReason(s lease.Status, caller string) string {
	const hint = "claim it with: agentbus lease claim --for <duration>"
	switch {
	case s.Holder == nil:
		return "nobody holds " + s.Checkout + "; " + hint
	case s.State == lease.StateNeedsAdopt && s.Holder.Handle == caller:
		return s.Checkout + " needs adopt before any change"
	case s.State == lease.StateNeedsAdopt:
		return s.Checkout + " needs adopt by " + who(s.Holder.Handle, s.Holder.Name) + "; " + hint
	case s.Holder.Handle == lease.UserHolder:
		return s.Checkout + " is paused by the user; " + hint
	default:
		return s.Checkout + " is held by " + holderLabel(s.Holder) + "; " + hint
	}
}

// holderLabel is the holder and, when it has one, its bid's end.
func holderLabel(h *lease.HolderView) string {
	out := who(h.Handle, h.Name)
	if h.Deadline != "" {
		out += ", bid ends " + clock(h.Deadline)
	}
	return out
}

func who(handle, name string) string {
	if name == "" {
		return handle
	}
	return name + " (" + handle + ")"
}

func printStatus(w io.Writer, s lease.Status) {
	fmt.Fprintf(w, "%s: %s\n", s.Checkout, s.State)
	if h := s.Holder; h != nil {
		line := "holder: " + who(h.Handle, h.Name) + ", since " + clock(h.Since)
		if h.Deadline != "" {
			line += ", bid ends " + clock(h.Deadline)
		}
		if h.Why != "" {
			line += ", why: " + h.Why
		}
		fmt.Fprintln(w, line)
	}
	if s.BackupRef != "" {
		fmt.Fprintf(w, "backup: %s\n", s.BackupRef)
	}
	printWith(w, s.With)
	if len(s.Queue) == 0 {
		return
	}
	fmt.Fprintln(w, "queue:")
	for i, q := range s.Queue {
		line := fmt.Sprintf("  %d. %s, for %s, since %s", i+1, who(q.Handle, q.Name), q.For, clock(q.Since))
		if q.Urgent {
			line += ", urgent"
		}
		fmt.Fprintln(w, line)
	}
}

// printWith prints the panes the user's pause is shared with.
func printWith(w io.Writer, with []string) {
	if len(with) > 0 {
		fmt.Fprintf(w, "with: %s\n", strings.Join(with, ", "))
	}
}

// clock renders an RFC3339 time as local HH:MM.
func clock(ts string) string {
	t, err := time.Parse(time.RFC3339, ts)
	if err != nil {
		return ts
	}
	return t.Local().Format("15:04")
}

func writeJSON(w io.Writer, v any) {
	data, err := json.MarshalIndent(v, "", "  ")
	if err != nil {
		fmt.Fprintf(w, "{\"error\": %q}\n", err.Error())
		return
	}
	fmt.Fprintln(w, string(data))
}
