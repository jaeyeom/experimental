package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"slices"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
)

// Exit codes of jobs claim besides 0, 1, and 2.
const (
	jobsNoneReady = 3
	jobsHeld      = 4
)

// jobsLogPoll is how often log --follow asks for new history; tests
// shorten it.
var jobsLogPoll = time.Second

const jobsFlagsHelp = `enqueue --kind <k> --subject <s> [--head <sha>] [--priority <n>]
        [--machine <m>|any] [--affinity tab|lane|owner]
        [--checkout <path>|none] [--executor cli|tab|new-tab] [--note <text>] [--json]
    add a job; one live job per kind + subject + head, so a repeat
    prints the live job with "(duplicate)". There is no --gate: gates
    come only from the user's policy
claim --runner <name> [--kind <k>]... [--machine <m>] [--json]
    take the next ready job. Exit 3: none ready; 4: jobs are on hold
heartbeat <id> --runner <name>
    tell the daemon the holder is still at it
done <id> [--runner <name>] [--note <text>]
fail <id> [--runner <name>] [--exit <code>] [--note <text>]
    close a running job as its holder, or settle an unknown or parked
    job without --runner. A retryable --exit retries after the backoff
ls [--state <s>] [--kind <k>] [--subject <s>] [--all] [--json]
    live jobs; --all adds done, failed, and dead ones
show <id> [--json]
log <id> [--follow]   state history; --follow waits until the job is final
stats [--json]        jobs per state, and the hold
hold [--note <text>]  stop every claim until unhold
unhold`

// kindList is a repeatable --kind.
type kindList []string

func (k *kindList) String() string { return strings.Join(*k, ",") }

func (k *kindList) Set(v string) error {
	*k = append(*k, v)
	return nil
}

// optionalInt is --exit: unset is nil.
type optionalInt struct{ v *int }

func (o *optionalInt) String() string {
	if o.v == nil {
		return ""
	}
	return strconv.Itoa(*o.v)
}

func (o *optionalInt) Set(s string) error {
	n, err := strconv.Atoi(s)
	if err != nil {
		return errors.New("want an integer exit code")
	}
	o.v = &n
	return nil
}

// jobsFlags are every jobs subcommand's flags; each subcommand reads
// only its own and refuses the rest through usedOnly.
type jobsFlags struct {
	a       jobs.Args
	kinds   kindList
	exit    optionalInt
	jsonOut bool
	follow  bool
}

var jobsSubFlags = map[string][]string{
	"enqueue":   {"kind", "subject", "head", "priority", "machine", "affinity", "checkout", "executor", "note", "json"},
	"claim":     {"runner", "kind", "machine", "json"},
	"heartbeat": {"runner"},
	"done":      {"runner", "note"},
	"fail":      {"runner", "exit", "note"},
	"ls":        {"state", "kind", "subject", "all", "json"},
	"show":      {"json"},
	"log":       {"follow"},
	"stats":     {"json"},
	"hold":      {"note"},
	"unhold":    {},
}

// jobsNeedID are the subcommands that take one job id.
var jobsNeedID = map[string]bool{"heartbeat": true, "done": true, "fail": true, "show": true, "log": true}

func runJobs(ctx context.Context, args, env []string, _ io.Reader, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintf(stderr, "agentbus jobs: want a subcommand: enqueue, claim, heartbeat, done, fail, ls, show, log, stats, hold, unhold\n")
		return 2
	}
	sub := args[0]
	allowed, ok := jobsSubFlags[sub]
	if !ok {
		fmt.Fprintf(stderr, "agentbus jobs: unknown subcommand %q\n", sub)
		return 2
	}
	f, err := parseJobsFlags(sub, allowed, args[1:], stderr)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus jobs %s: %s\n", sub, err)
		return 2
	}
	switch sub {
	case "log":
		return jobsLog(ctx, f, env, stdout, stderr)
	case "claim":
		return jobsClaim(ctx, f, env, stdout, stderr)
	}
	r, code := jobsCall(ctx, sub, f.a, env, stderr)
	if code != 0 {
		return code
	}
	return printJobs(sub, f, r, stdout)
}

// parseJobsFlags reads one subcommand's flags and its job id, which may
// come before or after the flags.
func parseJobsFlags(sub string, allowed, args []string, stderr io.Writer) (jobsFlags, error) {
	var f jobsFlags
	fs := flag.NewFlagSet("jobs "+sub, flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.StringVar(&f.a.Subject, "subject", "", "owner/repo#N or a work-item key")
	fs.StringVar(&f.a.Head, "head", "", "head SHA, part of the dedupe key")
	fs.IntVar(&f.a.Priority, "priority", 0, "higher runs first")
	fs.StringVar(&f.a.Needs.Machine, "machine", "", "the machine the job needs, or the claiming runner's machine")
	fs.StringVar(&f.a.Needs.Affinity, "affinity", "", "tab, lane, or owner")
	fs.StringVar(&f.a.Needs.Checkout, "checkout", "", "none, or a checkout path")
	fs.StringVar(&f.a.Needs.Executor, "executor", "", "cli, tab, or new-tab")
	fs.StringVar(&f.a.Note, "note", "", "a note kept with the change")
	fs.StringVar(&f.a.Runner, "runner", "", "the runner's name")
	fs.StringVar(&f.a.State, "state", "", "list only jobs in this state")
	fs.BoolVar(&f.a.All, "all", false, "list final jobs too")
	fs.Var(&f.kinds, "kind", "job kind")
	fs.Var(&f.exit, "exit", "the job's exit code")
	fs.BoolVar(&f.jsonOut, "json", false, "print JSON")
	fs.BoolVar(&f.follow, "follow", false, "wait for new history")
	var id string
	if len(args) > 0 && !strings.HasPrefix(args[0], "-") {
		id, args = args[0], args[1:]
	}
	if err := fs.Parse(args); err != nil {
		return f, fmt.Errorf("%w", err)
	}
	var unused []string
	fs.Visit(func(fl *flag.Flag) {
		if !slices.Contains(allowed, fl.Name) {
			unused = append(unused, "--"+fl.Name)
		}
	})
	if len(unused) > 0 {
		return f, fmt.Errorf("%s does not take %s", sub, strings.Join(unused, ", "))
	}
	rest := fs.Args()
	if id == "" && len(rest) > 0 {
		id, rest = rest[0], rest[1:]
	}
	if len(rest) > 0 || (id != "" && !jobsNeedID[sub]) {
		return f, errors.New("unexpected arguments")
	}
	if jobsNeedID[sub] && id == "" {
		return f, errors.New("want a job id")
	}
	f.a.ID, f.a.Exit = id, f.exit.v
	if err := jobsRequired(sub, &f); err != nil {
		return f, err
	}
	return f, nil
}

// jobsRequired checks the required flags and moves --kind and
// --machine to where each subcommand reads them.
func jobsRequired(sub string, f *jobsFlags) error {
	switch sub {
	case "enqueue":
		if len(f.kinds) != 1 || f.a.Subject == "" {
			return errors.New("want one --kind and a --subject")
		}
		f.a.Kind = f.kinds[0]
	case "claim":
		if f.a.Runner == "" {
			return errors.New("want --runner")
		}
		f.a.Kinds, f.a.Machine, f.a.Needs = f.kinds, f.a.Needs.Machine, jobs.Needs{}
	case "heartbeat":
		if f.a.Runner == "" {
			return errors.New("want --runner")
		}
	case "ls":
		if len(f.kinds) > 1 {
			return errors.New("want at most one --kind")
		}
		if len(f.kinds) == 1 {
			f.a.Kind = f.kinds[0]
		}
	}
	return nil
}

// jobsCall runs one jobs op. A refused op prints the daemon's reason
// and exits 1.
func jobsCall(ctx context.Context, sub string, a jobs.Args, env []string, stderr io.Writer) (jobs.Reply, int) {
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "jobs-" + sub, Env: env, Jobs: &a}, stderr)
	if code != 0 {
		return jobs.Reply{}, 1
	}
	if !resp.OK || resp.Jobs == nil {
		fmt.Fprintf(stderr, "agentbus jobs %s: %s\n", sub, resp.Error)
		return jobs.Reply{}, 1
	}
	return *resp.Jobs, 0
}

func jobsClaim(ctx context.Context, f jobsFlags, env []string, stdout, stderr io.Writer) int {
	r, code := jobsCall(ctx, "claim", f.a, env, stderr)
	switch {
	case code != 0:
		return code
	case r.Held:
		fmt.Fprintf(stderr, "agentbus jobs claim: jobs are on hold: %s\n", r.HoldNote)
		return jobsHeld
	case r.Job == nil:
		fmt.Fprintln(stderr, "agentbus jobs claim: no job ready")
		return jobsNoneReady
	}
	return printJobs("claim", f, r, stdout)
}

func printJobs(sub string, f jobsFlags, r jobs.Reply, stdout io.Writer) int {
	switch {
	case sub == "ls" && f.jsonOut:
		writeJSON(stdout, r.Jobs)
	case sub == "ls":
		printJobTable(stdout, r.Jobs)
	case sub == "stats" && f.jsonOut:
		writeJSON(stdout, r.Stats)
	case sub == "stats":
		printJobStats(stdout, r.Stats)
	case sub == "hold" || sub == "unhold":
		printHold(stdout, r.Held, r.HoldNote)
	case sub == "heartbeat":
	case f.jsonOut:
		writeJSON(stdout, r.Job)
	case sub == "show":
		printJob(stdout, *r.Job)
	case sub == "claim":
		j := r.Job
		fmt.Fprintf(stdout, "%s %s %s %s\n", j.ID, j.Kind, j.Subject, j.Head)
	case r.Duplicate:
		fmt.Fprintf(stdout, "%s %s (duplicate)\n", r.Job.ID, r.Job.State)
	default:
		fmt.Fprintf(stdout, "%s %s\n", r.Job.ID, r.Job.State)
	}
	return 0
}

func printHold(w io.Writer, held bool, note string) {
	if held {
		fmt.Fprintf(w, "held: yes (%s)\n", note)
		return
	}
	fmt.Fprintln(w, "held: no")
}

func printJobStats(w io.Writer, st *jobs.Stats) {
	printHold(w, st.Held, st.HoldNote)
	for _, s := range jobs.AllStates {
		fmt.Fprintf(w, "%s %d\n", s, st.States[s])
	}
}

func printJobTable(w io.Writer, list []jobs.Job) {
	tw := tabwriter.NewWriter(w, 0, 4, 2, ' ', 0)
	fmt.Fprintln(tw, "ID\tSTATE\tKIND\tSUBJECT\tHEAD\tATTEMPTS\tHOLDER")
	for _, j := range list {
		fmt.Fprintf(tw, "%s\t%s\t%s\t%s\t%s\t%d/%d\t%s\n", j.ID, j.State, j.Kind, j.Subject, dash(j.Head), j.Attempts, j.MaxAttempts, dash(j.Holder))
	}
	_ = tw.Flush()
}

func dash(s string) string {
	if s == "" {
		return "-"
	}
	return s
}

func printJob(w io.Writer, j jobs.Job) {
	state := string(j.State)
	if j.ParkedFrom != "" {
		state += " (from " + string(j.ParkedFrom) + ")"
	}
	fmt.Fprintf(w, "id: %s\nstate: %s\nkind: %s\nsubject: %s\n", j.ID, state, j.Kind, j.Subject)
	if j.Head != "" {
		fmt.Fprintf(w, "head: %s\n", j.Head)
	}
	fmt.Fprintf(w, "needs: machine %s, checkout %s, executor %s", j.Needs.Machine, j.Needs.Checkout, j.Needs.Executor)
	if j.Needs.Affinity != "" {
		fmt.Fprintf(w, ", affinity %s", j.Needs.Affinity)
	}
	fmt.Fprintf(w, "\npriority: %d\nattempts: %d/%d\n", j.Priority, j.Attempts, j.MaxAttempts)
	if !j.NotBefore.IsZero() {
		fmt.Fprintf(w, "not before: %s\n", j.NotBefore.Format(time.RFC3339))
	}
	if j.Holder != "" {
		fmt.Fprintf(w, "holder: %s\nheartbeat: %s\n", j.Holder, j.Heartbeat.Format(time.RFC3339))
	}
	if j.Exit != nil {
		fmt.Fprintf(w, "exit: %d\n", *j.Exit)
	}
	if j.Note != "" {
		fmt.Fprintf(w, "note: %s\n", j.Note)
	}
}

func formatEntry(e jobs.Entry) string {
	line := e.At.Format(time.RFC3339) + " "
	if e.From != "" {
		line += string(e.From) + " -> "
	}
	line += string(e.To) + " by " + e.Actor
	if e.Note != "" {
		line += ": " + e.Note
	}
	return line
}

// jobsLog prints a job's history. With --follow it asks again every
// jobsLogPoll and prints new entries until the job is final or ctx ends.
func jobsLog(ctx context.Context, f jobsFlags, env []string, stdout, stderr io.Writer) int {
	printed := 0
	for {
		r, code := jobsCall(ctx, "show", f.a, env, stderr)
		if code != 0 {
			return code
		}
		for _, e := range r.Job.History[printed:] {
			fmt.Fprintln(stdout, formatEntry(e))
		}
		printed = len(r.Job.History)
		if !f.follow || r.Job.State.Final() {
			return 0
		}
		select {
		case <-ctx.Done():
			return 0
		case <-time.After(jobsLogPoll):
		}
	}
}
