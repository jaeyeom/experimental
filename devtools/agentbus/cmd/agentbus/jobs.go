package main

import (
	"cmp"
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os/exec"
	"slices"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

// Exit codes of jobs claim and reserve besides 0, 1, and 2.
const (
	jobsNoneReady = 3
	jobsHeld      = 4
	// jobsNoBudget is a reserve the GitHub budget refused.
	jobsNoBudget = 3
)

// jobsLogPoll is how often log --follow asks for new history; tests
// shorten it.
var jobsLogPoll = time.Second

const jobsFlagsHelp = `enqueue --kind <k> --subject <s> [--head <sha>] [--priority <n>]
        [--machine <m>|any] [--affinity tab|lane|owner]
        [--checkout <path>|none] [--executor cli|tab|new-tab] [--item <id>]...
        [--note <text>] [--json]
    add a job; one live job per kind + subject + head, so a repeat
    prints the live job with "(duplicate)". There is no --gate: gates
    come only from the user's jobs policy. An ask kind waits gated for
    the user's answer in decide; an off kind is refused. --item lists
    the job's work items; items a live job holds parked are dropped
claim --runner <name> [--kind <k>]... [--machine <m>] [--json]
    take the next ready job, and the lease on its checkout on this
    machine with it. Exit 3: none ready; 4: jobs are on hold
heartbeat <id> --runner <name>
    tell the daemon the holder is still at it
done <id> [--runner <name>] [--note <text>]
fail <id> [--runner <name>] [--exit <code>] [--note <text>]
    close a running job as its holder, or settle an unknown or parked
    job without --runner. A retryable --exit retries after the backoff
gate <id> --runner <name> --note <why>
    the holder hands a running job it has not acted on to the user: it
    is gated, lets go of its lease, and the user is asked in decide
park <id> --runner <name> --decide <item> --saved <branch> --note <resume note>
        [--item <id>]...
    the holder parks a running job while the user answers decide item
    <item>: its work is on branch <branch>, never a stash, and its
    checkout must be clean. With --item the handled part is done and a
    gated child job holds those items. The user's answer in the inbox
    makes it ready again, for its tab, or a new agent from <branch>
runner --name <unit> [--machine <m>] [--config <path>] [--dry-run]
    run jobs on this machine until stopped; see docs/jobs.org
ls [--state <s>] [--kind <k>] [--subject <s>] [--all] [--json]
    live jobs; --all adds done, failed, and dead ones
show <id> [--json]
log <id> [--follow]   state history; --follow waits until the job is final
stats [--json]        jobs per state, the hold, and the GitHub budget
status [--json]
    per machine: its leases, and the count and oldest age of its
    queued (and ready), running, gated, parked, and unknown jobs
reserve --calls <n> [--kind <k>] [--json]
    take n GitHub API calls from the shared budget before making them.
    With --kind the kind's floor exemption applies. Exit 3: they do
    not fit; the budget resets at the time printed
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
	items   kindList
	exit    optionalInt
	calls   int
	jsonOut bool
	follow  bool
}

var jobsSubFlags = map[string][]string{
	"enqueue":   {"kind", "subject", "head", "priority", "machine", "affinity", "checkout", "executor", "item", "note", "json"},
	"claim":     {"runner", "kind", "machine", "json"},
	"heartbeat": {"runner"},
	"done":      {"runner", "note"},
	"fail":      {"runner", "exit", "note"},
	"gate":      {"runner", "note"},
	"park":      {"runner", "decide", "saved", "note", "item"},
	"ls":        {"state", "kind", "subject", "all", "json"},
	"show":      {"json"},
	"log":       {"follow"},
	"stats":     {"json"},
	"status":    {"json"},
	"reserve":   {"calls", "kind", "json"},
	"hold":      {"note"},
	"unhold":    {},
}

// jobsNeedID are the subcommands that take one job id.
var jobsNeedID = map[string]bool{"heartbeat": true, "done": true, "fail": true, "gate": true, "park": true, "show": true, "log": true}

func runJobs(ctx context.Context, args, env []string, _ io.Reader, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintf(stderr, "agentbus jobs: want a subcommand: enqueue, claim, heartbeat, done, fail, gate, park, runner, ls, show, log, stats, status, reserve, hold, unhold\n")
		return 2
	}
	sub := args[0]
	if sub == "runner" {
		return jobsRunner(ctx, args[1:], env, stderr)
	}
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
	case "park":
		return jobsPark(ctx, f, env, stdout, stderr)
	case "reserve":
		return jobsReserve(ctx, f, env, stdout, stderr)
	case "status":
		return jobsStatus(ctx, f, env, stdout, stderr)
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
	fs.Var(&f.items, "item", "a work item of the job")
	fs.StringVar(&f.a.Decide, "decide", "", "the decide item a parked job waits on")
	fs.StringVar(&f.a.Saved, "saved", "", "the branch that holds a parked job's work")
	fs.Var(&f.exit, "exit", "the job's exit code")
	fs.IntVar(&f.calls, "calls", 0, "GitHub API calls to reserve")
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
	f.a.ID, f.a.Exit, f.a.Items = id, f.exit.v, f.items
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
	case "heartbeat", "gate", "park":
		return holderRequired(sub, f.a)
	case "reserve":
		if f.calls < 1 || len(f.kinds) > 1 {
			return errors.New("want --calls of at least 1 and at most one --kind")
		}
		f.a.Calls = f.calls
		if len(f.kinds) == 1 {
			f.a.Kind = f.kinds[0]
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

// holderRequired checks the flags of the ops a job's holder runs.
func holderRequired(sub string, a jobs.Args) error {
	note := strings.TrimSpace(a.Note) != ""
	switch {
	case a.Runner == "":
		return errors.New("want --runner")
	case sub == "gate" && !note:
		return errors.New("want --runner and --note")
	case sub == "park" && (a.Decide == "" || a.Saved == "" || !note):
		return errors.New("want --runner, --decide, --saved, and --note")
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
	fmt.Fprintf(w, "budget: %s\n", budgetText(st.Budget))
}

func jobsStatus(ctx context.Context, f jobsFlags, env []string, stdout, stderr io.Writer) int {
	r, code := jobsCall(ctx, "status", f.a, env, stderr)
	switch {
	case code != 0:
		return code
	case r.Status == nil:
		fmt.Fprintln(stderr, "agentbus jobs status: the daemon sent no status; is it older than this agentbus?")
		return 1
	case f.jsonOut:
		writeJSON(stdout, r.Status)
	default:
		printJobStatus(stdout, *r.Status)
	}
	return 0
}

// printJobStatus prints one row per machine: each state's job count
// and the oldest job's time in that state, and the leases that are not
// free.
func printJobStatus(w io.Writer, v jobs.StatusView) {
	if len(v.Machines) == 0 {
		fmt.Fprintln(w, "no jobs or leases")
		return
	}
	tw := tabwriter.NewWriter(w, 0, 4, 2, ' ', 0)
	fmt.Fprint(tw, "MACHINE")
	for _, st := range jobs.StatusStates {
		fmt.Fprintf(tw, "\t%s", strings.ToUpper(string(st)))
	}
	fmt.Fprintln(tw, "\tLEASES")
	for _, m := range v.Machines {
		fmt.Fprint(tw, m.Machine)
		for _, st := range jobs.StatusStates {
			cell := "-"
			if c := m.Jobs[st]; c.Count > 0 {
				cell = fmt.Sprintf("%d (%s)", c.Count, lease.Short(v.At.Sub(c.Oldest).Truncate(time.Second)))
			}
			fmt.Fprintf(tw, "\t%s", cell)
		}
		leases := make([]string, 0, len(m.Leases))
		for _, l := range m.Leases {
			leases = append(leases, l.Checkout+" "+l.State+" by "+cmp.Or(l.Holder, "nobody"))
		}
		fmt.Fprintf(tw, "\t%s\n", dash(strings.Join(leases, ", ")))
	}
	_ = tw.Flush()
}

// budgetText is the GitHub budget's calls left, floor, and reset.
func budgetText(b *jobs.Budget) string {
	if b == nil {
		return "none"
	}
	text := fmt.Sprintf("%d of %d left, floor %d", b.Remaining, b.Limit, b.Floor)
	if !b.Reset.IsZero() {
		text += ", resets " + b.Reset.Format(time.RFC3339)
	}
	return text
}

// jobsReserve takes calls from the GitHub budget, or exits 3 when they
// do not fit.
func jobsReserve(ctx context.Context, f jobsFlags, env []string, stdout, stderr io.Writer) int {
	r, code := jobsCall(ctx, "reserve", f.a, env, stderr)
	switch {
	case code != 0:
		return code
	case f.jsonOut:
		writeJSON(stdout, r)
	case r.Reserved:
		fmt.Fprintf(stdout, "reserved %d: %s\n", f.a.Calls, budgetText(r.Budget))
	}
	if !r.Reserved {
		fmt.Fprintf(stderr, "agentbus jobs reserve: %d calls do not fit: %s\n", f.a.Calls, budgetText(r.Budget))
		return jobsNoBudget
	}
	return 0
}

func printJobTable(w io.Writer, list []jobs.Job) {
	tw := tabwriter.NewWriter(w, 0, 4, 2, ' ', 0)
	fmt.Fprintln(tw, "ID\tSTATE\tKIND\tSUBJECT\tHEAD\tATTEMPTS\tHOLDER")
	for _, j := range list {
		fmt.Fprintf(tw, "%s\t%s\t%s\t%s\t%s\t%d/%d\t%s\n", j.ID, stateLabel(j), j.Kind, j.Subject, dash(j.Head), j.Attempts, j.MaxAttempts, dash(j.Holder))
	}
	_ = tw.Flush()
}

func dash(s string) string {
	if s == "" {
		return "-"
	}
	return s
}

// stateLabel tells a parked job, clean and waiting on a decide item,
// from a job its runner gated because the pane is blocked.
func stateLabel(j jobs.Job) string {
	state := string(j.State)
	switch {
	case j.ParkedOn != "":
		state += " on " + j.ParkedOn
	case j.ParkedFrom != "":
		state += " (from " + string(j.ParkedFrom) + ")"
	case j.State == jobs.StateGated && len(j.History) > 0 && j.History[len(j.History)-1].From == jobs.StateRunning:
		state += " (blocked)"
	}
	return state
}

func printJob(w io.Writer, j jobs.Job) {
	fmt.Fprintf(w, "id: %s\nstate: %s\nkind: %s\nsubject: %s\n", j.ID, stateLabel(j), j.Kind, j.Subject)
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
	if j.Lease != "" {
		fmt.Fprintf(w, "lease: %s\n", j.Lease)
	}
	if j.Decide != "" {
		fmt.Fprintf(w, "decide: %s\n", j.Decide)
	}
	for _, f := range [][2]string{
		{"saved", j.Saved},
		{"resume", j.Resume},
		{"tab", j.Tab},
		{"items", strings.Join(j.Items, ", ")},
		{"parent", j.Parent},
	} {
		if f[1] != "" {
			fmt.Fprintf(w, "%s: %s\n", f[0], f[1])
		}
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

// jobsPark parks a job after checking its checkout, and prints the job
// and the child a partial park made.
func jobsPark(ctx context.Context, f jobsFlags, env []string, stdout, stderr io.Writer) int {
	if code := jobsParkCheck(ctx, f.a, env, stderr); code != 0 {
		return code
	}
	r, code := jobsCall(ctx, "park", f.a, env, stderr)
	if code != 0 {
		return code
	}
	fmt.Fprintf(stdout, "%s %s\n", r.Job.ID, stateLabel(*r.Job))
	if r.Child != nil {
		fmt.Fprintf(stdout, "%s %s\n", r.Child.ID, stateLabel(*r.Child))
	}
	return 0
}

// jobsParkCheck refuses a park whose checkout has uncommitted changes,
// or whose saved branch is not there: the park releases the checkout's
// lease, and the work must be on the branch, never in a stash.
func jobsParkCheck(ctx context.Context, a jobs.Args, env []string, stderr io.Writer) int {
	r, code := jobsCall(ctx, "show", jobs.Args{ID: a.ID}, env, stderr)
	if code != 0 {
		return code
	}
	checkout := r.Job.Needs.Checkout
	if checkout == "none" {
		return 0
	}
	// The ref is the caller's own branch name, passed after
	// --end-of-options.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	verify := exec.CommandContext(ctx, "git", "-C", checkout, "rev-parse", "--verify", "--quiet", "--end-of-options", a.Saved+"^{commit}") //nolint:gosec // see above
	if err := verify.Run(); err != nil {
		fmt.Fprintf(stderr, "agentbus jobs park: no branch %s in %s; commit the work there first\n", a.Saved, checkout)
		return 1
	}
	dirty, err := localHost{env: env}.Dirty(ctx, checkout)
	switch {
	case err != nil:
		fmt.Fprintf(stderr, "agentbus jobs park: cannot read checkout %s: %s\n", checkout, err)
		return 1
	case dirty:
		fmt.Fprintf(stderr, "agentbus jobs park: checkout %s has uncommitted changes; commit them to %s first. Park never stashes\n", checkout, a.Saved)
		return 1
	}
	return 0
}
