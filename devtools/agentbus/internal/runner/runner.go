// Package runner is the per-machine jobs runner: it claims jobs from
// the agentbus queue and runs them with the cli, tab, or new-tab
// executor, within per-kind limits and the start pacing.
//
// It never writes agent settings, hooks, or permissions, and never
// answers a dialog or types into a pane as the user: a tab job whose
// target shows a dialog is gated for the user instead. A job is never
// sent twice: once a command or prompt may have acted, the runner only
// reports or stops heartbeating, and the daemon settles a lost job.
package runner

import (
	"context"
	"fmt"
	"io"
	"slices"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
)

// Queue is the agentbus job queue as the runner uses it, acting as the
// runner on its machine.
type Queue interface {
	// Claim takes the next ready job of kinds. held is true while jobs
	// are on hold, with why.
	Claim(ctx context.Context, kinds []string) (job *jobs.Job, held bool, why string, err error)
	Heartbeat(ctx context.Context, id string) error
	Done(ctx context.Context, id, note string) error
	Fail(ctx context.Context, id string, exit *int, note string) error
	Gate(ctx context.Context, id, note string) error
	Show(ctx context.Context, id string) (jobs.Job, error)
	// List lists jobs in state of kind; an empty kind is every kind.
	List(ctx context.Context, state jobs.State, kind string) ([]jobs.Job, error)
	// Held reports the hold without claiming.
	Held(ctx context.Context) (bool, string, error)
}

// Host is the runner's machine.
type Host interface {
	// Run runs argv in dir (none when empty) with env added, and
	// returns its exit code. An error means it did not run to an exit.
	Run(ctx context.Context, argv []string, dir string, env []string) (int, error)
	// Dirty reports whether the checkout has changes git would stash.
	Dirty(ctx context.Context, checkout string) (bool, error)
	// FreeDisk is the free space, in bytes, on the file system of path.
	FreeDisk(path string) (uint64, error)
}

// Panes are the agent panes a tab job prompts.
type Panes interface {
	// Dialog names the dialog target shows, or is empty when it shows
	// none.
	Dialog(ctx context.Context, target string) (string, error)
	// Prompt sends text to target as an agentbus message under the
	// retry key key, so a repeat is never stored twice.
	Prompt(ctx context.Context, target, key, text string) error
	// Live reports whether target's pane still runs its agent.
	Live(ctx context.Context, target string) (bool, error)
}

// Runner runs jobs on one machine. Set the fields, then call Run, or
// Tick in tests.
type Runner struct {
	// Name is the runner's name in the queue: its unit name.
	Name    string
	Machine string
	Config  Config
	Queue   Queue
	Host    Host
	Panes   Panes
	// DryRun logs what the runner would claim and run, and changes
	// nothing.
	DryRun bool
	Now    func() time.Time
	Log    io.Writer

	mu sync.Mutex
	// active are the jobs this runner holds, by id.
	active    map[string]*active
	results   []result
	lastStart time.Time
	lastLog   string
	wg        sync.WaitGroup
}

// active is one held job. An executing job runs its command or
// prompt; a waiting one is an agent's, until the agent reports or
// until passes.
type active struct {
	job     jobs.Job
	waiting bool
	until   time.Time
}

type outcome int

const (
	outDone outcome = iota
	outFail
	outGate
	outWait
)

type result struct {
	id   string
	out  outcome
	exit *int
	note string
}

// Run ticks every Config.Poll until ctx ends, then waits for the
// commands and prompts under way and reports them.
func (r *Runner) Run(ctx context.Context) error {
	poll := r.Config.Poll
	if poll <= 0 {
		poll = DefaultPoll
	}
	t := time.NewTicker(poll)
	defer t.Stop()
	for {
		r.Tick(ctx)
		select {
		case <-ctx.Done():
			r.wg.Wait()
			r.report(context.WithoutCancel(ctx))
			return nil
		case <-t.C:
		}
	}
}

// Wait blocks until every command and prompt under way has ended.
func (r *Runner) Wait() { r.wg.Wait() }

// Tick reports finished jobs, heartbeats held ones, and claims more
// while limits allow.
func (r *Runner) Tick(ctx context.Context) {
	r.report(ctx)
	r.heartbeat(ctx)
	if ctx.Err() == nil {
		r.fill(ctx)
	}
}

func (r *Runner) now() time.Time {
	if r.Now == nil {
		return time.Now()
	}
	return r.Now()
}

func (r *Runner) logf(format string, args ...any) {
	if r.Log != nil {
		fmt.Fprintf(r.Log, "%s %s\n", r.now().UTC().Format(time.RFC3339), fmt.Sprintf(format, args...))
	}
}

// logChange logs line only when it differs from the last such line, so
// an idle poll does not repeat itself.
func (r *Runner) logChange(line string) {
	r.mu.Lock()
	same := r.lastLog == line
	r.lastLog = line
	r.mu.Unlock()
	if !same {
		r.logf("%s", line)
	}
}

// report hands finished executions to the queue.
func (r *Runner) report(ctx context.Context) {
	r.mu.Lock()
	done := r.results
	r.results = nil
	r.mu.Unlock()
	for _, res := range done {
		r.mu.Lock()
		a := r.active[res.id]
		r.mu.Unlock()
		if a == nil {
			continue
		}
		var err error
		switch res.out {
		case outDone:
			err = r.Queue.Done(ctx, res.id, res.note)
		case outFail:
			err = r.Queue.Fail(ctx, res.id, res.exit, res.note)
		case outGate:
			err = r.Queue.Gate(ctx, res.id, res.note)
		case outWait:
			r.mu.Lock()
			a.waiting, a.until = true, r.now().Add(r.Config.Kinds[a.job.Kind].Timeout)
			r.mu.Unlock()
			r.logf("%s %s %s: %s", res.id, a.job.Kind, a.job.Subject, res.note)
			continue
		}
		if err != nil {
			// The job stays running without heartbeats; the daemon
			// settles it. Reporting again could not undo what it did.
			r.logf("%s: report: %s", res.id, err)
		} else {
			r.logf("%s %s %s: %s", res.id, a.job.Kind, a.job.Subject, describe(res))
		}
		r.drop(res.id)
	}
}

func describe(res result) string {
	switch res.out {
	case outDone:
		return "done " + res.note
	case outGate:
		return "gated: " + res.note
	}
	if res.exit != nil {
		return fmt.Sprintf("failed (exit %d) %s", *res.exit, res.note)
	}
	return "failed: " + res.note
}

func (r *Runner) drop(id string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	delete(r.active, id)
}

// heartbeat tells the queue the runner still holds its jobs. A waiting
// job the agent closed is dropped. Past its timeout the runner stops
// heartbeating it, and the daemon settles it as lost: it is never sent
// again.
func (r *Runner) heartbeat(ctx context.Context) {
	r.mu.Lock()
	held := make([]active, 0, len(r.active))
	for _, a := range r.active {
		held = append(held, *a)
	}
	r.mu.Unlock()
	now := r.now()
	for _, a := range held {
		id := a.job.ID
		if a.waiting {
			j, err := r.Queue.Show(ctx, id)
			if err == nil && (j.State != jobs.StateRunning || j.Holder != r.Name) {
				r.logf("%s %s %s: %s by the agent", id, j.Kind, j.Subject, j.State)
				r.drop(id)
				continue
			}
			if now.After(a.until) {
				r.logf("%s: no report from the agent within %s; stopped heartbeating, so the daemon settles it", id, r.Config.Kinds[a.job.Kind].Timeout)
				r.drop(id)
				continue
			}
		}
		if err := r.Queue.Heartbeat(ctx, id); err != nil {
			r.logf("%s: heartbeat: %s", id, err)
		}
	}
}

// fill claims jobs while a kind has room.
func (r *Runner) fill(ctx context.Context) {
	for {
		kinds, why := r.claimable(ctx)
		if len(kinds) == 0 {
			r.logChange("idle: " + why)
			return
		}
		if r.DryRun {
			r.plan(ctx, kinds)
			return
		}
		j, held, holdWhy, err := r.Queue.Claim(ctx, kinds)
		switch {
		case err != nil:
			r.logChange("claim: " + err.Error())
			return
		case held:
			r.logChange("on hold: " + holdWhy)
			return
		case j == nil:
			r.logChange("idle: no job ready")
			return
		}
		r.start(ctx, *j)
	}
}

// claimable are the configured kinds with room now, and why there are
// none.
func (r *Runner) claimable(ctx context.Context) ([]string, string) {
	r.mu.Lock()
	running := map[string]int{}
	starts := 0
	for _, a := range r.active {
		running[a.job.Kind]++
		if r.Config.Kinds[a.job.Kind].Start {
			starts++
		}
	}
	r.mu.Unlock()
	var kinds, full []string
	paced := ""
	for _, name := range r.Config.KindNames() {
		k := r.Config.Kinds[name]
		if running[name] >= k.Parallel {
			full = append(full, name)
			continue
		}
		if k.Start {
			if paced == "" {
				paced = r.pace(ctx, starts)
			}
			if paced != "ok" {
				continue
			}
		}
		kinds = append(kinds, name)
	}
	why := "every kind is at its parallel limit"
	if paced != "" && paced != "ok" {
		why = "start kinds wait: " + paced
		if len(full) > 0 {
			why += "; at the parallel limit: " + strings.Join(full, ", ")
		}
	}
	return kinds, why
}

// pace is "ok" when a start kind may start now, else why not.
func (r *Runner) pace(ctx context.Context, starts int) string {
	p := r.Config.Start
	now := r.now()
	r.mu.Lock()
	last := r.lastStart
	r.mu.Unlock()
	if p.MinSpacing > 0 && !last.IsZero() && now.Sub(last) < p.MinSpacing {
		return fmt.Sprintf("last start %s ago, spacing %s", now.Sub(last).Round(time.Second), p.MinSpacing)
	}
	if p.LaneCap > 0 && starts >= p.LaneCap {
		return fmt.Sprintf("%d start jobs on this lane, cap %d", starts, p.LaneCap)
	}
	if p.MaxInFlight > 0 {
		n := 0
		for name, k := range r.Config.Kinds {
			if !k.Start {
				continue
			}
			list, err := r.Queue.List(ctx, jobs.StateRunning, name)
			if err != nil {
				return "cannot count start jobs in flight: " + err.Error()
			}
			n += len(list)
		}
		if n >= p.MaxInFlight {
			return fmt.Sprintf("%d start jobs in flight, max %d", n, p.MaxInFlight)
		}
	}
	if p.MinFreeDisk > 0 {
		free, err := r.Host.FreeDisk(p.DiskPath)
		if err != nil {
			return "cannot read free disk: " + err.Error()
		}
		if free < p.MinFreeDisk {
			return fmt.Sprintf("%d MB free on %s, want %d MB", free>>20, p.DiskPath, p.MinFreeDisk>>20)
		}
	}
	return "ok"
}

// plan logs the job a claim would take now, and how it would run it.
func (r *Runner) plan(ctx context.Context, kinds []string) {
	if held, why, err := r.Queue.Held(ctx); err != nil || held {
		if err != nil {
			why = err.Error()
		}
		r.logChange("dry-run: on hold: " + why)
		return
	}
	ready, err := r.Queue.List(ctx, jobs.StateReady, "")
	if err != nil {
		r.logChange("dry-run: list: " + err.Error())
		return
	}
	now := r.now()
	var cands []jobs.Job
	for _, j := range ready {
		if slices.Contains(kinds, j.Kind) && (j.Needs.Machine == "any" || j.Needs.Machine == r.Machine) && !now.Before(j.NotBefore) {
			cands = append(cands, j)
		}
	}
	if len(cands) == 0 {
		r.logChange("dry-run: no job ready")
		return
	}
	slices.SortStableFunc(cands, func(x, y jobs.Job) int { return y.Priority - x.Priority })
	j := cands[0]
	r.logChange(fmt.Sprintf("dry-run: would claim %s %s %s and %s", j.ID, j.Kind, j.Subject, r.what(j)))
}

// what says how the runner would run j.
func (r *Runner) what(j jobs.Job) string {
	k := r.Config.Kinds[j.Kind]
	if j.Saved != "" {
		how := "resume from " + j.Saved + " in a new agent"
		if j.Tab != "" {
			how = "resume in tab " + j.Tab + " if it lives, else from " + j.Saved + " in a new agent"
		}
		return how
	}
	switch j.Needs.Executor {
	case "tab":
		return "prompt " + Expand(k.Target, j, r.Name, "") + " unless it shows a dialog"
	case "new-tab":
		return "start an agent with " + strings.Join(expandAll(k.Command, j, r.Name, r.prompt(j)), " ")
	}
	return "run " + strings.Join(expandAll(k.Command, j, r.Name, ""), " ")
}

// start runs a claimed job, after refusing a dirty checkout for a start
// kind.
func (r *Runner) start(ctx context.Context, j jobs.Job) {
	k := r.Config.Kinds[j.Kind]
	r.mu.Lock()
	if r.active == nil {
		r.active = map[string]*active{}
	}
	r.active[j.ID] = &active{job: j}
	r.mu.Unlock()
	r.logf("%s %s %s: claimed", j.ID, j.Kind, j.Subject)
	if k.Start && j.Needs.Checkout != "none" {
		dirty, err := r.Host.Dirty(ctx, j.Needs.Checkout)
		if err != nil || dirty {
			note := "checkout " + j.Needs.Checkout + " has uncommitted changes; the runner never stashes"
			if err != nil {
				note = "cannot read checkout " + j.Needs.Checkout + ": " + err.Error()
			}
			r.finish(result{id: j.ID, out: outFail, note: note})
			return
		}
	}
	if k.Start {
		r.mu.Lock()
		r.lastStart = r.now()
		r.mu.Unlock()
	}
	r.wg.Add(1)
	go func() {
		defer r.wg.Done()
		res := r.execute(context.WithoutCancel(ctx), j, k)
		res.id = j.ID
		r.finish(res)
	}()
}

func (r *Runner) finish(res result) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.results = append(r.results, res)
}

// execute runs one job with its executor. A job resumed from a park
// goes back to the tab it parked from while that tab lives, and
// otherwise to a new agent started from its saved ref.
func (r *Runner) execute(ctx context.Context, j jobs.Job, k Kind) result {
	if j.Saved != "" {
		if j.Tab != "" {
			if live, err := r.Panes.Live(ctx, j.Tab); err == nil && live {
				return r.tab(ctx, j, j.Tab)
			}
		}
		return r.newTab(ctx, j, k)
	}
	switch j.Needs.Executor {
	case "tab":
		target := Expand(k.Target, j, r.Name, "")
		if target == "" {
			return result{out: outFail, note: "the runner has no target for a tab " + j.Kind + " job"}
		}
		return r.tab(ctx, j, target)
	case "new-tab":
		return r.newTab(ctx, j, k)
	default:
		if len(k.Command) == 0 {
			return result{out: outFail, note: "the runner has no command for a cli " + j.Kind + " job"}
		}
		return r.command(ctx, j, k, "")
	}
}

// newTab starts an agent with the kind's command and the prompt, and
// waits for its report.
func (r *Runner) newTab(ctx context.Context, j jobs.Job, k Kind) result {
	if len(k.Command) == 0 {
		return result{out: outFail, note: "the runner has no command for a new-tab " + j.Kind + " job"}
	}
	res := r.command(ctx, j, k, r.prompt(j))
	if res.out == outDone {
		return result{out: outWait, note: "agent started; waiting for its report"}
	}
	return res
}

func (r *Runner) command(ctx context.Context, j jobs.Job, k Kind, prompt string) result {
	ctx, cancel := context.WithTimeout(ctx, k.Timeout)
	defer cancel()
	dir := ""
	if j.Needs.Checkout != "none" {
		dir = j.Needs.Checkout
	}
	env := []string{
		"JOB_ID=" + j.ID, "JOB_KIND=" + j.Kind, "JOB_SUBJECT=" + j.Subject, "JOB_HEAD=" + j.Head,
		"JOB_CHECKOUT=" + j.Needs.Checkout, "JOB_RUNNER=" + r.Name, "JOB_SAVED=" + j.Saved,
	}
	if prompt != "" {
		env = append(env, "JOB_PROMPT="+prompt)
	}
	code, err := r.Host.Run(ctx, expandAll(k.Command, j, r.Name, prompt), dir, env)
	switch {
	case err != nil:
		return result{out: outFail, note: err.Error()}
	case code != 0:
		return result{out: outFail, exit: &code}
	}
	return result{out: outDone}
}

// tab prompts the target agent, unless it shows a dialog: then the job
// is gated and the dialog is left for the user.
func (r *Runner) tab(ctx context.Context, j jobs.Job, target string) result {
	dialog, err := r.Panes.Dialog(ctx, target)
	if err != nil {
		return result{out: outFail, note: "cannot read target " + target + ": " + err.Error()}
	}
	if dialog != "" {
		return result{out: outGate, note: "target " + target + " shows a " + dialog}
	}
	key := r.Machine + ":job-" + j.ID + "-" + strconv.Itoa(j.Attempts)
	if err := r.Panes.Prompt(ctx, target, key, r.prompt(j)); err != nil {
		return result{out: outFail, note: "prompt " + target + ": " + err.Error()}
	}
	return result{out: outWait, note: "prompted " + target + "; waiting for its report"}
}

// prompt is the text an agent gets for j: the kind's prompt, the job,
// and how to report.
func (r *Runner) prompt(j jobs.Job) string {
	var b strings.Builder
	if p := Expand(r.Config.Kinds[j.Kind].Prompt, j, r.Name, ""); p != "" {
		b.WriteString(p + "\n\n")
	}
	fmt.Fprintf(&b, "agentbus job %s: %s %s", j.ID, j.Kind, j.Subject)
	if j.Head != "" {
		b.WriteString(" at " + j.Head)
	}
	if j.Needs.Checkout != "none" {
		b.WriteString(" in " + j.Needs.Checkout)
	}
	if j.Saved != "" {
		fmt.Fprintf(&b, ".\nIt was parked: resume from branch %s. Resume note: %s", j.Saved, j.Resume)
	}
	fmt.Fprintf(&b, ".\nWhen you finish, run: agentbus jobs done %s --runner %s --note <what you did>\n"+
		"If you cannot, run: agentbus jobs fail %s --runner %s --note <why>", j.ID, r.Name, j.ID, r.Name)
	return b.String()
}

// Expand fills a template's placeholders from j: {id}, {kind},
// {subject}, {head}, {checkout}, {saved}, {runner}, and {prompt}.
func Expand(tmpl string, j jobs.Job, runner, prompt string) string {
	return strings.NewReplacer(
		"{id}", j.ID, "{kind}", j.Kind, "{subject}", j.Subject, "{head}", j.Head,
		"{checkout}", j.Needs.Checkout, "{saved}", j.Saved, "{runner}", runner, "{prompt}", prompt,
	).Replace(tmpl)
}

func expandAll(argv []string, j jobs.Job, runner, prompt string) []string {
	out := make([]string, len(argv))
	for i, a := range argv {
		out[i] = Expand(a, j, runner, prompt)
	}
	return out
}
