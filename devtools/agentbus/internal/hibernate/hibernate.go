package hibernate

import (
	"cmp"
	"context"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/memreport"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

// DefaultStopTimeout is how long an agent has to leave its pane's
// foreground after its exit input.
const DefaultStopTimeout = 20 * time.Second

// defaultPoll is how often the pane is looked at while waiting.
const defaultPoll = 500 * time.Millisecond

// Outcomes of one agent.
const (
	OutcomeHibernated = "hibernated"
	OutcomeRefused    = "refused"
	OutcomeWould      = "would-hibernate"
	OutcomeFailed     = "failed"
)

// Panes is the local herdr server.
type Panes interface {
	Inventory(ctx context.Context) ([]adapter.Agent, error)
	ProcessInfo(ctx context.Context, pane string) (herdr.ProcessInfo, error)
	Screen(ctx context.Context, pane string) (string, error)
	SendText(ctx context.Context, pane, text string) error
	SendKeys(ctx context.Context, pane string, keys ...string) error
	RunInPane(ctx context.Context, pane, command string) error
}

// Bus is the daemon that owns the names, the leases, and the records:
// the local one, or the hub's in hub mode.
type Bus interface {
	// State is the names, what the daemon has seen of each agent, the
	// records, and the lease table.
	State(ctx context.Context) (Reply, error)
	// Begin writes rec as stopping. The daemon refuses an agent that
	// holds or is queued for a lease.
	Begin(ctx context.Context, rec Record) (Record, error)
	// End marks the record hibernated or stop-failed.
	End(ctx context.Context, handle, status, reason string) error
	// Refused logs a refusal.
	Refused(ctx context.Context, handle string, reasons []string) error
}

// Clock is the time hibernate reads and waits on.
type Clock interface {
	Now() time.Time
	Sleep(ctx context.Context, d time.Duration) error
}

// Hibernator stops idle agents on this machine.
type Hibernator struct {
	Panes Panes
	Bus   Bus
	// Procs reads the local process table, once per run.
	Procs func(ctx context.Context) ([]procinfo.Process, error)
	// Cwd reads a process's working directory.
	Cwd   procinfo.CwdFunc
	Clock Clock
	// Machine is this machine's label on the hub, or empty when the bus
	// is the local daemon. Bus handles are herdr:<Machine>/... then.
	Machine string
	// Exempt are hibernate_exempt: names, handles, or globs.
	Exempt []string
	// StopTimeout is how long to wait for the exit. Zero is
	// DefaultStopTimeout.
	StopTimeout time.Duration
	// Self is this process's pid. It and its ancestors are not build
	// tools.
	Self int
}

// Request picks the agents: Targets by name or handle, or every agent
// idle at least IdleOver. DryRun reports what would happen and changes
// nothing.
type Request struct {
	Targets  []string
	IdleOver time.Duration
	DryRun   bool
}

// Result is what happened to one agent.
type Result struct {
	// Name is the agent's first registered name, else its handle, or
	// the target as given when it named no agent.
	Name     string   `json:"name"`
	Handle   string   `json:"handle,omitempty"`
	Pane     string   `json:"pane,omitempty"`
	Outcome  string   `json:"outcome"`
	Reasons  []string `json:"reasons,omitempty"`
	RSSBytes int64    `json:"rss_bytes"` //nolint:tagliatelle // hibernate JSON shape
	// Warning is a problem that did not stop the hibernation, such as a
	// note that could not be printed.
	Warning string `json:"warning,omitempty"`
}

// Line is the one-line report of r.
func (r Result) Line() string {
	switch r.Outcome {
	case OutcomeHibernated:
		return fmt.Sprintf("hibernated %s (%s freed)", r.Name, memreport.HumanBytes(r.RSSBytes))
	case OutcomeWould:
		return fmt.Sprintf("would hibernate %s (%s)", r.Name, memreport.HumanBytes(r.RSSBytes))
	case OutcomeFailed:
		return fmt.Sprintf("failed %s: %s", r.Name, strings.Join(r.Reasons, "; "))
	default:
		return fmt.Sprintf("refused %s: %s", r.Name, strings.Join(r.Reasons, "; "))
	}
}

// candidate is one agent pane to check.
type candidate struct {
	agent  adapter.Agent
	handle string
	names  []string
}

func (c candidate) name() string {
	if len(c.names) > 0 {
		return c.names[0]
	}
	return c.handle
}

// Run checks and stops the agents req picks, in order: targets as
// given, or idle agents by pane. An error means nothing was checked.
func (h *Hibernator) Run(ctx context.Context, req Request) ([]Result, error) {
	agents, err := h.Panes.Inventory(ctx)
	if err != nil {
		return nil, fmt.Errorf("herdr: %w", err)
	}
	state, err := h.Bus.State(ctx)
	if err != nil {
		return nil, fmt.Errorf("agentbus daemon: %w", err)
	}
	procs, err := h.Procs(ctx)
	if err != nil {
		return nil, fmt.Errorf("process table: %w", err)
	}
	var results []Result
	var picked []candidate
	if len(req.Targets) > 0 {
		results, picked = h.resolve(agents, state, req.Targets)
	} else {
		picked = h.idle(agents, state, req.IdleOver)
	}
	for _, r := range results {
		h.logRefusal(ctx, req, r)
	}
	for _, c := range picked {
		r := h.one(ctx, c, state, procs, req.DryRun)
		if r.Outcome == OutcomeRefused {
			h.logRefusal(ctx, req, r)
		}
		results = append(results, r)
	}
	return results, nil
}

// logRefusal logs a refusal unless it is a dry run. A refusal that
// cannot be logged is still reported.
func (h *Hibernator) logRefusal(ctx context.Context, req Request, r Result) {
	if req.DryRun || r.Outcome != OutcomeRefused {
		return
	}
	handle := r.Handle
	if handle == "" {
		handle = r.Name
	}
	_ = h.Bus.Refused(ctx, handle, r.Reasons)
}

func (h *Hibernator) busHandle(local string) string {
	if h.Machine == "" {
		return local
	}
	if handle, err := hubcall.OnMachine(local, h.Machine); err == nil {
		return handle
	}
	return local
}

// resolve finds the agent of each target. A target that names no local
// agent is a refusal.
func (h *Hibernator) resolve(agents []adapter.Agent, state Reply, targets []string) ([]Result, []candidate) {
	var refused []Result
	var picked []candidate
	seen := map[string]bool{}
	for _, target := range targets {
		var handle string
		if strings.HasPrefix(target, "herdr:") {
			handle = h.busHandle(target)
		} else if handle = handleOf(state.Names, target); handle == "" {
			refused = append(refused, Result{Name: target, Outcome: OutcomeRefused, Reasons: []string{"not a registered name or handle"}})
			continue
		}
		if seen[handle] {
			continue
		}
		seen[handle] = true
		c, ok := h.agentOf(agents, state, handle)
		if !ok {
			reason := "no agent in a pane on this machine"
			if rec, found := recordOf(state.Records, handle); found && Asleep(rec.Status) {
				reason = "already hibernated"
			}
			refused = append(refused, Result{Name: target, Handle: handle, Outcome: OutcomeRefused, Reasons: []string{reason}})
			continue
		}
		picked = append(picked, c)
	}
	return refused, picked
}

// idle picks the agents the daemon has seen idle or done for at least
// d, by pane.
func (h *Hibernator) idle(agents []adapter.Agent, state Reply, d time.Duration) []candidate {
	var out []candidate
	for _, a := range agents {
		if a.Kind == "" {
			continue
		}
		handle := h.busHandle(a.Handle)
		seen, ok := state.Seen[handle]
		if !ok || !resting(seen.Status) || !resting(a.Status) || seen.For < d {
			continue
		}
		out = append(out, candidate{agent: a, handle: handle, names: state.Names[handle]})
	}
	slices.SortStableFunc(out, func(a, b candidate) int { return strings.Compare(a.agent.PaneID, b.agent.PaneID) })
	return out
}

func (h *Hibernator) agentOf(agents []adapter.Agent, state Reply, handle string) (candidate, bool) {
	for _, a := range agents {
		if a.Kind != "" && h.busHandle(a.Handle) == handle {
			return candidate{agent: a, handle: handle, names: state.Names[handle]}, true
		}
	}
	return candidate{}, false
}

func handleOf(names map[string][]string, name string) string {
	for handle, held := range names {
		if slices.Contains(held, name) {
			return handle
		}
	}
	return ""
}

func recordOf(records []Record, handle string) (Record, bool) {
	for _, r := range records {
		if r.Handle == handle {
			return r, true
		}
	}
	return Record{}, false
}

// resting is idle or done: the agent waits for input.
func resting(status string) bool {
	return status == "idle" || status == "done"
}

// facts are what the checks found about one agent.
type facts struct {
	proc    herdr.ForegroundProcess
	rss     int64
	reasons []string
}

// one checks c and, unless dryRun, stops it.
func (h *Hibernator) one(ctx context.Context, c candidate, state Reply, procs []procinfo.Process, dryRun bool) Result {
	f := h.check(ctx, c, state, procs)
	r := Result{Name: c.name(), Handle: c.handle, Pane: c.agent.PaneID, RSSBytes: f.rss}
	switch {
	case len(f.reasons) > 0:
		r.Outcome, r.Reasons = OutcomeRefused, f.reasons
		return r
	case dryRun:
		r.Outcome = OutcomeWould
		return r
	}
	return h.stop(ctx, c, f, r)
}

// check runs every eligibility check and names each that fails.
func (h *Hibernator) check(ctx context.Context, c candidate, state Reply, procs []procinfo.Process) facts {
	var f facts
	a := c.agent
	if !resting(a.Status) {
		f.reasons = append(f.reasons, "agent is "+cmp.Or(a.Status, "in an unknown state"))
	}
	h.checkProcesses(ctx, c, procs, memreport.LeasesOf(state.Leases, c.handle), &f)
	h.checkScreen(ctx, a, &f)
	f.reasons = append(f.reasons, LeaseReasons(state.Leases, c.handle)...)
	if pattern, ok := h.exempt(c); ok {
		f.reasons = append(f.reasons, fmt.Sprintf("exempt by hibernate_exempt %q", pattern))
	}
	return f
}

// checkProcesses reads the pane's foreground and refuses background
// jobs and build tools in the agent's cwd, by the rules agentbus mem
// and prsync use. A build tool counts only when it is in the agent's
// own process tree (its pane's shell and foreground), unless the agent
// holds or is queued for a lease: then any build in its cwd counts.
func (h *Hibernator) checkProcesses(ctx context.Context, c candidate, procs []procinfo.Process, leases []memreport.Lease, f *facts) {
	info, err := h.Panes.ProcessInfo(ctx, c.agent.PaneID)
	if err != nil {
		f.reasons = append(f.reasons, "cannot read the pane's processes: "+err.Error())
		return
	}
	proc, ok := agentProcess(info)
	if !ok {
		f.reasons = append(f.reasons, "no foreground process in the pane")
		return
	}
	f.proc = proc
	roots := info.ForegroundPIDs()
	f.rss = procinfo.SumRSS(procinfo.Tree(procs, roots))
	for _, j := range procinfo.Jobs(procs, roots, procinfo.DefaultJobMinAge, procinfo.DefaultHelpers) {
		f.reasons = append(f.reasons, fmt.Sprintf("background job pid %d %s", j.PID, procinfo.CommandLine(j)))
	}
	if proc.Cwd == "" {
		f.reasons = append(f.reasons, "cannot tell the agent's cwd")
		return
	}
	own := make(map[int]bool)
	treeRoots := roots
	if info.ShellPID > 0 {
		treeRoots = append([]int{info.ShellPID}, roots...)
	}
	for _, p := range procinfo.Tree(procs, treeRoots) {
		own[p.PID] = true
	}
	leaseNote := ""
	if len(leases) > 0 {
		leaseNote = "agent is queued for the lease"
		if slices.ContainsFunc(leases, func(l memreport.Lease) bool { return l.Role == memreport.RoleHolder }) {
			leaseNote = "agent holds the lease"
		}
	}
	skip := procinfo.Ancestors(procs, h.Self)
	for _, p := range procinfo.BuildTools(ctx, procs, procinfo.DefaultBuildTools, procinfo.DefaultBuildToolIgnore, skip, proc.Cwd, h.Cwd) {
		switch {
		case own[p.PID]:
			f.reasons = append(f.reasons, fmt.Sprintf("own build tool pid %d %s in %s", p.PID, procinfo.CommandLine(p), p.Cwd))
		case leaseNote != "":
			f.reasons = append(f.reasons, fmt.Sprintf("checkout build pid %d %s in %s (%s)", p.PID, procinfo.CommandLine(p), p.Cwd, leaseNote))
		}
	}
}

// agentProcess is the leader of the pane's foreground process group,
// else its first foreground process.
func agentProcess(info herdr.ProcessInfo) (herdr.ForegroundProcess, bool) {
	for _, p := range info.ForegroundProcesses {
		if p.PID == info.ForegroundProcessGroupID {
			return p, true
		}
	}
	if len(info.ForegroundProcesses) > 0 {
		return info.ForegroundProcesses[0], true
	}
	return herdr.ForegroundProcess{}, false
}

// checkScreen reads the pane's screen for a dialog and for unsent
// composer text. A kind whose composer or exit cannot be read is
// refused.
func (h *Hibernator) checkScreen(ctx context.Context, a adapter.Agent, f *facts) {
	if _, _, ok := herdr.ExitInput(a.Kind); !ok {
		f.reasons = append(f.reasons, fmt.Sprintf("no clean exit input for a %s agent", a.Kind))
	}
	screen, err := h.Panes.Screen(ctx, a.PaneID)
	if err != nil {
		f.reasons = append(f.reasons, "cannot read the screen: "+err.Error())
		return
	}
	if dialog, ok := herdr.PendingDialog(a.Kind, screen); ok {
		f.reasons = append(f.reasons, "pending dialog: "+dialog)
		return
	}
	empty, ok := herdr.ComposerOf(a.Kind, screen)
	switch {
	case !ok:
		f.reasons = append(f.reasons, fmt.Sprintf("cannot read the composer of a %s agent", a.Kind))
	case !empty:
		f.reasons = append(f.reasons, "unsent text in the composer")
	}
}

// exempt is the first hibernate_exempt pattern that matches c's names
// or handle.
func (h *Hibernator) exempt(c candidate) (string, bool) {
	keys := append([]string{c.handle, c.agent.Handle}, c.names...)
	for _, pattern := range h.Exempt {
		re := procinfo.Glob(pattern, true)
		if slices.ContainsFunc(keys, re.MatchString) {
			return pattern, true
		}
	}
	return "", false
}

// stop writes the record, sends the exit input, and waits for the agent
// process to leave the pane's foreground. It never kills the agent.
func (h *Hibernator) stop(ctx context.Context, c candidate, f facts, r Result) Result {
	a := c.agent
	rec := Record{
		Handle: c.handle, Pane: a.PaneID, Kind: a.Kind, Session: a.SessionID,
		Cwd: f.proc.Cwd, Argv: f.proc.Argv, Names: c.names, At: h.Clock.Now(), RSSBytes: f.rss,
	}
	if _, err := h.Bus.Begin(ctx, rec); err != nil {
		r.Outcome, r.Reasons = OutcomeRefused, []string{err.Error()}
		return r
	}
	fail := func(reason string) Result {
		// A record that cannot be marked stays stopping, which still
		// keeps the names.
		_ = h.Bus.End(ctx, c.handle, StatusStopFailed, reason)
		r.Outcome, r.Reasons = OutcomeFailed, []string{reason}
		return r
	}
	text, keys, _ := herdr.ExitInput(a.Kind)
	if err := h.Panes.SendText(ctx, a.PaneID, text); err != nil {
		return fail("send the exit input: " + err.Error())
	}
	if err := h.Panes.SendKeys(ctx, a.PaneID, keys...); err != nil {
		return fail("send the exit input: " + err.Error())
	}
	if err := h.waitExit(ctx, a.PaneID, f.proc.PID); err != nil {
		return fail(err.Error())
	}
	if err := h.Bus.End(ctx, c.handle, StatusHibernated, ""); err != nil {
		r.Warning = "the record could not be marked hibernated: " + err.Error()
	}
	if err := h.Panes.RunInPane(ctx, a.PaneID, noteCommand(c.name())); err != nil {
		r.Warning = strings.TrimPrefix(r.Warning+"; ", "; ") + "the note could not be printed: " + err.Error()
	}
	r.Outcome = OutcomeHibernated
	return r
}

// waitExit polls the pane until pid is no longer in its foreground, for
// up to the stop timeout.
func (h *Hibernator) waitExit(ctx context.Context, pane string, pid int) error {
	timeout := h.StopTimeout
	if timeout <= 0 {
		timeout = DefaultStopTimeout
	}
	deadline := h.Clock.Now().Add(timeout)
	for {
		info, err := h.Panes.ProcessInfo(ctx, pane)
		if err == nil && !slices.Contains(info.ForegroundPIDs(), pid) {
			return nil
		}
		if !h.Clock.Now().Before(deadline) {
			return fmt.Errorf("agent pid %d did not exit within %s; record kept as %s, agent not killed", pid, timeout, StatusStopFailed)
		}
		if err := h.Clock.Sleep(ctx, defaultPoll); err != nil {
			return fmt.Errorf("wait for the agent to exit: %w", err)
		}
	}
}

// noteCommand prints the hibernation note in the pane's shell.
func noteCommand(name string) string {
	note := fmt.Sprintf("agentbus: hibernated %s; wakes on the next message or 'agentbus wake %s'", name, name)
	return `printf '%s\n' ` + shellQuote(note)
}

func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// LeaseReasons names each lease in table that handle holds or is queued
// for. The daemon checks them again when a record is begun.
func LeaseReasons(table []lease.Status, handle string) []string {
	var out []string
	for _, l := range memreport.LeasesOf(table, handle) {
		if l.Role == memreport.RoleHolder {
			out = append(out, "holds the lease on "+l.Checkout)
		} else {
			out = append(out, "queued for the lease on "+l.Checkout)
		}
	}
	return out
}

// RealClock is the wall clock.
type RealClock struct{}

// Now is time.Now.
func (RealClock) Now() time.Time { return time.Now() }

// Sleep waits d or until ctx ends.
func (RealClock) Sleep(ctx context.Context, d time.Duration) error {
	t := time.NewTimer(d)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return fmt.Errorf("sleep: %w", ctx.Err())
	case <-t.C:
		return nil
	}
}
