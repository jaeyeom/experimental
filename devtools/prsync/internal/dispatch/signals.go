package dispatch

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// Gate signals: why a tab or checkout holds the gate.
const (
	SignalWorking         = "working"
	SignalBlocked         = "blocked"
	SignalBackgroundJob   = "background-job"
	SignalBuildTool       = "build-tool"
	SignalPendingQuestion = "pending-question"
	// SignalMachineUnreachable holds the gate for every tab on a
	// machine whose agent list could not be read.
	SignalMachineUnreachable = "machine-unreachable"
	// SignalUnavailableHold holds a remote tab when one of its signals
	// could not be evaluated: a remote tab never counts as free unseen.
	SignalUnavailableHold = "signal-unavailable"
)

// signalExemptTabs is the Unavailable signal when tab labels for
// gate_exempt_tabs could not be read.
const signalExemptTabs = "exempt-tabs"

// ErrDecideMissing is returned by a DecideSource when decide is not installed.
var ErrDecideMissing = errors.New("decide not installed")

// Process is one row of the machine's process table. Cwd is set when
// the listing reports it (a remote pane's process-info); otherwise it is
// read through ProcessTable.Cwd when needed.
type Process = procinfo.Process

// ProcessTable lists the local processes.
type ProcessTable interface {
	List(ctx context.Context) ([]Process, error)
	// Cwd is pid's working directory.
	Cwd(ctx context.Context, pid int) (string, error)
}

// PaneInspector reads a pane's foreground processes from herdr.
type PaneInspector interface {
	PaneProcessInfo(ctx context.Context, paneID string) (herdr.ProcessInfo, error)
}

// TabLister lists herdr tabs, for their labels.
type TabLister interface {
	TabList(ctx context.Context) ([]herdr.Tab, error)
}

// RemoteHerdr is the herdr surface the gate reads on another machine:
// its agents, and for its tabs the pane process-info and tab labels.
type RemoteHerdr interface {
	AgentList(ctx context.Context) ([]herdr.Agent, error)
	PaneInspector
	TabLister
}

// DecideItem is the part of a `decide list --json` item the gate reads.
type DecideItem struct {
	ID         string    `json:"id"`
	State      string    `json:"state"`
	NotifyPane string    `json:"notifyPane"`
	CreatedAt  time.Time `json:"createdAt"`
	AnsweredAt time.Time `json:"answeredAt"`
}

// DecideSource lists the decide items that are open or answered.
type DecideSource interface {
	Items(ctx context.Context) ([]DecideItem, error)
}

// Probes are what the gate reads beyond `herdr agent list`. A nil field
// turns off the signals that need it; the zero value checks herdr
// status only.
type Probes struct {
	// Panes and Procs feed background-job. Procs also feeds build-tool.
	Panes PaneInspector
	Procs ProcessTable
	// Decide feeds pending-question.
	Decide DecideSource
	// Tabs feeds gate_exempt_tabs.
	Tabs TabLister
	// Lease feeds lease (gate_lease): the agentbus workdir lease of a
	// guarded dispatch checkout.
	Lease LeaseSource
	// Remote returns the herdr client for one of cfg.Machines. The gate
	// reads the agents on each machine in scope through it, and the
	// pane process-info and tab labels of their tabs. Without it, a
	// machine in scope holds the gate.
	Remote func(machine string) RemoteHerdr
	// Self is this process. It and its ancestors never hold the gate as
	// a build tool, so `make` running prsync does not block itself.
	Self int
}

type probesCtxKey struct{}

// WithProbes attaches the probes the gate uses for background-job,
// build-tool, pending-question, lease, and tab exemptions.
func WithProbes(ctx context.Context, p Probes) context.Context {
	return context.WithValue(ctx, probesCtxKey{}, p)
}

func probesFrom(ctx context.Context) Probes {
	p, _ := ctx.Value(probesCtxKey{}).(Probes)
	return p
}

type roundStartCtxKey struct{}

// WithRoundStart sets when the current dispatch round started. A decide
// item opened after it is a fresh question that holds its tab. Without
// it, the round starts when the gate check or wait starts.
func WithRoundStart(ctx context.Context, start time.Time) context.Context {
	return context.WithValue(ctx, roundStartCtxKey{}, start)
}

func roundStartFrom(ctx context.Context, fallback time.Time) time.Time {
	if start, ok := ctx.Value(roundStartCtxKey{}).(time.Time); ok {
		return start
	}
	return fallback
}

// Unavailable is a signal the gate could not evaluate.
type Unavailable struct {
	Machine  string `json:"machine,omitempty"`
	TabID    string `json:"tab_id,omitempty"`   //nolint:tagliatelle // brief outbound contract
	PaneID   string `json:"pane_id,omitempty"`  //nolint:tagliatelle // brief outbound contract
	Checkout string `json:"checkout,omitempty"` //nolint:tagliatelle // brief outbound contract
	Signal   string `json:"signal"`
	Error    string `json:"error"`
}

// Ignored is a descendant of an idle agent skipped by a
// gate_ignore_procs pattern, with the pattern that matched.
type Ignored struct {
	Machine string `json:"machine,omitempty"`
	Pane    string `json:"pane"`
	PID     int    `json:"pid"`
	Command string `json:"command"`
	Pattern string `json:"pattern"`
}

// Exempt is a waited-on tab the gate skipped, with the config rule
// that exempts it.
type Exempt struct {
	Machine string `json:"machine,omitempty"`
	PaneID  string `json:"pane_id"`         //nolint:tagliatelle // brief outbound contract
	TabID   string `json:"tab_id"`          //nolint:tagliatelle // brief outbound contract
	Label   string `json:"label,omitempty"` //nolint:tagliatelle // brief outbound contract
	Rule    string `json:"rule"`
}

// gateRun is state kept across the polls of one gate check or wait.
type gateRun struct {
	warnedDecide bool
	// waitLoop is set for Wait: a background job then also needs to
	// be seen in the previous sample.
	waitLoop bool
	// roundStart is when the dispatch round started.
	roundStart time.Time
	// prevJobs are the jobKeys of the background-job candidates in the
	// previous sample.
	prevJobs map[string]struct{}
	// warnedMachines is the last unreachable cause logged per machine.
	warnedMachines map[string]string
	// loggedIgnored keeps one gate_ignored debug line per process.
	loggedIgnored map[string]struct{}
	// loggedOutside keeps one gate_signal_out_of_checkout debug line
	// per skipped signal.
	loggedOutside map[string]struct{}
	// leaseGuarded is whether the dispatch checkout is guarded, once
	// leaseGuardedKnown.
	leaseGuarded      bool
	leaseGuardedKnown bool
	// leaseWarned is the last lease error logged.
	leaseWarned string
}

// snapshotProbes caches one process table, decide listing, and tab
// listing per poll.
type snapshotProbes struct {
	Probes
	run *gateRun

	procs    []Process
	procsErr error
	procsOK  bool

	items    []DecideItem
	itemsErr error
	itemsOK  bool

	// tabs caches each machine's tab listing ("" is local).
	tabs map[string]tabListing
	// remotes are the clients of the machines read this sample.
	remotes map[string]RemoteHerdr

	// jobs are this sample's background-job candidates, by jobKey.
	jobs    map[string]struct{}
	ignored []Ignored
}

type tabListing struct {
	tabs []herdr.Tab
	err  error
}

// jobKey keeps pids on different machines apart.
func jobKey(machine string, pid int) string {
	return machine + "\x00" + strconv.Itoa(pid)
}

func (s *snapshotProbes) processes(ctx context.Context) ([]Process, error) {
	if !s.procsOK {
		s.procs, s.procsErr = s.Procs.List(ctx)
		s.procsOK = true
	}
	return s.procs, s.procsErr
}

func (s *snapshotProbes) decideItems(ctx context.Context) ([]DecideItem, error) {
	if !s.itemsOK {
		s.items, s.itemsErr = s.Decide.Items(ctx)
		s.itemsOK = true
		if errors.Is(s.itemsErr, ErrDecideMissing) && !s.run.warnedDecide {
			s.run.warnedDecide = true
			runlog.FromContext(ctx).Warn("gate_decide_missing", "error", s.itemsErr.Error())
			writeStatus(ctx, "decide is not installed; pending-question uses herdr blocked status only")
		}
	}
	return s.items, s.itemsErr
}

// tabLister lists machine's tabs, or is nil.
func (s *snapshotProbes) tabLister(machine string) TabLister {
	if machine == "" {
		return s.Tabs
	}
	if r := s.remotes[machine]; r != nil {
		return r
	}
	return nil
}

func (s *snapshotProbes) tabList(ctx context.Context, machine string, lister TabLister) ([]herdr.Tab, error) {
	got, ok := s.tabs[machine]
	if !ok {
		got.tabs, got.err = lister.TabList(ctx)
		if s.tabs == nil {
			s.tabs = make(map[string]tabListing)
		}
		s.tabs[machine] = got
	}
	return got.tabs, got.err
}

// exempt reports whether agent's pane or tab is exempt from the gate.
func (s *snapshotProbes) exempt(ctx context.Context, agent herdr.Agent, policy gatePolicy) (Exempt, bool, []Unavailable) {
	out := Exempt{Machine: agent.Machine, PaneID: agent.PaneID, TabID: agent.TabID}
	if pattern, ok := matchGlob(policy.exemptPanes, agent.PaneID); ok {
		out.Rule = "gate_exempt_panes=" + pattern
		return out, true, nil
	}
	lister := s.tabLister(agent.Machine)
	if len(policy.exemptTabs) == 0 || lister == nil {
		return Exempt{}, false, nil
	}
	tabs, err := s.tabList(ctx, agent.Machine, lister)
	if err != nil {
		return Exempt{}, false, []Unavailable{{Machine: agent.Machine, TabID: agent.TabID, PaneID: agent.PaneID, Signal: signalExemptTabs, Error: err.Error()}}
	}
	for _, tab := range tabs {
		if tab.TabID != agent.TabID || tab.Machine != agent.Machine {
			continue
		}
		if pattern, ok := matchGlob(policy.exemptTabs, tab.Label); ok {
			out.Label = tab.Label
			out.Rule = "gate_exempt_tabs=" + pattern
			return out, true, nil
		}
	}
	return Exempt{}, false, nil
}

// tabSignal is the first extra signal that holds agent's tab: a
// background job, then a pending question. Signals it could not
// evaluate are returned as unavailable. When the dispatch checkout is
// known and the tab is outside it, a pending question does not hold,
// and a background job holds only when its own cwd is inside it.
func (s *snapshotProbes) tabSignal(ctx context.Context, agent herdr.Agent, policy gatePolicy) (string, string, []Unavailable) {
	var unavailable []Unavailable
	miss := func(signal string, err error) {
		unavailable = append(unavailable, Unavailable{Machine: agent.Machine, TabID: agent.TabID, PaneID: agent.PaneID, Signal: signal, Error: err.Error()})
	}
	outside := policy.checkout != "" && !inDispatchCheckout(agent, policy.checkout)
	if s.Panes != nil && (s.Procs != nil || agent.Machine != "") {
		detail, err := s.backgroundJob(ctx, agent, policy, outside)
		if err != nil {
			miss(SignalBackgroundJob, err)
		} else if detail != "" {
			return SignalBackgroundJob, detail, unavailable
		}
	}
	if s.Decide != nil && policy.pendingQuestion != "off" {
		detail, err := s.pendingQuestion(ctx, agent, outside)
		if err != nil {
			miss(SignalPendingQuestion, err)
		} else if detail != "" {
			return SignalPendingQuestion, detail, unavailable
		}
	}
	return "", "", unavailable
}

// pendingQuestion describes the decide item that holds agent's tab, or
// is empty. A tab outside the dispatch checkout is never held, so a
// decide failure is not an error for it.
func (s *snapshotProbes) pendingQuestion(ctx context.Context, agent herdr.Agent, outside bool) (string, error) {
	items, err := s.decideItems(ctx)
	switch {
	case errors.Is(err, ErrDecideMissing):
		return "", nil
	case err != nil && outside:
		return "", nil
	case err != nil:
		return "", err
	}
	detail := pendingItemFor(items, agent, s.run.roundStart)
	if detail != "" && outside {
		s.noteOutside(ctx, agent, SignalPendingQuestion, detail)
		return "", nil
	}
	return detail, nil
}

// backgroundJob describes the first child of the pane's foreground
// agent that is a background job, or is empty. A child is a job when
// it matches none of the ignore patterns, is at least the minimum age,
// and, inside a wait loop, was also a candidate in the previous sample.
// For a tab outside the dispatch checkout, the child's cwd must also be
// inside it. An ignored child is recorded with its pattern; its own
// children are never looked at.
func (s *snapshotProbes) backgroundJob(ctx context.Context, agent herdr.Agent, policy gatePolicy, outside bool) (string, error) {
	children, err := s.agentChildren(ctx, agent)
	if err != nil {
		return "", err
	}
	detail := ""
	for _, child := range children {
		if pattern, ok := procinfo.MatchingPattern(policy.ignoreProcs, child); ok {
			s.noteIgnored(ctx, Ignored{Machine: agent.Machine, Pane: agent.PaneID, PID: child.PID, Command: procinfo.CommandLine(child), Pattern: pattern})
			continue
		}
		if !procinfo.OldEnough(child, policy.jobMinAge) {
			continue
		}
		if outside && !insideCheckout(s.childCwd(ctx, child), policy.checkout) {
			s.noteOutside(ctx, agent, SignalBackgroundJob, "pid "+strconv.Itoa(child.PID)+" "+procinfo.CommandLine(child))
			continue
		}
		key := jobKey(agent.Machine, child.PID)
		s.jobs[key] = struct{}{}
		if detail != "" {
			continue
		}
		if _, seen := s.run.prevJobs[key]; s.run.waitLoop && !seen {
			continue
		}
		detail = "pid " + strconv.Itoa(child.PID) + " " + ageTag(child.Age) + procinfo.CommandLine(child)
	}
	return detail, nil
}

// agentChildren lists the children of the pane's foreground agent. A
// local agent's come from the process table. A remote agent's are the
// other members of its foreground process group from `herdr --machine
// <m> pane process-info`, of unknown age; with no foreground process
// listed there, they cannot be read.
func (s *snapshotProbes) agentChildren(ctx context.Context, agent herdr.Agent) ([]Process, error) {
	panes := s.Panes
	if agent.Machine != "" {
		r := s.remotes[agent.Machine]
		if r == nil {
			return nil, fmt.Errorf("no herdr client for machine %s", agent.Machine)
		}
		panes = r
	}
	info, err := panes.PaneProcessInfo(ctx, agent.PaneID)
	if err != nil {
		return nil, fmt.Errorf("pane process-info: %w", err)
	}
	root := info.ForegroundProcessGroupID
	if root <= 0 && len(info.ForegroundProcesses) > 0 {
		root = info.ForegroundProcesses[0].PID
	}
	if agent.Machine != "" {
		if root <= 0 {
			return nil, fmt.Errorf("pane process-info on %s lists no foreground processes", agent.Machine)
		}
		var out []Process
		for _, fp := range info.ForegroundProcesses {
			if fp.PID != root {
				out = append(out, Process{PID: fp.PID, PPID: root, Name: fp.Name, Argv: fp.Argv, Age: -1, Cwd: fp.Cwd})
			}
		}
		return procinfo.SortedByPID(out), nil
	}
	if root <= 0 {
		return nil, nil
	}
	procs, err := s.processes(ctx)
	if err != nil {
		return nil, fmt.Errorf("process table: %w", err)
	}
	return procinfo.Children(procs)[root], nil
}

// childCwd is child's working directory, or empty when unknown.
func (s *snapshotProbes) childCwd(ctx context.Context, child Process) string {
	if child.Cwd != "" || s.Procs == nil {
		return child.Cwd
	}
	cwd, err := s.Procs.Cwd(ctx, child.PID)
	if err != nil {
		return ""
	}
	return cwd
}

// noteOutside logs, once per run, a signal skipped because agent's tab
// is outside the dispatch checkout.
func (s *snapshotProbes) noteOutside(ctx context.Context, agent herdr.Agent, signal, detail string) {
	key := agent.Machine + "\x00" + agent.PaneID + "\x00" + signal + "\x00" + detail
	if _, ok := s.run.loggedOutside[key]; ok {
		return
	}
	if s.run.loggedOutside == nil {
		s.run.loggedOutside = make(map[string]struct{})
	}
	s.run.loggedOutside[key] = struct{}{}
	runlog.FromContext(ctx).Debug("gate_signal_out_of_checkout", "machine", agent.Machine, "tab_id", agent.TabID, "pane", agent.PaneID, "signal", signal, "detail", detail, "cwd", agent.Cwd, "foreground_cwd", agent.ForegroundCwd)
}

func (s *snapshotProbes) noteIgnored(ctx context.Context, ig Ignored) {
	s.ignored = append(s.ignored, ig)
	key := ig.Machine + "\x00" + ig.Pane + "\x00" + strconv.Itoa(ig.PID) + "\x00" + ig.Pattern
	if _, ok := s.run.loggedIgnored[key]; ok {
		return
	}
	if s.run.loggedIgnored == nil {
		s.run.loggedIgnored = make(map[string]struct{})
	}
	s.run.loggedIgnored[key] = struct{}{}
	runlog.FromContext(ctx).Debug("gate_ignored", "machine", ig.Machine, "pane", ig.Pane, "pid", ig.PID, "command", ig.Command, "pattern", ig.Pattern)
}

// ageTag is "(2m13s) " for a known age, else empty.
func ageTag(age time.Duration) string {
	if age < 0 {
		return ""
	}
	return "(" + age.Round(time.Second).String() + ") "
}

// buildTool describes the first build-tool client running in checkout,
// or is empty.
func (s *snapshotProbes) buildTool(ctx context.Context, policy gatePolicy) (string, error) {
	procs, err := s.processes(ctx)
	if err != nil {
		return "", fmt.Errorf("process table: %w", err)
	}
	skip := ancestorsOf(procs, s.Self)
	for _, p := range procinfo.SortedByPID(procs) {
		if _, ok := skip[p.PID]; ok {
			continue
		}
		if !matchAnyName(policy.buildTools, p) {
			continue
		}
		if _, ignored := procinfo.MatchingPattern(policy.buildToolIgnore, p); ignored {
			continue
		}
		cwd, err := s.Procs.Cwd(ctx, p.PID)
		if err != nil || !insideCheckout(cwd, policy.checkout) {
			continue
		}
		return fmt.Sprintf("pid %d %s (cwd %s)", p.PID, procinfo.CommandLine(p), cwd), nil
	}
	return "", nil
}

// pendingItemFor describes the first decide item for agent's pane that
// holds its tab: one answered but not yet done (the agent is about to
// resume), or one opened after roundStart. An open item from before the
// round is parked and does not hold.
func pendingItemFor(items []DecideItem, agent herdr.Agent, roundStart time.Time) string {
	if agent.PaneID == "" {
		return ""
	}
	for _, it := range items {
		if !notifiesPane(it.NotifyPane, agent) {
			continue
		}
		switch it.State {
		case "answered":
			return "answered item " + it.ID
		case "", "open":
			if it.CreatedAt.After(roundStart) {
				return "open item " + it.ID
			}
		}
	}
	return ""
}

// notifiesPane reports whether a decide notifyPane names agent's pane:
// the bare pane id for a local agent, or the remote handle
// herdr:<machine>/<session>/<pane> for an agent on another machine.
func notifiesPane(notify string, agent herdr.Agent) bool {
	if agent.Machine == "" {
		return notify == agent.PaneID
	}
	rest, ok := strings.CutPrefix(notify, "herdr:"+agent.Machine+"/")
	if !ok {
		return false
	}
	session, pane, ok := strings.Cut(rest, "/")
	return ok && session != "" && pane == agent.PaneID
}

// ancestorsOf is pid and every ancestor of it in procs.
func ancestorsOf(procs []Process, pid int) map[int]struct{} {
	out := make(map[int]struct{})
	if pid <= 0 {
		return out
	}
	parent := make(map[int]int, len(procs))
	for _, p := range procs {
		parent[p.PID] = p.PPID
	}
	for pid > 0 {
		if _, seen := out[pid]; seen {
			break
		}
		out[pid] = struct{}{}
		pid = parent[pid]
	}
	return out
}

// matchAnyName reports whether a pattern matches p's whole name or
// argv[0] base, exactly or as a glob.
func matchAnyName(patterns []string, p Process) bool {
	for _, pattern := range patterns {
		re := procinfo.Glob(pattern, true)
		if re.MatchString(p.Name) || re.MatchString(procinfo.Argv0Base(p)) {
			return true
		}
	}
	return false
}

// matchGlob is the first pattern that matches all of s as a glob.
func matchGlob(patterns []string, s string) (string, bool) {
	for _, pattern := range patterns {
		if procinfo.Glob(pattern, true).MatchString(s) {
			return pattern, true
		}
	}
	return "", false
}

// holdLine is the log line for one hold, e.g.
// gate: busy tab w3:t2 (pane w3:p2): background-job pid 4242 (2m13s) sh -c "bazel build ...".
func holdLine(b Busy) string {
	what := b.Signal
	if what == "" {
		what = b.Status
	}
	if b.Detail != "" {
		what += " " + b.Detail
	}
	if b.TabID == "" && b.PaneID == "" {
		switch {
		case b.Checkout != "":
			return fmt.Sprintf("gate: busy checkout %s: %s", b.Checkout, what)
		case b.Machine != "":
			return fmt.Sprintf("gate: busy machine %s: %s", b.Machine, what)
		}
	}
	return fmt.Sprintf("gate: busy tab %s (pane %s): %s", scan.TabKey(b.Machine, b.TabID), b.PaneID, what)
}

// extraSignal reports whether b holds for a reason other than its herdr status.
func extraSignal(b Busy) bool {
	switch b.Signal {
	case SignalBackgroundJob, SignalBuildTool, SignalPendingQuestion, SignalMachineUnreachable, SignalUnavailableHold, SignalLease:
		return true
	}
	return false
}
