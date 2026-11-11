package dispatch

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
)

// Gate signals: why a tab or checkout holds the gate.
const (
	SignalWorking         = "working"
	SignalBlocked         = "blocked"
	SignalBackgroundJob   = "background-job"
	SignalBuildTool       = "build-tool"
	SignalPendingQuestion = "pending-question"
)

// signalExemptTabs is the Unavailable signal when tab labels for
// gate_exempt_tabs could not be read.
const signalExemptTabs = "exempt-tabs"

// ErrDecideMissing is returned by a DecideSource when decide is not installed.
var ErrDecideMissing = errors.New("decide not installed")

// Process is one row of the machine's process table.
type Process struct {
	PID  int
	PPID int
	// Name is the kernel's process name (comm), or the base of argv[0]
	// when the platform does not report one separately.
	Name string
	Argv []string
	// Age is how long the process had run when it was listed. It is
	// negative when unknown; an unknown age counts as old.
	Age time.Duration
}

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
	// Self is this process. It and its ancestors never hold the gate as
	// a build tool, so `make` running prsync does not block itself.
	Self int
}

type probesCtxKey struct{}

// WithProbes attaches the probes the gate uses for background-job,
// build-tool, pending-question, and tab exemptions.
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
	TabID    string `json:"tab_id,omitempty"`   //nolint:tagliatelle // brief outbound contract
	PaneID   string `json:"pane_id,omitempty"`  //nolint:tagliatelle // brief outbound contract
	Checkout string `json:"checkout,omitempty"` //nolint:tagliatelle // brief outbound contract
	Signal   string `json:"signal"`
	Error    string `json:"error"`
}

// Ignored is a descendant of an idle agent skipped by a
// gate_ignore_procs pattern, with the pattern that matched.
type Ignored struct {
	Pane    string `json:"pane"`
	PID     int    `json:"pid"`
	Command string `json:"command"`
	Pattern string `json:"pattern"`
}

// Exempt is a waited-on tab the gate skipped, with the config rule
// that exempts it.
type Exempt struct {
	PaneID string `json:"pane_id"`         //nolint:tagliatelle // brief outbound contract
	TabID  string `json:"tab_id"`          //nolint:tagliatelle // brief outbound contract
	Label  string `json:"label,omitempty"` //nolint:tagliatelle // brief outbound contract
	Rule   string `json:"rule"`
}

// gateRun is state kept across the polls of one gate check or wait.
type gateRun struct {
	warnedDecide bool
	// waitLoop is set for Wait: a background job then also needs to
	// be seen in the previous sample.
	waitLoop bool
	// roundStart is when the dispatch round started.
	roundStart time.Time
	// prevJobs are the pids that were background-job candidates in the
	// previous sample.
	prevJobs map[int]struct{}
	// loggedIgnored keeps one gate_ignored debug line per process.
	loggedIgnored map[string]struct{}
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

	tabs    []herdr.Tab
	tabsErr error
	tabsOK  bool

	// jobs are this sample's background-job candidates.
	jobs    map[int]struct{}
	ignored []Ignored
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

func (s *snapshotProbes) tabList(ctx context.Context) ([]herdr.Tab, error) {
	if !s.tabsOK {
		s.tabs, s.tabsErr = s.Tabs.TabList(ctx)
		s.tabsOK = true
	}
	return s.tabs, s.tabsErr
}

// exempt reports whether agent's pane or tab is exempt from the gate.
func (s *snapshotProbes) exempt(ctx context.Context, agent herdr.Agent, policy gatePolicy) (Exempt, bool, []Unavailable) {
	out := Exempt{PaneID: agent.PaneID, TabID: agent.TabID}
	if pattern, ok := matchGlob(policy.exemptPanes, agent.PaneID); ok {
		out.Rule = "gate_exempt_panes=" + pattern
		return out, true, nil
	}
	if len(policy.exemptTabs) == 0 || s.Tabs == nil {
		return Exempt{}, false, nil
	}
	tabs, err := s.tabList(ctx)
	if err != nil {
		return Exempt{}, false, []Unavailable{{TabID: agent.TabID, PaneID: agent.PaneID, Signal: signalExemptTabs, Error: err.Error()}}
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
// evaluate are returned as unavailable.
func (s *snapshotProbes) tabSignal(ctx context.Context, agent herdr.Agent, policy gatePolicy) (string, string, []Unavailable) {
	var unavailable []Unavailable
	miss := func(signal string, err error) {
		unavailable = append(unavailable, Unavailable{TabID: agent.TabID, PaneID: agent.PaneID, Signal: signal, Error: err.Error()})
	}
	if s.Panes != nil && s.Procs != nil {
		detail, err := s.backgroundJob(ctx, agent.PaneID, policy)
		if err != nil {
			miss(SignalBackgroundJob, err)
		} else if detail != "" {
			return SignalBackgroundJob, detail, unavailable
		}
	}
	if s.Decide != nil && policy.pendingQuestion != "off" {
		items, err := s.decideItems(ctx)
		switch {
		case errors.Is(err, ErrDecideMissing):
		case err != nil:
			miss(SignalPendingQuestion, err)
		default:
			if detail := pendingItemFor(items, agent.PaneID, s.run.roundStart); detail != "" {
				return SignalPendingQuestion, detail, unavailable
			}
		}
	}
	return "", "", unavailable
}

// backgroundJob describes the first child of the pane's foreground
// agent that is a background job, or is empty. A child is a job when
// it matches none of the ignore patterns, is at least the minimum age,
// and, inside a wait loop, was also a candidate in the previous sample.
// An ignored child is recorded with its pattern; its own children are
// never looked at.
func (s *snapshotProbes) backgroundJob(ctx context.Context, paneID string, policy gatePolicy) (string, error) {
	info, err := s.Panes.PaneProcessInfo(ctx, paneID)
	if err != nil {
		return "", fmt.Errorf("pane process-info: %w", err)
	}
	root := info.ForegroundProcessGroupID
	if root <= 0 && len(info.ForegroundProcesses) > 0 {
		root = info.ForegroundProcesses[0].PID
	}
	if root <= 0 {
		return "", nil
	}
	procs, err := s.processes(ctx)
	if err != nil {
		return "", fmt.Errorf("process table: %w", err)
	}
	detail := ""
	for _, child := range childrenOf(procs)[root] {
		if pattern, ok := matchingPattern(policy.ignoreProcs, child); ok {
			s.noteIgnored(ctx, Ignored{Pane: paneID, PID: child.PID, Command: commandLine(child), Pattern: pattern})
			continue
		}
		if child.Age >= 0 && child.Age < policy.jobMinAge {
			continue
		}
		s.jobs[child.PID] = struct{}{}
		if detail != "" {
			continue
		}
		if _, seen := s.run.prevJobs[child.PID]; s.run.waitLoop && !seen {
			continue
		}
		detail = "pid " + strconv.Itoa(child.PID) + " " + ageTag(child.Age) + commandLine(child)
	}
	return detail, nil
}

func (s *snapshotProbes) noteIgnored(ctx context.Context, ig Ignored) {
	s.ignored = append(s.ignored, ig)
	key := ig.Pane + "\x00" + strconv.Itoa(ig.PID) + "\x00" + ig.Pattern
	if _, ok := s.run.loggedIgnored[key]; ok {
		return
	}
	if s.run.loggedIgnored == nil {
		s.run.loggedIgnored = make(map[string]struct{})
	}
	s.run.loggedIgnored[key] = struct{}{}
	runlog.FromContext(ctx).Debug("gate_ignored", "pane", ig.Pane, "pid", ig.PID, "command", ig.Command, "pattern", ig.Pattern)
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
	for _, p := range sortedByPID(procs) {
		if _, ok := skip[p.PID]; ok {
			continue
		}
		if !matchAnyName(policy.buildTools, p) {
			continue
		}
		if _, ignored := matchingPattern(policy.buildToolIgnore, p); ignored {
			continue
		}
		cwd, err := s.Procs.Cwd(ctx, p.PID)
		if err != nil || !insideCheckout(cwd, policy.checkout) {
			continue
		}
		return fmt.Sprintf("pid %d %s (cwd %s)", p.PID, commandLine(p), cwd), nil
	}
	return "", nil
}

// pendingItemFor describes the first decide item for paneID that holds
// its tab: one answered but not yet done (the agent is about to
// resume), or one opened after roundStart. An open item from before the
// round is parked and does not hold.
func pendingItemFor(items []DecideItem, paneID string, roundStart time.Time) string {
	if paneID == "" {
		return ""
	}
	for _, it := range items {
		if it.NotifyPane != paneID {
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

// childrenOf maps each pid to its children, lowest pid first.
func childrenOf(procs []Process) map[int][]Process {
	out := make(map[int][]Process)
	for _, p := range sortedByPID(procs) {
		if p.PID != p.PPID {
			out[p.PPID] = append(out[p.PPID], p)
		}
	}
	return out
}

func sortedByPID(procs []Process) []Process {
	out := slices.Clone(procs)
	slices.SortFunc(out, func(a, b Process) int { return a.PID - b.PID })
	return out
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

// maxCommandLine caps a command line in a hold's detail.
const maxCommandLine = 160

// commandLine is argv joined by spaces, quoting arguments with
// whitespace, and cut to maxCommandLine runes.
func commandLine(p Process) string {
	if len(p.Argv) == 0 {
		return p.Name
	}
	parts := make([]string, len(p.Argv))
	for i, arg := range p.Argv {
		if arg == "" || strings.ContainsAny(arg, " \t\n") {
			arg = strconv.Quote(arg)
		}
		parts[i] = arg
	}
	line := strings.Join(parts, " ")
	if r := []rune(line); len(r) > maxCommandLine {
		line = string(r[:maxCommandLine]) + "..."
	}
	return line
}

func argv0Base(p Process) string {
	if len(p.Argv) == 0 {
		return ""
	}
	return filepath.Base(p.Argv[0])
}

// matchAnyName reports whether a pattern matches p's whole name or
// argv[0] base, exactly or as a glob.
func matchAnyName(patterns []string, p Process) bool {
	for _, pattern := range patterns {
		re := globRegexp(pattern, true)
		if re.MatchString(p.Name) || re.MatchString(argv0Base(p)) {
			return true
		}
	}
	return false
}

// matchingPattern is the first pattern that matches p (see matchProc).
func matchingPattern(patterns []string, p Process) (string, bool) {
	for _, pattern := range patterns {
		if matchProc(pattern, p) {
			return pattern, true
		}
	}
	return "", false
}

// matchProc reports whether pattern appears anywhere in p's name,
// argv[0] base, or whole command line (argv joined by spaces). A
// pattern with *, ?, or [ is a glob, where * also matches /.
func matchProc(pattern string, p Process) bool {
	fields := []string{p.Name, argv0Base(p), strings.Join(p.Argv, " ")}
	if !strings.ContainsAny(pattern, "*?[") {
		return slices.ContainsFunc(fields, func(f string) bool { return strings.Contains(f, pattern) })
	}
	re := globRegexp(pattern, false)
	return slices.ContainsFunc(fields, re.MatchString)
}

// matchGlob is the first pattern that matches all of s as a glob.
func matchGlob(patterns []string, s string) (string, bool) {
	for _, pattern := range patterns {
		if globRegexp(pattern, true).MatchString(s) {
			return pattern, true
		}
	}
	return "", false
}

// globRegexp compiles a glob: * is any run (including /), ? one rune,
// and [...] a class ([!...] negated). Anchored matches the whole
// string; otherwise the glob may match anywhere in it.
func globRegexp(pattern string, anchored bool) *regexp.Regexp {
	var b strings.Builder
	if anchored {
		b.WriteString("^")
	}
	runes := []rune(pattern)
	for i := 0; i < len(runes); i++ {
		switch r := runes[i]; r {
		case '*':
			b.WriteString(".*")
		case '?':
			b.WriteString(".")
		case '[':
			if class, n := globClass(runes[i:]); n > 0 {
				b.WriteString(class)
				i += n - 1
				continue
			}
			b.WriteString(regexp.QuoteMeta("["))
		default:
			b.WriteString(regexp.QuoteMeta(string(r)))
		}
	}
	if anchored {
		b.WriteString("$")
	}
	re, err := regexp.Compile(b.String())
	if err != nil {
		return regexp.MustCompile(regexp.QuoteMeta(pattern))
	}
	return re
}

// globClass converts the class at the start of runes ("[...]") to a
// regexp class, and returns how many runes it used, or 0 when the
// class is not closed.
func globClass(runes []rune) (string, int) {
	var b strings.Builder
	b.WriteString("[")
	i := 1
	if i < len(runes) && (runes[i] == '!' || runes[i] == '^') {
		b.WriteString("^")
		i++
	}
	for start := i; i < len(runes); i++ {
		switch r := runes[i]; {
		case r == ']' && i > start:
			b.WriteString("]")
			return b.String(), i + 1
		case r == '\\' && i+1 < len(runes):
			i++
			b.WriteString(regexp.QuoteMeta(string(runes[i])))
		case r == '[' || r == ']' || r == '\\' || r == '^':
			b.WriteString(`\` + string(r))
		default:
			b.WriteRune(r)
		}
	}
	return "", 0
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
	if b.TabID == "" && b.PaneID == "" && b.Checkout != "" {
		return fmt.Sprintf("gate: busy checkout %s: %s", b.Checkout, what)
	}
	return fmt.Sprintf("gate: busy tab %s (pane %s): %s", b.TabID, b.PaneID, what)
}

// extraSignal reports whether b holds for a reason other than its herdr status.
func extraSignal(b Busy) bool {
	switch b.Signal {
	case SignalBackgroundJob, SignalBuildTool, SignalPendingQuestion:
		return true
	}
	return false
}
