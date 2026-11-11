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

// DecideItem is the part of a `decide list --json` item the gate reads.
type DecideItem struct {
	ID         string `json:"id"`
	State      string `json:"state"`
	NotifyPane string `json:"notifyPane"`
}

// DecideSource lists open decide items.
type DecideSource interface {
	OpenItems(ctx context.Context) ([]DecideItem, error)
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
	// Self is this process. It and its ancestors never hold the gate as
	// a build tool, so `make` running prsync does not block itself.
	Self int
}

type probesCtxKey struct{}

// WithProbes attaches the probes the gate uses for background-job,
// build-tool, and pending-question.
func WithProbes(ctx context.Context, p Probes) context.Context {
	return context.WithValue(ctx, probesCtxKey{}, p)
}

func probesFrom(ctx context.Context) Probes {
	p, _ := ctx.Value(probesCtxKey{}).(Probes)
	return p
}

// Unavailable is a signal the gate could not evaluate.
type Unavailable struct {
	TabID    string `json:"tab_id,omitempty"`   //nolint:tagliatelle // brief outbound contract
	PaneID   string `json:"pane_id,omitempty"`  //nolint:tagliatelle // brief outbound contract
	Checkout string `json:"checkout,omitempty"` //nolint:tagliatelle // brief outbound contract
	Signal   string `json:"signal"`
	Error    string `json:"error"`
}

// gateRun is state kept across the polls of one gate check or wait.
type gateRun struct {
	warnedDecide bool
}

// snapshotProbes caches one process table and decide listing per poll.
type snapshotProbes struct {
	Probes
	run *gateRun

	procs    []Process
	procsErr error
	procsOK  bool

	items    []DecideItem
	itemsErr error
	itemsOK  bool
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
		s.items, s.itemsErr = s.Decide.OpenItems(ctx)
		s.itemsOK = true
		if errors.Is(s.itemsErr, ErrDecideMissing) && !s.run.warnedDecide {
			s.run.warnedDecide = true
			runlog.FromContext(ctx).Warn("gate_decide_missing", "error", s.itemsErr.Error())
			writeStatus(ctx, "decide is not installed; pending-question uses herdr blocked status only")
		}
	}
	return s.items, s.itemsErr
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
		detail, err := s.backgroundJob(ctx, agent.PaneID, policy.ignoreProcs)
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
			if id := openItemFor(items, agent.PaneID); id != "" {
				return SignalPendingQuestion, "open item " + id, unavailable
			}
		}
	}
	return "", "", unavailable
}

// backgroundJob describes the first live descendant of the pane's
// foreground agent that is not an ignored helper, or is empty.
func (s *snapshotProbes) backgroundJob(ctx context.Context, paneID string, ignore []string) (string, error) {
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
	children := childrenOf(procs)
	queue := []int{root}
	for len(queue) > 0 {
		pid := queue[0]
		queue = queue[1:]
		for _, child := range children[pid] {
			if matchAny(ignore, child) {
				continue
			}
			return "pid " + strconv.Itoa(child.PID) + " " + commandLine(child), nil
		}
	}
	return "", nil
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
		if !matchAnyName(policy.buildTools, p) || matchAny(policy.buildToolIgnore, p) {
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

func openItemFor(items []DecideItem, paneID string) string {
	for _, it := range items {
		if (it.State == "" || it.State == "open") && it.NotifyPane != "" && it.NotifyPane == paneID {
			return it.ID
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

// matchAnyName reports whether a pattern matches p's name or argv[0] base.
func matchAnyName(patterns []string, p Process) bool {
	for _, pattern := range patterns {
		re := globRegexp(pattern)
		if re.MatchString(p.Name) || re.MatchString(argv0Base(p)) {
			return true
		}
	}
	return false
}

func matchAny(patterns []string, p Process) bool {
	for _, pattern := range patterns {
		if matchProc(pattern, p) {
			return true
		}
	}
	return false
}

// matchProc reports whether glob pattern matches p's name, argv[0]
// base, or whole command line. In these globs * also matches /.
func matchProc(pattern string, p Process) bool {
	re := globRegexp(pattern)
	return re.MatchString(p.Name) || re.MatchString(argv0Base(p)) || re.MatchString(strings.Join(p.Argv, " "))
}

func globRegexp(pattern string) *regexp.Regexp {
	var b strings.Builder
	b.WriteString("^")
	for _, r := range pattern {
		switch r {
		case '*':
			b.WriteString(".*")
		case '?':
			b.WriteString(".")
		default:
			b.WriteString(regexp.QuoteMeta(string(r)))
		}
	}
	b.WriteString("$")
	return regexp.MustCompile(b.String())
}

// holdLine is the log line for one hold, e.g.
// gate: busy tab w3:t2 (pane w3:p2): background-job pid 4242 sh -c "bazel build ...".
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
