// Package dispatch implements eligibility, dry-run and live dispatch, and the concurrency gate.
package dispatch

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

type statusCtxKey struct{}

// WithStatusWriter attaches w for live gate-wait progress lines on stderr.
func WithStatusWriter(ctx context.Context, w io.Writer) context.Context {
	if w == nil {
		return ctx
	}
	return context.WithValue(ctx, statusCtxKey{}, w)
}

const herdrMinVersion = "0.8.0"

// settleDebouncePolls is how many consecutive empty-busy samples the gate needs.
const settleDebouncePolls = 3

// ErrTimeout is returned when Wait expires with a non-empty busy set.
var ErrTimeout = errors.New("gate timeout")

// ErrDirtyTree is returned when Wait finds the dispatch checkout dirty.
// It does not poll: a dirty tree is unsafe at once.
var ErrDirtyTree = errors.New("gate unsafe")

// Clock is the injected clock for Wait.
type Clock interface {
	Now() time.Time
}

// Sleeper is the injected sleeper for Wait.
type Sleeper interface {
	Sleep(ctx context.Context, d time.Duration) error
}

// Herdr is the herdr surface the gate and live dispatch use.
type Herdr interface {
	RequireMin(ctx context.Context, minimum string) error
	AgentList(ctx context.Context) ([]herdr.Agent, error)
	Prompt(ctx context.Context, paneID, text string, until []string, timeout time.Duration) herdr.PromptOutcome
	Wait(ctx context.Context, paneID string, until []string, timeout time.Duration) herdr.PromptOutcome
}

// Result is the outbound gate JSON document.
// Reason is set when the dispatch checkout is dirty; Safe is then false.
type Result struct {
	Safe   bool   `json:"safe"`
	Busy   []Busy `json:"busy"`
	Reason string `json:"reason,omitempty"`
	// SignalUnavailable lists signals that could not be evaluated. They
	// do not change Safe.
	SignalUnavailable []Unavailable `json:"signal_unavailable,omitempty"` //nolint:tagliatelle // brief outbound contract
}

// Busy is one tab, or the dispatch checkout, that holds the gate.
// A checkout-level hold (build-tool) has Checkout and no tab or pane.
type Busy struct {
	PaneID   string `json:"pane_id"`            //nolint:tagliatelle // brief outbound contract
	TabID    string `json:"tab_id"`             //nolint:tagliatelle // brief outbound contract
	Status   string `json:"agent_status"`       //nolint:tagliatelle // brief outbound contract
	Checkout string `json:"checkout,omitempty"` //nolint:tagliatelle // brief outbound contract
	// Signal is working, blocked, background-job, build-tool, or
	// pending-question. Detail names the process or decide item.
	Signal string `json:"signal,omitempty"`
	Detail string `json:"detail,omitempty"`
}

// Check is a one-shot busy-set evaluation. It does not sleep.
// A blocked agent does not hold the gate. Directories are not consulted.
func Check(ctx context.Context, h Herdr, waitOn, runnerPane string, matchedTabs map[string]struct{}) (Result, error) {
	cfg := config.Defaults()
	cfg.ConcurrencyWaitOn = waitOn
	return GateCheck(ctx, h, cfg, runnerPane, matchedTabs)
}

// GateCheck is a one-shot busy-set evaluation for cfg. It does not sleep.
func GateCheck(ctx context.Context, h Herdr, cfg config.Config, runnerPane string, matchedTabs map[string]struct{}) (Result, error) {
	if err := h.RequireMin(ctx, herdrMinVersion); err != nil {
		return Result{}, fmt.Errorf("herdr version: %w", err)
	}
	return snapshot(ctx, h, gatePolicyFrom(cfg, runnerPane, matchedTabs), &gateRun{})
}

// Wait polls until the busy set stays empty for settleDebouncePolls
// consecutive samples or cfg.GateTimeout elapses. A single idle/done
// sample is not safe: startup and mid-run flap still count as busy.
// By default only a working agent holds the gate. A blocked agent holds
// it when concurrency_ignore_blocked is false. Progress is logged (and
// written to WithStatusWriter) when the waited-on tab or status changes.
func Wait(ctx context.Context, h Herdr, cfg config.Config, runnerPane string, matchedTabs map[string]struct{}, clock Clock, sleeper Sleeper) (res Result, err error) {
	log := runlog.FromContext(ctx)
	log.Info("gate_wait_start", "wait_on", cfg.ConcurrencyWaitOn, "runner_pane", runnerPane)
	defer func() {
		args := []any{"safe", res.Safe, "busy_count", len(res.Busy)}
		if res.Reason != "" {
			args = append(args, "reason", res.Reason)
		}
		if len(res.Busy) > 0 {
			b := res.Busy[0]
			args = append(args, "tab_id", b.TabID, "agent_status", b.Status, "signal", b.Signal)
			if b.Detail != "" {
				args = append(args, "detail", b.Detail)
			}
		}
		if err != nil {
			args = append(args, "error", err.Error())
		}
		log.Info("gate_wait_end", args...)
	}()
	if err = h.RequireMin(ctx, herdrMinVersion); err != nil {
		return Result{}, fmt.Errorf("herdr version: %w", err)
	}
	start := clock.Now()
	held := 0
	lastStatus := ""
	lastHolds := ""
	run := &gateRun{}
	for {
		res, err = snapshot(ctx, h, gatePolicyFrom(cfg, runnerPane, matchedTabs), run)
		if err != nil {
			return res, err
		}
		if res.Reason != "" {
			return res, ErrDirtyTree
		}
		lastHolds = logHolds(ctx, res.Busy, lastHolds)
		if res.Safe {
			held++
			if held >= settleDebouncePolls {
				return res, nil
			}
		} else {
			held = 0
			line := gateWaitLine(res.Busy)
			if line != "" && line != lastStatus {
				lastStatus = line
				b := res.Busy[0]
				log.Info("gate_wait", "tab_id", b.TabID, "agent_status", b.Status, "signal", b.Signal)
				writeStatus(ctx, line)
			}
		}
		if clock.Now().Sub(start) >= cfg.GateTimeout {
			return res, ErrTimeout
		}
		if err = sleeper.Sleep(ctx, cfg.GatePoll); err != nil {
			return res, fmt.Errorf("gate sleep: %w", err)
		}
	}
}

// MatchedTabs collects tab keys (scan.TabKey) from a scan document,
// including tabs with a nil pane_id. A tab on another machine keys as
// <tab id>@<machine>, so it never matches a local tab with the same id.
func MatchedTabs(doc scan.Document) map[string]struct{} {
	out := make(map[string]struct{})
	for _, pr := range doc.PRs {
		if pr.Tab != nil && pr.Tab.TabID != "" {
			out[scan.TabKey(pr.Tab.Machine, pr.Tab.TabID)] = struct{}{}
		}
	}
	return out
}

type gatePolicy struct {
	waitOn          string
	runnerPane      string
	matched         map[string]struct{}
	ignoreBlocked   bool
	cwdMode         string
	checkout        string
	ignoreProcs     []string
	buildTools      []string
	buildToolIgnore []string
	pendingQuestion string
}

func gatePolicyFrom(cfg config.Config, runnerPane string, matched map[string]struct{}) gatePolicy {
	mode := cfg.ConcurrencyCwd
	if mode == "" {
		mode = "any"
	}
	return gatePolicy{
		waitOn:          cfg.ConcurrencyWaitOn,
		runnerPane:      runnerPane,
		matched:         matched,
		ignoreBlocked:   cfg.ConcurrencyIgnoreBlocked,
		cwdMode:         mode,
		checkout:        cfg.Checkout,
		ignoreProcs:     cfg.GateIgnoreProcs,
		buildTools:      cfg.GateBuildTools,
		buildToolIgnore: cfg.GateBuildToolIgnore,
		pendingQuestion: cfg.GatePendingQuestion,
	}
}

func snapshot(ctx context.Context, h Herdr, policy gatePolicy, run *gateRun) (Result, error) {
	agents, err := h.AgentList(ctx)
	if err != nil {
		return Result{}, fmt.Errorf("agent list: %w", err)
	}
	busy, unavailable := busySet(ctx, agents, policy, &snapshotProbes{Probes: probesFrom(ctx), run: run})
	res := Result{Safe: len(busy) == 0, Busy: busy, SignalUnavailable: unavailable}
	if reason := dirtyTreeReason(ctx, policy.checkout); reason != "" {
		res.Safe = false
		res.Reason = reason
	}
	return res, nil
}

// dirtyTreeReason describes why checkout is unsafe to dispatch into: a
// rebase or merge in progress, or staged or unstaged changes to tracked
// paths. Untracked files do not count. It is empty for a clean tree and
// when checkout is empty or not a git work tree.
func dirtyTreeReason(ctx context.Context, checkout string) string {
	if checkout == "" {
		return ""
	}
	gitDir, err := gitOutput(ctx, checkout, "rev-parse", "--absolute-git-dir")
	if err != nil {
		return ""
	}
	gitDir = strings.TrimSpace(gitDir)
	switch {
	case pathExists(filepath.Join(gitDir, "rebase-merge")) || pathExists(filepath.Join(gitDir, "rebase-apply")):
		return "dirty tree: rebase in progress"
	case pathExists(filepath.Join(gitDir, "MERGE_HEAD")):
		return "dirty tree: merge in progress"
	}
	out, err := gitOutput(ctx, checkout, "status", "--porcelain=v1", "-z", "--untracked-files=no", "--no-renames")
	if err != nil {
		return ""
	}
	n := 0
	for _, entry := range strings.Split(out, "\x00") {
		if entry != "" {
			n++
		}
	}
	if n == 0 {
		return ""
	}
	return fmt.Sprintf("dirty tree: %d files", n)
}

func gitOutput(ctx context.Context, dir string, args ...string) (string, error) {
	cmd := exec.CommandContext(ctx, "git", append([]string{"-C", dir}, args...)...) //nolint:gosec // G204: fixed git binary with an argument list, not a shell
	cmd.Env = append(os.Environ(), "GIT_OPTIONAL_LOCKS=0")
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("git %s: %w", strings.Join(args, " "), err)
	}
	return string(out), nil
}

func pathExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// busySet is every waited-on tab that holds the gate, each with the
// first signal that holds it, then the checkout when a build tool runs
// in it.
func busySet(ctx context.Context, agents []herdr.Agent, policy gatePolicy, probes *snapshotProbes) ([]Busy, []Unavailable) {
	out := make([]Busy, 0)
	var unavailable []Unavailable
	for _, agent := range agents {
		if !waitedOn(agent, policy) {
			continue
		}
		b := Busy{PaneID: agent.PaneID, TabID: agent.TabID, Status: agent.AgentStatus}
		if holdsGate(agent.AgentStatus, policy.ignoreBlocked) {
			b.Signal = agent.AgentStatus
			out = append(out, b)
			continue
		}
		signal, detail, miss := probes.tabSignal(ctx, agent, policy)
		unavailable = append(unavailable, miss...)
		if signal != "" {
			b.Signal, b.Detail = signal, detail
			out = append(out, b)
		}
	}
	if probes.Procs != nil && policy.checkout != "" && len(policy.buildTools) > 0 {
		detail, err := probes.buildTool(ctx, policy)
		switch {
		case err != nil:
			unavailable = append(unavailable, Unavailable{Checkout: policy.checkout, Signal: SignalBuildTool, Error: err.Error()})
		case detail != "":
			out = append(out, Busy{Checkout: policy.checkout, Signal: SignalBuildTool, Detail: detail})
		}
	}
	return out, unavailable
}

// logHolds logs one gate_hold line per hold when the holds differ from
// last, and returns the holds for the next comparison.
func logHolds(ctx context.Context, busy []Busy, last string) string {
	lines := make([]string, len(busy))
	for i, b := range busy {
		lines[i] = holdLine(b)
	}
	joined := strings.Join(lines, "\n")
	if joined == last {
		return last
	}
	log := runlog.FromContext(ctx)
	for i, line := range lines {
		log.Info("gate_hold", "line", line, "signal", busy[i].Signal)
	}
	return joined
}

// checkoutOfRepo is the git toplevel of the current directory when its
// origin is repo. An unknown checkout is empty.
func checkoutOfRepo(repo string) string {
	return CheckoutOfRepos([]string{repo})
}

// CheckoutOfRepos is the git toplevel of the current directory when its
// origin is one of repos. An unknown checkout is empty.
func CheckoutOfRepos(repos []string) string {
	top := CheckoutOfCWD()
	if top == "" {
		return ""
	}
	got, err := scan.GitHubRepoFromCWD(top)
	if err != nil || got == "" || !slices.Contains(repos, got) {
		return ""
	}
	return top
}

// CheckoutOfCWD is the git toplevel of the current directory.
func CheckoutOfCWD() string {
	cwd, err := os.Getwd()
	if err != nil {
		return ""
	}
	out, err := exec.Command("git", "-C", cwd, "rev-parse", "--show-toplevel").Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

// insideCheckout reports whether path is checkout or a directory inside it.
func insideCheckout(path, checkout string) bool {
	if path == "" || checkout == "" {
		return false
	}
	path = filepath.Clean(path)
	checkout = filepath.Clean(checkout)
	rel, err := filepath.Rel(checkout, path)
	if err != nil {
		return false
	}
	return rel == "." || !strings.HasPrefix(rel, "..")
}

func gateWaitLine(busy []Busy) string {
	if len(busy) == 0 {
		return ""
	}
	b := busy[0]
	name := holdName(b)
	if extraSignal(b) {
		return fmt.Sprintf("waiting on %s — %s %s", name, b.Signal, b.Detail)
	}
	if b.Status == "blocked" {
		return fmt.Sprintf("waiting on %s — blocked awaiting your input", name)
	}
	if b.Status != "" {
		return fmt.Sprintf("waiting on %s — %s", name, b.Status)
	}
	return fmt.Sprintf("waiting on %s", name)
}

func writeStatus(ctx context.Context, line string) {
	w, _ := ctx.Value(statusCtxKey{}).(io.Writer)
	if w == nil {
		return
	}
	_, _ = fmt.Fprintf(w, "prsync: %s\n", line)
}

// holdName names a hold's tab, pane, or checkout.
func holdName(b Busy) string {
	switch {
	case b.TabID != "":
		return b.TabID
	case b.PaneID != "":
		return b.PaneID
	case b.Checkout != "":
		return "checkout " + b.Checkout
	}
	return ""
}

// waitedOn reports whether the gate waits on agent's tab: not the
// runner pane, inside the checkout in checkout mode, and matched in
// managed mode.
func waitedOn(agent herdr.Agent, policy gatePolicy) bool {
	if policy.runnerPane != "" && agent.PaneID == policy.runnerPane {
		return false
	}
	if policy.cwdMode == "checkout" && !inDispatchCheckout(agent, policy.checkout) {
		return false
	}
	if policy.waitOn == "managed" {
		_, ok := policy.matched[scan.TabKey(agent.Machine, agent.TabID)]
		return ok
	}
	return true
}

func inDispatchCheckout(agent herdr.Agent, checkout string) bool {
	return insideCheckout(agent.Cwd, checkout) || insideCheckout(agent.ForegroundCwd, checkout)
}

// holdsGate reports whether status counts toward the busy set.
// A blocked agent counts only when ignoreBlocked is false.
func holdsGate(status string, ignoreBlocked bool) bool {
	if status == "working" {
		return true
	}
	return status == "blocked" && !ignoreBlocked
}
