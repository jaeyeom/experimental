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
}

// Busy is one agent that holds the gate.
type Busy struct {
	PaneID string `json:"pane_id"`      //nolint:tagliatelle // brief outbound contract
	TabID  string `json:"tab_id"`       //nolint:tagliatelle // brief outbound contract
	Status string `json:"agent_status"` //nolint:tagliatelle // brief outbound contract
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
	return snapshot(ctx, h, gatePolicyFrom(cfg, runnerPane, matchedTabs))
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
			args = append(args, "tab_id", res.Busy[0].TabID, "agent_status", res.Busy[0].Status)
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
	for {
		res, err = snapshot(ctx, h, gatePolicyFrom(cfg, runnerPane, matchedTabs))
		if err != nil {
			return res, err
		}
		if res.Reason != "" {
			return res, ErrDirtyTree
		}
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
				log.Info("gate_wait", "tab_id", b.TabID, "agent_status", b.Status)
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
	waitOn        string
	runnerPane    string
	matched       map[string]struct{}
	ignoreBlocked bool
	cwdMode       string
	checkout      string
}

func gatePolicyFrom(cfg config.Config, runnerPane string, matched map[string]struct{}) gatePolicy {
	mode := cfg.ConcurrencyCwd
	if mode == "" {
		mode = "any"
	}
	return gatePolicy{
		waitOn:        cfg.ConcurrencyWaitOn,
		runnerPane:    runnerPane,
		matched:       matched,
		ignoreBlocked: cfg.ConcurrencyIgnoreBlocked,
		cwdMode:       mode,
		checkout:      cfg.Checkout,
	}
}

func snapshot(ctx context.Context, h Herdr, policy gatePolicy) (Result, error) {
	agents, err := h.AgentList(ctx)
	if err != nil {
		return Result{}, fmt.Errorf("agent list: %w", err)
	}
	busy := busySet(agents, policy)
	res := Result{Safe: len(busy) == 0, Busy: busy}
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

func busySet(agents []herdr.Agent, policy gatePolicy) []Busy {
	out := make([]Busy, 0)
	for _, agent := range agents {
		if !isBusy(agent, policy) {
			continue
		}
		out = append(out, Busy{PaneID: agent.PaneID, TabID: agent.TabID, Status: agent.AgentStatus})
	}
	return out
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
	name := b.TabID
	if name == "" {
		name = b.PaneID
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

func isBusy(agent herdr.Agent, policy gatePolicy) bool {
	if !holdsGate(agent.AgentStatus, policy.ignoreBlocked) {
		return false
	}
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
