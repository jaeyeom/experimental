// Package dispatch implements eligibility, dry-run and live dispatch, and the concurrency gate.
package dispatch

import (
	"context"
	"errors"
	"fmt"
	"io"
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
type Result struct {
	Safe bool   `json:"safe"`
	Busy []Busy `json:"busy"`
}

// Busy is one working or blocked agent in the busy set.
type Busy struct {
	PaneID string `json:"pane_id"`      //nolint:tagliatelle // brief outbound contract
	TabID  string `json:"tab_id"`       //nolint:tagliatelle // brief outbound contract
	Status string `json:"agent_status"` //nolint:tagliatelle // brief outbound contract
}

// Check is a one-shot busy-set evaluation. It does not sleep.
func Check(ctx context.Context, h Herdr, waitOn, runnerPane string, matchedTabs map[string]struct{}) (Result, error) {
	if err := h.RequireMin(ctx, herdrMinVersion); err != nil {
		return Result{}, fmt.Errorf("herdr version: %w", err)
	}
	return snapshot(ctx, h, waitOn, runnerPane, matchedTabs)
}

// Wait polls until the busy set stays empty for settleDebouncePolls
// consecutive samples or cfg.GateTimeout elapses. A single idle/done
// sample is not safe: startup and mid-run flap still count as busy.
// Working and blocked tabs are both waited on: blocked means the
// agent is awaiting human input, not that the next dispatch should
// skip. Progress is logged (and written to WithStatusWriter) when
// the waited-on tab or status changes.
func Wait(ctx context.Context, h Herdr, cfg config.Config, runnerPane string, matchedTabs map[string]struct{}, clock Clock, sleeper Sleeper) (res Result, err error) {
	log := runlog.FromContext(ctx)
	log.Info("gate_wait_start", "wait_on", cfg.ConcurrencyWaitOn, "runner_pane", runnerPane)
	defer func() {
		args := []any{"safe", res.Safe, "busy_count", len(res.Busy)}
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
		res, err = snapshot(ctx, h, cfg.ConcurrencyWaitOn, runnerPane, matchedTabs)
		if err != nil {
			return res, err
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

// MatchedTabs collects tab IDs from a scan document, including tabs with a nil pane_id.
func MatchedTabs(doc scan.Document) map[string]struct{} {
	out := make(map[string]struct{})
	for _, pr := range doc.PRs {
		if pr.Tab != nil && pr.Tab.TabID != "" {
			out[pr.Tab.TabID] = struct{}{}
		}
	}
	return out
}

func snapshot(ctx context.Context, h Herdr, waitOn, runnerPane string, matchedTabs map[string]struct{}) (Result, error) {
	agents, err := h.AgentList(ctx)
	if err != nil {
		return Result{}, fmt.Errorf("agent list: %w", err)
	}
	busy := busySet(agents, waitOn, runnerPane, matchedTabs)
	return Result{Safe: len(busy) == 0, Busy: busy}, nil
}

func busySet(agents []herdr.Agent, waitOn, runnerPane string, matchedTabs map[string]struct{}) []Busy {
	out := make([]Busy, 0)
	for _, agent := range agents {
		if !isBusy(agent, waitOn, runnerPane, matchedTabs) {
			continue
		}
		out = append(out, Busy{PaneID: agent.PaneID, TabID: agent.TabID, Status: agent.AgentStatus})
	}
	return out
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

func isBusy(agent herdr.Agent, waitOn, runnerPane string, matchedTabs map[string]struct{}) bool {
	if !holdsGate(agent.AgentStatus) {
		return false
	}
	if runnerPane != "" && agent.PaneID == runnerPane {
		return false
	}
	if waitOn == "managed" {
		_, ok := matchedTabs[agent.TabID]
		return ok
	}
	return true
}

func holdsGate(status string) bool {
	return status == "working" || status == "blocked"
}
