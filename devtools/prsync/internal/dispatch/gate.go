// Package dispatch implements the prsync concurrency gate.
package dispatch

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

const herdrMinVersion = "0.8.0"

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

// Herdr is the herdr surface the gate uses.
type Herdr interface {
	RequireMin(ctx context.Context, minimum string) error
	AgentList(ctx context.Context) ([]herdr.Agent, error)
}

// Result is the outbound gate JSON document.
type Result struct {
	Safe bool   `json:"safe"`
	Busy []Busy `json:"busy"`
}

// Busy is one working agent in the busy set.
type Busy struct {
	PaneID string `json:"pane_id"` //nolint:tagliatelle // brief outbound contract
	TabID  string `json:"tab_id"`  //nolint:tagliatelle // brief outbound contract
}

// Check is a one-shot busy-set evaluation. It does not sleep.
func Check(ctx context.Context, h Herdr, waitOn, runnerPane string, matchedTabs map[string]struct{}) (Result, error) {
	if err := h.RequireMin(ctx, herdrMinVersion); err != nil {
		return Result{}, fmt.Errorf("herdr version: %w", err)
	}
	return snapshot(ctx, h, waitOn, runnerPane, matchedTabs)
}

// Wait polls until the busy set is empty or cfg.GateTimeout elapses.
func Wait(ctx context.Context, h Herdr, cfg config.Config, runnerPane string, matchedTabs map[string]struct{}, clock Clock, sleeper Sleeper) (Result, error) {
	if err := h.RequireMin(ctx, herdrMinVersion); err != nil {
		return Result{}, fmt.Errorf("herdr version: %w", err)
	}
	start := clock.Now()
	for {
		res, err := snapshot(ctx, h, cfg.ConcurrencyWaitOn, runnerPane, matchedTabs)
		if err != nil {
			return res, err
		}
		if res.Safe {
			return res, nil
		}
		if clock.Now().Sub(start) >= cfg.GateTimeout {
			return res, ErrTimeout
		}
		if err := sleeper.Sleep(ctx, cfg.GatePoll); err != nil {
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
		out = append(out, Busy{PaneID: agent.PaneID, TabID: agent.TabID})
	}
	return out
}

func isBusy(agent herdr.Agent, waitOn, runnerPane string, matchedTabs map[string]struct{}) bool {
	if agent.AgentStatus != "working" {
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
