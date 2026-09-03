package dispatch

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
)

const settleDebouncePolls = 3

var errSettleTimeout = errors.New("settle timeout")

type settleWatch struct {
	armed      bool
	sawBlocked bool
	held       int
	last       herdr.Agent
}

// waitForSettle polls until the pane leaves its pre-send baseline and the
// requested status holds for settleDebouncePolls consecutive samples.
//
// It does not honor idle/done until the agent is observed working, or
// state_change_seq/revision advances without a blocked sample (a fast job
// whose working window was missed). Startup flap (idle↔blocked) is not a
// settle.
func waitForSettle(ctx context.Context, h Herdr, paneID string, baseline herdr.Agent, until []string, timeout, poll time.Duration, clock Clock, sleeper Sleeper) (herdr.Agent, error) {
	start := clock.Now()
	var watch settleWatch
	for {
		if err := ctx.Err(); err != nil {
			return watch.last, fmt.Errorf("settle: %w", err)
		}
		agents, err := h.AgentList(ctx)
		if err != nil {
			return watch.last, fmt.Errorf("settle agent list: %w", err)
		}
		if cur, ok := findAgent(agents, paneID); ok {
			if watch.observe(cur, baseline, until) {
				return cur, nil
			}
		}
		if clock.Now().Sub(start) >= timeout {
			return watch.last, errSettleTimeout
		}
		if err := sleeper.Sleep(ctx, poll); err != nil {
			return watch.last, fmt.Errorf("settle sleep: %w", err)
		}
	}
}

func (w *settleWatch) observe(cur, baseline herdr.Agent, until []string) bool {
	w.last = cur
	if cur.AgentStatus == "blocked" {
		w.sawBlocked = true
	}
	if cur.AgentStatus == "working" {
		w.armed = true
	}
	if !w.armed && agentProgressed(cur, baseline) && matchesUntil(cur.AgentStatus, until) && !w.sawBlocked {
		w.armed = true
	}
	if w.armed && matchesUntil(cur.AgentStatus, until) {
		w.held++
		return w.held >= settleDebouncePolls
	}
	w.held = 0
	return false
}

func findAgent(agents []herdr.Agent, paneID string) (herdr.Agent, bool) {
	for _, a := range agents {
		if a.PaneID == paneID {
			return a, true
		}
	}
	return herdr.Agent{}, false
}

func agentProgressed(cur, baseline herdr.Agent) bool {
	if cur.StateChangeSeq > 0 || baseline.StateChangeSeq > 0 {
		return cur.StateChangeSeq > baseline.StateChangeSeq
	}
	return cur.Revision > baseline.Revision
}

func matchesUntil(status string, until []string) bool {
	for _, tok := range until {
		if status == tok {
			return true
		}
	}
	return false
}
