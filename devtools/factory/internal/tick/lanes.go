package tick

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const outcomeSkipped = "skipped"

// LaneMachine is the herdr surface on one lane's machine.
type LaneMachine struct {
	Sessions Sessions       // Workspaces, tabs, and panes on the machine.
	Closer   TabCloser      // Closes a settled item's tab there.
	Agent    start.HerdrAPI // Creates the tab and starts the worker there.
	// WaitAgent waits for a working merge-mode worker tab to finish its turn
	// before closing. Nil falls back to Agent.WaitAgent when Agent is set.
	WaitAgent func(ctx context.Context, target string, until []string, timeout time.Duration) (string, error)
	// Why explains an unreachable machine for the skip note. Nil uses the
	// call error.
	Why func(ctx context.Context, err error) string
}

// laneCheck is one lane after its gate check.
type laneCheck struct {
	lane    config.Lane
	machine LaneMachine
	tabs    []herdr.Tab   // The lane machine's tabs. Nil when skipped.
	claims  []store.Claim // Every active claim, for the lane's worker count.
	result  gate.LaneResult
}

// machineFor returns the herdr surface for lane. A local lane, and the
// zero lane of a claim made without lanes, use Sessions, Closer, and Agent.
func (e *Engine) machineFor(lane config.Lane) (LaneMachine, error) {
	if !lane.Remote() {
		return LaneMachine{Sessions: e.Sessions, Closer: e.Closer, Agent: e.Agent, WaitAgent: e.WaitAgent}, nil
	}
	if e.Machine == nil {
		return LaneMachine{}, fmt.Errorf("lane %s: expected a herdr client for machine %s, found nil", lane.Name, lane.Machine)
	}
	m, err := e.Machine(lane.Machine)
	if err != nil {
		return LaneMachine{}, fmt.Errorf("lane %s: %w", lane.Name, err)
	}
	if m.Sessions == nil || m.Closer == nil || m.Agent == nil {
		return LaneMachine{}, fmt.Errorf("lane %s: expected sessions, a tab closer, and an agent client for machine %s, found a nil one", lane.Name, lane.Machine)
	}
	return m, nil
}

// claimLane resolves the lane recorded on a claim. An empty name is the
// local herdr server. A name that is no longer configured is an error.
func (e *Engine) claimLane(name string) (config.Lane, error) {
	if name == "" {
		return config.Lane{}, nil
	}
	lane, ok := e.Config.LaneNamed(name)
	if !ok {
		return config.Lane{}, fmt.Errorf("claim lane %q: expected a configured [[lane]], found none", name)
	}
	return lane, nil
}

// pickLanes checks the global gate once, then each lane in order, and
// starts at most one item per lane that passes, while max_in_flight has room.
// Each lane writes one line and one run row that names it.
func (e *Engine) pickLanes(ctx context.Context, at time.Time, ranked []candidate, tabs []herdr.Tab, panes []herdr.Pane, open []gh.PR) error {
	if len(ranked) == 0 {
		return e.finish(ctx, at, outcomeIdle, "no ready items")
	}
	claims, err := e.Store.ActiveClaims(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	started, err := e.Store.LastStartedAt(ctx)
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	global := gate.CheckGlobal(gate.Inputs{
		Tabs:           tabs,
		HoldFiles:      e.Config.Gate.ActiveHoldFiles(),
		HoldTabs:       e.Config.Gate.HoldTabs,
		HoldTTL:        e.Config.Gate.HoldTTL,
		ActiveClaims:   claims,
		Pulls:          open,
		PRTitlePattern: e.Config.PRTitlePattern,
		LastStartedAt:  started,
		Now:            at,
	}, e.Config.Defaults)
	if !global.OK {
		if len(global.Reasons) == 0 {
			return errors.New("gate: expected reasons for a refusal, found none")
		}
		return e.finish(ctx, at, outcomeGated, "gate: "+joinParts(global.Reasons))
	}
	room := global.MaxInFlight - global.InFlight
	taken := map[string]struct{}{}
	for _, lane := range e.Config.OrderedLanes() {
		if room <= 0 {
			line := fmt.Sprintf("lane %s: gate: max_in_flight %d reached by this tick's starts", lane.Name, global.MaxInFlight)
			if err := e.finish(ctx, at, outcomeGated, line); err != nil {
				return err
			}
			continue
		}
		didStart, err := e.tickLane(ctx, at, lane, ranked, claims, tabs, panes, global, taken)
		if err != nil {
			return err
		}
		if didStart {
			room--
		}
	}
	return nil
}

// tickLane checks one lane and starts the top ready item routed to it.
// It reports whether an item was started (or would be, in a dry run).
func (e *Engine) tickLane(ctx context.Context, at time.Time, lane config.Lane, ranked []candidate, claims []store.Claim,
	tabs []herdr.Tab, panes []herdr.Pane, global gate.Result, taken map[string]struct{},
) (bool, error) {
	view, err := e.checkLane(ctx, lane, claims, tabs, panes)
	if err != nil {
		return false, err
	}
	res := view.result
	switch {
	case res.Skipped != "":
		return false, e.finish(ctx, at, outcomeSkipped, fmt.Sprintf("lane %s: skipped: %s", lane.Name, res.Skipped))
	case !res.OK:
		return false, e.finish(ctx, at, outcomeGated, fmt.Sprintf("lane %s: gate: %s", lane.Name, joinParts(res.Reasons)))
	}
	if res.Note != "" {
		slog.Info("lane note", "lane", lane.Name, "note", res.Note)
	}
	cands, err := routedTo(e.Config, ranked, lane, taken)
	if err != nil {
		return false, err
	}
	if len(cands) == 0 {
		return false, e.finish(ctx, at, outcomeIdle, fmt.Sprintf("lane %s: no ready items", lane.Name))
	}
	return e.startOnLane(ctx, at, view, cands, global, taken)
}

// routedTo returns the ranked candidates that may start on lane and that
// no earlier lane took this tick.
func routedTo(cfg *config.Config, ranked []candidate, lane config.Lane, taken map[string]struct{}) ([]candidate, error) {
	var out []candidate
	for _, cand := range ranked {
		if _, ok := taken[closedKey(cand.key, cand.item.ID)]; ok {
			continue
		}
		routed, err := cfg.RouteLanes(cand.item.Labels)
		if err != nil {
			return nil, fmt.Errorf("tick %s: %w", cand.item.ID, err)
		}
		if slices.ContainsFunc(routed, func(l config.Lane) bool { return l.Name == lane.Name }) {
			out = append(out, cand)
		}
	}
	return out, nil
}

// startOnLane starts the first candidate whose claim it wins, trying at
// most maxStartAttempts after lost claim races.
func (e *Engine) startOnLane(ctx context.Context, at time.Time, view laneCheck, cands []candidate, global gate.Result, taken map[string]struct{}) (bool, error) {
	lost := make([]string, 0, maxStartAttempts)
	for i, cand := range cands {
		if i == maxStartAttempts {
			break
		}
		taken[closedKey(cand.key, cand.item.ID)] = struct{}{}
		err := e.startLaneOne(ctx, view, cand, global)
		if err == nil {
			return true, e.afterStartOn(ctx, at, cand, view.lane.Name)
		}
		if errors.Is(err, start.ErrLostRace) {
			slog.Info("lost the claim race", "id", cand.item.ID, "lane", view.lane.Name)
			lost = append(lost, cand.item.ID)
			continue
		}
		return false, fmt.Errorf("tick: lane %s: %w", view.lane.Name, err)
	}
	line := fmt.Sprintf("lane %s: lost the claim race for %s", view.lane.Name, strings.Join(lost, ", "))
	return false, e.finish(ctx, at, outcomeLost, line)
}

func (e *Engine) startLaneOne(ctx context.Context, view laneCheck, cand candidate, global gate.Result) error {
	src, err := e.backend(cand.key)
	if err != nil {
		return err
	}
	snap := cand.snap
	if view.lane.Remote() {
		// The ready check saw the local server. The worker's tab goes on
		// the lane machine, so the existing-tab check must see that one too.
		ws, err := view.machine.Sessions.Workspaces(ctx)
		if err != nil {
			return fmt.Errorf("herdr workspaces: %w", err)
		}
		snap.Workspaces = append(slices.Clone(snap.Workspaces), ws...)
		snap.Tabs = append(slices.Clone(snap.Tabs), view.tabs...)
	}
	starter := &start.Starter{
		Src:       src,
		Cfg:       config.LaneSource(cand.cfg, view.lane),
		Kind:      cand.cfg.Kind,
		Defaults:  e.Config.Defaults,
		Rules:     e.Config.AgentRules,
		Store:     e.Store,
		Herdr:     view.machine.Agent,
		Hostname:  e.Hostname,
		Now:       e.Now,
		Out:       e.Out,
		Caller:    e.Caller,
		CallerBus: e.CallerBus,
		Lane:      view.lane.Name,
		Machine:   view.lane.Machine,
		Wake:      e.Wake,
		Sleep:     e.Sleep,
	}
	if !view.lane.Remote() {
		// A remote checkout is not read locally.
		starter.Stash = e.Stash
		starter.LeaseStatus = e.Lease
		starter.WorktreeAdd = e.WorktreeAdd
		starter.WorktreeRemove = e.WorktreeRemove
	}
	return starter.Start(ctx, cand.item.ID, snap, global, start.Options{Apply: e.Apply}) //nolint:wrapcheck // propagate context cancellation
}

// checkLane lists the lane machine's tabs and panes and checks the lane.
// A remote machine that does not answer is a skipped lane, not an error.
// A local lane reuses the tabs and panes the tick already listed.
func (e *Engine) checkLane(ctx context.Context, lane config.Lane, claims []store.Claim, localTabs []herdr.Tab, localPanes []herdr.Pane) (laneCheck, error) {
	g := gate.Lane{
		Name:       lane.Name,
		Machine:    lane.Machine,
		Checkout:   lane.Checkout,
		MaxWorkers: lane.MaxWorkers,
		MaxActive:  lane.MaxActive,
	}
	m, err := e.machineFor(lane)
	if err != nil {
		return laneCheck{}, err
	}
	view := laneCheck{lane: lane, machine: m, claims: claims}
	tabs, panes, caller := localTabs, localPanes, e.Config.CallerPaneID
	if lane.Remote() {
		caller = ""
		if tabs, err = m.Sessions.Tabs(ctx); err != nil {
			return e.skipLane(ctx, view, g, err)
		}
		if panes, err = m.Sessions.Panes(ctx); err != nil {
			return e.skipLane(ctx, view, g, err)
		}
	}
	view.tabs = tabs
	view.claims = claims
	view.result = gate.CheckLane(ctx, gate.Inputs{
		ActiveClaims: claims,
		Leases:       e.Leases,
		Tabs:         tabs,
		Panes:        panes,
		CallerPaneID: caller,
		IgnorePanes:  e.Config.Gate.IgnorePanes,
		IgnoreTabs:   e.Config.Gate.IgnoreTabs,
		GitStatus:    e.Git,
		BaseBranch:   e.Config.Gate.BaseBranch,
		Require:      e.Config.Gate.Require,
		GitBranch:    e.GitBranch,
		GitAhead:     e.GitAhead,
		LeaseGate:    e.Config.Lease.Gate,
		Lease:        e.Lease,
	}, g)
	return view, nil
}

func (e *Engine) skipLane(ctx context.Context, view laneCheck, g gate.Lane, err error) (laneCheck, error) {
	if !herdr.Unreachable(err) {
		return laneCheck{}, fmt.Errorf("lane %s: %w", view.lane.Name, err)
	}
	why := err.Error()
	if view.machine.Why != nil {
		why = view.machine.Why(ctx, err)
	}
	view.result = gate.SkippedLane(g, view.claims, why)
	return view, nil
}
