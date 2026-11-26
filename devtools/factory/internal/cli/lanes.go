package cli

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// laneView is one lane after its gate check.
type laneView struct {
	lane   config.Lane
	client *herdr.Client // Runs herdr with --machine for a remote lane.
	tabs   []herdr.Tab   // The lane machine's tabs. Nil when the lane was skipped.
	result gate.LaneResult
}

// laneEnv is what every lane check shares.
type laneEnv struct {
	exec   executor.Executor
	cfg    *config.Config
	claims []store.Claim // Active claims. Each lane counts the ones recorded for it.
	// localTabs are the local herdr server's tabs when localLoaded is set.
	// A local lane reuses them instead of listing tabs again.
	localTabs   []herdr.Tab
	localLoaded bool
	// owner is the agent a start is for. A lease it holds is usable.
	// The gate command leaves it zero, so only a free lease is usable.
	owner agentbus.Owner
}

// globalInputs are the gate inputs that do not depend on a lane.
// tabs are the local herdr server's tabs, read by the hold-tab check.
func globalInputs(cfg *config.Config, tabs []herdr.Tab, claims []store.Claim, pulls []gh.PR, started time.Time) gate.Inputs {
	return gate.Inputs{
		Tabs:           tabs,
		HoldFiles:      cfg.Gate.ActiveHoldFiles(),
		HoldTabs:       cfg.Gate.HoldTabs,
		HoldTTL:        cfg.Gate.HoldTTL,
		ActiveClaims:   claims,
		Pulls:          pulls,
		PRTitlePattern: cfg.PRTitlePattern,
		LastStartedAt:  started,
		Now:            time.Now().UTC(),
	}
}

// check lists the lane machine's tabs and panes and checks the lane.
// A remote machine that does not answer is a skipped lane, not an error.
// A local herdr failure is still an error, as it is without lanes.
func (e laneEnv) check(ctx context.Context, lane config.Lane) (laneView, error) {
	if e.exec == nil {
		return laneView{}, errors.New("lane: expected an executor, found nil")
	}
	client := herdr.NewClient(e.exec, "herdr").OnMachine(lane.Machine)
	g := gate.Lane{
		Name:       lane.Name,
		Machine:    lane.Machine,
		Checkout:   lane.Checkout,
		MaxWorkers: lane.MaxWorkers,
		MaxActive:  lane.MaxActive,
	}
	view := laneView{lane: lane, client: client}
	tabs := e.localTabs
	if lane.Remote() || !e.localLoaded {
		listed, err := client.Tabs(ctx)
		if err != nil {
			return e.skip(ctx, view, g, err)
		}
		tabs = listed
	}
	panes, err := client.Panes(ctx)
	if err != nil {
		return e.skip(ctx, view, g, err)
	}
	caller := ""
	if !lane.Remote() {
		caller = e.cfg.CallerPaneID
	}
	view.tabs = tabs
	leases := agentbus.NewClient(e.exec, "agentbus")
	view.result = gate.CheckLane(ctx, gate.Inputs{
		ActiveClaims: e.claims,
		Leases:       leases.All,
		Tabs:         tabs,
		Panes:        panes,
		CallerPaneID: caller,
		IgnorePanes:  e.cfg.Gate.IgnorePanes,
		IgnoreTabs:   e.cfg.Gate.IgnoreTabs,
		GitStatus:    gate.GitStatus(e.exec),
		BaseBranch:   e.cfg.Gate.BaseBranch,
		Require:      e.cfg.Gate.Require,
		GitBranch:    gate.GitBranch(e.exec),
		GitAhead:     gate.GitAhead(e.exec),
		LeaseGate:    e.cfg.Lease.Gate,
		Lease:        gate.LeaseReader(leases),
		LeaseOwner:   e.owner,
	}, g)
	return view, nil
}

func (e laneEnv) skip(ctx context.Context, view laneView, g gate.Lane, err error) (laneView, error) {
	if !view.lane.Remote() || !herdr.Unreachable(err) {
		return laneView{}, fmt.Errorf("lane %s: %w", view.lane.Name, err)
	}
	view.result = gate.SkippedLane(g, e.claims, view.client.WhyUnreachable(ctx, err))
	return view, nil
}

func anyLocalLane(lanes []config.Lane) bool {
	return slices.ContainsFunc(lanes, func(lane config.Lane) bool { return !lane.Remote() })
}

// withLaneSessions adds a remote lane's workspaces and tabs to snap, so the
// ready tab check and the existing-tab check see the machine the worker
// starts on as well as the local server.
func withLaneSessions(ctx context.Context, snap ready.Snapshot, view laneView) (ready.Snapshot, error) {
	if !view.lane.Remote() {
		return snap, nil
	}
	ws, err := view.client.Workspaces(ctx)
	if err != nil {
		return ready.Snapshot{}, fmt.Errorf("lane %s: herdr workspaces: %w", view.lane.Name, err)
	}
	snap.Workspaces = append(slices.Clone(snap.Workspaces), ws...)
	snap.Tabs = append(slices.Clone(snap.Tabs), view.tabs...)
	return snap, nil
}

// claimLane resolves the lane recorded on a claim. An empty name is the
// local herdr server, which is also every claim made without lanes.
// A name that is no longer a configured lane is an error.
func claimLane(cfg *config.Config, name string) (config.Lane, error) {
	if name == "" {
		return config.Lane{}, nil
	}
	lane, ok := cfg.LaneNamed(name)
	if !ok {
		return config.Lane{}, fmt.Errorf("claim lane %q: expected a configured [[lane]], found none", name)
	}
	return lane, nil
}

// laneHerdr returns the herdr client for lane. A zero lane is local.
func laneHerdr(lane config.Lane) (*herdr.Client, error) {
	exec := newCommandExec()
	if exec == nil {
		return nil, errors.New("lane: expected an executor, found nil")
	}
	return herdr.NewClient(exec, "herdr").OnMachine(lane.Machine), nil
}

// onLane replaces snap's workspaces and tabs with the remote lane
// machine's, where the claim's tab lives. A local lane keeps snap.
func onLane(ctx context.Context, snap ready.Snapshot, lane config.Lane, client *herdr.Client) (ready.Snapshot, error) {
	if !lane.Remote() {
		return snap, nil
	}
	ws, err := client.Workspaces(ctx)
	if err != nil {
		return ready.Snapshot{}, fmt.Errorf("lane %s: herdr workspaces: %w", lane.Name, err)
	}
	tabs, err := client.Tabs(ctx)
	if err != nil {
		return ready.Snapshot{}, fmt.Errorf("lane %s: herdr tabs: %w", lane.Name, err)
	}
	snap.Workspaces = ws
	snap.Tabs = tabs
	return snap, nil
}

// claimSnapshot points snap at the tabs of the lane recorded on a claim.
func claimSnapshot(ctx context.Context, cfg *config.Config, snap ready.Snapshot, name string) (ready.Snapshot, error) {
	lane, err := claimLane(cfg, name)
	if err != nil {
		return ready.Snapshot{}, err
	}
	client, err := laneHerdr(lane)
	if err != nil {
		return ready.Snapshot{}, err
	}
	return onLane(ctx, snap, lane, client)
}

// checkMachineFlag rejects an empty --machine, and --machine without lanes.
func checkMachineFlag(cmd *cobra.Command, cfg *config.Config, value string) error {
	if cmd.Flags().Changed("machine") && value == "" {
		return usageError{err: fmt.Errorf("%s: expected a non-empty --machine, found empty", cmd.Name())}
	}
	if value != "" && !cfg.HasLanes() {
		return usageError{err: fmt.Errorf("%s --machine %s: expected [[lane]] config, found none", cmd.Name(), value)}
	}
	return nil
}
