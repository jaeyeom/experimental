package cli

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// laneStartInput is what runStart already resolved before lanes apply.
type laneStartInput struct {
	st      *store.Store
	cfg     *config.Config
	it      source.Item
	src     config.Source
	caller  string
	machine string // --machine, a lane name or a lane machine. Empty picks a lane.
}

// laneChoice is the lane start uses and the gate result Start sees.
type laneChoice struct {
	view    laneView
	gateRes gate.Result
	// chosen is false when no lane passed and view is only shown in the
	// blocked plan.
	chosen bool
}

// runLaneStart routes the item to its lanes, checks the global gate once,
// then checks each lane in preference order and takes the first that
// passes. The plan, the claim, and every herdr call for the worker use
// that lane. No passing lane is a gate refusal that names each lane.
func runLaneStart(ctx context.Context, cmd *cobra.Command, in laneStartInput, opts start.Options) error {
	routed, err := in.cfg.RouteLanes(in.it.Labels)
	if err != nil {
		return fmt.Errorf("start %s: %w", in.it.ID, err)
	}
	if in.machine != "" {
		lane, err := in.cfg.PickLane(in.machine, routed)
		if err != nil {
			return fmt.Errorf("start %s: %w", in.it.ID, err)
		}
		routed = []config.Lane{lane}
	}
	snap, err := loadSnapshot(ctx, in.st, in.src)
	if err != nil {
		return err
	}
	exec := newCommandExec()
	if exec == nil {
		return errors.New("start: expected an executor, found nil")
	}
	owner, err := start.LeaseOwner(in.it, in.src, in.src.Kind)
	if err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	env := laneEnv{exec: exec, cfg: in.cfg, localTabs: snap.Tabs, localLoaded: true, owner: owner}
	choice, err := chooseLane(ctx, env, in, routed, snap.OpenPRs, opts.Force)
	if err != nil {
		return err
	}
	if choice.chosen {
		snap, err = withLaneSessions(ctx, snap, choice.view)
		if err != nil {
			return err
		}
	}
	backend, err := oneSource(in.cfg, in.it.Source)
	if err != nil {
		return err
	}
	host, err := startHostname()
	if err != nil {
		return err
	}
	starter := &start.Starter{
		Src:      backend,
		Cfg:      config.LaneSource(in.src, choice.view.lane),
		Kind:     in.src.Kind,
		Defaults: in.cfg.Defaults,
		Rules:    in.cfg.AgentRules,
		Store:    in.st,
		Hostname: host,
		Now:      func() time.Time { return time.Now().UTC() },
		Out:      cmd.OutOrStdout(),
		Caller:   in.caller,
		Lane:     choice.view.lane.Name,
		Machine:  choice.view.lane.Machine,
		Wake:     promptWaker(in.cfg, exec),

		CallerBus: resolveCallerBus(ctx, in.cfg, exec),
		Sleep:     startSleep,
	}
	if !choice.view.lane.Remote() {
		starter.Stash = gate.GitStashCount(exec)
		starter.LeaseStatus = gate.LeaseReader(agentbus.NewClient(exec, "agentbus"))
		starter.WorktreeAdd = gate.GitWorktreeAdd(exec)
		starter.WorktreeRemove = gate.GitWorktreeRemove(exec)
	}
	agent, err := start.NewHerdr(choice.view.client)
	if err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	starter.Herdr = agent
	return starter.Start(ctx, in.it.ID, snap, choice.gateRes, opts) //nolint:wrapcheck // propagate context cancellation
}

// chooseLane checks the global gate and then each routed lane in order.
// The first lane that passes is used. --force takes the first lane that
// answered, without its gate. When no lane passes, the result is a refusal
// that names the global reasons and every lane, and the plan shows the
// first lane that answered, or the first routed lane.
func chooseLane(ctx context.Context, env laneEnv, in laneStartInput, routed []config.Lane, pulls []gh.PR, force bool) (laneChoice, error) {
	if len(routed) == 0 {
		return laneChoice{}, fmt.Errorf("start %s: expected at least one lane, found none", in.it.ID)
	}
	claims, err := in.st.ActiveClaims(ctx)
	if err != nil {
		return laneChoice{}, err //nolint:wrapcheck // propagate context cancellation
	}
	started, err := in.st.LastStartedAt(ctx)
	if err != nil {
		return laneChoice{}, err //nolint:wrapcheck // propagate context cancellation
	}
	env.claims = claims
	global := gate.CheckGlobal(globalInputs(in.cfg, env.localTabs, claims, pulls, started), in.cfg.Defaults)
	var lines []string
	var shown *laneView
	for _, lane := range routed {
		view, err := env.check(ctx, lane)
		if err != nil {
			return laneChoice{}, err
		}
		if view.result.Skipped == "" && shown == nil {
			shown = &view
		}
		if view.result.OK || (force && view.result.Skipped == "") {
			return laneChoice{view: view, gateRes: global, chosen: true}, nil
		}
		lines = append(lines, "lane "+view.result.Line())
	}
	if force {
		return laneChoice{}, fmt.Errorf("start %s: expected a lane that answers, found none: %s", in.it.ID, strings.Join(lines, "; "))
	}
	if shown == nil {
		shown = &laneView{lane: routed[0], client: herdr.NewClient(env.exec, "herdr").OnMachine(routed[0].Machine)}
	}
	reasons := append(append([]string{}, global.Reasons...), lines...)
	return laneChoice{view: *shown, gateRes: gate.Result{
		OK:          false,
		Reasons:     reasons,
		InFlight:    global.InFlight,
		MaxInFlight: global.MaxInFlight,
	}}, nil
}
