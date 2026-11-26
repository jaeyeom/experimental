package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func newStartCmd(ctx context.Context) *cobra.Command {
	var apply, force, forceTakeOwnership, asJSON bool
	var agentArgs []string
	var model, machine, sourceOnly string
	cmd := &cobra.Command{
		Use:           "start <id>",
		Short:         "Claim an item and start an agent in herdr",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 1 {
				return usageError{err: fmt.Errorf("start: expected one id, found %d", len(args))}
			}
			if args[0] == "" {
				return usageError{err: errors.New("start: expected a non-empty id, found empty")}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runStart(ctx, cmd, args[0], startFlags{
				apply:              apply,
				force:              force,
				forceTakeOwnership: forceTakeOwnership,
				asJSON:             asJSON,
				agentArgs:          agentArgs,
				model:              model,
				machine:            machine,
				sourceOnly:         sourceOnly,
			})
		},
	}
	cmd.Flags().BoolVar(&apply, "apply", false, "claim the item and start the agent (implied when dry_run is false)")
	cmd.Flags().BoolVar(&force, "force", false, "skip queue checks and the start gate; still refuse closed/done, untrusted, non-leaf, blocked, foreign ownership, in-progress, and existing tab")
	cmd.Flags().BoolVar(&forceTakeOwnership, "force-taking-ownership", false, "take over an item assigned to someone else, already in progress, or claimed on another machine")
	cmd.Flags().StringVar(&sourceOnly, "source", "", "limit to this source key (kind:name, for example github:repo)")
	cmd.Flags().BoolVar(&asJSON, "json", false, "print the start outcome as JSON")
	cmd.Flags().StringArrayVar(&agentArgs, "agent-arg", nil, "agent argument after -- (repeatable; overrides rules)")
	cmd.Flags().StringVar(&model, "model", "", "shorthand for --agent-arg=--model --agent-arg=MODEL")
	cmd.Flags().StringVar(&machine, "machine", "", "lane name or lane herdr machine to start on (needs [[lane]] config)")
	addCallerFlag(cmd)
	return cmd
}

func startOverride(cmd *cobra.Command, agentArgs []string, model string) ([]string, bool, error) {
	modelSet := cmd.Flags().Changed("model")
	argsSet := cmd.Flags().Changed("agent-arg")
	if modelSet && argsSet {
		return nil, false, usageError{err: errors.New("start: expected --agent-arg or --model, found both")}
	}
	if modelSet {
		if model == "" {
			return nil, false, usageError{err: errors.New("start: expected a non-empty --model, found empty")}
		}
		return []string{"--model", model}, true, nil
	}
	if !argsSet {
		return nil, false, nil
	}
	if len(agentArgs) == 0 {
		return nil, false, usageError{err: errors.New("start: expected at least one --agent-arg, found none")}
	}
	for _, arg := range agentArgs {
		if arg == "" {
			return nil, false, usageError{err: errors.New("start: expected a non-empty --agent-arg, found empty")}
		}
	}
	return agentArgs, true, nil
}

// startFlags are the start command flags.
type startFlags struct {
	apply, force, forceTakeOwnership, asJSON bool
	agentArgs                                []string
	model                                    string
	machine                                  string
	sourceOnly                               string
}

func runStart(ctx context.Context, cmd *cobra.Command, id string, flags startFlags) (err error) {
	apply, force := flags.apply, flags.force
	override, overrideSet, err := startOverride(cmd, flags.agentArgs, flags.model)
	if err != nil {
		return err
	}
	if ctx == nil {
		return errors.New("start: expected a context, found nil")
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	caller, err := resolveCaller(cmd, cfg)
	if err != nil {
		return err
	}
	if err := checkMachineFlag(cmd, cfg, flags.machine); err != nil {
		return err
	}
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); cerr != nil {
			err = errors.Join(err, fmt.Errorf("close store: %w", cerr))
		}
	}()
	it, srcCfg, err := startTargetScoped(ctx, cmd, st, cfg, id, flags.sourceOnly)
	if err != nil {
		return err
	}
	opts := start.Options{
		Apply:              apply || !cfg.Defaults.DryRun,
		Force:              force,
		ForceTakeOwnership: flags.forceTakeOwnership,
		AgentArgs:          override,
		AgentArgsSet:       overrideSet,
		JSON:               flags.asJSON,
	}
	in := laneStartInput{st: st, cfg: cfg, it: it, src: srcCfg, caller: caller, machine: flags.machine}
	if cfg.HasLanes() {
		return runLaneStart(ctx, cmd, in, opts)
	}
	return startLocal(ctx, cmd, in, opts)
}

// startLocal is start without lanes: the local herdr server and the source cwd.
func startLocal(ctx context.Context, cmd *cobra.Command, in laneStartInput, opts start.Options) error {
	st, cfg, it, srcCfg, caller, id := in.st, in.cfg, in.it, in.src, in.caller, in.it.ID
	snap, err := loadSnapshot(ctx, st, srcCfg)
	if err != nil {
		return err
	}
	owner, err := start.LeaseOwner(it, srcCfg, srcCfg.Kind)
	if err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	gateRes, err := startGate(ctx, st, snap, srcCfg.Cwd, cfg, owner)
	if err != nil {
		return err
	}
	backend, err := oneSource(cfg, it.Source)
	if err != nil {
		return err
	}
	client, err := startClient()
	if err != nil {
		return err
	}
	host, err := startHostname()
	if err != nil {
		return err
	}
	exec := newCommandExec()
	if exec == nil {
		return errors.New("start: expected an executor, found nil")
	}
	starter := &start.Starter{
		Src:      backend,
		Cfg:      srcCfg,
		Kind:     srcCfg.Kind,
		Defaults: cfg.Defaults,
		Rules:    cfg.AgentRules,
		Store:    st,
		Herdr:    client,
		Hostname: host,
		Now:      func() time.Time { return time.Now().UTC() },
		Out:      cmd.OutOrStdout(),
		Caller:   caller,
		Stash:    gate.GitStashCount(exec),

		LeaseStatus:    gate.LeaseReader(agentbus.NewClient(exec, "agentbus")),
		Wake:           promptWaker(cfg, exec),
		CallerBus:      resolveCallerBus(ctx, cfg, exec),
		Sleep:          startSleep,
		WorktreeAdd:    gate.GitWorktreeAdd(exec),
		WorktreeRemove: gate.GitWorktreeRemove(exec),
	}
	return starter.Start(ctx, id, snap, gateRes, opts) //nolint:wrapcheck // propagate context cancellation
}

// startGate checks the tabs already loaded into snap.
// It lists panes once so the caller's pane can be left out.
// A checkout lease held by owner is usable.
func startGate(ctx context.Context, st *store.Store, snap ready.Snapshot, cwd string, cfg *config.Config, owner agentbus.Owner) (gate.Result, error) {
	if cfg == nil {
		return gate.Result{}, errors.New("start: expected a config, found nil")
	}
	claims, err := st.ActiveClaims(ctx)
	if err != nil {
		return gate.Result{}, err //nolint:wrapcheck // propagate context cancellation
	}
	started, err := st.LastStartedAt(ctx)
	if err != nil {
		return gate.Result{}, err //nolint:wrapcheck // propagate context cancellation
	}
	exec := newCommandExec()
	if exec == nil {
		return gate.Result{}, errors.New("start: expected an executor, found nil")
	}
	panes, err := herdr.NewClient(exec, "herdr").Panes(ctx)
	if err != nil {
		return gate.Result{}, err //nolint:wrapcheck // propagate context cancellation
	}
	checkouts, err := cfg.SourceCheckouts()
	if err != nil {
		return gate.Result{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	return gate.Check(ctx, gate.Inputs{
		Tabs:           snap.Tabs,
		Panes:          panes,
		CallerPaneID:   cfg.CallerPaneID,
		IgnorePanes:    cfg.Gate.IgnorePanes,
		IgnoreTabs:     cfg.Gate.IgnoreTabs,
		Checkouts:      checkouts,
		HoldFiles:      cfg.Gate.ActiveHoldFiles(),
		HoldTabs:       cfg.Gate.HoldTabs,
		HoldTTL:        cfg.Gate.HoldTTL,
		GitStatus:      gate.GitStatus(exec),
		BaseBranch:     cfg.Gate.BaseBranch,
		Require:        cfg.Gate.Require,
		GitBranch:      gate.GitBranch(exec),
		GitAhead:       gate.GitAhead(exec),
		ActiveClaims:   claims,
		Pulls:          snap.OpenPRs,
		PRTitlePattern: cfg.PRTitlePattern,
		LastStartedAt:  started,
		Now:            time.Now().UTC(),
		LeaseGate:      cfg.Lease.Gate,
		Lease:          gate.LeaseReader(agentbus.NewClient(exec, "agentbus")),
		LeaseOwner:     owner,
		MaxWorkers:     cfg.Worker.MaxWorkers,
		Worktree:       cfg.Worker.Worktree,
	}, cwd, cfg.Defaults), nil
}

func oneSource(cfg *config.Config, key string) (source.Source, error) {
	deps, err := sourceDeps()
	if err != nil {
		return nil, err
	}
	srcs, err := buildSources(cfg, deps)
	if err != nil {
		return nil, err
	}
	backend, ok := srcs[key]
	if !ok {
		return nil, fmt.Errorf("source %s: expected a configured source, found none", key)
	}
	return backend, nil
}

func startClient() (start.HerdrAPI, error) {
	exec := newCommandExec()
	if exec == nil {
		return nil, errors.New("start: expected an executor, found nil")
	}
	return start.NewHerdr(herdr.NewClient(exec, "herdr")) //nolint:wrapcheck // internal factory error already carries context
}

func startHostname() (string, error) {
	host, err := os.Hostname()
	if err != nil {
		return "", fmt.Errorf("start: hostname: %w", err)
	}
	if host == "" {
		return "", errors.New("start: expected a hostname, found empty")
	}
	return host, nil
}
