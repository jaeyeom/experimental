package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/signal"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
	"golang.org/x/sys/unix"

	"github.com/jaeyeom/experimental/devtools/factory/internal/agentbus"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/remote"
	"github.com/jaeyeom/experimental/devtools/factory/internal/start"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/tick"
	"github.com/jaeyeom/experimental/devtools/factory/internal/verify"
)

func newTickCmd(ctx context.Context) *cobra.Command {
	var apply, enqueue bool
	var scope scopeFlags
	cmd := &cobra.Command{
		Use:           "tick",
		Short:         "Sync, settle merged work, and start at most one ready item",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("tick: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			if enqueue && apply {
				return usageError{err: errors.New("tick: expected --enqueue or --apply, found both")}
			}
			return runTick(ctx, cmd, apply, enqueue, scope)
		},
	}
	cmd.Flags().BoolVar(&apply, "apply", false, "claim the top item and start the agent (implied when dry_run is false)")
	cmd.Flags().BoolVar(&enqueue, "enqueue", false, "print a start-item job (JSON line) per ready item to stdout instead of starting one; summary goes to stderr")
	addScopeFlags(cmd, &scope)
	addCallerFlag(cmd)
	return cmd
}

func newSettleCmd(ctx context.Context) *cobra.Command {
	cmd := &cobra.Command{
		Use:           "settle [id...]",
		Short:         "Finish merged work without starting an agent",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			seen := map[string]struct{}{}
			for _, id := range args {
				if id == "" {
					return usageError{err: errors.New("settle: expected a non-empty id, found empty")}
				}
				if _, ok := seen[id]; ok {
					return usageError{err: fmt.Errorf("settle: expected distinct ids, found %s repeated", id)}
				}
				seen[id] = struct{}{}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runSettle(ctx, cmd, args)
		},
	}
	return cmd
}

func newRunCmd(ctx context.Context) *cobra.Command {
	var (
		apply    bool
		interval time.Duration
		scope    scopeFlags
	)
	cmd := &cobra.Command{
		Use:           "run",
		Short:         "Run factory tick on an interval until interrupted",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("run: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			if interval < time.Minute {
				return usageError{err: fmt.Errorf("run: expected interval >= 1m, found %s", interval)}
			}
			return runLoop(ctx, cmd, interval, apply, scope)
		},
	}
	cmd.Flags().BoolVar(&apply, "apply", false, "claim the top item and start the agent (implied when dry_run is false)")
	addScopeFlags(cmd, &scope)
	addCallerFlag(cmd)
	cmd.Flags().DurationVar(&interval, "interval", 0, "time between ticks (minimum 1m)")
	return cmd
}

func runTick(ctx context.Context, cmd *cobra.Command, apply, enqueue bool, scope scopeFlags) error {
	return runEngine(ctx, cmd, apply, scope, func(ctx context.Context, eng *tick.Engine) error {
		if enqueue {
			// Jobs alone on stdout, so it can be piped to a queue.
			eng.Enqueue = true
			eng.Apply = false
			eng.Jobs = cmd.OutOrStdout()
			eng.Out = cmd.ErrOrStderr()
		}
		return eng.Tick(ctx)
	})
}

func runSettle(ctx context.Context, cmd *cobra.Command, ids []string) error {
	return runEngine(ctx, cmd, true, scopeFlags{all: true}, func(ctx context.Context, eng *tick.Engine) error {
		return eng.Settle(ctx, ids)
	})
}

func runLoop(ctx context.Context, cmd *cobra.Command, interval time.Duration, apply bool, scope scopeFlags) error {
	// NotifyContext cancels when the process is interrupted. Run lets the
	// current tick finish, then returns.
	ctx, stop := signal.NotifyContext(ctx, unix.SIGINT, unix.SIGTERM)
	defer stop()
	return runEngine(ctx, cmd, apply, scope, func(ctx context.Context, eng *tick.Engine) error {
		return eng.Run(ctx, interval)
	})
}

func runEngine(ctx context.Context, cmd *cobra.Command, apply bool, scope scopeFlags, fn func(context.Context, *tick.Engine) error) (err error) {
	if ctx == nil {
		return errors.New("tick: expected a context, found nil")
	}
	if err := scope.check(cmd.Name()); err != nil {
		return err
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	only, err := scope.resolve(cmd, cfg)
	if err != nil {
		return err
	}
	if _, err := sourcesFor(cfg, only); err != nil {
		return err
	}
	caller, err := resolveCaller(cmd, cfg)
	if err != nil {
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
	eng, err := buildEngine(ctx, cmd, cfg, st, apply, caller)
	if err != nil {
		return err
	}
	eng.Only = only
	return fn(ctx, eng)
}

// laneMachine returns the herdr surface on a remote lane machine:
// every herdr call runs as `herdr --machine MACHINE`.
func laneMachine(exec executor.Executor) func(string) (tick.LaneMachine, error) {
	return func(machine string) (tick.LaneMachine, error) {
		if machine == "" {
			return tick.LaneMachine{}, errors.New("lane machine: expected a machine name, found empty")
		}
		client := herdr.NewClient(exec, "herdr").OnMachine(machine)
		agent, err := start.NewHerdr(client)
		if err != nil {
			return tick.LaneMachine{}, err //nolint:wrapcheck // internal factory error already carries context
		}
		return tick.LaneMachine{Sessions: client, Closer: client, Agent: agent, WaitAgent: agent.WaitAgent, Why: client.WhyUnreachable}, nil
	}
}

func buildEngine(ctx context.Context, cmd *cobra.Command, cfg *config.Config, st *store.Store, apply bool, caller string) (*tick.Engine, error) {
	deps, err := sourceDeps()
	if err != nil {
		return nil, err
	}
	srcs, err := buildSources(cfg, deps)
	if err != nil {
		return nil, err
	}
	herd, pulls, err := externalClients(cfg.GHBin)
	if err != nil {
		return nil, err
	}
	agent, err := start.NewHerdr(herd)
	if err != nil {
		return nil, err //nolint:wrapcheck // internal factory error already carries context
	}
	host, err := os.Hostname()
	if err != nil {
		return nil, fmt.Errorf("tick: hostname: %w", err)
	}
	if host == "" {
		return nil, errors.New("tick: expected a hostname, found empty")
	}
	if deps.Exec == nil {
		return nil, errors.New("tick: expected an executor, found nil")
	}
	return &tick.Engine{
		Store:          st,
		Sources:        srcs,
		Config:         cfg,
		Pulls:          pulls,
		Sessions:       herd,
		Closer:         herd,
		Agent:          agent,
		WaitAgent:      agent.WaitAgent,
		Git:            gate.GitStatus(deps.Exec),
		GitBranch:      gate.GitBranch(deps.Exec),
		GitAhead:       gate.GitAhead(deps.Exec),
		Stash:          gate.GitStashCount(deps.Exec),
		GitMerge:       verify.FindLinearCommit(deps.Exec),
		RemoteGitMerge: remoteMergeChecker(deps.Exec),
		Lease:          gate.LeaseReader(agentbus.NewClient(deps.Exec, "agentbus")),
		Wake:           promptWaker(cfg, deps.Exec),
		Machine:        laneMachine(deps.Exec),
		Leases:         agentbus.NewClient(deps.Exec, "agentbus").All,
		Now:            func() time.Time { return time.Now().UTC() },
		Hostname:       host,
		Out:            cmd.OutOrStdout(),
		Apply:          apply || !cfg.Defaults.DryRun,
		Caller:         caller,
		CallerBus:      resolveCallerBus(ctx, cfg, deps.Exec),
		Sleep:          startSleep,
		WorktreeAdd:    gate.GitWorktreeAdd(deps.Exec),
		WorktreeRemove: gate.GitWorktreeRemove(deps.Exec),
	}, nil
}

func remoteMergeChecker(exec executor.Executor) func(config.Lane) (tick.MergeChecker, error) {
	return func(lane config.Lane) (tick.MergeChecker, error) {
		rexec, err := remote.New(exec, lane.Exec)
		if err != nil {
			return nil, err //nolint:wrapcheck // internal factory error already carries context
		}
		return verify.FindLinearCommit(rexec), nil
	}
}
