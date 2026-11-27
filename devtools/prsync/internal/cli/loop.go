package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"path/filepath"
	"strconv"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/loop"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

// ensureSpawner launches detached `prsync loop` processes for `--ensure`.
// Tests may replace it to verify spawning without execing the test binary.
var ensureSpawner = loop.DefaultSpawner

// ensureIsAlive reports whether pid is alive for `--ensure`. Tests may
// replace it to simulate live or dead PIDs deterministically.
var ensureIsAlive = loop.ProcessAlive

func newLoopCmd(stdout io.Writer, exec executor.Executor) *cobra.Command {
	var (
		configPath string
		maxRounds  int
		pause      time.Duration
		parallel   int
		dryRun     bool
		ensure     bool
	)
	cmd := &cobra.Command{
		Use:   "loop",
		Short: "Run scan and dispatch continuously in rounds, or ensure the loop is alive",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, _ []string) error {
			if cmd.Flags().Changed("max-rounds") && maxRounds < 0 {
				return &ExitError{Code: ExitUsage, Err: errors.New("--max-rounds: must be >= 0")}
			}
			if cmd.Flags().Changed("pause") && pause < 0 {
				return &ExitError{Code: ExitUsage, Err: errors.New("--pause: must be >= 0")}
			}
			if cmd.Flags().Changed("parallel") && parallel < 1 {
				return &ExitError{Code: ExitUsage, Err: errors.New("--parallel: must be >= 1")}
			}
			cfg, err := config.Load(configPath)
			if err != nil {
				return fmt.Errorf("load config: %w", err)
			}
			if cmd.Flags().Changed("parallel") {
				cfg.DispatchMaxParallel = parallel
			}
			if err := cfg.Validate(); err != nil {
				return fmt.Errorf("validate config: %w", err)
			}
			cfg.DryRun = dryRun

			if ensure {
				args := buildEnsureArgs(cmd, cfg, configPath, maxRounds, pause, parallel, dryRun)
				if _, err := loop.Ensure(cfg, loop.EnsureOptions{
					Args:    args,
					IsAlive: ensureIsAlive,
					Spawner: ensureSpawner,
				}); err != nil {
					return fmt.Errorf("loop ensure: %w", err)
				}
				return nil
			}

			logStartup(cmd.Context(), "loop", cfg)
			ctx := dispatch.WithStatusWriter(cmd.Context(), cmd.ErrOrStderr())
			runner := cliLoopRunner(exec)
			err = loop.Run(ctx, cfg, runner, loop.Options{
				MaxRounds: maxRounds,
				Pause:     pause,
				Stdout:    stdout,
				Stderr:    cmd.ErrOrStderr(),
			})
			if err != nil {
				if errors.Is(err, loop.ErrAlreadyRunning) {
					return &ExitError{Code: ExitPrecondition, Err: err}
				}
				return fmt.Errorf("loop run: %w", err)
			}
			return nil
		},
	}
	cmd.Flags().StringVar(&configPath, "config", "", "config file path")
	cmd.Flags().IntVar(&maxRounds, "max-rounds", 0, "stop after this many rounds (0 runs until signaled)")
	cmd.Flags().DurationVar(&pause, "pause", 0, "delay between rounds (default 0 starts the next scan at once)")
	cmd.Flags().IntVar(&parallel, "parallel", 0, "max concurrent PR dispatches (overrides dispatch_max_parallel)")
	cmd.Flags().BoolVar(&dryRun, "dry-run", false, "run dispatch in dry-run mode without sending prompts")
	cmd.Flags().BoolVar(&ensure, "ensure", false, "check heartbeat and PID; exit 0 if healthy or start the loop detached")
	return cmd
}

func buildEnsureArgs(cmd *cobra.Command, cfg config.Config, configPath string, maxRounds int, pause time.Duration, parallel int, dryRun bool) []string {
	args := []string{"loop"}
	if configPath != "" {
		if abs, err := filepath.Abs(configPath); err == nil {
			configPath = abs
		}
		args = append(args, "--config", configPath)
	} else if cfg.SourcePath != "" {
		if abs, err := filepath.Abs(cfg.SourcePath); err == nil {
			args = append(args, "--config", abs)
		} else {
			args = append(args, "--config", cfg.SourcePath)
		}
	}
	if cmd.Flags().Changed("max-rounds") {
		args = append(args, "--max-rounds", strconv.Itoa(maxRounds))
	}
	if cmd.Flags().Changed("pause") {
		args = append(args, "--pause", pause.String())
	}
	if cmd.Flags().Changed("parallel") {
		args = append(args, "--parallel", strconv.Itoa(parallel))
	}
	if dryRun {
		args = append(args, "--dry-run")
	}
	if f := cmd.Flag("log-level"); f != nil && f.Changed {
		args = append(args, "--log-level", f.Value.String())
	}
	return args
}

func cliLoopRunner(exec executor.Executor) loop.RoundRunner {
	return loop.RoundFunc(func(ctx context.Context, cfg config.Config, round int) loop.RoundResult {
		started := time.Now().UTC()
		res := loop.RoundResult{Round: round, StartedAt: started}

		scanDoc, err := loadFullScan(ctx, cfg, exec)
		if err != nil {
			res.EndedAt = time.Now().UTC()
			res.Err = err.Error()
			return res
		}
		res.Scan = scanDoc
		res.Scanned = len(scanDoc.PRs)

		out, err := dispatchWithDoc(ctx, exec, cfg, scanDoc, nil, dispatchModes{}, "", false)
		if out != nil {
			res.Dispatch = *out
		}
		if err != nil {
			res.EndedAt = time.Now().UTC()
			if errors.Is(err, dispatch.ErrLeaseRequired) {
				res.FatalErr = dispatchExit(err, cfg)
			}
			res.Err = err.Error()
			return res
		}
		if cfg.Policy.CIFix != config.PolicyOff {
			inboxDoc, inboxErr := processCIFixInboxWithDoc(ctx, exec, cfg, scanDoc, nil, false, dispatchedPRSet(out))
			if inboxDoc != nil && len(inboxDoc.Results) > 0 {
				res.CIFixInbox = inboxDoc.Results
			}
			if inboxErr != nil {
				if errors.Is(inboxErr, dispatch.ErrLeaseRequired) {
					res.FatalErr = dispatchExit(inboxErr, cfg)
				}
				res.Err = inboxErr.Error()
			}
		}
		res.EndedAt = time.Now().UTC()
		return res
	})
}

func dispatchedPRSet(doc *dispatch.Document) map[string]bool {
	if doc == nil {
		return nil
	}
	var out map[string]bool
	for _, r := range doc.Results {
		switch r.Action {
		case dispatch.ActionDispatched, dispatch.ActionDispatchedTimeout,
			dispatch.ActionDispatchedBlocked, dispatch.ActionOpenedTabAndDispatched:
			if out == nil {
				out = make(map[string]bool)
			}
			out[fmt.Sprintf("%s#%d", r.Repo, r.Number)] = true
		}
	}
	return out
}
