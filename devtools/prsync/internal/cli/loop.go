package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/loop"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/policy"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
	"golang.org/x/sys/unix"
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
			wakeCh, stopWake := startUSR1Forwarder()
			defer stopWake()
			var wakeWaiter loop.WakeWaiter
			if cfg.LoopWakeAgentbusName != "" {
				wakeWaiter = loop.AgentbusWakeWaiter{
					Bin:  cfg.AgentbusBin,
					Name: cfg.LoopWakeAgentbusName,
				}
			}
			runner := cliLoopRunner(exec)
			err = loop.Run(ctx, cfg, runner, loop.Options{
				MaxRounds:  maxRounds,
				Pause:      pause,
				WakeCh:     wakeCh,
				WakeWaiter: wakeWaiter,
				Stdout:     stdout,
				Stderr:     cmd.ErrOrStderr(),
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

func startUSR1Forwarder() (<-chan struct{}, func()) {
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, unix.SIGUSR1)
	wakeCh := make(chan struct{}, 1)
	stopCh := make(chan struct{})
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		for {
			select {
			case <-stopCh:
				return
			case <-sigCh:
				select {
				case wakeCh <- struct{}{}:
				default:
				}
			}
		}
	}()
	return wakeCh, func() {
		signal.Stop(sigCh)
		close(stopCh)
		wg.Wait()
	}
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

		if checkLoopPreRoundGate(ctx, exec, cfg, &res) {
			finalizeLoopRound(ctx, exec, cfg, &res)
			return res
		}

		out, err := dispatchWithDoc(ctx, exec, cfg, scanDoc, nil, dispatchModes{}, "", false)
		if out != nil {
			res.Dispatch = *out
		}
		if err != nil {
			if errors.Is(err, dispatch.ErrLeaseRequired) {
				res.FatalErr = dispatchExit(err, cfg)
			}
			res.Err = err.Error()
			finalizeLoopRound(ctx, exec, cfg, &res)
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
		if cfg.Policy.BotRereview != config.PolicyOff {
			botDecisions, botErr := runLoopBotRereview(ctx, exec, cfg, scanDoc)
			if len(botDecisions) > 0 {
				res.BotRereview = botDecisions
			}
			if botErr != nil {
				if res.Err != "" {
					res.Err += "; " + botErr.Error()
				} else {
					res.Err = botErr.Error()
				}
			}
		}
		finalizeLoopRound(ctx, exec, cfg, &res)
		return res
	})
}

func checkLoopPreRoundGate(ctx context.Context, exec executor.Executor, cfg config.Config, res *loop.RoundResult) bool {
	if len(cfg.LoopPreRoundCmd) == 0 {
		return false
	}
	gate := loop.CheckPreRoundGate(ctx, exec, cfg, time.Now().UTC())
	res.GateChecked = gate.Checked
	if !gate.Gated {
		return false
	}
	res.Gated = true
	res.GateReason = gate.Reason
	res.GatePause = gate.Pause
	res.Dispatch = dispatch.Document{
		GeneratedAt: time.Now().UTC().Format(time.RFC3339),
		DryRun:      cfg.DryRun,
		Results:     []dispatch.Item{},
	}
	return true
}

func finalizeLoopRound(ctx context.Context, exec executor.Executor, cfg config.Config, res *loop.RoundResult) {
	res.EndedAt = time.Now().UTC()
	if res.FatalErr == nil &&
		cfg.Policy.BotRereview == config.PolicyOff &&
		strings.TrimSpace(cfg.Policy.BotRereviewLogin) != "" &&
		strings.TrimSpace(cfg.Policy.BotRereviewBody) != "" {
		res.RereviewPreview = runLoopBotRereviewPreview(ctx, exec, cfg, res.Scan)
	}

	var blockedSince map[string]time.Time
	if cfg.StateFile != "" {
		tracker, err := loop.LoadBlockedTracker(loop.BlockedStatePath(cfg.StateDir(), cfg.SourcePath))
		if err != nil {
			runlog.FromContext(ctx).Warn("loop_blocked_state_load", "error", err.Error())
		} else {
			tracker.Update(res.Scan, *res, res.EndedAt)
			if err := tracker.Save(); err != nil {
				runlog.FromContext(ctx).Warn("loop_blocked_state_save", "error", err.Error())
			}
			blockedSince = tracker.Since
		}
	}

	var lgr *ledger.Store
	if cfg.Policy.LedgerFileConfigured || cfg.StateFile != "" {
		if path := cfg.ResolvedLedgerFile(); path != "" {
			lgr = &ledger.Store{Path: path}
		}
	}
	loop.PopulateNeedsJudgment(ctx, res, loop.JudgmentInput{
		Config:       cfg,
		Now:          res.EndedAt,
		Ledger:       lgr,
		BlockedSince: blockedSince,
	})
}

func runLoopBotRereviewPreview(ctx context.Context, exec executor.Executor, cfg config.Config, scanDoc scan.Document) []policy.Decision {
	previewCfg := cfg
	previewCfg.Policy.BotRereview = config.PolicyDryRun
	previewCfg.DryRun = true
	decisions, err := runLoopBotRereview(ctx, exec, previewCfg, scanDoc)
	if err != nil {
		runlog.FromContext(ctx).Warn("loop_rereview_due_check", "error", err.Error())
	}
	return decisions
}

func runLoopBotRereview(ctx context.Context, exec executor.Executor, cfg config.Config, scanDoc scan.Document) ([]policy.Decision, error) {
	botCfg := cfg
	botCfg.Policy.CatchUp = config.PolicyOff
	botCfg.Policy.Rebase = config.PolicyOff
	botCfg.Policy.NitReply = config.PolicyOff
	botCfg.Policy.CIFix = config.PolicyOff
	botCfg.Policy.QueueFix = config.PolicyOff

	eng := &policy.Engine{
		Ledger: &ledger.Store{Path: cfg.ResolvedLedgerFile()},
		Guard:  policy.ExecGuardRunner{Exec: exec},
		GH:     gh.NewClient(exec, cfg.GHBin),
	}
	doc, err := eng.Run(ctx, botCfg, scanDoc, policy.RunOptions{DryRun: cfg.DryRun})
	if err != nil {
		return doc.Decisions, fmt.Errorf("bot_rereview: %w", err)
	}
	for _, d := range doc.Decisions {
		if d.Decision == policy.DecisionError {
			return doc.Decisions, fmt.Errorf("bot_rereview %s: %s", d.PR, d.Detail)
		}
	}
	return doc.Decisions, nil
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
