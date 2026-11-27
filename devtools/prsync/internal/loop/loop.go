// Package loop runs prsync scan+dispatch rounds back-to-back with single-instance locking and heartbeats.
package loop

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/policy"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// Default timing bounds for loop heartbeat maintenance and stale detection.
const (
	DefaultHeartbeatInterval = 30 * time.Second
	DefaultStaleAfter        = 10 * time.Minute
)

// Clock supplies current time for loop heartbeats and scheduling.
type Clock interface {
	Now() time.Time
}

// Sleeper pauses between loop rounds.
type Sleeper interface {
	Sleep(ctx context.Context, d time.Duration) error
}

type realClock struct{}

func (realClock) Now() time.Time { return time.Now() }

type realSleeper struct{}

func (realSleeper) Sleep(ctx context.Context, d time.Duration) error {
	t := time.NewTimer(d)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return fmt.Errorf("loop sleep: %w", ctx.Err())
	case <-t.C:
		return nil
	}
}

// RoundResult captures the outcome of one scan+dispatch round in loop mode.
type RoundResult struct {
	Round              int               `json:"round"`
	StartedAt          time.Time         `json:"started_at"` //nolint:tagliatelle // outbound contract
	EndedAt            time.Time         `json:"ended_at"`   //nolint:tagliatelle // outbound contract
	Scan               scan.Document     `json:"-"`
	Scanned            int               `json:"scanned"`
	Dispatch           dispatch.Document `json:"dispatch"`
	CIFixInbox         []dispatch.Item   `json:"ci_fix_inbox,omitempty"` //nolint:tagliatelle // outbound contract
	BotRereview        []policy.Decision `json:"bot_rereview,omitempty"` //nolint:tagliatelle // outbound contract
	RereviewPreview    []policy.Decision `json:"-"`
	NeedsJudgment      []JudgmentItem    `json:"needs_judgment"`       //nolint:tagliatelle // outbound contract
	NeedsJudgmentCount int               `json:"needs_judgment_count"` //nolint:tagliatelle // outbound contract
	Err                string            `json:"error,omitempty"`
	FatalErr           error             `json:"-"`
}

// RoundRunner executes a single scan+dispatch round. Implementations may wrap
// another RoundRunner to add pre-round gates or prompt transformations.
type RoundRunner interface {
	RunRound(ctx context.Context, cfg config.Config, round int) RoundResult
}

// RoundFunc adapts a function into a RoundRunner.
type RoundFunc func(ctx context.Context, cfg config.Config, round int) RoundResult

// RunRound calls f(ctx, cfg, round).
func (f RoundFunc) RunRound(ctx context.Context, cfg config.Config, round int) RoundResult {
	return f(ctx, cfg, round)
}

// Options configures Run.
type Options struct {
	MaxRounds         int
	Pause             time.Duration
	HeartbeatInterval time.Duration
	Clock             Clock
	Sleeper           Sleeper
	Stdout            io.Writer
	Stderr            io.Writer
	OnRound           func(RoundResult)
}

// Run executes scan+dispatch rounds until ctx is canceled (stopping cleanly
// after the current round finishes), MaxRounds is reached, or a fatal setup
// error occurs. Only one loop per config may run at a time.
func Run(ctx context.Context, cfg config.Config, runner RoundRunner, opts Options) error {
	if runner == nil {
		return errors.New("loop: nil round runner")
	}
	opts = defaultLoopOptions(opts)

	stateDir := filepath.Dir(cfg.StateFile)
	lock, err := AcquireLock(LockPath(stateDir, cfg.SourcePath))
	if err != nil {
		return err
	}
	defer func() { _ = lock.Close() }()

	hbPath := HeartbeatPath(stateDir, cfg.SourcePath)
	now := opts.Clock.Now().UTC()
	hb := &heartbeatState{
		path: hbPath,
		hb: Heartbeat{
			PID:        os.Getpid(),
			ConfigPath: NormalizedConfigPath(cfg.SourcePath),
			Round:      0,
			State:      "starting",
			StartedAt:  now,
			UpdatedAt:  now,
		},
	}
	if err := hb.write(); err != nil {
		return err
	}
	stopTicker := hb.startTicker(opts.HeartbeatInterval, opts.Clock)
	defer func() {
		stopTicker()
		_ = RemoveHeartbeat(hbPath)
	}()

	log := runlog.FromContext(ctx)
	log.Info("loop_start", "pid", hb.hb.PID, "config_path", cfg.SourcePath, "max_rounds", opts.MaxRounds, "pause", opts.Pause.String())

	for round := 1; opts.MaxRounds <= 0 || round <= opts.MaxRounds; round++ {
		if ctx.Err() != nil {
			log.Info("loop_stop", "reason", "signal_before_round", "round", round-1)
			return nil
		}
		done, err := executeRound(ctx, cfg, runner, hb, round, opts)
		if err != nil || done {
			return err
		}
	}
	return nil
}

func defaultLoopOptions(opts Options) Options {
	if opts.Clock == nil {
		opts.Clock = realClock{}
	}
	if opts.Sleeper == nil {
		opts.Sleeper = realSleeper{}
	}
	if opts.HeartbeatInterval <= 0 {
		opts.HeartbeatInterval = DefaultHeartbeatInterval
	}
	return opts
}

func executeRound(ctx context.Context, cfg config.Config, runner RoundRunner, hb *heartbeatState, round int, opts Options) (bool, error) {
	log := runlog.FromContext(ctx)
	roundStart := opts.Clock.Now().UTC()
	if err := hb.update(func(h *Heartbeat) {
		h.Round = round
		h.State = "running"
		h.LastRoundStart = roundStart
		h.UpdatedAt = roundStart
	}); err != nil {
		return false, err
	}

	// Finish the in-progress round before honoring SIGTERM/SIGINT.
	res := runner.RunRound(context.WithoutCancel(ctx), cfg, round)
	res.Round = round
	if res.StartedAt.IsZero() {
		res.StartedAt = roundStart
	}
	roundEnd := opts.Clock.Now().UTC()
	if res.EndedAt.IsZero() {
		res.EndedAt = roundEnd
	}
	if err := hb.update(func(h *Heartbeat) {
		h.Round = round
		h.State = "idle"
		h.LastRoundEnd = roundEnd
		h.UpdatedAt = roundEnd
	}); err != nil {
		return false, err
	}

	if err := emitRoundResult(opts, &res); err != nil {
		return false, err
	}
	log.Info("loop_round_end", "round", round, "scanned", res.Scanned, "results", len(res.Dispatch.Results), "needs_judgment", res.NeedsJudgmentCount, "error", res.Err)

	if res.FatalErr != nil {
		return false, res.FatalErr
	}
	if ctx.Err() != nil {
		log.Info("loop_stop", "reason", "signal_after_round", "round", round)
		return true, nil
	}
	if opts.MaxRounds > 0 && round >= opts.MaxRounds {
		log.Info("loop_stop", "reason", "max_rounds", "round", round)
		return true, nil
	}
	return pauseAfterRound(ctx, hb, round, opts)
}

func pauseAfterRound(ctx context.Context, hb *heartbeatState, round int, opts Options) (bool, error) {
	if opts.Pause <= 0 {
		return false, nil
	}
	sleepAt := opts.Clock.Now().UTC()
	if err := hb.update(func(h *Heartbeat) {
		h.State = "sleeping"
		h.UpdatedAt = sleepAt
	}); err != nil {
		return false, err
	}
	if err := opts.Sleeper.Sleep(ctx, opts.Pause); err != nil {
		runlog.FromContext(ctx).Info("loop_stop", "reason", "signal_during_pause", "round", round)
		return true, nil
	}
	return false, nil
}

func emitRoundResult(opts Options, res *RoundResult) error {
	if res.NeedsJudgment == nil {
		res.NeedsJudgment = []JudgmentItem{}
	} else {
		sort.Slice(res.NeedsJudgment, func(i, j int) bool {
			if res.NeedsJudgment[i].Repo != res.NeedsJudgment[j].Repo {
				return res.NeedsJudgment[i].Repo < res.NeedsJudgment[j].Repo
			}
			if res.NeedsJudgment[i].PR != res.NeedsJudgment[j].PR {
				return res.NeedsJudgment[i].PR < res.NeedsJudgment[j].PR
			}
			return res.NeedsJudgment[i].Reason < res.NeedsJudgment[j].Reason
		})
	}
	res.NeedsJudgmentCount = len(res.NeedsJudgment)
	if opts.OnRound != nil {
		opts.OnRound(*res)
	}
	if opts.Stdout != nil {
		line, err := json.Marshal(*res)
		if err != nil {
			return fmt.Errorf("encode round %d result: %w", res.Round, err)
		}
		if _, err := fmt.Fprintf(opts.Stdout, "%s\n", line); err != nil {
			return fmt.Errorf("write round %d result: %w", res.Round, err)
		}
	}
	if opts.Stderr != nil && res.Err != "" {
		_, _ = fmt.Fprintf(opts.Stderr, "prsync: loop round %d: %s\n", res.Round, res.Err)
	}
	return nil
}

type heartbeatState struct {
	mu   sync.Mutex
	path string
	hb   Heartbeat
}

func (s *heartbeatState) write() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	return WriteHeartbeat(s.path, s.hb)
}

func (s *heartbeatState) update(fn func(*Heartbeat)) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	fn(&s.hb)
	return WriteHeartbeat(s.path, s.hb)
}

func (s *heartbeatState) startTicker(interval time.Duration, clock Clock) func() {
	stop := make(chan struct{})
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		t := time.NewTicker(interval)
		defer t.Stop()
		for {
			select {
			case <-stop:
				return
			case <-t.C:
				_ = s.update(func(h *Heartbeat) {
					h.UpdatedAt = clock.Now().UTC()
				})
			}
		}
	}()
	return func() {
		close(stop)
		wg.Wait()
	}
}
