package loop

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
)

type fakeClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fakeClock) Advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = c.now.Add(d)
}

type fakeSleeper struct {
	clock  *fakeClock
	sleeps []time.Duration
	after  func(roundPause time.Duration)
}

func (s *fakeSleeper) Sleep(ctx context.Context, d time.Duration) error {
	s.sleeps = append(s.sleeps, d)
	s.clock.Advance(d)
	if s.after != nil {
		s.after(d)
	}
	if err := ctx.Err(); err != nil {
		return fmt.Errorf("fake sleep: %w", err)
	}
	return nil
}

func testCfg(t *testing.T, configName string) config.Config {
	t.Helper()
	dir := t.TempDir()
	cfg := config.Defaults()
	cfg.DryRun = false
	cfg.StateFile = filepath.Join(dir, "state.json")
	if configName != "" {
		cfg.SourcePath = filepath.Join(dir, configName)
	}
	return cfg
}

func TestLoopRunsRoundsBackToBackAndCleansUpHeartbeat(t *testing.T) {
	t.Parallel()

	cfg := testCfg(t, "a.config")
	clk := &fakeClock{now: time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)}
	slp := &fakeSleeper{clock: clk}
	hbPath := HeartbeatPath(filepath.Dir(cfg.StateFile), cfg.SourcePath)

	var seenHBs []Heartbeat
	var stdout bytes.Buffer
	runner := RoundFunc(func(_ context.Context, _ config.Config, round int) RoundResult {
		hb, ok, err := ReadHeartbeat(hbPath)
		if err != nil || !ok {
			t.Fatalf("round %d ReadHeartbeat = %+v, %v, %v", round, hb, ok, err)
		}
		seenHBs = append(seenHBs, hb)
		clk.Advance(time.Minute)
		return RoundResult{
			Scanned:  round,
			Dispatch: dispatch.Document{Results: []dispatch.Item{}},
		}
	})

	err := Run(context.Background(), cfg, runner, Options{
		MaxRounds:         3,
		Pause:             0,
		HeartbeatInterval: time.Hour,
		Clock:             clk,
		Sleeper:           slp,
		Stdout:            &stdout,
	})
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if len(slp.sleeps) != 0 {
		t.Fatalf("pause=0 slept %v, want back-to-back with no sleeps", slp.sleeps)
	}
	if len(seenHBs) != 3 {
		t.Fatalf("rounds = %d, want 3", len(seenHBs))
	}
	if seenHBs[0].Round != 1 || seenHBs[1].Round != 2 || seenHBs[2].Round != 3 {
		t.Fatalf("heartbeat rounds = %+v", seenHBs)
	}
	if seenHBs[1].LastRoundEnd.IsZero() || !seenHBs[1].LastRoundEnd.Equal(seenHBs[0].LastRoundStart.Add(time.Minute)) {
		t.Fatalf("round 2 heartbeat missing round 1 end time: %+v", seenHBs[1])
	}
	if _, ok, err := ReadHeartbeat(hbPath); err != nil || ok {
		t.Fatalf("clean stop left heartbeat on disk: ok=%v err=%v", ok, err)
	}
	if lines := strings.Count(strings.TrimSpace(stdout.String()), "\n") + 1; lines != 3 {
		t.Fatalf("stdout lines = %d, want 3:\n%s", lines, stdout.String())
	}
}

func TestLoopStopsCleanlyOnSignalAfterCurrentRound(t *testing.T) {
	t.Parallel()

	cfg := testCfg(t, "sig.config")
	clk := &fakeClock{now: time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)}
	slp := &fakeSleeper{clock: clk}

	ctx, cancel := context.WithCancel(context.Background())
	roundsRun := 0
	var roundCtxCanceled bool
	runner := RoundFunc(func(rctx context.Context, _ config.Config, _ int) RoundResult {
		roundsRun++
		cancel()
		roundCtxCanceled = rctx.Err() != nil
		return RoundResult{}
	})

	err := Run(ctx, cfg, runner, Options{
		MaxRounds:         10,
		HeartbeatInterval: time.Hour,
		Clock:             clk,
		Sleeper:           slp,
	})
	if err != nil {
		t.Fatalf("Run() after SIGTERM/cancel = %v, want nil clean exit", err)
	}
	if roundsRun != 1 {
		t.Fatalf("roundsRun = %d, want 1 (stop after current round)", roundsRun)
	}
	if roundCtxCanceled {
		t.Fatal("in-progress round context was canceled when signal arrived")
	}
}

func TestLoopRefusesSecondInstancePerConfigAndAllowsDifferentConfig(t *testing.T) {
	t.Parallel()

	cfgA := testCfg(t, "a.config")
	cfgB := cfgA
	cfgB.SourcePath = filepath.Join(filepath.Dir(cfgA.StateFile), "b.config")

	entered := make(chan struct{})
	release := make(chan struct{})
	firstErr := make(chan error, 1)

	go func() {
		firstErr <- Run(context.Background(), cfgA, RoundFunc(func(_ context.Context, _ config.Config, _ int) RoundResult {
			close(entered)
			<-release
			return RoundResult{}
		}), Options{MaxRounds: 1, HeartbeatInterval: time.Hour})
	}()

	<-entered
	// Same config must refuse immediately while first loop holds the lock.
	err := Run(context.Background(), cfgA, RoundFunc(func(_ context.Context, _ config.Config, _ int) RoundResult {
		t.Fatal("second loop on same config should not execute a round")
		return RoundResult{}
	}), Options{MaxRounds: 1, HeartbeatInterval: time.Hour})
	if !errors.Is(err, ErrAlreadyRunning) {
		t.Fatalf("second same-config Run() = %v, want ErrAlreadyRunning", err)
	}

	// Different config in the same state_dir succeeds concurrently.
	var ranB int
	if err := Run(context.Background(), cfgB, RoundFunc(func(_ context.Context, _ config.Config, _ int) RoundResult {
		ranB++
		return RoundResult{}
	}), Options{MaxRounds: 1, HeartbeatInterval: time.Hour}); err != nil {
		t.Fatalf("different config Run() error = %v", err)
	}
	if ranB != 1 {
		t.Fatalf("ranB = %d, want 1", ranB)
	}

	close(release)
	if err := <-firstErr; err != nil {
		t.Fatalf("first loop error = %v", err)
	}
}

func TestEnsureHealthyVsStaleAndDead(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 26, 13, 0, 0, 0, time.UTC)
	clk := &fakeClock{now: now}

	t.Run("healthy fresh heartbeat exits without spawning", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "h.config")
		hbPath := HeartbeatPath(filepath.Dir(cfg.StateFile), cfg.SourcePath)
		if err := WriteHeartbeat(hbPath, Heartbeat{
			PID:       4242,
			Round:     3,
			State:     "running",
			StartedAt: now.Add(-20 * time.Minute),
			UpdatedAt: now.Add(-2 * time.Minute),
		}); err != nil {
			t.Fatal(err)
		}
		spawned := 0
		res, err := Ensure(cfg, EnsureOptions{
			StaleAfter: 10 * time.Minute,
			Clock:      clk,
			IsAlive:    func(pid int) bool { return pid == 4242 },
			Spawner: func([]string) (int, error) {
				spawned++
				return 9999, nil
			},
		})
		if err != nil {
			t.Fatalf("Ensure() error = %v", err)
		}
		if spawned != 0 || res.Action != EnsureHealthy || res.PID != 4242 || res.Round != 3 {
			t.Fatalf("Ensure() = %+v (spawned=%d), want healthy", res, spawned)
		}
	})

	t.Run("stale heartbeat restarts detached", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "s.config")
		hbPath := HeartbeatPath(filepath.Dir(cfg.StateFile), cfg.SourcePath)
		if err := WriteHeartbeat(hbPath, Heartbeat{
			PID:       4242,
			Round:     3,
			State:     "running",
			StartedAt: now.Add(-2 * time.Hour),
			UpdatedAt: now.Add(-30 * time.Minute),
		}); err != nil {
			t.Fatal(err)
		}
		var gotArgs []string
		res, err := Ensure(cfg, EnsureOptions{
			StaleAfter: 10 * time.Minute,
			Args:       []string{"loop", "--config", cfg.SourcePath},
			Clock:      clk,
			IsAlive:    func(int) bool { return true },
			Spawner: func(args []string) (int, error) {
				gotArgs = append([]string(nil), args...)
				return 7777, nil
			},
		})
		if err != nil {
			t.Fatalf("Ensure() error = %v", err)
		}
		if res.Action != EnsureStarted || res.PID != 7777 {
			t.Fatalf("Ensure() = %+v, want started pid 7777", res)
		}
		if len(gotArgs) != 3 || gotArgs[0] != "loop" {
			t.Fatalf("spawned args = %v", gotArgs)
		}
	})

	t.Run("dead pid starts detached", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "d.config")
		hbPath := HeartbeatPath(filepath.Dir(cfg.StateFile), cfg.SourcePath)
		if err := WriteHeartbeat(hbPath, Heartbeat{
			PID:       4242,
			Round:     1,
			State:     "running",
			StartedAt: now.Add(-time.Minute),
			UpdatedAt: now.Add(-10 * time.Second),
		}); err != nil {
			t.Fatal(err)
		}
		res, err := Ensure(cfg, EnsureOptions{
			StaleAfter: 10 * time.Minute,
			Clock:      clk,
			IsAlive:    func(int) bool { return false },
			Spawner:    func([]string) (int, error) { return 8888, nil },
		})
		if err != nil || res.Action != EnsureStarted || res.PID != 8888 {
			t.Fatalf("Ensure() = %+v, %v; want started 8888", res, err)
		}
	})

	t.Run("missing heartbeat starts detached", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "m.config")
		res, err := Ensure(cfg, EnsureOptions{
			Clock:   clk,
			Spawner: func([]string) (int, error) { return 5555, nil },
		})
		if err != nil || res.Action != EnsureStarted || res.PID != 5555 {
			t.Fatalf("Ensure() = %+v, %v; want started 5555", res, err)
		}
	})
}

func TestInspectStates(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 26, 13, 0, 0, 0, time.UTC)
	cfg := testCfg(t, "doc.config")
	stateDir := filepath.Dir(cfg.StateFile)

	// 1. Not running.
	if got := Inspect(stateDir, cfg.SourcePath, true, now, DefaultStaleAfter, nil); got.State != StateNotRunning {
		t.Fatalf("empty dir state = %+v, want %q", got, StateNotRunning)
	}

	// 2. Running.
	hbPath := HeartbeatPath(stateDir, cfg.SourcePath)
	if err := WriteHeartbeat(hbPath, Heartbeat{
		PID:        3333,
		ConfigPath: cfg.SourcePath,
		Round:      4,
		State:      "running",
		StartedAt:  now.Add(-5 * time.Minute),
		UpdatedAt:  now.Add(-15 * time.Second),
	}); err != nil {
		t.Fatal(err)
	}
	if got := Inspect(stateDir, cfg.SourcePath, true, now, DefaultStaleAfter, func(pid int) bool { return pid == 3333 }); got.State != StateRunning || !strings.Contains(got.Detail, "running") {
		t.Fatalf("live state = %+v, want running", got)
	}

	// Also visible to explicitConfig=false fallback glob from default doctor.
	if got := Inspect(stateDir, "", false, now, DefaultStaleAfter, func(pid int) bool { return pid == 3333 }); got.State != StateRunning {
		t.Fatalf("fallback glob state = %+v, want running", got)
	}

	// 3. Stale heartbeat (old timestamp).
	if got := Inspect(stateDir, cfg.SourcePath, true, now.Add(time.Hour), DefaultStaleAfter, func(int) bool { return true }); got.State != StateStale || !strings.Contains(got.Detail, "stale heartbeat") {
		t.Fatalf("stale timestamp = %+v, want stale heartbeat", got)
	}

	// 4. Stale heartbeat (dead PID).
	if got := Inspect(stateDir, cfg.SourcePath, true, now, DefaultStaleAfter, func(int) bool { return false }); got.State != StateStale || !strings.Contains(got.Detail, "stale heartbeat") {
		t.Fatalf("dead pid = %+v, want stale heartbeat", got)
	}
}
