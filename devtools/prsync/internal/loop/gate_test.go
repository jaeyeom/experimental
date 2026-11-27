package loop

import (
	"bytes"
	"context"
	"encoding/json"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	executor "github.com/jaeyeom/go-cmdexec"
)

func writeExecScript(t *testing.T, dir, name, body string) string {
	t.Helper()
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, []byte("#!/bin/sh\n"+body), 0o700); err != nil { //nolint:gosec // executable test fixture
		t.Fatal(err)
	}
	return p
}

func TestCheckPreRoundGate(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 10, 0, 0, 0, time.UTC)

	t.Run("exit 0 passes", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		script := writeExecScript(t, dir, "ok.sh", "echo ok\nexit 0\n")
		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{script}

		got := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if !got.Checked || got.Gated {
			t.Fatalf("got = %+v, want Checked=true Gated=false", got)
		}
	})

	t.Run("direct argv does not invoke sh -c", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		pwned := filepath.Join(dir, "pwned")
		argsOut := filepath.Join(dir, "args.txt")
		script := writeExecScript(t, dir, "echoargs.sh", "printf '%s\\n' \"$@\" > "+argsOut+"\nexit 0\n")
		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{script, "safe; touch " + pwned}

		got := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if got.Gated {
			t.Fatalf("got = %+v, want ungated", got)
		}
		if _, err := os.Stat(pwned); err == nil {
			t.Fatal("shell metacharacter executed; command ran through shell")
		}
		raw, err := os.ReadFile(argsOut)
		if err != nil {
			t.Fatal(err)
		}
		if strings.TrimSpace(string(raw)) != "safe; touch "+pwned {
			t.Fatalf("argv[1] = %q", string(raw))
		}
	})

	t.Run("non-zero uses first non-empty stdout line and default pause", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		script := writeExecScript(t, dir, "gate.sh", "printf '\\nquota low (4%% left)\\nmore detail\\n'\nexit 2\n")
		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{script}
		cfg.LoopGatePause = 7 * time.Minute

		got := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if !got.Checked || !got.Gated || got.Reason != "quota low (4% left)" || got.Pause != 7*time.Minute {
			t.Fatalf("got = %+v, want gated reason='quota low (4%% left)' pause=7m", got)
		}
	})

	t.Run("retry_after duration honored and capped", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		script := writeExecScript(t, dir, "gate.sh", "printf 'quota low\\nretry_after=15m\\n'\nexit 1\n")
		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{script}
		cfg.LoopGatePause = 5 * time.Minute
		cfg.LoopGatePauseMax = 30 * time.Minute

		got := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if !got.Gated || got.Pause != 15*time.Minute {
			t.Fatalf("got = %+v, want pause=15m", got)
		}

		cfg.LoopGatePauseMax = 10 * time.Minute
		gotCapped := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if !gotCapped.Gated || gotCapped.Pause != 10*time.Minute {
			t.Fatalf("gotCapped = %+v, want capped pause=10m", gotCapped)
		}
	})

	t.Run("retry_after RFC3339 honored, capped, and past clamped", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		future := now.Add(45 * time.Minute).Format(time.RFC3339)
		script := writeExecScript(t, dir, "gate.sh", "printf 'rate limit\\nretry_after="+future+"\\n'\nexit 1\n")
		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{script}
		cfg.LoopGatePauseMax = 20 * time.Minute

		got := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if !got.Gated || got.Reason != "rate limit" || got.Pause != 20*time.Minute {
			t.Fatalf("got = %+v, want gated pause=20m", got)
		}

		past := now.Add(-5 * time.Minute).Format(time.RFC3339)
		scriptPast := writeExecScript(t, dir, "gate-past.sh", "printf 'rate limit\\nretry_after="+past+"\\n'\nexit 1\n")
		cfg.LoopPreRoundCmd = []string{scriptPast}
		gotPast := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if !gotPast.Gated || gotPast.Pause != 0 {
			t.Fatalf("gotPast = %+v, want gated pause=0", gotPast)
		}
	})

	t.Run("invalid retry_after falls back to loop_gate_pause", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		script := writeExecScript(t, dir, "gate-bad.sh", "printf 'quota low\\nretry_after=not-a-duration\\n'\nexit 1\n")
		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{script}
		cfg.LoopGatePause = 3 * time.Minute

		got := CheckPreRoundGate(context.Background(), executor.NewBasicExecutor(), cfg, now)
		if !got.Gated || got.Pause != 3*time.Minute {
			t.Fatalf("got = %+v, want fallback pause=3m", got)
		}
	})

	t.Run("missing command counts as gated and logs WARNING", func(t *testing.T) {
		t.Parallel()
		var logBuf bytes.Buffer
		logger := slog.New(slog.NewJSONHandler(&logBuf, &slog.HandlerOptions{Level: slog.LevelWarn}))
		ctx := runlog.WithLogger(context.Background(), logger)

		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{filepath.Join(t.TempDir(), "does-not-exist")}
		cfg.LoopGatePause = 4 * time.Minute

		got := CheckPreRoundGate(ctx, executor.NewBasicExecutor(), cfg, now)
		if !got.Checked || !got.Gated || got.Pause != 4*time.Minute || got.Reason == "" {
			t.Fatalf("got = %+v, want gated error", got)
		}
		if !strings.Contains(logBuf.String(), `"level":"WARN"`) || !strings.Contains(logBuf.String(), "loop_pre_round_gate_error") {
			t.Fatalf("expected WARN log for missing command, got %q", logBuf.String())
		}
	})

	t.Run("timeout counts as gated and logs WARNING", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		script := writeExecScript(t, dir, "slow.sh", "sleep 2\nexit 0\n")
		var logBuf bytes.Buffer
		logger := slog.New(slog.NewJSONHandler(&logBuf, &slog.HandlerOptions{Level: slog.LevelWarn}))
		ctx := runlog.WithLogger(context.Background(), logger)

		cfg := config.Defaults()
		cfg.LoopPreRoundCmd = []string{script}
		cfg.LoopPreRoundTimeout = 50 * time.Millisecond
		cfg.LoopGatePause = 6 * time.Minute

		got := CheckPreRoundGate(ctx, executor.NewBasicExecutor(), cfg, now)
		if !got.Checked || !got.Gated || got.Pause != 6*time.Minute {
			t.Fatalf("got = %+v, want gated on timeout", got)
		}
		if !strings.Contains(logBuf.String(), `"level":"WARN"`) || !strings.Contains(logBuf.String(), "loop_pre_round_gate_error") {
			t.Fatalf("expected WARN log for timeout, got %q", logBuf.String())
		}
	})
}

func TestLoopGatePauseHeartbeatAndEarlyWake(t *testing.T) {
	t.Parallel()

	t.Run("gated round updates heartbeat pause fields and clears on pass", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "gate-hb.config")
		clk := &fakeClock{now: time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)}
		slp := &fakeSleeper{clock: clk}
		hbPath := HeartbeatPath(filepath.Dir(cfg.StateFile), cfg.SourcePath)

		var pauseHB Heartbeat
		slp.after = func(time.Duration) {
			hb, ok, err := ReadHeartbeat(hbPath)
			if err != nil || !ok {
				t.Fatalf("ReadHeartbeat during pause = %+v, %v, %v", hb, ok, err)
			}
			pauseHB = hb
		}

		var round2HB Heartbeat
		var round1Out, round2Out RoundResult
		runner := RoundFunc(func(_ context.Context, _ config.Config, round int) RoundResult {
			if round == 1 {
				return RoundResult{
					Scanned:     2,
					GateChecked: true,
					Gated:       true,
					GateReason:  "quota 1%",
					GatePause:   12 * time.Minute,
				}
			}
			hb, _, _ := ReadHeartbeat(hbPath)
			round2HB = hb
			return RoundResult{
				Scanned:     2,
				GateChecked: true,
				Gated:       false,
			}
		})

		var stdout bytes.Buffer
		err := Run(context.Background(), cfg, runner, Options{
			MaxRounds:         2,
			HeartbeatInterval: time.Hour,
			Clock:             clk,
			Sleeper:           slp,
			Stdout:            &stdout,
			OnRound: func(r RoundResult) {
				if r.Round == 1 {
					round1Out = r
				} else {
					round2Out = r
				}
			},
		})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(slp.sleeps) != 1 || slp.sleeps[0] != 12*time.Minute {
			t.Fatalf("sleeps = %v, want [12m]", slp.sleeps)
		}
		if !round1Out.Gated || round1Out.GateReason != "quota 1%" || round2Out.Gated {
			t.Fatalf("round results = %+v / %+v", round1Out, round2Out)
		}
		if pauseHB.State != StatePaused || pauseHB.LastGateResult != GateResultGated || pauseHB.PauseReason != "quota 1%" || pauseHB.PausedUntil.IsZero() {
			t.Fatalf("pause heartbeat = %+v", pauseHB)
		}
		if round2HB.State != "running" || round2HB.PauseReason != "" || !round2HB.PausedUntil.IsZero() {
			t.Fatalf("round 2 heartbeat did not clear pause fields: %+v", round2HB)
		}
		// Check JSON output includes gated & gate_reason on round 1 only.
		lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
		var r1Map map[string]any
		if err := json.Unmarshal([]byte(lines[0]), &r1Map); err != nil {
			t.Fatal(err)
		}
		if r1Map["gated"] != true || r1Map["gate_reason"] != "quota 1%" {
			t.Fatalf("round 1 JSON = %v", r1Map)
		}
	})

	t.Run("WakeCh wakes paused loop early", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "wake-sig.config")
		wakeCh := make(chan struct{}, 1)
		hbPath := HeartbeatPath(filepath.Dir(cfg.StateFile), cfg.SourcePath)

		var mu sync.Mutex
		rounds := 0
		runner := RoundFunc(func(_ context.Context, _ config.Config, round int) RoundResult {
			mu.Lock()
			rounds++
			mu.Unlock()
			if round == 1 {
				go func() {
					// Wait briefly until heartbeat enters paused state, then signal wake.
					for i := 0; i < 100; i++ {
						if hb, ok, err := ReadHeartbeat(hbPath); err == nil && ok && hb.State == StatePaused {
							wakeCh <- struct{}{}
							return
						}
						time.Sleep(5 * time.Millisecond)
					}
					wakeCh <- struct{}{}
				}()
				return RoundResult{
					GateChecked: true,
					Gated:       true,
					GateReason:  "quota low",
					GatePause:   30 * time.Second,
				}
			}
			return RoundResult{GateChecked: true}
		})

		start := time.Now()
		err := Run(context.Background(), cfg, runner, Options{
			MaxRounds:         2,
			HeartbeatInterval: time.Hour,
			WakeCh:            wakeCh,
		})
		elapsed := time.Since(start)
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if rounds != 2 {
			t.Fatalf("rounds = %d, want 2", rounds)
		}
		if elapsed > 3*time.Second {
			t.Fatalf("wake took %s, expected instant wake from 30s pause", elapsed)
		}
	})

	t.Run("AgentbusWakeWaiter wakes early on message and cancels cleanly", func(t *testing.T) {
		t.Parallel()
		dir := t.TempDir()
		argsFile := filepath.Join(dir, "args.txt")
		fakeBus := writeExecScript(t, dir, "agentbus",
			"printf '%s\\n' \"$*\" > "+argsFile+"\n"+
				"sleep 0.03\n"+
				"printf '{\"ts\":\"2026-11-27T12:00:01.000Z\",\"event\":\"send\",\"name\":\"prsync-wake\",\"msg_id\":\"m-1\"}\\n'\n"+
				"sleep 10\n",
		)
		cfg := testCfg(t, "wake-bus.config")
		cfg.AgentbusBin = fakeBus
		cfg.LoopWakeAgentbusName = "prsync-wake"

		rounds := 0
		runner := RoundFunc(func(_ context.Context, _ config.Config, round int) RoundResult {
			rounds++
			if round == 1 {
				return RoundResult{
					GateChecked: true,
					Gated:       true,
					GateReason:  "quota low",
					GatePause:   30 * time.Second,
				}
			}
			return RoundResult{GateChecked: true}
		})

		start := time.Now()
		err := Run(context.Background(), cfg, runner, Options{
			MaxRounds:         2,
			HeartbeatInterval: time.Hour,
			WakeWaiter:        AgentbusWakeWaiter{Bin: cfg.AgentbusBin, Name: cfg.LoopWakeAgentbusName},
		})
		elapsed := time.Since(start)
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if rounds != 2 {
			t.Fatalf("rounds = %d, want 2", rounds)
		}
		if elapsed > 3*time.Second {
			t.Fatalf("agentbus wake took %s, expected early wake", elapsed)
		}
		rawArgs, err := os.ReadFile(argsFile)
		if err != nil {
			t.Fatal(err)
		}
		gotArgs := string(rawArgs)
		for _, want := range []string{"log", "--follow", "--name prsync-wake", "--event send", "--since ", "--json"} {
			if !strings.Contains(gotArgs, want) {
				t.Fatalf("agentbus args = %q, missing %q", gotArgs, want)
			}
		}
	})

	t.Run("missing agentbus falls back to timer and logs WARNING", func(t *testing.T) {
		t.Parallel()
		var logBuf bytes.Buffer
		logger := slog.New(slog.NewJSONHandler(&logBuf, &slog.HandlerOptions{Level: slog.LevelWarn}))
		ctx := runlog.WithLogger(context.Background(), logger)

		cfg := testCfg(t, "wake-missing-bus.config")
		rounds := 0
		runner := RoundFunc(func(_ context.Context, _ config.Config, round int) RoundResult {
			rounds++
			if round == 1 {
				return RoundResult{
					GateChecked: true,
					Gated:       true,
					GateReason:  "quota low",
					GatePause:   30 * time.Millisecond,
				}
			}
			return RoundResult{GateChecked: true}
		})

		err := Run(ctx, cfg, runner, Options{
			MaxRounds:         2,
			HeartbeatInterval: time.Hour,
			WakeWaiter:        AgentbusWakeWaiter{Bin: filepath.Join(t.TempDir(), "missing-agentbus"), Name: "prsync-wake"},
		})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if rounds != 2 {
			t.Fatalf("rounds = %d, want 2", rounds)
		}
		if !strings.Contains(logBuf.String(), `"level":"WARN"`) || !strings.Contains(logBuf.String(), "loop_wake_waiter_error") {
			t.Fatalf("expected WARN log for missing agentbus, got %q", logBuf.String())
		}
	})
}

func TestPausedHeartbeatFreshnessAndDoctorInspect(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 13, 0, 0, 0, time.UTC)
	cfg := testCfg(t, "paused-doc.config")
	stateDir := filepath.Dir(cfg.StateFile)
	hbPath := HeartbeatPath(stateDir, cfg.SourcePath)

	// Paused for 30m, UpdatedAt was 15m ago (> DefaultStaleAfter=10m), but PausedUntil is 15m in the future.
	if err := WriteHeartbeat(hbPath, Heartbeat{
		PID:            4242,
		ConfigPath:     cfg.SourcePath,
		Round:          7,
		State:          StatePaused,
		StartedAt:      now.Add(-time.Hour),
		UpdatedAt:      now.Add(-15 * time.Minute),
		LastGateResult: GateResultGated,
		LastGateReason: "quota exhausted",
		PauseReason:    "quota exhausted",
		PausedUntil:    now.Add(15 * time.Minute),
	}); err != nil {
		t.Fatal(err)
	}

	// Ensure must see it as healthy and NOT spawn a second loop.
	spawned := 0
	ensRes, err := Ensure(cfg, EnsureOptions{
		StaleAfter: DefaultStaleAfter,
		Clock:      &fakeClock{now: now},
		IsAlive:    func(pid int) bool { return pid == 4242 },
		Spawner: func([]string) (int, error) {
			spawned++
			return 9999, nil
		},
	})
	if err != nil || spawned != 0 || ensRes.Action != EnsureHealthy {
		t.Fatalf("Ensure paused = %+v (spawned=%d, err=%v), want healthy", ensRes, spawned, err)
	}

	// Inspect must report StateRunning and show last gate result & pause reason.
	rep := Inspect(stateDir, cfg.SourcePath, true, now, DefaultStaleAfter, func(pid int) bool { return pid == 4242 })
	if rep.State != StateRunning {
		t.Fatalf("Inspect paused state = %+v, want %q", rep, StateRunning)
	}
	if rep.LastGateResult != GateResultGated || rep.PauseReason != "quota exhausted" {
		t.Fatalf("Inspect missing gate/pause fields: %+v", rep)
	}
	for _, want := range []string{"paused", "last_gate=gated", `pause_reason="quota exhausted"`} {
		if !strings.Contains(rep.Detail, want) {
			t.Fatalf("Inspect detail = %q, missing %q", rep.Detail, want)
		}
	}

	// Dead PID while paused must still be reported stale / restarted.
	staleRep := Inspect(stateDir, cfg.SourcePath, true, now, DefaultStaleAfter, func(int) bool { return false })
	if staleRep.State != StateStale {
		t.Fatalf("Inspect dead paused pid = %+v, want %q", staleRep, StateStale)
	}

	// Well past PausedUntil + StaleAfter without heartbeat update must become stale.
	expiredRep := Inspect(stateDir, cfg.SourcePath, true, now.Add(30*time.Minute), DefaultStaleAfter, func(pid int) bool { return pid == 4242 })
	if expiredRep.State != StateStale {
		t.Fatalf("Inspect expired pause = %+v, want %q", expiredRep, StateStale)
	}
}
