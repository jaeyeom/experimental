package quota_test

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/quota"
	"github.com/jaeyeom/experimental/devtools/quotapace"
)

type fakeUsageReader struct {
	fn    func(ctx context.Context) (quota.ClaudeUsageReading, error)
	calls atomic.Int32
}

func (f *fakeUsageReader) Read(ctx context.Context) (quota.ClaudeUsageReading, error) {
	f.calls.Add(1)
	return f.fn(ctx)
}

func floatPtr(v float64) *float64 {
	return &v
}

func timePtr(t time.Time) *time.Time {
	return &t
}

func testConfig(t *testing.T) quota.Config {
	t.Helper()
	cfg := quota.DefaultConfig()
	cfg.QuotapaceDir = t.TempDir()
	return cfg
}

func TestClaudeGuardAllowsGreenWithinCaps(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	// Halfway through both windows so 20% (5h) and 30% (7d) are well below
	// the on-track lines (37.5% and 40%).
	fiveHourReset := now.Add(2*time.Hour + 30*time.Minute)
	sevenDayReset := now.Add(84 * time.Hour)

	reader := &fakeUsageReader{
		fn: func(context.Context) (quota.ClaudeUsageReading, error) {
			return quota.ClaudeUsageReading{
				PlanName:        "Max",
				FiveHour:        floatPtr(20),
				SevenDay:        floatPtr(30),
				FiveHourResetAt: timePtr(fiveHourReset),
				SevenDayResetAt: timePtr(sevenDayReset),
			}, nil
		},
	}

	guard, err := quota.NewClaudeGuard(testConfig(t), reader, quota.WithClock(func() time.Time { return now }))
	if err != nil {
		t.Fatalf("NewClaudeGuard: %v", err)
	}

	dec := guard.CanAccept(context.Background())
	if !dec.Allow {
		t.Fatalf("expected Allow=true, got false (reason=%q, state=%q)", dec.Reason, dec.Health.State)
	}
	if dec.Health.State != quotapace.Green {
		t.Errorf("Health.State = %q, want %q", dec.Health.State, quotapace.Green)
	}
	if dec.Health.FiveHourRemainingPct == nil || *dec.Health.FiveHourRemainingPct != 80 {
		t.Errorf("FiveHourRemainingPct = %v, want 80", dec.Health.FiveHourRemainingPct)
	}
	if dec.Health.SevenDayRemainingPct == nil || *dec.Health.SevenDayRemainingPct != 70 {
		t.Errorf("SevenDayRemainingPct = %v, want 70", dec.Health.SevenDayRemainingPct)
	}
	if dec.Health.FiveHourResetsAt == nil || !dec.Health.FiveHourResetsAt.Equal(fiveHourReset) {
		t.Errorf("FiveHourResetsAt = %v, want %v", dec.Health.FiveHourResetsAt, fiveHourReset)
	}
	if dec.Health.SevenDayResetsAt == nil || !dec.Health.SevenDayResetsAt.Equal(sevenDayReset) {
		t.Errorf("SevenDayResetsAt = %v, want %v", dec.Health.SevenDayResetsAt, sevenDayReset)
	}
}

func TestClaudeGuardBlocksOnFiveHourCap(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	reader := &fakeUsageReader{
		fn: func(context.Context) (quota.ClaudeUsageReading, error) {
			return quota.ClaudeUsageReading{
				PlanName:        "Max",
				FiveHour:        floatPtr(76),
				SevenDay:        floatPtr(20),
				FiveHourResetAt: timePtr(now.Add(2 * time.Hour)),
				SevenDayResetAt: timePtr(now.Add(84 * time.Hour)),
			}, nil
		},
	}

	cfg := testConfig(t)
	cfg.MaxFiveHourPct = 75
	guard, err := quota.NewClaudeGuard(cfg, reader, quota.WithClock(func() time.Time { return now }))
	if err != nil {
		t.Fatalf("NewClaudeGuard: %v", err)
	}

	dec := guard.CanAccept(context.Background())
	if dec.Allow {
		t.Fatalf("expected Allow=false when 5h usage (76%%) >= cap (75%%)")
	}
	if !strings.Contains(dec.Reason, "5h") {
		t.Errorf("Reason = %q, expected mention of 5h cap", dec.Reason)
	}
}

func TestClaudeGuardBlocksOnSevenDayReserveFloor(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	reader := &fakeUsageReader{
		fn: func(context.Context) (quota.ClaudeUsageReading, error) {
			return quota.ClaudeUsageReading{
				PlanName:        "Max",
				FiveHour:        floatPtr(20),
				SevenDay:        floatPtr(81),
				FiveHourResetAt: timePtr(now.Add(2 * time.Hour)),
				SevenDayResetAt: timePtr(now.Add(84 * time.Hour)),
			}, nil
		},
	}

	cfg := testConfig(t)
	cfg.MaxSevenDayPct = 80
	guard, err := quota.NewClaudeGuard(cfg, reader, quota.WithClock(func() time.Time { return now }))
	if err != nil {
		t.Fatalf("NewClaudeGuard: %v", err)
	}

	dec := guard.CanAccept(context.Background())
	if dec.Allow {
		t.Fatalf("expected Allow=false when 7d usage (81%%) >= cap (80%%)")
	}
	if !strings.Contains(dec.Reason, "7d") {
		t.Errorf("Reason = %q, expected mention of 7d cap", dec.Reason)
	}
}

func TestClaudeGuardBlocksOnQuotapaceAmberRed(t *testing.T) {
	t.Parallel()

	baseNow := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	fiveHourReset := baseNow.Add(4 * time.Hour)
	sevenDayReset := baseNow.Add(84 * time.Hour)

	runScenario := func(t *testing.T, requireGreen bool) quota.Decision {
		t.Helper()
		now := baseNow
		readingIdx := 0
		readings := []quota.ClaudeUsageReading{
			{
				PlanName:        "Max",
				FiveHour:        floatPtr(5),
				SevenDay:        floatPtr(10),
				FiveHourResetAt: timePtr(fiveHourReset),
				SevenDayResetAt: timePtr(sevenDayReset),
			},
			{
				// Rapid jump of +40% in 10 minutes -> pace 240%/h -> projected well over 100%,
				// while still below the 75% hard cap.
				PlanName:        "Max",
				FiveHour:        floatPtr(45),
				SevenDay:        floatPtr(15),
				FiveHourResetAt: timePtr(fiveHourReset),
				SevenDayResetAt: timePtr(sevenDayReset),
			},
		}
		reader := &fakeUsageReader{
			fn: func(context.Context) (quota.ClaudeUsageReading, error) {
				r := readings[readingIdx]
				if readingIdx+1 < len(readings) {
					readingIdx++
				}
				return r, nil
			},
		}

		cfg := testConfig(t)
		cfg.RequireGreenPace = requireGreen
		guard, err := quota.NewClaudeGuard(cfg, reader, quota.WithClock(func() time.Time { return now }))
		if err != nil {
			t.Fatalf("NewClaudeGuard: %v", err)
		}

		// Record first baseline reading.
		if _, err := guard.Refresh(context.Background()); err != nil {
			t.Fatalf("first Refresh: %v", err)
		}

		// Advance clock 10 minutes and record the rapid usage spike.
		now = now.Add(10 * time.Minute)
		if _, err := guard.Refresh(context.Background()); err != nil {
			t.Fatalf("second Refresh: %v", err)
		}

		return guard.CanAccept(context.Background())
	}

	blocked := runScenario(t, true)
	if blocked.Allow {
		t.Fatalf("expected Allow=false with RequireGreenPace=true on rapid usage jump, got Allow=true (state=%q)", blocked.Health.State)
	}
	if blocked.Health.State != quotapace.Amber && blocked.Health.State != quotapace.Red {
		t.Errorf("Health.State = %q, want amber or red", blocked.Health.State)
	}

	allowed := runScenario(t, false)
	if !allowed.Allow {
		t.Fatalf("expected Allow=true with RequireGreenPace=false while below hard caps, got false (reason=%q)", allowed.Reason)
	}
}

func TestClaudeGuardFailClosedOnError(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	reader := &fakeUsageReader{
		fn: func(context.Context) (quota.ClaudeUsageReading, error) {
			return quota.ClaudeUsageReading{}, errors.New("claude-usage exited 1")
		},
	}

	cfgClosed := testConfig(t)
	cfgClosed.FailClosed = true
	guardClosed, err := quota.NewClaudeGuard(cfgClosed, reader, quota.WithClock(func() time.Time { return now }))
	if err != nil {
		t.Fatalf("NewClaudeGuard (fail closed): %v", err)
	}
	decClosed := guardClosed.CanAccept(context.Background())
	if decClosed.Allow {
		t.Fatalf("expected Allow=false when FailClosed=true and reader errors")
	}
	if decClosed.Health.State != quota.StateUnknown {
		t.Errorf("Health.State = %q, want %q", decClosed.Health.State, quota.StateUnknown)
	}

	cfgOpen := testConfig(t)
	cfgOpen.FailClosed = false
	guardOpen, err := quota.NewClaudeGuard(cfgOpen, reader, quota.WithClock(func() time.Time { return now }))
	if err != nil {
		t.Fatalf("NewClaudeGuard (fail open): %v", err)
	}
	decOpen := guardOpen.CanAccept(context.Background())
	if !decOpen.Allow {
		t.Fatalf("expected Allow=true when FailClosed=false and reader errors (reason=%q)", decOpen.Reason)
	}
	if decOpen.Health.State != quota.StateUnknown {
		t.Errorf("Health.State = %q, want %q", decOpen.Health.State, quota.StateUnknown)
	}
}

func TestClaudeGuardCachesWithinPollInterval(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	reader := &fakeUsageReader{
		fn: func(context.Context) (quota.ClaudeUsageReading, error) {
			return quota.ClaudeUsageReading{
				PlanName:        "Max",
				FiveHour:        floatPtr(15),
				SevenDay:        floatPtr(25),
				FiveHourResetAt: timePtr(now.Add(2*time.Hour + 30*time.Minute)),
				SevenDayResetAt: timePtr(now.Add(84 * time.Hour)),
			}, nil
		},
	}

	cfg := testConfig(t)
	cfg.PollInterval = 5 * time.Minute
	guard, err := quota.NewClaudeGuard(cfg, reader, quota.WithClock(func() time.Time { return now }))
	if err != nil {
		t.Fatalf("NewClaudeGuard: %v", err)
	}

	dec1 := guard.CanAccept(context.Background())
	if !dec1.Allow {
		t.Fatalf("first CanAccept denied: %s", dec1.Reason)
	}
	now = now.Add(2 * time.Minute)
	dec2 := guard.CanAccept(context.Background())
	if !dec2.Allow {
		t.Fatalf("second CanAccept denied: %s", dec2.Reason)
	}
	if got := reader.calls.Load(); got != 1 {
		t.Fatalf("reader.Read calls = %d, want 1 within PollInterval", got)
	}

	// Explicit Refresh forces a fresh read even within PollInterval.
	if _, err := guard.Refresh(context.Background()); err != nil {
		t.Fatalf("Refresh: %v", err)
	}
	if got := reader.calls.Load(); got != 2 {
		t.Fatalf("reader.Read calls after Refresh = %d, want 2", got)
	}
}

func TestExecClaudeUsageReaderIsolatesConfigDir(t *testing.T) {
	t.Parallel()

	tmpDir := t.TempDir()
	fakeBin := filepath.Join(tmpDir, "claude-usage")
	script := `#!/bin/sh
if [ "$1" != "--json" ]; then
  echo "expected --json" >&2
  exit 2
fi
if [ "$CLAUDE_CONFIG_DIR" != "` + tmpDir + `" ]; then
  echo "unexpected CLAUDE_CONFIG_DIR=$CLAUDE_CONFIG_DIR" >&2
  exit 3
fi
cat <<'EOF'
{"planName":"Team","fiveHour":12.5,"sevenDay":34.0,"fiveHourResetAt":"2026-11-24T17:00:00Z","sevenDayResetAt":"2026-12-01T12:00:00Z"}
EOF
`
	if err := os.WriteFile(fakeBin, []byte(script), 0o700); err != nil {
		t.Fatalf("write fake claude-usage: %v", err)
	}

	reader := quota.NewExecClaudeUsageReader(tmpDir)
	reader.Binary = fakeBin

	reading, err := reader.Read(context.Background())
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if reading.PlanName != "Team" {
		t.Errorf("PlanName = %q, want Team", reading.PlanName)
	}
	if reading.FiveHour == nil || *reading.FiveHour != 12.5 {
		t.Errorf("FiveHour = %v, want 12.5", reading.FiveHour)
	}
	if reading.SevenDay == nil || *reading.SevenDay != 34.0 {
		t.Errorf("SevenDay = %v, want 34.0", reading.SevenDay)
	}

	emptyReader := quota.NewExecClaudeUsageReader("")
	if _, err := emptyReader.Read(context.Background()); err == nil {
		t.Fatal("expected error when ClaudeConfigDir is empty")
	}
}

func TestLedgerRecordAndStats(t *testing.T) {
	t.Parallel()

	dbPath := filepath.Join(t.TempDir(), "usage.db")
	ledger, err := quota.OpenLedger(dbPath)
	if err != nil {
		t.Fatalf("OpenLedger: %v", err)
	}
	defer func() {
		if err := ledger.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	}()

	ctx := context.Background()
	t0 := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)
	t1 := time.Date(2026, 11, 24, 10, 0, 0, 0, time.UTC)
	t2 := time.Date(2026, 11, 24, 11, 0, 0, 0, time.UTC)
	t3 := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)

	runs := []quota.RunRecord{
		{
			JobID:               "job-old",
			Repo:                "org/repo-a",
			Skill:               "review",
			Status:              protocol.JobStatusSucceeded,
			Provider:            "claude-usage",
			Model:               "claude-sonnet-4-5",
			InputTokens:         1000,
			CacheReadTokens:     500,
			CacheCreationTokens: 200,
			OutputTokens:        300,
			EffectiveUnits:      2800,
			TurnsUsed:           4,
			DurationMs:          15000,
			PreFiveHourPct:      floatPtr(10),
			PostFiveHourPct:     floatPtr(12),
			PreSevenDayPct:      floatPtr(20),
			PostSevenDayPct:     floatPtr(21),
			FinishedAt:          t0,
		},
		{
			JobID:               "job-1",
			Repo:                "org/repo-a",
			Skill:               "review",
			Status:              protocol.JobStatusSucceeded,
			Provider:            "claude-usage",
			Model:               "claude-sonnet-4-5",
			InputTokens:         2000,
			CacheReadTokens:     1000,
			CacheCreationTokens: 400,
			OutputTokens:        500,
			EffectiveUnits:      5100,
			TurnsUsed:           6,
			DurationMs:          20000,
			PreFiveHourPct:      floatPtr(12),
			PostFiveHourPct:     floatPtr(15),
			PreSevenDayPct:      floatPtr(21),
			PostSevenDayPct:     floatPtr(22),
			FinishedAt:          t1,
		},
		{
			JobID:               "job-2",
			Repo:                "org/repo-b",
			Skill:               "triage",
			Status:              protocol.JobStatusBudgetExceeded,
			Provider:            "claude-usage",
			Model:               "claude-opus-4-1",
			InputTokens:         3000,
			CacheReadTokens:     2000,
			CacheCreationTokens: 0,
			OutputTokens:        800,
			EffectiveUnits:      7200,
			TurnsUsed:           10,
			DurationMs:          45000,
			PreFiveHourPct:      floatPtr(15),
			PostFiveHourPct:     floatPtr(22),
			FinishedAt:          t2,
		},
		{
			JobID:           "job-3",
			Repo:            "org/repo-c",
			Skill:           "summarize",
			Status:          protocol.JobStatusDLPBlocked,
			Provider:        "claude-usage",
			Model:           "claude-sonnet-4-5",
			InputTokens:     500,
			OutputTokens:    100,
			EffectiveUnits:  1000,
			TurnsUsed:       2,
			DurationMs:      5000,
			PreFiveHourPct:  floatPtr(22),
			PostFiveHourPct: floatPtr(23),
			FinishedAt:      t3,
		},
	}

	for _, run := range runs {
		if err := ledger.RecordRun(ctx, run); err != nil {
			t.Fatalf("RecordRun(%s): %v", run.JobID, err)
		}
	}

	// All-time stats.
	allStats, err := ledger.Stats(ctx, time.Time{})
	if err != nil {
		t.Fatalf("Stats(all): %v", err)
	}
	if allStats.TotalJobs != 4 {
		t.Errorf("all TotalJobs = %d, want 4", allStats.TotalJobs)
	}
	if allStats.JobsByStatus[protocol.JobStatusSucceeded] != 2 {
		t.Errorf("all SUCCEEDED = %d, want 2", allStats.JobsByStatus[protocol.JobStatusSucceeded])
	}
	if allStats.JobsByStatus[protocol.JobStatusBudgetExceeded] != 1 {
		t.Errorf("all BUDGET_EXCEEDED = %d, want 1", allStats.JobsByStatus[protocol.JobStatusBudgetExceeded])
	}
	if allStats.JobsByStatus[protocol.JobStatusDLPBlocked] != 1 {
		t.Errorf("all DLP_BLOCKED = %d, want 1", allStats.JobsByStatus[protocol.JobStatusDLPBlocked])
	}

	// Filtered stats since t1 (excludes job-old).
	recentStats, err := ledger.Stats(ctx, t1)
	if err != nil {
		t.Fatalf("Stats(since t1): %v", err)
	}
	if recentStats.TotalJobs != 3 {
		t.Errorf("recent TotalJobs = %d, want 3", recentStats.TotalJobs)
	}
	if recentStats.InputTokens != 5500 {
		t.Errorf("recent InputTokens = %d, want 5500", recentStats.InputTokens)
	}
	if recentStats.CacheReadTokens != 3000 {
		t.Errorf("recent CacheReadTokens = %d, want 3000", recentStats.CacheReadTokens)
	}
	if recentStats.CacheCreationTokens != 400 {
		t.Errorf("recent CacheCreationTokens = %d, want 400", recentStats.CacheCreationTokens)
	}
	if recentStats.OutputTokens != 1400 {
		t.Errorf("recent OutputTokens = %d, want 1400", recentStats.OutputTokens)
	}
	if recentStats.TotalTokens != 10300 {
		t.Errorf("recent TotalTokens = %d, want 10300", recentStats.TotalTokens)
	}
	if recentStats.EffectiveUnits != 13300 {
		t.Errorf("recent EffectiveUnits = %d, want 13300", recentStats.EffectiveUnits)
	}
	if recentStats.TotalDurationMs != 70000 {
		t.Errorf("recent TotalDurationMs = %d, want 70000", recentStats.TotalDurationMs)
	}
	if len(recentStats.RecentRuns) != 3 {
		t.Fatalf("len(RecentRuns) = %d, want 3", len(recentStats.RecentRuns))
	}
	if recentStats.RecentRuns[0].JobID != "job-3" || recentStats.RecentRuns[2].JobID != "job-1" {
		t.Errorf("RecentRuns order = [%s, %s, %s], want [job-3, job-2, job-1]",
			recentStats.RecentRuns[0].JobID, recentStats.RecentRuns[1].JobID, recentStats.RecentRuns[2].JobID)
	}
	if recentStats.RecentRuns[1].PreSevenDayPct != nil {
		t.Errorf("job-2 PreSevenDayPct = %v, want nil", recentStats.RecentRuns[1].PreSevenDayPct)
	}
}
