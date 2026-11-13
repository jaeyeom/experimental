package diskwatch

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"strings"
	"testing"
	"time"
)

func percentMount() Config {
	urgent := Amount{IsPercent: true, Percent: 5}
	return Config{
		Cooldown:    6 * time.Hour,
		SummaryTop:  10,
		OnLow:       []string{"/diskwatch/notify", "low;disk"},
		ReportPaths: []string{"/tree"},
		Mounts: []Mount{{
			Path:     "/",
			Low:      Amount{IsPercent: true, Percent: 15},
			High:     Amount{IsPercent: true, Percent: 25},
			Urgent:   &urgent,
			Cooldown: 6 * time.Hour,
		}},
	}
}

type scriptedStat struct {
	free uint64
}

func (s scriptedStat) stat(string) (Usage, error) {
	return Usage{Free: s.free, Total: 1000}, nil
}

func TestThresholdStateMachine(t *testing.T) {
	t.Parallel()
	cfg := percentMount()
	cfg.ReportPaths = nil
	now := time.Date(2026, 11, 13, 0, 0, 0, 0, time.UTC)
	var state State
	calls := 0
	run := func(context.Context, []string, []string) error {
		calls++
		return nil
	}
	step := func(at time.Time, free uint64) Report {
		t.Helper()
		report, next, err := Check(context.Background(), Input{
			Config: cfg, State: state, Now: at, Stat: scriptedStat{free}.stat, Run: run,
			SummaryDir: t.TempDir(),
		})
		if err != nil {
			t.Fatal(err)
		}
		state = next
		return report
	}
	if report := step(now, 100); !report.Mounts[0].Triggered || calls != 1 {
		t.Fatalf("first trigger: %+v calls %d", report.Mounts[0], calls)
	}
	if state.Mounts["/"].Armed {
		t.Fatal("trigger disarms")
	}
	if report := step(now.Add(time.Hour), 100); report.Mounts[0].Triggered || calls != 1 {
		t.Fatal("quiet during cooldown")
	}
	if report := step(now.Add(time.Hour), 300); report.Mounts[0].Triggered || !state.Mounts["/"].Armed {
		t.Fatal("re-arm above high without triggering")
	}
	if report := step(now.Add(2*time.Hour), 100); report.Mounts[0].Triggered || calls != 1 {
		t.Fatal("still quiet after re-arm while cooldown remains")
	}
	if report := step(now.Add(6*time.Hour+time.Second), 100); !report.Mounts[0].Triggered || calls != 2 {
		t.Fatal("trigger again after cooldown")
	}
	if report := step(now.Add(7*time.Hour), 40); !report.Mounts[0].Triggered || report.Mounts[0].Kind != "urgent" || calls != 3 {
		t.Fatalf("urgent = %+v calls %d", report.Mounts[0], calls)
	}
}

func TestActionFailureIsRetried(t *testing.T) {
	t.Parallel()
	cfg := percentMount()
	cfg.ReportPaths = nil
	calls := 0
	run := func(context.Context, []string, []string) error {
		calls++
		return errors.New("boom")
	}
	in := Input{
		Config: cfg, Now: time.Now(), Stat: scriptedStat{100}.stat, Run: run, SummaryDir: t.TempDir(),
	}
	report, state, err := Check(context.Background(), in)
	if err != nil {
		t.Fatal(err)
	}
	if report.Mounts[0].Triggered || report.Mounts[0].ActionError == "" {
		t.Fatalf("report = %+v", report.Mounts[0])
	}
	if !state.Mounts["/"].Armed || !state.Mounts["/"].TriggeredAt.IsZero() {
		t.Fatalf("failure stored as a trigger: %+v", state.Mounts["/"])
	}
	in.State = state
	if _, _, err := Check(context.Background(), in); err != nil {
		t.Fatal(err)
	}
	if calls != 2 {
		t.Fatalf("calls = %d, want a retry", calls)
	}
}

func TestOnLowReceivesEnvWithoutShell(t *testing.T) {
	t.Parallel()
	cfg := percentMount()
	cfg.ReportPaths = nil
	dir := t.TempDir()
	var argv []string
	var env []string
	run := func(_ context.Context, gotArgv []string, gotEnv []string) error {
		argv = gotArgv
		env = gotEnv
		return nil
	}
	_, _, err := Check(context.Background(), Input{
		Config: cfg, Now: time.Now(), Stat: scriptedStat{100}.stat, Run: run,
		BaseEnv: []string{"PATH=/usr/bin"}, Hostname: "build-host", SummaryDir: dir,
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(argv) != 2 || argv[0] != "/diskwatch/notify" || argv[1] != "low;disk" {
		t.Fatalf("argv = %#v", argv)
	}
	want := map[string]string{
		"DISKWATCH_MOUNT":       "/",
		"DISKWATCH_FREE_BYTES":  "100",
		"DISKWATCH_FREE_PCT":    "10.00",
		"DISKWATCH_TOTAL_BYTES": "1000",
		"DISKWATCH_HOST":        "build-host",
		"DISKWATCH_SUMMARY":     "",
	}
	got := envMap(env)
	for key, value := range want {
		if got[key] != value {
			t.Errorf("%s = %q, want %q", key, got[key], value)
		}
	}
	file := got["DISKWATCH_SUMMARY_FILE"]
	data, err := os.ReadFile(file)
	if err != nil {
		t.Fatal(err)
	}
	var body struct {
		Mount string `json:"mount"`
	}
	if err := json.Unmarshal(data, &body); err != nil || body.Mount != "/" {
		t.Fatalf("summary file: %s %v", data, err)
	}
	if !strings.Contains(got["PATH"], "/usr/bin") {
		t.Fatalf("base env dropped: %#v", env)
	}
}

func TestDryRunDoesNotRunAction(t *testing.T) {
	t.Parallel()
	cfg := percentMount()
	cfg.ReportPaths = nil
	run := func(context.Context, []string, []string) error {
		t.Fatal("dry-run ran the action")
		return nil
	}
	report, state, err := Check(context.Background(), Input{
		Config: cfg, Now: time.Now(), Stat: scriptedStat{100}.stat, Run: run, DryRun: true,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !report.Mounts[0].Triggered || !report.Mounts[0].DryRun {
		t.Fatalf("report = %+v", report.Mounts[0])
	}
	if !state.Mounts["/"].Armed || !state.Mounts["/"].TriggeredAt.IsZero() {
		t.Fatalf("dry-run changed state: %+v", state.Mounts["/"])
	}
}

func TestHookRateLimit(t *testing.T) {
	t.Parallel()
	cfg := percentMount()
	cfg.ReportPaths = nil
	now := time.Date(2026, 11, 13, 0, 0, 0, 0, time.UTC)
	calls := 0
	run := func(context.Context, []string, []string) error {
		calls++
		return nil
	}
	var state State
	// First hook is healthy, so it only records the rate-limit timestamp.
	report, state, err := Check(context.Background(), Input{
		Config: cfg, State: state, Now: now, Stat: scriptedStat{800}.stat, Run: run,
		Hook: true, SummaryDir: t.TempDir(),
	})
	if err != nil || report.Skipped || calls != 0 {
		t.Fatalf("first hook skipped=%v calls=%d err=%v", report.Skipped, calls, err)
	}
	report, _, err = Check(context.Background(), Input{
		Config: cfg, State: state, Now: now.Add(30 * time.Second), Stat: scriptedStat{100}.stat, Run: run,
		Hook: true, SummaryDir: t.TempDir(),
	})
	if err != nil || !report.Skipped || calls != 0 {
		t.Fatalf("second hook skipped=%v calls=%d err=%v", report.Skipped, calls, err)
	}
	report, _, err = Check(context.Background(), Input{
		Config: cfg, State: state, Now: now.Add(time.Minute), Stat: scriptedStat{100}.stat, Run: run,
		Hook: true, SummaryDir: t.TempDir(),
	})
	if err != nil || report.Skipped || calls != 1 || !report.Mounts[0].Triggered {
		t.Fatalf("third hook skipped=%v calls=%d report=%+v err=%v", report.Skipped, calls, report.Mounts, err)
	}
}

func envMap(env []string) map[string]string {
	out := make(map[string]string, len(env))
	for _, item := range env {
		key, value, ok := splitEnv(item)
		if ok {
			out[key] = value
		}
	}
	return out
}
