package healthsnap

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
	"time"
)

type fakeHostReader struct {
	load    LoadStats
	loadErr error
	mem     MemoryStats
	memErr  error
	disk    DiskStats
	diskErr error
	diskAt  string
}

func (f *fakeHostReader) Load(context.Context) (LoadStats, error) {
	return f.load, f.loadErr
}

func (f *fakeHostReader) Memory(context.Context) (MemoryStats, error) {
	return f.mem, f.memErr
}

func (f *fakeHostReader) Disk(_ context.Context, path string) (DiskStats, error) {
	f.diskAt = path
	return f.disk, f.diskErr
}

func TestExecuteCommandParsers(t *testing.T) {
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)

	t.Run("exit code", func(t *testing.T) {
		cfg := Config{Checks: []CheckConfig{{ID: "c1", Argv: []string{"check"}}}}
		if err := PrepareConfig(&cfg); err != nil {
			t.Fatal(err)
		}
		deps := CheckDeps{
			Now: func() time.Time { return now },
			RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
				return CmdResult{ExitCode: 2}, nil
			},
		}
		res := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if res.execErr != "" || res.vars["exit"] != 2 || res.vars["value"] != 2 {
			t.Fatalf("unexpected res: %+v", res)
		}
	})

	t.Run("jsonpath parser", func(t *testing.T) {
		cfg := Config{Checks: []CheckConfig{{
			ID:    "c2",
			Argv:  []string{"agentbus", "held", "--json"},
			Parse: ParseConfig{Mode: ParseJSONPath, JSONPath: "$[*].age_seconds.max()"},
			Rules: []RuleConfig{{ID: "r1", Condition: "value > 30m", State: SeverityWarn}},
		}}}
		if err := PrepareConfig(&cfg); err != nil {
			t.Fatal(err)
		}
		deps := CheckDeps{
			Now: func() time.Time { return now },
			RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
				return CmdResult{ExitCode: 1, Stdout: `[{"age_seconds": 2400}]`}, nil
			},
		}
		res := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if res.execErr != "" || res.vars["value"] != float64(2400) || res.vars["exit"] != 1 {
			t.Fatalf("unexpected res: %+v", res)
		}
	})

	t.Run("regex capture group", func(t *testing.T) {
		cfg := Config{Checks: []CheckConfig{{
			ID:    "c3",
			Argv:  []string{"tool"},
			Parse: ParseConfig{Mode: ParseRegex, Regex: `usage:\s+(\d+)%`},
			Rules: []RuleConfig{{ID: "r1", Condition: "value > 80", State: SeverityFail}},
		}}}
		if err := PrepareConfig(&cfg); err != nil {
			t.Fatal(err)
		}
		deps := CheckDeps{
			Now: func() time.Time { return now },
			RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
				return CmdResult{Stdout: "ok\nusage: 92%\ndone\n"}, nil
			},
		}
		res := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if res.execErr != "" || res.vars["value"] != float64(92) || res.vars["matched"] != true {
			t.Fatalf("unexpected res: %+v", res)
		}
	})

	t.Run("regex count without capture group", func(t *testing.T) {
		cfg := Config{Checks: []CheckConfig{{
			ID:    "c4",
			Argv:  []string{"git", "status", "--porcelain"},
			Parse: ParseConfig{Mode: ParseRegex, Regex: `^.`},
			Rules: []RuleConfig{{ID: "r1", Condition: "value > 0", State: SeverityWarn}},
		}}}
		if err := PrepareConfig(&cfg); err != nil {
			t.Fatal(err)
		}
		deps := CheckDeps{
			Now: func() time.Time { return now },
			RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
				return CmdResult{Stdout: " M a.go\n?? b.go\n"}, nil
			},
		}
		res := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if res.execErr != "" || res.vars["value"] != float64(2) || res.vars["matches"] != 2 {
			t.Fatalf("unexpected res: %+v", res)
		}

		// Clean git status -> 0 matches -> value = 0.
		deps.RunCmd = func(context.Context, CmdRequest) (CmdResult, error) {
			return CmdResult{Stdout: ""}, nil
		}
		clean := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if clean.execErr != "" || clean.vars["value"] != float64(0) || clean.vars["matched"] != false {
			t.Fatalf("unexpected clean res: %+v", clean)
		}
	})

	t.Run("timeout or missing command is exec error", func(t *testing.T) {
		cfg := Config{Checks: []CheckConfig{{ID: "c5", Argv: []string{"missing-cmd"}, Timeout: "50ms"}}}
		if err := PrepareConfig(&cfg); err != nil {
			t.Fatal(err)
		}
		deps := CheckDeps{
			Now: func() time.Time { return now },
			RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
				return CmdResult{}, context.DeadlineExceeded
			},
		}
		res := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if res.execErr == "" {
			t.Fatal("expected execErr for timeout")
		}
	})
}

func TestExecuteFileAgeAndHostWithFakes(t *testing.T) {
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)

	t.Run("file age", func(t *testing.T) {
		dir := t.TempDir()
		hb := filepath.Join(dir, "heartbeat")
		if err := os.WriteFile(hb, []byte("ok"), 0o600); err != nil {
			t.Fatal(err)
		}
		old := now.Add(-3 * time.Hour)
		if err := os.Chtimes(hb, old, old); err != nil {
			t.Fatal(err)
		}

		cfg := Config{Checks: []CheckConfig{{
			ID:    "hb",
			Kind:  KindFileAge,
			Path:  hb,
			Rules: []RuleConfig{{ID: "hb-stale", Condition: "age > 2h", State: SeverityFail}},
		}}}
		if err := PrepareConfig(&cfg); err != nil {
			t.Fatal(err)
		}
		deps := CheckDeps{
			Now:      func() time.Time { return now },
			StatFile: os.Stat,
		}
		res := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if res.execErr != "" || res.vars["age"] != (3*time.Hour).Seconds() {
			t.Fatalf("unexpected file-age res: %+v", res)
		}

		// Missing file -> execErr (fail state).
		cfg.Checks[0].Path = filepath.Join(dir, "missing")
		miss := executeCheck(context.Background(), cfg.Checks[0], nil, deps)
		if miss.execErr == "" {
			t.Fatal("expected execErr for missing file")
		}
		if !errors.Is(func() error { _, err := deps.StatFile(cfg.Checks[0].Path); return err }(), fs.ErrNotExist) {
			t.Fatal("expected ErrNotExist")
		}
	})

	t.Run("host checks", func(t *testing.T) {
		host := &fakeHostReader{
			load: LoadStats{Load1: 6.0, Load5: 4.5, Load15: 3.0, CPUs: 4, LoadPerCPU: 1.5},
			mem:  MemoryStats{TotalBytes: 1000, AvailableBytes: 100, UsedPercent: 90, PressureLevel: "warn"},
			disk: DiskStats{FreeBytes: 5 << 30, TotalBytes: 100 << 30, FreePercent: 5, UsedPercent: 95, FreeGiB: 5},
		}
		cfg := Config{Checks: []CheckConfig{
			{ID: "load", Kind: KindHost, Metric: "load-per-cpu", Rules: []RuleConfig{{ID: "r1", Condition: "value > 1.0", State: SeverityWarn}}},
			{ID: "mem", Kind: KindHost, Metric: "memory", Rules: []RuleConfig{{ID: "r2", Condition: `pressure_level == "warn"`, State: SeverityWarn}}},
			{ID: "disk", Kind: KindHost, Metric: "disk", Path: "~/data", Rules: []RuleConfig{{ID: "r3", Condition: "value < 10", State: SeverityFail}}},
		}}
		if err := PrepareConfig(&cfg); err != nil {
			t.Fatal(err)
		}
		deps := CheckDeps{Now: func() time.Time { return now }, Host: host}
		env := []string{"HOME=/home/ops"}

		rLoad := executeCheck(context.Background(), cfg.Checks[0], env, deps)
		if rLoad.execErr != "" || rLoad.vars["value"] != 1.5 {
			t.Fatalf("load res = %+v", rLoad)
		}
		rMem := executeCheck(context.Background(), cfg.Checks[1], env, deps)
		if rMem.execErr != "" || rMem.vars["usedPercent"] != float64(90) || rMem.vars["pressure_level"] != "warn" {
			t.Fatalf("mem res = %+v", rMem)
		}
		rDisk := executeCheck(context.Background(), cfg.Checks[2], env, deps)
		if rDisk.execErr != "" || rDisk.vars["value"] != float64(5) || host.diskAt != "/home/ops/data" {
			t.Fatalf("disk res = %+v diskAt=%q", rDisk, host.diskAt)
		}
	})
}

func TestExecCmdRealCommandAndTimeout(t *testing.T) {
	ctx := context.Background()
	res, err := ExecCmd(ctx, CmdRequest{Argv: []string{"true"}, Timeout: 2 * time.Second})
	if err != nil || res.ExitCode != 0 {
		t.Fatalf("ExecCmd(true) = %+v, %v", res, err)
	}
	resFalse, err := ExecCmd(ctx, CmdRequest{Argv: []string{"false"}, Timeout: 2 * time.Second})
	if err != nil || resFalse.ExitCode == 0 {
		t.Fatalf("ExecCmd(false) = %+v, %v", resFalse, err)
	}
	if _, err := ExecCmd(ctx, CmdRequest{Argv: []string{"healthsnap-nonexistent-binary-12345"}, Timeout: time.Second}); err == nil {
		t.Fatal("expected error for missing command")
	}
	if _, err := ExecCmd(ctx, CmdRequest{Argv: []string{"sleep", "2"}, Timeout: 20 * time.Millisecond}); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("expected DeadlineExceeded, got %v", err)
	}
}
