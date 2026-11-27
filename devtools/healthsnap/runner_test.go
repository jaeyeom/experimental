package healthsnap

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

type recordedAlert struct {
	argv []string
	env  []string
}

func TestRunTransitionsDedupeRepeatAndJudgmentRouting(t *testing.T) {
	stateDir := t.TempDir()
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)

	cfg := Config{
		StateDir:      stateDir,
		KeepSnapshots: 5,
		Markdown:      true,
		Notify: NotifyConfig{
			Recipient:         "ops/inbox",
			JudgmentRecipient: "ops/model",
		},
		Checks: []CheckConfig{
			{
				ID:   "mem",
				Kind: KindCommand,
				Argv: []string{"fake-mem"},
				Parse: ParseConfig{
					Mode:     ParseJSONPath,
					JSONPath: "$.used",
				},
				Rules: []RuleConfig{
					{ID: "mem-warn", Condition: "value > 80", State: SeverityWarn, Repeat: "4h", Message: "memory high"},
					{ID: "mem-crit", Condition: "value > 95", State: SeverityFail, Judgment: true, Message: "memory critical"},
				},
			},
		},
	}

	var used float64
	var sent []recordedAlert
	runPass := func(at time.Time) RunOutput {
		t.Helper()
		out, err := Run(context.Background(), RunInput{
			Config:   cfg,
			StateDir: stateDir,
			Deps: CheckDeps{
				Now: func() time.Time { return at },
				RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
					return CmdResult{Stdout: `{"used":` + formatScalar(used) + `}`}, nil
				},
			},
			Notify: func(_ context.Context, argv, env []string) error {
				sent = append(sent, recordedAlert{argv: argv, env: env})
				return nil
			},
		})
		if err != nil {
			t.Fatalf("Run error = %v", err)
		}
		return out
	}

	// Pass 1 (t=0): OK (50%) -> 0 alerts.
	used = 50
	out1 := runPass(now)
	if out1.Snapshot.State != SeverityOK || len(sent) != 0 {
		t.Fatalf("pass 1: state=%s sent=%d", out1.Snapshot.State, len(sent))
	}

	// Pass 2 (t=1h): warn fires (85%) -> 1 alert to ops/inbox.
	used = 85
	out2 := runPass(now.Add(1 * time.Hour))
	if out2.Snapshot.State != SeverityWarn || len(sent) != 1 {
		t.Fatalf("pass 2: state=%s sent=%d", out2.Snapshot.State, len(sent))
	}
	if sent[0].argv[2] != "ops/inbox" || !strings.Contains(sent[0].argv[3], "FIRE mem-warn") {
		t.Fatalf("pass 2 argv = %v", sent[0].argv)
	}

	// Pass 3 (t=2h): still warn (86%) -> deduped, 0 new alerts.
	used = 86
	out3 := runPass(now.Add(2 * time.Hour))
	if out3.Snapshot.State != SeverityWarn || len(sent) != 1 {
		t.Fatalf("pass 3 should dedupe: state=%s sent=%d", out3.Snapshot.State, len(sent))
	}

	// Pass 4 (t=5h): repeat interval 4h elapsed since t=1h -> 1 repeat alert for mem-warn!
	used = 87
	out4 := runPass(now.Add(5 * time.Hour))
	if out4.Snapshot.State != SeverityWarn || len(sent) != 2 {
		t.Fatalf("pass 4 should repeat: state=%s sent=%d", out4.Snapshot.State, len(sent))
	}
	if !strings.Contains(sent[1].argv[3], "REPEAT mem-warn") {
		t.Fatalf("pass 4 argv = %v", sent[1].argv)
	}

	// Pass 5 (t=6h): crosses 97% -> mem-crit fires to judgmentRecipient ops/model!
	used = 97
	out5 := runPass(now.Add(6 * time.Hour))
	if out5.Snapshot.State != SeverityFail || len(sent) != 3 {
		t.Fatalf("pass 5 should fire judgment rule: state=%s sent=%d", out5.Snapshot.State, len(sent))
	}
	if sent[2].argv[2] != "ops/model" || !strings.Contains(sent[2].argv[3], "FIRE mem-crit") {
		t.Fatalf("pass 5 argv = %v", sent[2].argv)
	}

	// Pass 6 (t=7h): returns to 40% -> both rules clear to their respective recipients!
	used = 40
	out6 := runPass(now.Add(7 * time.Hour))
	if out6.Snapshot.State != SeverityOK || len(sent) != 5 {
		t.Fatalf("pass 6 should clear both rules: state=%s sent=%d", out6.Snapshot.State, len(sent))
	}
	if sent[3].argv[2] != "ops/inbox" || !strings.Contains(sent[3].argv[3], "CLEAR mem-warn") {
		t.Fatalf("clear mem-warn argv = %v", sent[3].argv)
	}
	if sent[4].argv[2] != "ops/model" || !strings.Contains(sent[4].argv[3], "CLEAR mem-crit") {
		t.Fatalf("clear mem-crit argv = %v", sent[4].argv)
	}
}

func TestRunCheckFailureDoesNotClearExistingRuleAndAlertsOnce(t *testing.T) {
	stateDir := t.TempDir()
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)

	cfg := Config{
		StateDir: stateDir,
		Notify:   NotifyConfig{Recipient: "ops/inbox"},
		Checks: []CheckConfig{{
			ID:   "svc",
			Argv: []string{"svc-check"},
			Rules: []RuleConfig{{
				ID:        "svc-nonzero",
				Condition: "exit != 0",
				State:     SeverityWarn,
			}},
		}},
	}

	var mode string
	var alerts []string
	runPass := func(at time.Time) RunOutput {
		out, err := Run(context.Background(), RunInput{
			Config:   cfg,
			StateDir: stateDir,
			Deps: CheckDeps{
				Now: func() time.Time { return at },
				RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
					switch mode {
					case "warn":
						return CmdResult{ExitCode: 1}, nil
					case "timeout":
						return CmdResult{}, context.DeadlineExceeded
					default:
						return CmdResult{ExitCode: 0}, nil
					}
				},
			},
			Notify: func(_ context.Context, argv, _ []string) error {
				alerts = append(alerts, argv[3])
				return nil
			},
		})
		if err != nil {
			t.Fatalf("Run error = %v", err)
		}
		return out
	}

	// Step 1: rule warns -> 1 alert.
	mode = "warn"
	runPass(now)
	if len(alerts) != 1 || !strings.Contains(alerts[0], "FIRE svc-nonzero") {
		t.Fatalf("step 1 alerts = %v", alerts)
	}

	// Step 2: command times out -> check state is fail, exec alert fires, svc-nonzero does NOT falsely clear!
	mode = "timeout"
	out2 := runPass(now.Add(time.Hour))
	if out2.Snapshot.State != SeverityFail {
		t.Fatalf("timeout state = %s, want fail", out2.Snapshot.State)
	}
	if len(alerts) != 2 || !strings.Contains(alerts[1], "FIRE svc/exec") {
		t.Fatalf("step 2 alerts = %v", alerts)
	}

	// Step 3: still timing out -> deduped.
	runPass(now.Add(2 * time.Hour))
	if len(alerts) != 2 {
		t.Fatalf("step 3 should dedupe exec error: alerts = %v", alerts)
	}

	// Step 4: command returns to warn (exit 1) -> exec error clears, svc-nonzero did NOT re-fire because it stayed warn!
	mode = "warn"
	runPass(now.Add(3 * time.Hour))
	if len(alerts) != 3 || !strings.Contains(alerts[2], "CLEAR svc/exec") {
		t.Fatalf("step 4 alerts = %v", alerts)
	}
}

func TestRunIntervalCachingDryRunAndNotifyRetry(t *testing.T) {
	stateDir := t.TempDir()
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)

	cfg := Config{
		StateDir: stateDir,
		Notify:   NotifyConfig{Recipient: "ops/inbox"},
		Checks: []CheckConfig{{
			ID:       "slow",
			Argv:     []string{"slow-cmd"},
			Interval: "6h",
		}},
	}

	cmdRuns := 0
	exitCode := 1
	failNotify := true

	runOnce := func(at time.Time, dryRun, force bool) RunOutput {
		out, err := Run(context.Background(), RunInput{
			Config:   cfg,
			StateDir: stateDir,
			DryRun:   dryRun,
			Force:    force,
			Deps: CheckDeps{
				Now: func() time.Time { return at },
				RunCmd: func(context.Context, CmdRequest) (CmdResult, error) {
					cmdRuns++
					return CmdResult{ExitCode: exitCode}, nil
				},
			},
			Notify: func(context.Context, []string, []string) error {
				if failNotify {
					return errors.New("bus down")
				}
				return nil
			},
		})
		if err != nil {
			t.Fatalf("Run error = %v", err)
		}
		return out
	}

	// DryRun does not save state or run notify.
	dry := runOnce(now, true, false)
	if len(dry.Snapshot.Alerts) != 1 || dry.Snapshot.Alerts[0].Sent || !dry.Snapshot.Alerts[0].DryRun {
		t.Fatalf("dry run alerts = %+v", dry.Snapshot.Alerts)
	}
	if cmdRuns != 1 {
		t.Fatalf("cmdRuns = %d, want 1", cmdRuns)
	}

	// Real run with failing notify: alert fails, rule state stays ok so next run retries!
	out1 := runOnce(now, false, false)
	if !out1.HasNotifyError() || cmdRuns != 2 {
		t.Fatalf("out1 = %+v cmdRuns=%d", out1, cmdRuns)
	}

	// Within interval (t+1h) without --force: cached, cmdRuns stays 2!
	outCached := runOnce(now.Add(1*time.Hour), false, false)
	if !outCached.Snapshot.Checks[0].Cached || cmdRuns != 2 {
		t.Fatalf("expected cached check within interval: %+v cmdRuns=%d", outCached.Snapshot.Checks[0], cmdRuns)
	}

	// With --force and notify succeeding: retries and delivers!
	failNotify = false
	outForced := runOnce(now.Add(1*time.Hour), false, true)
	if cmdRuns != 3 || outForced.HasNotifyError() || len(outForced.Snapshot.Alerts) != 1 || !outForced.Snapshot.Alerts[0].Sent {
		t.Fatalf("forced retry failed: %+v cmdRuns=%d", outForced, cmdRuns)
	}
}
