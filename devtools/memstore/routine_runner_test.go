package memstore

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

type fakeDecideSubmitter struct {
	reqs []DecideRequest
	id   string
	err  error
}

func (f *fakeDecideSubmitter) Submit(_ context.Context, req DecideRequest) (string, error) {
	f.reqs = append(f.reqs, req)
	return f.id, f.err
}

func setupRoutineTestEnv(t *testing.T) (Config, *Vault, string) {
	t.Helper()
	dir := t.TempDir()
	dbPath := filepath.Join(dir, "vaults", "work.db")
	cfg := Config{
		Vaults: map[string]VaultConfig{
			"work": {Name: "work", Path: dbPath, Residency: ResidencyLocalOnly},
		},
		Profiles: map[string]ProfileConfig{
			"automation": {
				Name:           "automation",
				Vaults:         []string{"work"},
				Compartments:   []string{"engineering"},
				MaxSensitivity: SensitivityNormal,
				Capabilities:   []Capability{CapabilityRead, CapabilityWrite, CapabilityWriteEpisodic},
			},
		},
		RetentionDays: DefaultRetentionDays,
	}
	v, err := InitVault(cfg.Vaults["work"])
	if err != nil {
		t.Fatalf("InitVault: %v", err)
	}
	t.Cleanup(func() { _ = v.Close() })
	return cfg, v, dir
}

func writeFakeHandler(t *testing.T, dir, name, script string) string {
	t.Helper()
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, []byte(script), 0o700); err != nil {
		t.Fatalf("write handler %s: %v", name, err)
	}
	return p
}

func TestRunRoutineCursorAndIdempotency(t *testing.T) {
	cfg, v, dir := setupRoutineTestEnv(t)
	ctx := context.Background()

	// Handler reads stdin envelope, echoes previous cursor into an event, and advances cursor.
	handlerPath := writeFakeHandler(t, dir, "cursor.sh", `#!/bin/sh
input=$(cat)
prev=$(printf '%s' "$input" | sed -n 's/.*"cursor":"\([^"]*\)".*/\1/p')
if [ -z "$prev" ]; then prev="none"; fi
printf '{"stateUpdates":{"cursor":"c2"},"events":[{"compartment":"engineering","summary":"prev=%s profile=%s"}]}\n' "$prev" "$MEMSTORE_PROFILE"
`)

	m := RoutineManifest{
		ID:          "cursor-sync",
		Vault:       "work",
		Profile:     "automation",
		Residency:   "local",
		Schedule:    "@every 1h",
		Timezone:    "UTC",
		Timeout:     "10s",
		Idempotency: "date",
		Enabled:     true,
		Handler: RoutineHandler{
			Command:      []string{handlerPath},
			OutputFormat: "json",
		},
	}

	day1 := time.Date(2026, 6, 1, 9, 0, 0, 0, time.UTC)
	res1, err := RunRoutine(ctx, cfg, m, RoutineRunOptions{Now: func() time.Time { return day1 }})
	if err != nil {
		t.Fatalf("RunRoutine day1: %v", err)
	}
	if res1.Status != RoutineRunOK || res1.StateUpdated != 1 || res1.EventsAdded != 1 {
		t.Fatalf("unexpected res1: %+v", res1)
	}

	// Same day -> idempotent skip.
	resDup, err := RunRoutine(ctx, cfg, m, RoutineRunOptions{Now: func() time.Time { return day1.Add(time.Hour) }})
	if err != nil {
		t.Fatalf("RunRoutine dup: %v", err)
	}
	if resDup.Status != RoutineRunSkipped || resDup.SkippedReason != "already ran" {
		t.Fatalf("unexpected dup res: %+v", resDup)
	}

	// Next day -> reads cursor "c2" written by run 1.
	day2 := day1.Add(24 * time.Hour)
	res2, err := RunRoutine(ctx, cfg, m, RoutineRunOptions{Now: func() time.Time { return day2 }})
	if err != nil {
		t.Fatalf("RunRoutine day2: %v", err)
	}
	if res2.Status != RoutineRunOK {
		t.Fatalf("unexpected res2: %+v", res2)
	}

	svc, err := OpenService(cfg, "automation")
	if err != nil {
		t.Fatalf("OpenService: %v", err)
	}
	defer svc.Close()

	events, err := svc.ListEvents(ctx, EventFilter{Vault: "work"}, Actor{Caller: "test"})
	if err != nil {
		t.Fatalf("ListEvents: %v", err)
	}
	if len(events) != 2 {
		t.Fatalf("len(events) = %d, want 2", len(events))
	}
	var summaries []string
	for _, ev := range events {
		summaries = append(summaries, ev.Summary)
	}
	joined := strings.Join(summaries, "|")
	if !strings.Contains(joined, "prev=none profile=automation") || !strings.Contains(joined, "prev=c2 profile=automation") {
		t.Errorf("unexpected event summaries: %v", summaries)
	}
	_ = v
}

func TestRunRoutineNonZeroExitAndTimeout(t *testing.T) {
	cfg, v, dir := setupRoutineTestEnv(t)
	ctx := context.Background()

	failHandler := writeFakeHandler(t, dir, "fail.sh", "#!/bin/sh\necho boom >&2\nexit 7\n")
	mFail := RoutineManifest{
		ID:          "fail-job",
		Vault:       "work",
		Profile:     "automation",
		Residency:   "local",
		Schedule:    "@every 1h",
		Timezone:    "UTC",
		Timeout:     "5s",
		Idempotency: "run",
		Enabled:     true,
		Handler:     RoutineHandler{Command: []string{failHandler}, OutputFormat: "none"},
	}

	res, err := RunRoutine(ctx, cfg, mFail, RoutineRunOptions{})
	if err == nil {
		t.Fatal("expected error for non-zero exit")
	}
	if res.Status != RoutineRunFailed || res.ExitCode == nil || *res.ExitCode != 7 {
		t.Fatalf("unexpected fail result: %+v", res)
	}

	slowHandler := writeFakeHandler(t, dir, "slow.sh", "#!/bin/sh\nsleep 2\n")
	mSlow := RoutineManifest{
		ID:          "slow-job",
		Vault:       "work",
		Profile:     "automation",
		Residency:   "local",
		Schedule:    "@every 1h",
		Timezone:    "UTC",
		Timeout:     "100ms",
		Idempotency: "run",
		Enabled:     true,
		Handler:     RoutineHandler{Command: []string{slowHandler}, OutputFormat: "none"},
	}

	resSlow, err := RunRoutine(ctx, cfg, mSlow, RoutineRunOptions{})
	if err == nil {
		t.Fatal("expected error for timeout")
	}
	if resSlow.Status != RoutineRunFailed || !strings.Contains(resSlow.ErrorSummary, "timeout") {
		t.Fatalf("unexpected slow result: %+v", resSlow)
	}

	runs, err := v.ListRoutineRuns(ctx, RoutineRunFilter{RoutineID: "slow-job"})
	if err != nil {
		t.Fatalf("ListRoutineRuns: %v", err)
	}
	if len(runs) != 1 || runs[0].Status != RoutineRunFailed || !strings.Contains(runs[0].ErrorSummary, "timeout") {
		t.Errorf("unexpected recorded slow run: %+v", runs)
	}
}

func TestRunRoutinePolicyRejection(t *testing.T) {
	cfg, v, dir := setupRoutineTestEnv(t)
	ctx := context.Background()

	// Handler attempts to write an event to forbidden compartment "finance".
	badHandler := writeFakeHandler(t, dir, "bad.sh", `#!/bin/sh
printf '{"stateUpdates":{"cursor":"should_not_commit"},"events":[{"compartment":"finance","summary":"secret"}]}\n'
`)
	m := RoutineManifest{
		ID:          "policy-test",
		Vault:       "work",
		Profile:     "automation",
		Residency:   "local",
		Schedule:    "@every 1h",
		Timezone:    "UTC",
		Timeout:     "5s",
		Idempotency: "run",
		Enabled:     true,
		Handler:     RoutineHandler{Command: []string{badHandler}, OutputFormat: "json"},
	}

	res, err := RunRoutine(ctx, cfg, m, RoutineRunOptions{})
	if !errors.Is(err, ErrForbidden) {
		t.Fatalf("err = %v, want ErrForbidden", err)
	}
	if res.Status != RoutineRunFailed {
		t.Fatalf("status = %q, want failed", res.Status)
	}

	if _, ok, err := v.GetRoutineState(ctx, "policy-test", "cursor"); err != nil || ok {
		t.Errorf("state cursor should not be committed on policy rejection (ok=%v, err=%v)", ok, err)
	}
}

func TestRunRoutineOutboxDecideBridge(t *testing.T) {
	cfg, v, dir := setupRoutineTestEnv(t)
	ctx := context.Background()

	outboxHandler := writeFakeHandler(t, dir, "outbox.sh", `#!/bin/sh
printf '{"outboxActions":[{"actionKind":"slack.post","payload":{"channel":"#ops","text":"hi"},"question":"Post to #ops?","verbatim":"hi"}]}\n'
`)
	m := RoutineManifest{
		ID:          "notifier",
		Vault:       "work",
		Profile:     "automation",
		Residency:   "local",
		Schedule:    "@every 1h",
		Timezone:    "UTC",
		Timeout:     "5s",
		Idempotency: "run",
		Enabled:     true,
		Handler:     RoutineHandler{Command: []string{outboxHandler}, OutputFormat: "json"},
	}

	fake := &fakeDecideSubmitter{id: "A-101"}
	res, err := RunRoutine(ctx, cfg, m, RoutineRunOptions{Decide: fake})
	if err != nil {
		t.Fatalf("RunRoutine: %v", err)
	}
	if res.OutboxAdded != 1 {
		t.Fatalf("OutboxAdded = %d, want 1", res.OutboxAdded)
	}
	if len(fake.reqs) != 1 || fake.reqs[0].Question != "Post to #ops?" || fake.reqs[0].Verbatim != "hi" {
		t.Fatalf("unexpected decide reqs: %+v", fake.reqs)
	}

	items, err := v.ListRoutineOutbox(ctx, RoutineOutboxFilter{RoutineID: "notifier"})
	if err != nil {
		t.Fatalf("ListRoutineOutbox: %v", err)
	}
	if len(items) != 1 || items[0].DecideID != "A-101" || items[0].Status != OutboxPendingApproval {
		t.Errorf("unexpected outbox items: %+v", items)
	}
}

func TestRunRoutineFlockSkipsWhenLocked(t *testing.T) {
	cfg, _, dir := setupRoutineTestEnv(t)
	unlock, locked, err := acquireRoutineLock(filepath.Join(dir, "vaults"), "locked-job")
	if err != nil || !locked {
		t.Fatalf("acquireRoutineLock: locked=%v err=%v", locked, err)
	}
	defer unlock()

	m := RoutineManifest{
		ID:          "locked-job",
		Vault:       "work",
		Profile:     "automation",
		Residency:   "local",
		Schedule:    "@every 1h",
		Timezone:    "UTC",
		Timeout:     "5s",
		Idempotency: "run",
		Enabled:     true,
		Handler:     RoutineHandler{Command: []string{"true"}, OutputFormat: "none"},
	}
	res, err := RunRoutine(context.Background(), cfg, m, RoutineRunOptions{})
	if err != nil {
		t.Fatalf("RunRoutine: %v", err)
	}
	if res.Status != RoutineRunSkipped || res.SkippedReason != "already running" {
		t.Errorf("unexpected result when locked: %+v", res)
	}
}

func TestTickDryRunAndExecution(t *testing.T) {
	cfg, v, dir := setupRoutineTestEnv(t)
	ctx := context.Background()

	marker := filepath.Join(dir, "ran.txt")
	h := writeFakeHandler(t, dir, "mark.sh", "#!/bin/sh\ntouch "+marker+"\n")

	manifests := []RoutineManifest{
		{
			ID: "local-due", Vault: "work", Profile: "automation", Residency: "local",
			Schedule: "@every 1h", Timezone: "UTC", Timeout: "5s", Idempotency: "date-hour", Enabled: true,
			Handler: RoutineHandler{Command: []string{h}, OutputFormat: "none"},
		},
		{
			ID: "disabled-job", Vault: "work", Profile: "automation", Residency: "local",
			Schedule: "@every 1h", Timezone: "UTC", Timeout: "5s", Idempotency: "date", Enabled: false,
			Handler: RoutineHandler{Command: []string{h}, OutputFormat: "none"},
		},
		{
			ID: "gcp-job", Vault: "work", Profile: "automation", Residency: "gcp",
			Schedule: "@every 1h", Timezone: "UTC", Timeout: "5s", Idempotency: "date", Enabled: true,
			Handler: RoutineHandler{Command: []string{h}, OutputFormat: "none"},
		},
	}

	now := time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC)
	dryRes, err := Tick(ctx, cfg, manifests, now, RoutineRunOptions{DryRun: true})
	if err != nil {
		t.Fatalf("Tick dry-run: %v", err)
	}
	if len(dryRes) != 1 || dryRes[0].RoutineID != "local-due" || !dryRes[0].DryRun {
		t.Fatalf("unexpected dry-run results: %+v", dryRes)
	}
	if _, err := os.Stat(marker); err == nil {
		t.Fatal("dry-run must not execute handler")
	}
	runs, err := v.ListRoutineRuns(ctx, RoutineRunFilter{})
	if err != nil {
		t.Fatalf("ListRoutineRuns: %v", err)
	}
	if len(runs) != 0 {
		t.Fatalf("dry-run must not record routine_runs, got %d", len(runs))
	}

	// Real tick executes local-due once.
	res, err := Tick(ctx, cfg, manifests, now, RoutineRunOptions{})
	if err != nil {
		t.Fatalf("Tick: %v", err)
	}
	if len(res) != 1 || res[0].Status != RoutineRunOK {
		t.Fatalf("unexpected tick results: %+v", res)
	}
	if _, err := os.Stat(marker); err != nil {
		t.Fatalf("handler did not run: %v", err)
	}

	// 30 minutes later (< 1h), local-due is not due yet.
	resLater, err := Tick(ctx, cfg, manifests, now.Add(30*time.Minute), RoutineRunOptions{})
	if err != nil {
		t.Fatalf("Tick later: %v", err)
	}
	if len(resLater) != 0 {
		t.Errorf("expected 0 due routines, got %+v", resLater)
	}
}
