package memstore

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestRoutineStateCRUD(t *testing.T) {
	v, err := InitVault(testVaultConfig(t))
	if err != nil {
		t.Fatal(err)
	}
	defer v.Close()

	ctx := context.Background()
	if _, ok, err := v.GetRoutineState(ctx, "digest", "cursor"); err != nil || ok {
		t.Fatalf("initial GetRoutineState = (%v, %v), want false nil", ok, err)
	}

	if err := v.SetRoutineState(ctx, "digest", "cursor", `{"seq":1}`); err != nil {
		t.Fatalf("SetRoutineState: %v", err)
	}
	if err := v.SetRoutineState(ctx, "digest", "cursor", `{"seq":2}`); err != nil {
		t.Fatalf("SetRoutineState update: %v", err)
	}
	if err := v.SetRoutineState(ctx, "digest", "mode", `"fast"`); err != nil {
		t.Fatalf("SetRoutineState mode: %v", err)
	}

	val, ok, err := v.GetRoutineState(ctx, "digest", "cursor")
	if err != nil || !ok || val != `{"seq":2}` {
		t.Errorf("GetRoutineState = (%q, %v, %v), want {\"seq\":2}, true, nil", val, ok, err)
	}

	all, err := v.AllRoutineState(ctx, "digest")
	if err != nil {
		t.Fatalf("AllRoutineState: %v", err)
	}
	if len(all) != 2 || all["cursor"] != `{"seq":2}` || all["mode"] != `"fast"` {
		t.Errorf("AllRoutineState = %v", all)
	}

	if err := v.SetRoutineState(ctx, "digest", "bad", `{not json`); err == nil {
		t.Error("SetRoutineState invalid JSON = nil, want error")
	}
}

func TestRoutineRunsIdempotencyAndLifecycle(t *testing.T) {
	v, err := InitVault(testVaultConfig(t))
	if err != nil {
		t.Fatal(err)
	}
	defer v.Close()

	now := time.Date(2026, 5, 1, 9, 0, 0, 0, time.UTC)
	v.now = func() time.Time { return now }
	ctx := context.Background()

	run1, err := v.BeginRoutineRun(ctx, "digest", "2026-05-01", "timer")
	if err != nil {
		t.Fatalf("BeginRoutineRun: %v", err)
	}
	if run1.Status != RoutineRunRunning || !run1.StartedAt.Equal(now) {
		t.Errorf("unexpected run1: %+v", run1)
	}

	// Duplicate (routine_id, idempotency_key) must return ErrAlreadyRan.
	if _, err := v.BeginRoutineRun(ctx, "digest", "2026-05-01", "manual"); !errors.Is(err, ErrAlreadyRan) {
		t.Fatalf("duplicate BeginRoutineRun = %v, want ErrAlreadyRan", err)
	}

	// Different routine or different key succeeds.
	run2, err := v.BeginRoutineRun(ctx, "other", "2026-05-01", "timer")
	if err != nil {
		t.Fatalf("BeginRoutineRun other routine: %v", err)
	}

	now = now.Add(5 * time.Second)
	code := 0
	if err := v.FinishRoutineRun(ctx, run1.ID, RoutineRunOK, &code, `{"count":3}`, ""); err != nil {
		t.Fatalf("FinishRoutineRun: %v", err)
	}
	if err := v.FinishRoutineRun(ctx, "run_missing", RoutineRunOK, nil, "", ""); !errors.Is(err, ErrNotFound) {
		t.Errorf("FinishRoutineRun missing = %v, want ErrNotFound", err)
	}

	runs, err := v.ListRoutineRuns(ctx, RoutineRunFilter{RoutineID: "digest"})
	if err != nil {
		t.Fatalf("ListRoutineRuns: %v", err)
	}
	if len(runs) != 1 || runs[0].Status != RoutineRunOK || runs[0].ExitCode == nil || *runs[0].ExitCode != 0 || runs[0].FinishedAt == nil {
		t.Errorf("unexpected runs: %+v", runs)
	}

	allRunning, err := v.ListRoutineRuns(ctx, RoutineRunFilter{Status: RoutineRunRunning})
	if err != nil {
		t.Fatalf("ListRoutineRuns running: %v", err)
	}
	if len(allRunning) != 1 || allRunning[0].ID != run2.ID {
		t.Errorf("running runs = %+v, want only %s", allRunning, run2.ID)
	}
}

func TestRoutineOutboxLifecycle(t *testing.T) {
	v, err := InitVault(testVaultConfig(t))
	if err != nil {
		t.Fatal(err)
	}
	defer v.Close()

	now := time.Date(2026, 5, 1, 10, 0, 0, 0, time.UTC)
	v.now = func() time.Time { return now }
	ctx := context.Background()

	item, err := v.AddRoutineOutbox(ctx, "digest", "send_email", `{"to":"a@example.com"}`, "dec-1")
	if err != nil {
		t.Fatalf("AddRoutineOutbox: %v", err)
	}
	if item.Status != OutboxPendingApproval || item.DecideID != "dec-1" {
		t.Errorf("unexpected outbox item: %+v", item)
	}

	if err := v.UpdateRoutineOutbox(ctx, item.ID, OutboxApproved, "", nil); err != nil {
		t.Fatalf("UpdateRoutineOutbox approved: %v", err)
	}
	now = now.Add(time.Minute)
	if err := v.UpdateRoutineOutbox(ctx, item.ID, OutboxExecuted, "", nil); err != nil {
		t.Fatalf("UpdateRoutineOutbox executed: %v", err)
	}
	if err := v.UpdateRoutineOutbox(ctx, "out_missing", OutboxRejected, "", nil); !errors.Is(err, ErrNotFound) {
		t.Errorf("UpdateRoutineOutbox missing = %v, want ErrNotFound", err)
	}

	items, err := v.ListRoutineOutbox(ctx, RoutineOutboxFilter{RoutineID: "digest", Status: OutboxExecuted})
	if err != nil {
		t.Fatalf("ListRoutineOutbox: %v", err)
	}
	if len(items) != 1 || items[0].DecideID != "dec-1" || items[0].ExecutedAt == nil || !items[0].ExecutedAt.Equal(now) {
		t.Errorf("unexpected outbox list: %+v", items)
	}
}
