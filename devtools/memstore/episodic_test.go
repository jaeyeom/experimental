package memstore

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestAddEventIdempotencyAndCapabilities(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "ingest", Reason: "sync"}

	// episodic-only profile has CapabilityWriteEpisodic (not CapabilityWrite).
	epSvc, err := OpenService(cfg, "episodic-only")
	if err != nil {
		t.Fatal(err)
	}
	defer epSvc.Close()

	ev1, err := epSvc.AddEvent(ctx, "work", EventInput{
		OccurredAt:  time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC),
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Channel:     "slack",
		SourceRef:   "msg-123",
		Summary:     "Deploy finished",
		Reporter:    "slackwatch",
	}, actor)
	if err != nil {
		t.Fatalf("AddEvent: %v", err)
	}

	// Duplicate (channel, source_ref) must be idempotent.
	ev2, err := epSvc.AddEvent(ctx, "work", EventInput{
		OccurredAt:  time.Date(2026, 6, 1, 10, 5, 0, 0, time.UTC),
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Channel:     "slack",
		SourceRef:   "msg-123",
		Summary:     "Deploy finished",
		Reporter:    "slackwatch",
	}, actor)
	if err != nil {
		t.Fatalf("AddEvent duplicate: %v", err)
	}
	if ev1.ID != ev2.ID {
		t.Errorf("duplicate AddEvent created new ID %q != %q", ev2.ID, ev1.ID)
	}

	// episodic-only lacks CapabilityRead.
	if _, err := epSvc.ListEvents(ctx, EventFilter{}, actor); !errors.Is(err, ErrForbidden) {
		t.Errorf("ListEvents without read = %v, want ErrForbidden", err)
	}
}

func TestListEventsFiltersAndCompacted(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatal(err)
	}
	defer svc.Close()

	ctx := context.Background()
	actor := Actor{Caller: "cli", Reason: "list"}
	t0 := time.Date(2026, 6, 1, 8, 0, 0, 0, time.UTC)

	e1, err := svc.AddEvent(ctx, "work", EventInput{
		OccurredAt:  t0,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Channel:     "git",
		Summary:     "Commit 1",
		Reporter:    "cli",
	}, actor)
	if err != nil {
		t.Fatal(err)
	}
	e2, err := svc.AddEvent(ctx, "work", EventInput{
		OccurredAt:  t0.Add(time.Hour),
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Channel:     "slack",
		Summary:     "Slack 1",
		Reporter:    "cli",
	}, actor)
	if err != nil {
		t.Fatal(err)
	}

	// Mark e1 as compacted into e2.
	if _, err := svc.vaults["work"].db.ExecContext(ctx,
		`UPDATE episodic_events SET compacted_into_id = ? WHERE id = ?`, e2.ID, e1.ID,
	); err != nil {
		t.Fatal(err)
	}

	uncompacted, err := svc.ListEvents(ctx, EventFilter{}, actor)
	if err != nil {
		t.Fatal(err)
	}
	if len(uncompacted) != 1 || uncompacted[0].ID != e2.ID {
		t.Errorf("uncompacted = %+v, want only %s", uncompacted, e2.ID)
	}

	all, err := svc.ListEvents(ctx, EventFilter{IncludeCompacted: true, Since: t0, Until: t0.Add(2 * time.Hour), Limit: 10}, actor)
	if err != nil {
		t.Fatal(err)
	}
	if len(all) != 2 {
		t.Errorf("all len = %d, want 2", len(all))
	}

	byChan, err := svc.ListEvents(ctx, EventFilter{Channel: "slack"}, actor)
	if err != nil {
		t.Fatal(err)
	}
	if len(byChan) != 1 || byChan[0].Channel != "slack" {
		t.Errorf("byChan = %+v, want slack event", byChan)
	}
}
