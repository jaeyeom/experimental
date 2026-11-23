package memstore

import (
	"context"
	"encoding/json"
	"errors"
	"testing"
)

func TestTriageEnqueueListApproveReject(t *testing.T) {
	ctx := context.Background()
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)
	svc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService: %v", err)
	}
	defer svc.Close()

	payload := TriagePayload{
		Entity: &EntityInput{
			Kind:    "person",
			Slug:    "alice",
			Title:   "Alice Smith",
			Aliases: []string{"Alice"},
		},
		Facts: []FactInput{
			{Key: "role", ValueJSON: `"Staff Engineer"`, SourceType: "aside_import"},
		},
		NoteBlocks: []NoteBlockInput{
			{SectionKey: "background", Heading: "Background", SortOrder: 10, BodyMD: "Met at KubeCon."},
		},
	}
	rawPayload, err := json.Marshal(payload)
	if err != nil {
		t.Fatalf("marshal payload: %v", err)
	}

	item, err := svc.EnqueueTriage(ctx, "work", TriageInput{
		Source:              "aside_import",
		ProposedCompartment: "general",
		ProposedSensitivity: SensitivityPrivate,
		TargetEntityID:      "person:alice",
		PayloadJSON:         string(rawPayload),
	})
	if err != nil {
		t.Fatalf("EnqueueTriage: %v", err)
	}
	if item.Status != TriageStatusPending {
		t.Fatalf("status = %q, want %q", item.Status, TriageStatusPending)
	}

	// Must remain invisible to GetEntity and Search until approved.
	actor := Actor{Caller: "test", Reason: "verify"}
	if _, err := svc.GetEntity(ctx, "work", "person:alice", actor); !errors.Is(err, ErrNotFound) {
		t.Fatalf("expected ErrNotFound before approve, got %v", err)
	}
	hits, err := NewSQLiteRetriever(svc).Search(ctx, SearchQuery{Query: "KubeCon", Actor: actor})
	if err != nil {
		t.Fatalf("Search: %v", err)
	}
	if len(hits) != 0 {
		t.Fatalf("expected 0 search hits before approve, got %d", len(hits))
	}

	// ListTriage respects profile scope: "work" profile cannot see private/general.
	workSvc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService work: %v", err)
	}
	defer workSvc.Close()

	scopedItems, err := workSvc.ListTriage(ctx, TriageFilter{Status: TriageStatusPending})
	if err != nil {
		t.Fatalf("ListTriage work: %v", err)
	}
	if len(scopedItems) != 0 {
		t.Fatalf("expected 0 scoped items for work, got %d", len(scopedItems))
	}

	adminItems, err := svc.ListTriage(ctx, TriageFilter{Status: TriageStatusPending})
	if err != nil {
		t.Fatalf("ListTriage admin: %v", err)
	}
	if len(adminItems) != 1 || adminItems[0].ID != item.ID {
		t.Fatalf("admin ListTriage = %+v, want item %s", adminItems, item.ID)
	}

	// Approve with override compartment/sensitivity.
	if err := svc.ApproveTriage(ctx, "work", item.ID, "engineering", SensitivityNormal, actor); err != nil {
		t.Fatalf("ApproveTriage: %v", err)
	}

	// Now visible via GetEntity and Search.
	view, err := svc.GetEntity(ctx, "work", "person:alice", actor)
	if err != nil {
		t.Fatalf("GetEntity after approve: %v", err)
	}
	if view.Entity.Title != "Alice Smith" || view.Entity.DefaultCompartment != "engineering" || view.Entity.Sensitivity != SensitivityNormal {
		t.Fatalf("unexpected entity after approve: %+v", view.Entity)
	}
	if len(view.Facts) != 1 || view.Facts[0].Key != "role" {
		t.Fatalf("unexpected facts after approve: %+v", view.Facts)
	}
	if len(view.NoteBlocks) != 1 || view.NoteBlocks[0].BodyMD != "Met at KubeCon." {
		t.Fatalf("unexpected note blocks after approve: %+v", view.NoteBlocks)
	}
	hits, err = NewSQLiteRetriever(svc).Search(ctx, SearchQuery{Query: "KubeCon", Actor: actor})
	if err != nil {
		t.Fatalf("Search after approve: %v", err)
	}
	if len(hits) != 1 {
		t.Fatalf("expected 1 search hit after approve, got %d", len(hits))
	}

	// Approving again fails.
	if err := svc.ApproveTriage(ctx, "work", item.ID, "", "", actor); err == nil {
		t.Fatalf("expected error re-approving resolved item")
	}

	// Enqueue and reject another item.
	item2, err := svc.EnqueueTriage(ctx, "work", TriageInput{
		Source:              "aside_import",
		ProposedCompartment: "general",
		ProposedSensitivity: SensitivityNormal,
		PayloadJSON:         string(rawPayload),
	})
	if err != nil {
		t.Fatalf("EnqueueTriage 2: %v", err)
	}
	if err := svc.RejectTriage(ctx, "work", item2.ID, actor); err != nil {
		t.Fatalf("RejectTriage: %v", err)
	}
	rejected, err := svc.ListTriage(ctx, TriageFilter{Status: TriageStatusRejected})
	if err != nil {
		t.Fatalf("ListTriage rejected: %v", err)
	}
	if len(rejected) != 1 || rejected[0].ID != item2.ID {
		t.Fatalf("rejected = %+v, want [%s]", rejected, item2.ID)
	}
}
