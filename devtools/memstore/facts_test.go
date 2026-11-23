package memstore

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestSetFactSupersedesAndRecordsRevision(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatal(err)
	}
	defer svc.Close()

	ctx := context.Background()
	actor := Actor{Caller: "cli", Reason: "update role"}

	if _, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
	}, actor); err != nil {
		t.Fatal(err)
	}

	f1, err := svc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "role",
		ValueJSON:   `"Engineer"`,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, actor)
	if err != nil {
		t.Fatalf("SetFact f1: %v", err)
	}

	f2, err := svc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "role",
		ValueJSON:   `"Staff Engineer"`,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, actor)
	if err != nil {
		t.Fatalf("SetFact f2: %v", err)
	}

	active, err := svc.ListFacts(ctx, FactFilter{EntityID: "person:ada", Key: "role"}, actor)
	if err != nil {
		t.Fatalf("ListFacts active: %v", err)
	}
	if len(active) != 1 || active[0].ID != f2.ID {
		t.Fatalf("active facts = %+v, want only %s", active, f2.ID)
	}

	all, err := svc.ListFacts(ctx, FactFilter{EntityID: "person:ada", Key: "role", IncludeSuperseded: true}, actor)
	if err != nil {
		t.Fatalf("ListFacts all: %v", err)
	}
	if len(all) != 2 {
		t.Fatalf("all facts len = %d, want 2", len(all))
	}
	byID := make(map[string]Fact, len(all))
	for _, f := range all {
		byID[f.ID] = f
	}
	if byID[f1.ID].SupersededBy != f2.ID {
		t.Errorf("f1 = %+v, want SupersededBy=%s", byID[f1.ID], f2.ID)
	}
	if byID[f2.ID].SupersededBy != "" {
		t.Errorf("f2 = %+v, want empty SupersededBy", byID[f2.ID])
	}

	// Verify revision entry for supersede.
	var op string
	err = svc.vaults["work"].db.QueryRowContext(ctx,
		`SELECT op FROM revisions WHERE table_name = 'facts' AND row_id = ? AND op = 'supersede'`,
		f1.ID,
	).Scan(&op)
	if err != nil {
		t.Errorf("supersede revision missing for %s: %v", f1.ID, err)
	}
}

func TestListFactsExpiredFilterAndForbiddenWrite(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatal(err)
	}
	defer svc.Close()

	ctx := context.Background()
	actor := Actor{Caller: "cli", Reason: "test"}
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)
	svc.vaults["work"].now = func() time.Time { return now }

	if _, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
	}, actor); err != nil {
		t.Fatal(err)
	}

	past := now.Add(-time.Hour)
	if _, err := svc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "temp_badge",
		ValueJSON:   `"B-12"`,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
		ValidUntil:  &past,
	}, actor); err != nil {
		t.Fatal(err)
	}

	active, err := svc.ListFacts(ctx, FactFilter{EntityID: "person:ada"}, actor)
	if err != nil {
		t.Fatal(err)
	}
	if len(active) != 0 {
		t.Errorf("active facts = %+v, want empty", active)
	}

	withExp, err := svc.ListFacts(ctx, FactFilter{EntityID: "person:ada", IncludeExpired: true}, actor)
	if err != nil {
		t.Fatal(err)
	}
	if len(withExp) != 1 {
		t.Errorf("withExp = %+v, want 1", withExp)
	}

	// Forbidden compartment write.
	_, err = svc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "salary",
		ValueJSON:   `"100"`,
		Compartment: "finance",
		Sensitivity: SensitivityPrivate,
		SourceType:  "manual",
	}, actor)
	if !errors.Is(err, ErrForbidden) {
		t.Errorf("SetFact finance = %v, want ErrForbidden", err)
	}
}
