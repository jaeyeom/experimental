package memstore

import (
	"context"
	"errors"
	"testing"
)

func TestPutAndGetEntityAndAliases(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatal(err)
	}
	defer svc.Close()

	ctx := context.Background()
	actor := Actor{Caller: "cli", Reason: "test"}

	ent, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "grace-hopper",
		Title:              "Grace Hopper",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Summary:            "Compiler pioneer",
		PinnedL1:           true,
		Aliases:            []string{"  Amazing   Grace ", "amazing grace", "Rear Admiral Hopper"},
	}, actor)
	if err != nil {
		t.Fatalf("PutEntity: %v", err)
	}
	if ent.ID != "person:grace-hopper" {
		t.Errorf("ID = %q, want person:grace-hopper", ent.ID)
	}

	// Update entity and replace aliases.
	ent2, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "grace-hopper",
		Title:              "Grace Murray Hopper",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Aliases:            []string{"Grandma COBOL"},
	}, actor)
	if err != nil {
		t.Fatalf("PutEntity update: %v", err)
	}
	if !ent2.CreatedAt.Equal(ent.CreatedAt) {
		t.Errorf("CreatedAt changed on update: %v -> %v", ent.CreatedAt, ent2.CreatedAt)
	}

	view, err := svc.GetEntity(ctx, "work", "person:grace-hopper", actor)
	if err != nil {
		t.Fatalf("GetEntity: %v", err)
	}
	if view.Entity.Title != "Grace Murray Hopper" {
		t.Errorf("Title = %q, want Grace Murray Hopper", view.Entity.Title)
	}
	if len(view.Aliases) != 1 || view.Aliases[0] != "Grandma COBOL" {
		t.Errorf("Aliases = %v, want [Grandma COBOL]", view.Aliases)
	}

	// Old alias should no longer resolve.
	if _, err := svc.ResolveEntity(ctx, "amazing grace", actor); !errors.Is(err, ErrNotFound) {
		t.Errorf("ResolveEntity(old alias) = %v, want ErrNotFound", err)
	}
}

func TestResolveEntityPriorityAndAmbiguity(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatal(err)
	}
	defer svc.Close()

	ctx := context.Background()
	actor := Actor{Caller: "cli", Reason: "resolve"}

	for _, in := range []EntityInput{
		{Kind: "person", Slug: "alex-chen", Title: "Alex Chen", DefaultCompartment: "engineering", Sensitivity: SensitivityNormal, Aliases: []string{"Alex C", "AC"}},
		{Kind: "person", Slug: "alex-kim", Title: "Alex Kim", DefaultCompartment: "engineering", Sensitivity: SensitivityNormal, Aliases: []string{"Alex K"}},
		{Kind: "project", Slug: "ac", Title: "Project AC", DefaultCompartment: "engineering", Sensitivity: SensitivityNormal},
	} {
		if _, err := svc.PutEntity(ctx, "work", in, actor); err != nil {
			t.Fatalf("PutEntity(%s): %v", in.Slug, err)
		}
	}

	// 1. Exact ID.
	m, err := svc.ResolveEntity(ctx, "person:alex-chen", actor)
	if err != nil || len(m) != 1 || m[0].Entity.ID != "person:alex-chen" || m[0].Match != "exact_id" {
		t.Errorf("exact ID = (%+v, %v), want person:alex-chen", m, err)
	}

	// 2. Exact normalized alias beats exact slug of project:ac.
	m, err = svc.ResolveEntity(ctx, "  aC ", actor)
	if err != nil || len(m) != 1 || m[0].Entity.ID != "person:alex-chen" || m[0].Match != "exact_alias" {
		t.Errorf("exact alias = (%+v, %v), want person:alex-chen", m, err)
	}

	// 3. Exact slug.
	m, err = svc.ResolveEntity(ctx, "alex-kim", actor)
	if err != nil || len(m) != 1 || m[0].Entity.ID != "person:alex-kim" || m[0].Match != "exact_slug" {
		t.Errorf("exact slug = (%+v, %v), want person:alex-kim", m, err)
	}

	// 4. Ambiguous prefix match returns ErrAmbiguous with candidate list.
	_, err = svc.ResolveEntity(ctx, "alex", actor)
	if !errors.Is(err, ErrAmbiguous) {
		t.Fatalf("ResolveEntity(alex) = %v, want ErrAmbiguous", err)
	}
	var amb *AmbiguousError
	if !errors.As(err, &amb) || len(amb.Candidates) != 2 {
		t.Errorf("AmbiguousError candidates = %+v, want 2", amb)
	}

	// 5. Not found.
	if _, err := svc.ResolveEntity(ctx, "nobody", actor); !errors.Is(err, ErrNotFound) {
		t.Errorf("ResolveEntity(nobody) = %v, want ErrNotFound", err)
	}
}
