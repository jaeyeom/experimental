package memstore

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func testMultiVaultConfig(t *testing.T) Config {
	t.Helper()
	dir := t.TempDir()
	return Config{
		Vaults: map[string]VaultConfig{
			"work": {
				Name:      "work",
				Path:      filepath.Join(dir, "work.db"),
				Residency: ResidencyLocalOnly,
			},
			"personal": {
				Name:      "personal",
				Path:      filepath.Join(dir, "personal.db"),
				Residency: ResidencyLocalOnly,
			},
		},
		Profiles: map[string]ProfileConfig{
			"admin": {
				Name:           "admin",
				Vaults:         []string{"work", "personal"},
				Compartments:   []string{"general", "engineering", "finance", "relationships", "personal"},
				MaxSensitivity: SensitivityRestricted,
				Capabilities:   []Capability{CapabilityRead, CapabilityWrite, CapabilityWriteEpisodic},
			},
			"work": {
				Name:           "work",
				Vaults:         []string{"work"},
				Compartments:   []string{"engineering"},
				MaxSensitivity: SensitivityNormal,
				Capabilities:   []Capability{CapabilityRead, CapabilityWrite},
			},
			"episodic-only": {
				Name:           "episodic-only",
				Vaults:         []string{"work"},
				Compartments:   []string{"engineering"},
				MaxSensitivity: SensitivityNormal,
				Capabilities:   []Capability{CapabilityWriteEpisodic},
			},
		},
		RetentionDays: DefaultRetentionDays,
	}
}

func initAllVaults(t *testing.T, cfg Config) {
	t.Helper()
	for _, vc := range cfg.Vaults {
		v, err := InitVault(vc)
		if err != nil {
			t.Fatalf("InitVault(%s): %v", vc.Name, err)
		}
		if err := v.Close(); err != nil {
			t.Fatalf("Close(%s): %v", vc.Name, err)
		}
	}
}

func TestOpenServiceProfileEnforcement(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	v, err := InitVault(cfg.Vaults["work"])
	if err != nil {
		t.Fatal(err)
	}
	_ = v.Close()
	// Intentionally do NOT initialize cfg.Vaults["personal"] — OpenService with
	// "work" profile must succeed because it never opens unpermitted vaults.
	cfg.Vaults["personal"] = VaultConfig{
		Name:      "personal",
		Path:      filepath.Join(t.TempDir(), "does-not-exist", "personal.db"),
		Residency: ResidencyLocalOnly,
	}

	if _, err := OpenService(cfg, ""); !errors.Is(err, ErrForbidden) {
		t.Errorf("OpenService empty profile = %v, want ErrForbidden", err)
	}
	if _, err := OpenService(cfg, "nonexistent"); !errors.Is(err, ErrForbidden) {
		t.Errorf("OpenService unknown profile = %v, want ErrForbidden", err)
	}

	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	defer svc.Close()

	if _, err := svc.openVault("personal"); !errors.Is(err, ErrForbidden) {
		t.Errorf("openVault(personal) = %v, want ErrForbidden", err)
	}
}

func TestCrossCompartmentAndCrossVaultLeakage(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	adminActor := Actor{Caller: "admin-cli", Reason: "seed"}

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

	// Shared entity in work vault.
	_, err = adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada Lovelace",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Aliases:            []string{"Ada"},
	}, adminActor)
	if err != nil {
		t.Fatalf("PutEntity work: %v", err)
	}
	_, err = adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "bob",
		Title:              "Bob Secret",
		DefaultCompartment: "finance",
		Sensitivity:        SensitivityPrivate,
		Aliases:            []string{"Bobby"},
	}, adminActor)
	if err != nil {
		t.Fatalf("PutEntity work finance: %v", err)
	}

	// Facts in engineering (normal), finance (private), relationships (restricted).
	for _, tc := range []FactInput{
		{EntityID: "person:ada", Key: "role", ValueJSON: `"Staff Engineer"`, Compartment: "engineering", Sensitivity: SensitivityNormal, SourceType: "manual"},
		{EntityID: "person:ada", Key: "salary", ValueJSON: `"SECRET_COMP"`, Compartment: "finance", Sensitivity: SensitivityPrivate, SourceType: "manual"},
		{EntityID: "person:ada", Key: "partner", ValueJSON: `"SECRET_PARTNER"`, Compartment: "relationships", Sensitivity: SensitivityRestricted, SourceType: "manual"},
	} {
		if _, err := adminSvc.SetFact(ctx, "work", tc, adminActor); err != nil {
			t.Fatalf("SetFact(%s): %v", tc.Key, err)
		}
	}

	// Note blocks in engineering, finance, relationships.
	for _, nb := range []NoteBlockInput{
		{EntityID: "person:ada", SectionKey: "arch", Heading: "Architecture", BodyMD: "eng notes", Compartment: "engineering", Sensitivity: SensitivityNormal},
		{EntityID: "person:ada", SectionKey: "comp", Heading: "Compensation", BodyMD: "SECRET_FINANCE_NOTE", Compartment: "finance", Sensitivity: SensitivityPrivate},
		{EntityID: "person:ada", SectionKey: "family", Heading: "Family", BodyMD: "SECRET_REL_NOTE", Compartment: "relationships", Sensitivity: SensitivityRestricted},
	} {
		if _, err := adminSvc.SetNoteBlock(ctx, "work", nb, adminActor); err != nil {
			t.Fatalf("SetNoteBlock(%s): %v", nb.SectionKey, err)
		}
	}

	// Links in engineering and finance.
	if err := adminSvc.LinkEntities(ctx, "work", LinkInput{
		FromEntityID: "person:ada",
		Relation:     "works_with",
		ToEntityID:   "person:bob",
		Compartment:  "engineering",
		Sensitivity:  SensitivityNormal,
	}, adminActor); err != nil {
		t.Fatalf("LinkEntities eng: %v", err)
	}
	if err := adminSvc.LinkEntities(ctx, "work", LinkInput{
		FromEntityID: "person:ada",
		Relation:     "invests_with",
		ToEntityID:   "person:bob",
		Compartment:  "finance",
		Sensitivity:  SensitivityPrivate,
	}, adminActor); err != nil {
		t.Fatalf("LinkEntities finance: %v", err)
	}

	// Episodic events in engineering, finance, relationships.
	now := time.Now().UTC()
	for _, ev := range []EventInput{
		{OccurredAt: now, EntityID: "person:ada", Compartment: "engineering", Sensitivity: SensitivityNormal, Summary: "Reviewed PR", Reporter: "bot"},
		{OccurredAt: now, EntityID: "person:ada", Compartment: "finance", Sensitivity: SensitivityPrivate, Summary: "SECRET_BONUS_EVENT", Reporter: "bot"},
		{OccurredAt: now, EntityID: "person:ada", Compartment: "relationships", Sensitivity: SensitivityRestricted, Summary: "SECRET_DINNER_EVENT", Reporter: "bot"},
	} {
		if _, err := adminSvc.AddEvent(ctx, "work", ev, adminActor); err != nil {
			t.Fatalf("AddEvent(%s): %v", ev.Compartment, err)
		}
	}

	// Separate personal vault data.
	_, err = adminSvc.PutEntity(ctx, "personal", EntityInput{
		Kind:               "person",
		Slug:               "mom",
		Title:              "Mom",
		DefaultCompartment: "personal",
		Sensitivity:        SensitivityNormal,
	}, adminActor)
	if err != nil {
		t.Fatalf("PutEntity personal: %v", err)
	}
	if _, err := adminSvc.SetFact(ctx, "personal", FactInput{
		EntityID: "person:mom", Key: "birthday", ValueJSON: `"1960-01-01"`, Compartment: "personal", Sensitivity: SensitivityNormal, SourceType: "manual",
	}, adminActor); err != nil {
		t.Fatalf("SetFact personal: %v", err)
	}
	if _, err := adminSvc.AddEvent(ctx, "personal", EventInput{
		OccurredAt: now, EntityID: "person:mom", Compartment: "personal", Sensitivity: SensitivityNormal, Summary: "Called Mom", Reporter: "bot",
	}, adminActor); err != nil {
		t.Fatalf("AddEvent personal: %v", err)
	}

	// Now open work profile and verify zero leakage.
	workSvc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	defer workSvc.Close()

	workActor := Actor{Caller: "work-agent", Reason: "standup"}

	view, err := workSvc.GetEntity(ctx, "work", "person:ada", workActor)
	if err != nil {
		t.Fatalf("GetEntity(person:ada): %v", err)
	}
	if len(view.Facts) != 1 || view.Facts[0].Key != "role" {
		t.Errorf("GetEntity facts leaked or missing: %+v", view.Facts)
	}
	if len(view.NoteBlocks) != 1 || view.NoteBlocks[0].SectionKey != "arch" {
		t.Errorf("GetEntity note blocks leaked or missing: %+v", view.NoteBlocks)
	}
	if len(view.Links) != 1 || view.Links[0].Relation != "works_with" {
		t.Errorf("GetEntity links leaked or missing: %+v", view.Links)
	}

	if _, err := workSvc.GetEntity(ctx, "personal", "person:mom", workActor); !errors.Is(err, ErrForbidden) {
		t.Errorf("GetEntity personal vault = %v, want ErrForbidden", err)
	}
	if _, err := workSvc.GetEntity(ctx, "work", "person:bob", workActor); !errors.Is(err, ErrForbidden) {
		t.Errorf("GetEntity finance entity = %v, want ErrForbidden", err)
	}

	if _, err := workSvc.ResolveEntity(ctx, "mom", workActor); !errors.Is(err, ErrNotFound) {
		t.Errorf("ResolveEntity(mom) = %v, want ErrNotFound", err)
	}
	if _, err := workSvc.ResolveEntity(ctx, "bob", workActor); !errors.Is(err, ErrNotFound) {
		t.Errorf("ResolveEntity(bob) = %v, want ErrNotFound", err)
	}

	facts, err := workSvc.ListFacts(ctx, FactFilter{}, workActor)
	if err != nil {
		t.Fatalf("ListFacts: %v", err)
	}
	if len(facts) != 1 || facts[0].Compartment != "engineering" {
		t.Errorf("ListFacts leaked: %+v", facts)
	}

	notes, err := workSvc.ListNoteBlocks(ctx, "work", "person:ada", workActor)
	if err != nil {
		t.Fatalf("ListNoteBlocks: %v", err)
	}
	if len(notes) != 1 || notes[0].Compartment != "engineering" {
		t.Errorf("ListNoteBlocks leaked: %+v", notes)
	}

	events, err := workSvc.ListEvents(ctx, EventFilter{}, workActor)
	if err != nil {
		t.Fatalf("ListEvents: %v", err)
	}
	if len(events) != 1 || events[0].Compartment != "engineering" {
		t.Errorf("ListEvents leaked: %+v", events)
	}

	// Verify access_log recorded every work-agent read without leaking values.
	workVault := adminSvc.vaults["work"]
	logs, err := workVault.QueryAccessLog(ctx, AccessLogFilter{Caller: "work-agent"})
	if err != nil {
		t.Fatalf("QueryAccessLog: %v", err)
	}
	if len(logs) != 6 {
		t.Errorf("access_log count = %d, want 6 (GetEntity, 2x ResolveEntity, ListFacts, ListNoteBlocks, ListEvents)", len(logs))
	}
	for _, l := range logs {
		if strings.Contains(l.Command+l.Reason+l.EntityID, "SECRET") || strings.Contains(l.Command+l.Reason+l.EntityID, "mom") {
			t.Errorf("access_log leaked sensitive data: %+v", l)
		}
	}
}
