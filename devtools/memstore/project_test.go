package memstore

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func testExportConfig(t *testing.T) Config {
	t.Helper()
	cfg := testMultiVaultConfig(t)
	admin := cfg.Profiles["admin"]
	admin.Capabilities = append(admin.Capabilities, CapabilityExport)
	cfg.Profiles["admin"] = admin

	work := cfg.Profiles["work"]
	work.Capabilities = append(work.Capabilities, CapabilityExport)
	cfg.Profiles["work"] = work

	cfg.Profiles["no-export"] = ProfileConfig{
		Name:           "no-export",
		Vaults:         []string{"work"},
		Compartments:   []string{"engineering"},
		MaxSensitivity: SensitivityNormal,
		Capabilities:   []Capability{CapabilityRead},
	}
	return cfg
}

func fixedNow() time.Time {
	return time.Date(2026, 6, 15, 10, 30, 0, 0, time.UTC)
}

func TestExportRequiresCapabilityAndRejectsGit(t *testing.T) {
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	outDir := filepath.Join(t.TempDir(), "export")

	noExpSvc, err := OpenService(cfg, "no-export")
	if err != nil {
		t.Fatalf("OpenService(no-export): %v", err)
	}
	defer noExpSvc.Close()

	if _, err := Export(ctx, noExpSvc, outDir, ExportOptions{Now: fixedNow}); !errors.Is(err, ErrForbidden) {
		t.Errorf("Export without CapExport = %v, want ErrForbidden", err)
	}

	workSvc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	defer workSvc.Close()

	gitRoot := t.TempDir()
	if err := os.Mkdir(filepath.Join(gitRoot, ".git"), 0o700); err != nil {
		t.Fatalf("mkdir .git: %v", err)
	}
	gitOut := filepath.Join(gitRoot, "exported")
	if _, err := Export(ctx, workSvc, gitOut, ExportOptions{Now: fixedNow}); err == nil {
		t.Errorf("Export inside git work tree succeeded, want error")
	}

	rep, err := Export(ctx, workSvc, gitOut, ExportOptions{AllowGit: true, Now: fixedNow})
	if err != nil {
		t.Fatalf("Export with AllowGit=true: %v", err)
	}
	if rep.Written == 0 {
		t.Errorf("expected written > 0, got %+v", rep)
	}
}

func TestExportIdempotentScopingAndStaleRemoval(t *testing.T) {
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "test-seed", Reason: "export-test"}

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

	// Seed work vault (engineering/normal + finance/private).
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada Lovelace",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Summary:            "Analytical engine pioneer",
		PinnedL1:           true,
		Aliases:            []string{"Countess", "Ada"},
	}, actor); err != nil {
		t.Fatalf("PutEntity ada: %v", err)
	}
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "agent",
		Slug:               "helper",
		Title:              "Helper Agent",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("PutEntity agent: %v", err)
	}
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "bob",
		Title:              "Bob Secret",
		DefaultCompartment: "finance",
		Sensitivity:        SensitivityPrivate,
	}, actor); err != nil {
		t.Fatalf("PutEntity bob: %v", err)
	}

	// Seed personal vault (must not leak to work profile).
	if _, err := adminSvc.PutEntity(ctx, "personal", EntityInput{
		Kind:               "user",
		Slug:               "me",
		Title:              "Personal User",
		DefaultCompartment: "personal",
		Sensitivity:        SensitivityPrivate,
	}, actor); err != nil {
		t.Fatalf("PutEntity user: %v", err)
	}
	if _, err := adminSvc.SetFact(ctx, "personal", FactInput{
		EntityID:    "user:me",
		Key:         "home_address",
		ValueJSON:   `"SECRET_HOME"`,
		Compartment: "personal",
		Sensitivity: SensitivityPrivate,
		SourceType:  "manual",
	}, actor); err != nil {
		t.Fatalf("SetFact personal: %v", err)
	}

	// Facts & notes on Ada: one engineering/normal, one finance/private.
	if _, err := adminSvc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "role",
		ValueJSON:   `"Staff Engineer"`,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, actor); err != nil {
		t.Fatalf("SetFact role: %v", err)
	}
	if _, err := adminSvc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "salary",
		ValueJSON:   `"SECRET_COMP"`,
		Compartment: "finance",
		Sensitivity: SensitivityPrivate,
		SourceType:  "manual",
	}, actor); err != nil {
		t.Fatalf("SetFact salary: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "person:ada",
		SectionKey:  "work_style",
		Heading:     "Work Style",
		SortOrder:   10,
		BodyMD:      "Prefers concise design docs.",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock work_style: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "person:ada",
		SectionKey:  "comp_notes",
		Heading:     "Comp Notes",
		SortOrder:   20,
		BodyMD:      "SECRET_NOTE",
		Compartment: "finance",
		Sensitivity: SensitivityPrivate,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock comp_notes: %v", err)
	}

	// Episodic events: one engineering/normal, one personal/private.
	if _, err := adminSvc.AddEvent(ctx, "work", EventInput{
		OccurredAt:  time.Date(2026, 6, 15, 9, 0, 0, 0, time.UTC),
		EntityID:    "person:ada",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Channel:     "chat",
		Summary:     "Reviewed parser PR",
		Reporter:    "alice",
	}, actor); err != nil {
		t.Fatalf("AddEvent work: %v", err)
	}
	if _, err := adminSvc.AddEvent(ctx, "personal", EventInput{
		OccurredAt:  time.Date(2026, 6, 15, 10, 0, 0, 0, time.UTC),
		Compartment: "personal",
		Sensitivity: SensitivityPrivate,
		Channel:     "journal",
		Summary:     "SECRET_PERSONAL_EVENT",
		Reporter:    "me",
	}, actor); err != nil {
		t.Fatalf("AddEvent personal: %v", err)
	}

	workSvc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	defer workSvc.Close()

	outDir := filepath.Join(t.TempDir(), "aside-work")

	// First export.
	rep1, err := Export(ctx, workSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow})
	if err != nil {
		t.Fatalf("first Export: %v", err)
	}
	if rep1.Written == 0 || rep1.Removed != 0 {
		t.Errorf("unexpected first Export report: %+v", rep1)
	}

	// Verify expected files exist and restricted ones do not.
	for _, rel := range []string{
		"MEMORY.md",
		"TAXONOMY.md",
		"people/ada.md",
		"agent/helper.md",
		"episodic/2026-06-15.md",
		".projection-manifest.json",
	} {
		if _, err := os.Stat(filepath.Join(outDir, rel)); err != nil {
			t.Errorf("expected file %s: %v", rel, err)
		}
	}
	for _, forbiddenRel := range []string{
		"USER.md",
		"people/bob.md",
		"users/me.md",
	} {
		if _, err := os.Stat(filepath.Join(outDir, forbiddenRel)); !errors.Is(err, os.ErrNotExist) {
			t.Errorf("forbidden file %s exists (err=%v)", forbiddenRel, err)
		}
	}

	// Verify no secret leakage anywhere in exported tree.
	root, err := os.OpenRoot(outDir)
	if err != nil {
		t.Fatalf("open root %s: %v", outDir, err)
	}
	defer root.Close()
	err = fs.WalkDir(root.FS(), ".", func(path string, d fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if d.IsDir() {
			return nil
		}
		data, readErr := fs.ReadFile(root.FS(), path)
		if readErr != nil {
			return fmt.Errorf("read %s: %w", path, readErr)
		}
		text := string(data)
		for _, secret := range []string{"SECRET_COMP", "SECRET_NOTE", "SECRET_HOME", "SECRET_PERSONAL_EVENT", "Bob Secret"} {
			if strings.Contains(text, secret) {
				t.Errorf("file %s leaked %q", path, secret)
			}
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk outDir: %v", err)
	}

	// Verify manifest structure.
	manifestRaw, err := os.ReadFile(filepath.Join(outDir, ".projection-manifest.json"))
	if err != nil {
		t.Fatalf("read manifest: %v", err)
	}
	var manifest ProjectionManifest
	if err := json.Unmarshal(manifestRaw, &manifest); err != nil {
		t.Fatalf("unmarshal manifest: %v", err)
	}
	if manifest.Profile != "work" || manifest.Version == 0 {
		t.Errorf("unexpected manifest header: %+v", manifest)
	}
	if entry, ok := manifest.Files["people/ada.md"]; !ok || entry.EntityID != "person:ada" || entry.SHA256 == "" || entry.Size == 0 {
		t.Errorf("unexpected manifest entry for people/ada.md: %+v (ok=%v)", entry, ok)
	}

	// Second export with unchanged DB -> written: 0.
	rep2, err := Export(ctx, workSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow})
	if err != nil {
		t.Fatalf("second Export: %v", err)
	}
	if rep2.Written != 0 || rep2.Unchanged != rep1.Written || rep2.Removed != 0 {
		t.Errorf("second Export = %+v, want Written=0 Unchanged=%d Removed=0", rep2, rep1.Written)
	}

	// Archive agent:helper and re-export -> agent/helper.md removed.
	archivedAt := fixedNow()
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "agent",
		Slug:               "helper",
		Title:              "Helper Agent",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		ArchivedAt:         &archivedAt,
	}, actor); err != nil {
		t.Fatalf("archive helper: %v", err)
	}

	rep3, err := Export(ctx, workSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow})
	if err != nil {
		t.Fatalf("third Export: %v", err)
	}
	if rep3.Removed != 1 {
		t.Errorf("third Export Removed = %d, want 1 (report=%+v)", rep3.Removed, rep3)
	}
	if _, err := os.Stat(filepath.Join(outDir, "agent/helper.md")); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("archived agent/helper.md still exists (err=%v)", err)
	}
}

func TestExportGoldenMarkdown(t *testing.T) {
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "golden", Reason: "test"}

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "user",
		Slug:               "jaeyeom",
		Title:              "Jae",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Summary:            "Repo owner",
		PinnedL1:           true,
	}, actor); err != nil {
		t.Fatalf("PutEntity user: %v", err)
	}
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "company",
		Slug:               "acme",
		Title:              "Acme Corp",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Aliases:            []string{"Acme", "ACME Inc"},
		Summary:            "Widget maker",
		PinnedL1:           true,
	}, actor); err != nil {
		t.Fatalf("PutEntity company: %v", err)
	}
	if _, err := adminSvc.SetFact(ctx, "work", FactInput{
		EntityID:    "company:acme",
		Key:         "domain",
		ValueJSON:   `"acme.example"`,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, actor); err != nil {
		t.Fatalf("SetFact: %v", err)
	}
	if err := adminSvc.LinkEntities(ctx, "work", LinkInput{
		FromEntityID: "user:jaeyeom",
		Relation:     "works_at",
		ToEntityID:   "company:acme",
		Compartment:  "engineering",
		Sensitivity:  SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("LinkEntities: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "company:acme",
		SectionKey:  "overview",
		Heading:     "Overview",
		SortOrder:   10,
		BodyMD:      "Builds reliable widgets.",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "user:jaeyeom",
		SectionKey:  "prefs",
		Heading:     "Preferences",
		SortOrder:   10,
		BodyMD:      "Uses Bazel and Go.",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock user: %v", err)
	}

	workSvc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	defer workSvc.Close()

	outDir := filepath.Join(t.TempDir(), "golden")
	if _, err := Export(ctx, workSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow}); err != nil {
		t.Fatalf("Export: %v", err)
	}

	gotCompany, err := os.ReadFile(filepath.Join(outDir, "companies/acme.md"))
	if err != nil {
		t.Fatalf("read companies/acme.md: %v", err)
	}
	wantCompany := strings.Join([]string{
		"---",
		"title: Acme Corp",
		"aliases:",
		"    - ACME Inc",
		"    - Acme",
		"generated_by: memstore",
		"profile: work",
		"entity_id: company:acme",
		"---",
		"",
		"# Acme Corp",
		"",
		"Widget maker",
		"",
		"## Structured Attributes",
		"",
		"- **domain**: acme.example",
		"- **link (works_at)**: <- user:jaeyeom",
		"",
		"## Overview",
		"",
		"Builds reliable widgets.",
		"",
	}, "\n")
	if string(gotCompany) != wantCompany {
		t.Errorf("companies/acme.md mismatch:\ngot:\n%s\nwant:\n%s", gotCompany, wantCompany)
	}

	gotMemory, err := os.ReadFile(filepath.Join(outDir, "MEMORY.md"))
	if err != nil {
		t.Fatalf("read MEMORY.md: %v", err)
	}
	wantMemory := strings.Join([]string{
		"# Memory Index",
		"",
		"- [Acme Corp](companies/acme.md) (`company:acme`): Widget maker",
		"- [Jae](users/jaeyeom.md) (`user:jaeyeom`): Repo owner",
		"",
	}, "\n")
	if string(gotMemory) != wantMemory {
		t.Errorf("MEMORY.md mismatch:\ngot:\n%s\nwant:\n%s", gotMemory, wantMemory)
	}

	gotUser, err := os.ReadFile(filepath.Join(outDir, "USER.md"))
	if err != nil {
		t.Fatalf("read USER.md: %v", err)
	}
	if !strings.Contains(string(gotUser), "# Jae") || !strings.Contains(string(gotUser), "## Preferences") || !strings.Contains(string(gotUser), "Uses Bazel and Go.") {
		t.Errorf("unexpected USER.md:\n%s", gotUser)
	}
}

func TestExportRoutineMemory(t *testing.T) {
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)
	ctx := context.Background()
	actor := Actor{Caller: "test", Reason: "routine export"}

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "routine",
		Slug:               "daily-digest",
		Title:              "Daily Digest",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("PutEntity routine: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "routine:daily-digest",
		SectionKey:  "instructions",
		Heading:     "Instructions",
		BodyMD:      "Summarize open PRs.",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock: %v", err)
	}

	v, err := OpenVault(cfg.Vaults["work"])
	if err != nil {
		t.Fatalf("OpenVault: %v", err)
	}
	if err := v.SetRoutineState(ctx, "daily-digest", "cursor", `"2026-06-01"`); err != nil {
		t.Fatalf("SetRoutineState: %v", err)
	}
	_ = v.Close()

	outDir := filepath.Join(t.TempDir(), "export")
	if _, err := Export(ctx, adminSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow}); err != nil {
		t.Fatalf("Export: %v", err)
	}

	got, err := os.ReadFile(filepath.Join(outDir, "routines", "daily-digest", "MEMORY.md"))
	if err != nil {
		t.Fatalf("read routines/daily-digest/MEMORY.md: %v", err)
	}
	text := string(got)
	for _, want := range []string{"# Daily Digest", "## Routine State", "- **cursor**: `\"2026-06-01\"`", "## Instructions", "Summarize open PRs."} {
		if !strings.Contains(text, want) {
			t.Errorf("routine MEMORY.md missing %q:\n%s", want, text)
		}
	}
}
