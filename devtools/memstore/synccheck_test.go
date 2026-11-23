package memstore

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestSyncCheckCleanAfterExport(t *testing.T) {
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "test", Reason: "sync-check-clean"}

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

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
	if _, err := adminSvc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "role",
		ValueJSON:   `"mathematician"`,
		ValueType:   "string",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, actor); err != nil {
		t.Fatalf("SetFact: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "person:ada",
		SectionKey:  "bio",
		Heading:     "Bio",
		SortOrder:   100,
		BodyMD:      "Writes algorithms.",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock: %v", err)
	}

	outDir := filepath.Join(t.TempDir(), "export")
	if _, err := Export(ctx, adminSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow}); err != nil {
		t.Fatalf("Export: %v", err)
	}

	rep, err := SyncCheck(ctx, adminSvc, outDir, SyncCheckOptions{Actor: actor})
	if err != nil {
		t.Fatalf("SyncCheck: %v", err)
	}
	if len(rep.Modified) != 0 || len(rep.Added) != 0 || len(rep.Deleted) != 0 {
		t.Errorf("expected clean SyncCheck, got modified=%v added=%v deleted=%v", rep.Modified, rep.Added, rep.Deleted)
	}
	if rep.Unchanged == 0 {
		t.Errorf("expected Unchanged > 0, got 0")
	}
}

func TestSyncCheckMissingManifestFailsCleanly(t *testing.T) {
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

	_, err = SyncCheck(context.Background(), adminSvc, t.TempDir(), SyncCheckOptions{})
	if err == nil {
		t.Fatalf("expected error when .projection-manifest.json is missing")
	}
	if !strings.Contains(err.Error(), ".projection-manifest.json") {
		t.Errorf("error %q should mention .projection-manifest.json", err.Error())
	}
}

func TestSyncCheckDetectsEditsStagesIdempotentlyAndRoundTrips(t *testing.T) {
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "test", Reason: "sync-check-edit"}

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada Lovelace",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Summary:            "Analytical engine pioneer",
		Aliases:            []string{"Countess"},
	}, actor); err != nil {
		t.Fatalf("PutEntity ada: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "person:ada",
		SectionKey:  "bio",
		Heading:     "Bio",
		SortOrder:   100,
		BodyMD:      "Original bio.",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock: %v", err)
	}

	outDir := filepath.Join(t.TempDir(), "export")
	if _, err := Export(ctx, adminSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow}); err != nil {
		t.Fatalf("Export: %v", err)
	}

	// Externally edit people/ada.md: update Bio, add a new section, and add an alias.
	adaPath := filepath.Join(outDir, "people", "ada.md")
	edited := `---
title: Ada Lovelace
aliases:
    - Countess
    - Ada King
generated_by: memstore
profile: admin
entity_id: person:ada
---

# Ada Lovelace

Analytical engine pioneer

## Bio

Updated bio from editor.

## Goals

- **focus**: computing
`
	if err := os.WriteFile(adaPath, []byte(edited), 0o600); err != nil {
		t.Fatalf("write edited ada.md: %v", err)
	}

	// Add a new untracked file and delete TAXONOMY.md.
	newPersonPath := filepath.Join(outDir, "people", "grace.md")
	if err := os.WriteFile(newPersonPath, []byte("# Grace Hopper\n\nCompiler pioneer\n\n## Notes\n\nInvented COBOL.\n"), 0o600); err != nil {
		t.Fatalf("write grace.md: %v", err)
	}
	if err := os.Remove(filepath.Join(outDir, "TAXONOMY.md")); err != nil {
		t.Fatalf("remove TAXONOMY.md: %v", err)
	}

	// Stage first time.
	rep1, err := SyncCheck(ctx, adminSvc, outDir, SyncCheckOptions{Stage: true, Actor: actor})
	if err != nil {
		t.Fatalf("SyncCheck(stage=true): %v", err)
	}
	if len(rep1.Modified) != 1 || rep1.Modified[0] != "people/ada.md" {
		t.Errorf("Modified = %v, want [people/ada.md]", rep1.Modified)
	}
	if len(rep1.Added) != 1 || rep1.Added[0] != "people/grace.md" {
		t.Errorf("Added = %v, want [people/grace.md]", rep1.Added)
	}
	if len(rep1.Deleted) != 1 || rep1.Deleted[0] != "TAXONOMY.md" {
		t.Errorf("Deleted = %v, want [TAXONOMY.md]", rep1.Deleted)
	}
	if rep1.Staged != 2 {
		t.Fatalf("Staged = %d, want 2", rep1.Staged)
	}

	// Canonical DB must NOT be mutated before approval.
	viewBefore, err := adminSvc.GetEntity(ctx, "work", "person:ada", actor)
	if err != nil {
		t.Fatalf("GetEntity(person:ada): %v", err)
	}
	if len(viewBefore.NoteBlocks) != 1 || viewBefore.NoteBlocks[0].BodyMD != "Original bio." {
		t.Errorf("canonical NoteBlocks mutated before approval: %+v", viewBefore.NoteBlocks)
	}
	if _, err := adminSvc.GetEntity(ctx, "work", "person:grace", actor); err == nil {
		t.Errorf("person:grace should not exist in DB before approval")
	}

	// Verify triage_inbox entries have source = "md_external_edit".
	pending, err := adminSvc.ListTriage(ctx, TriageFilter{Status: TriageStatusPending})
	if err != nil {
		t.Fatalf("ListTriage: %v", err)
	}
	if len(pending) != 2 {
		t.Fatalf("pending triage count = %d, want 2", len(pending))
	}
	for _, it := range pending {
		if it.Source != SyncSourceExternalEdit {
			t.Errorf("triage item %s source = %q, want %q", it.ID, it.Source, SyncSourceExternalEdit)
		}
	}

	// Stage second time: must be idempotent (0 new staged, 2 skipped).
	rep2, err := SyncCheck(ctx, adminSvc, outDir, SyncCheckOptions{Stage: true, Actor: actor})
	if err != nil {
		t.Fatalf("SyncCheck(stage=true, 2nd): %v", err)
	}
	if rep2.Staged != 0 || rep2.Skipped != 2 {
		t.Errorf("2nd SyncCheck Staged=%d Skipped=%d, want Staged=0 Skipped=2", rep2.Staged, rep2.Skipped)
	}

	// Approve both pending triage items.
	for _, it := range pending {
		if err := adminSvc.ApproveTriage(ctx, it.Vault, it.ID, "", "", actor); err != nil {
			t.Fatalf("ApproveTriage(%s): %v", it.ID, err)
		}
	}

	// Verify DB updated.
	viewAfter, err := adminSvc.GetEntity(ctx, "work", "person:ada", actor)
	if err != nil {
		t.Fatalf("GetEntity(person:ada) after approve: %v", err)
	}
	if viewAfter.NoteBlocks[0].BodyMD != "Updated bio from editor." {
		t.Errorf("updated bio = %q, want %q", viewAfter.NoteBlocks[0].BodyMD, "Updated bio from editor.")
	}
	if len(viewAfter.Facts) != 1 || viewAfter.Facts[0].Key != "focus" {
		t.Errorf("expected fact focus after approve, got %+v", viewAfter.Facts)
	}
	if len(viewAfter.Aliases) != 2 {
		t.Errorf("expected 2 aliases after approve, got %v", viewAfter.Aliases)
	}
	if _, err := adminSvc.GetEntity(ctx, "personal", "person:grace", actor); err != nil {
		t.Fatalf("GetEntity(person:grace) after approve: %v", err)
	}

	// Re-export + sync-check must return clean.
	if _, err := Export(ctx, adminSvc, outDir, ExportOptions{Actor: actor, Now: fixedNow}); err != nil {
		t.Fatalf("re-Export: %v", err)
	}
	repClean, err := SyncCheck(ctx, adminSvc, outDir, SyncCheckOptions{Actor: actor})
	if err != nil {
		t.Fatalf("SyncCheck after re-export: %v", err)
	}
	if len(repClean.Modified) != 0 || len(repClean.Added) != 0 || len(repClean.Deleted) != 0 {
		t.Errorf("expected clean SyncCheck after re-export, got modified=%v added=%v deleted=%v",
			repClean.Modified, repClean.Added, repClean.Deleted)
	}
}
