package memstore

import (
	"context"
	"testing"
)

func TestSetAndListNoteBlocks(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatal(err)
	}
	defer svc.Close()

	ctx := context.Background()
	actor := Actor{Caller: "cli", Reason: "notes"}

	if _, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "project",
		Slug:               "memstore",
		Title:              "Memstore",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
	}, actor); err != nil {
		t.Fatal(err)
	}

	if _, err := svc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "project:memstore",
		SectionKey:  "zebra",
		Heading:     "Zebra",
		SortOrder:   200,
		BodyMD:      "later",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatal(err)
	}
	if _, err := svc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "project:memstore",
		SectionKey:  "alpha",
		Heading:     "Alpha",
		SortOrder:   50,
		BodyMD:      "first",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatal(err)
	}
	// Upsert alpha.
	if _, err := svc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "project:memstore",
		SectionKey:  "alpha",
		Heading:     "Alpha Updated",
		SortOrder:   50,
		BodyMD:      "updated",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatal(err)
	}

	blocks, err := svc.ListNoteBlocks(ctx, "work", "project:memstore", actor)
	if err != nil {
		t.Fatal(err)
	}
	if len(blocks) != 2 {
		t.Fatalf("len(blocks) = %d, want 2", len(blocks))
	}
	if blocks[0].SectionKey != "alpha" || blocks[0].BodyMD != "updated" || blocks[1].SectionKey != "zebra" {
		t.Errorf("unexpected blocks order/content: %+v", blocks)
	}
}
