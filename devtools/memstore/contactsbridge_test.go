package memstore

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

func TestContactsBridgeEnrichmentReadOnly(t *testing.T) {
	base := t.TempDir()
	contactsDir := filepath.Join(base, "contacts-data")
	if err := contacts.Init(contactsDir); err != nil {
		t.Fatalf("contacts.Init: %v", err)
	}
	contactsCfgPath := filepath.Join(base, "contacts.toml")
	if err := os.WriteFile(contactsCfgPath, []byte(fmt.Sprintf("data_dir = %q\n", contactsDir)), 0o600); err != nil {
		t.Fatalf("write contacts.toml: %v", err)
	}

	cs, err := contacts.Open(contactsDir, fixedNow)
	if err != nil {
		t.Fatalf("contacts.Open: %v", err)
	}
	personID, err := cs.Add(contacts.Person{
		Name: "Ada Lovelace",
		Team: "Platform",
		Role: "Staff Engineer",
		TZ:   "America/Los_Angeles",
		Handles: []contacts.Handle{
			{Kind: "chat", Value: "@ada"},
			{Kind: "code", Value: "adalovelace"},
		},
	})
	if err != nil {
		t.Fatalf("contacts.Add: %v", err)
	}
	if _, err := cs.Commit(contacts.Commitment{
		PersonID:  personID,
		Direction: "mine",
		Text:      "Send architecture review",
		Due:       "2026-06-20",
		Reporter:  "test",
	}); err != nil {
		t.Fatalf("contacts.Commit: %v", err)
	}
	if err := cs.Close(); err != nil {
		t.Fatalf("contacts.Close: %v", err)
	}

	cfg := testExportConfig(t)
	workVC := cfg.Vaults["work"]
	workVC.ContactsConfig = contactsCfgPath
	cfg.Vaults["work"] = workVC
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "bridge-test", Reason: "seed"}
	svc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	defer svc.Close()

	if _, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada Lovelace",
		ContactsRef:        personID,
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("PutEntity: %v", err)
	}

	outDir := filepath.Join(base, "export")
	if _, err := Export(ctx, svc, outDir, ExportOptions{Actor: actor, Now: fixedNow}); err != nil {
		t.Fatalf("Export: %v", err)
	}

	got, err := os.ReadFile(filepath.Join(outDir, "people/ada.md"))
	if err != nil {
		t.Fatalf("read people/ada.md: %v", err)
	}
	text := string(got)
	for _, want := range []string{
		"- **team**: Platform",
		"- **role**: Staff Engineer",
		"- **timezone**: America/Los_Angeles",
		"- **handles**: chat:@ada, code:adalovelace",
		"- **open_commitments**: [mine] Send architecture review (due 2026-06-20)",
	} {
		if !strings.Contains(text, want) {
			t.Errorf("people/ada.md missing %q:\n%s", want, text)
		}
	}

	// Verify contacts DB was not mutated (access_log remains empty) and memstore has no contacts facts written.
	csCheck, err := contacts.Open(contactsDir, fixedNow)
	if err != nil {
		t.Fatalf("contacts.Open check: %v", err)
	}
	defer csCheck.Close()
	logs, err := csCheck.AccessLog(contacts.AccessFilter{})
	if err != nil {
		t.Fatalf("contacts.AccessLog: %v", err)
	}
	if len(logs) != 0 {
		t.Errorf("contacts access_log mutated: %+v", logs)
	}
	facts, err := svc.ListFacts(ctx, FactFilter{Vault: "work", EntityID: "person:ada"}, actor)
	if err != nil {
		t.Fatalf("ListFacts: %v", err)
	}
	if len(facts) != 0 {
		t.Errorf("memstore facts mutated: %+v", facts)
	}
}
