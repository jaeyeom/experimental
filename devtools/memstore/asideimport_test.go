package memstore

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func writeAsideFixture(t *testing.T, dir string) {
	t.Helper()
	files := map[string]string{
		"MEMORY.md": `# Memory Index

- [Alice Smith](people/alice-smith.md) (` + "`person:alice-smith`" + `): Staff engineer on Platform
`,
		"USER.md": `# Jaeyeom

Maintainer of experimental.

## Preferences

- **editor**: neovim

## Notes

Prefers concise output.
`,
		"people/alice-smith.md": `---
title: Alice Smith
aliases:
  - Alice
  - A. Smith
---

# Alice Smith

Staff engineer on Platform.

## Structured Attributes

- **role**: Staff Engineer
- **team**: Platform

## Background

Met at KubeCon 2025.

## Working Style

Async first, writes design docs.
`,
		"sites/prod-cluster.md": `---
title: Prod Cluster
---

# Prod Cluster

## Structured Attributes

- **region**: us-west-2

## Runbook

Check dashboards first.
`,
		"routines/weekly-review/MEMORY.md": `---
title: Weekly Review
---

# Weekly Review

## Checklist

- Review triage inbox
- Compact old events
`,
		"episodic/2026-06-01.md": `# Episodic Log: 2026-06-01

- **12:00:00Z** [person:alice-smith] (slack): Reviewed memstore design
  Agreed on SQLite schema.
`,
	}

	for rel, content := range files {
		abs := filepath.Join(dir, filepath.FromSlash(rel))
		if err := os.MkdirAll(filepath.Dir(abs), 0o700); err != nil {
			t.Fatalf("mkdir %s: %v", abs, err)
		}
		if err := os.WriteFile(abs, []byte(content), 0o600); err != nil {
			t.Fatalf("write %s: %v", abs, err)
		}
	}
}

func TestImportAsideDryRunAndRoundTrip(t *testing.T) {
	ctx := context.Background()
	cfg := testExportConfig(t)
	initAllVaults(t, cfg)
	srcDir := t.TempDir()
	writeAsideFixture(t, srcDir)

	rulesPath := filepath.Join(t.TempDir(), "rules.toml")
	rulesTOML := `
[[rule]]
glob = "sites/*"
vault = "work"
compartment = "engineering"
sensitivity = "normal"
mode = "direct"
`
	if err := os.WriteFile(rulesPath, []byte(rulesTOML), 0o600); err != nil {
		t.Fatalf("write rules: %v", err)
	}

	fixedNow := func() time.Time { return time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC) }

	// 1. Dry-run makes zero DB changes and accurately reports counts.
	dryReport, err := ImportAside(ctx, cfg, srcDir, ImportOptions{
		Vault:     "personal",
		RulesPath: rulesPath,
		DryRun:    true,
		Now:       fixedNow,
	})
	if err != nil {
		t.Fatalf("ImportAside dry-run: %v", err)
	}
	if dryReport.Entities != 4 || dryReport.Facts != 4 || dryReport.NoteBlocks != 5 || dryReport.EpisodicEvents != 1 {
		t.Fatalf("unexpected dry-run report: %+v", dryReport)
	}
	if dryReport.DirectWrites != 1 || dryReport.TriageStaged != 4 {
		t.Fatalf("unexpected dry-run mode counts: %+v", dryReport)
	}

	svc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService: %v", err)
	}
	defer svc.Close()

	actor := Actor{Caller: "test", Reason: "verify"}
	if _, err := svc.GetEntity(ctx, "work", "site:prod-cluster", actor); !errors.Is(err, ErrNotFound) {
		t.Fatalf("dry-run should not write site:prod-cluster, got %v", err)
	}
	triageBefore, err := svc.ListTriage(ctx, TriageFilter{})
	if err != nil {
		t.Fatalf("ListTriage before: %v", err)
	}
	if len(triageBefore) != 0 {
		t.Fatalf("dry-run should stage 0 triage items, got %d", len(triageBefore))
	}

	// 2. Real import: sites/* goes direct to work; everything else goes to triage in personal.
	report, err := ImportAside(ctx, cfg, srcDir, ImportOptions{
		Vault:     "personal",
		RulesPath: rulesPath,
		Now:       fixedNow,
	})
	if err != nil {
		t.Fatalf("ImportAside: %v", err)
	}
	if report.DirectWrites != 1 || report.TriageStaged != 4 || len(report.TriageIDs) != 4 {
		t.Fatalf("unexpected import report: %+v", report)
	}

	// Direct-matched site is immediately visible in work.
	siteView, err := svc.GetEntity(ctx, "work", "site:prod-cluster", actor)
	if err != nil {
		t.Fatalf("GetEntity site:prod-cluster: %v", err)
	}
	if siteView.Entity.Title != "Prod Cluster" || len(siteView.Facts) != 1 || len(siteView.NoteBlocks) != 1 {
		t.Fatalf("unexpected site view: %+v", siteView)
	}

	// Unclassified files remain invisible until triage approve.
	if _, err := svc.GetEntity(ctx, "personal", "person:alice-smith", actor); !errors.Is(err, ErrNotFound) {
		t.Fatalf("unclassified person:alice-smith should not be visible before approve, got %v", err)
	}

	// Approve all staged triage items.
	pending, err := svc.ListTriage(ctx, TriageFilter{Vault: "personal", Status: TriageStatusPending})
	if err != nil {
		t.Fatalf("ListTriage pending: %v", err)
	}
	if len(pending) != 4 {
		t.Fatalf("expected 4 pending triage items, got %d", len(pending))
	}
	for _, item := range pending {
		if err := svc.ApproveTriage(ctx, "personal", item.ID, "", "", actor); err != nil {
			t.Fatalf("ApproveTriage %s: %v", item.ID, err)
		}
	}

	// Verify pinned_l1 from MEMORY.md and all fields on person:alice-smith.
	aliceView, err := svc.GetEntity(ctx, "personal", "person:alice-smith", actor)
	if err != nil {
		t.Fatalf("GetEntity person:alice-smith: %v", err)
	}
	if !aliceView.Entity.PinnedL1 {
		t.Errorf("expected person:alice-smith to have PinnedL1=true from MEMORY.md")
	}
	if len(aliceView.Aliases) != 2 {
		t.Errorf("expected 2 aliases, got %v", aliceView.Aliases)
	}
	if len(aliceView.NoteBlocks) != 2 || aliceView.NoteBlocks[0].Heading != "Background" || aliceView.NoteBlocks[1].Heading != "Working Style" {
		t.Errorf("unexpected note block order: %+v", aliceView.NoteBlocks)
	}

	// 3. Export and verify round-trip preserves titles, aliases, attributes, and prose sections.
	outDir := filepath.Join(t.TempDir(), "export")
	if _, err := Export(ctx, svc, outDir, ExportOptions{Actor: actor, Now: fixedNow}); err != nil {
		t.Fatalf("Export: %v", err)
	}

	exportedAlice, err := os.ReadFile(filepath.Join(outDir, "people", "alice-smith.md"))
	if err != nil {
		t.Fatalf("read exported alice: %v", err)
	}
	aliceText := string(exportedAlice)
	for _, want := range []string{
		"title: Alice Smith",
		"- Alice",
		"- A. Smith",
		"- **role**: Staff Engineer",
		"- **team**: Platform",
		"## Background",
		"Met at KubeCon 2025.",
		"## Working Style",
		"Async first, writes design docs.",
	} {
		if !strings.Contains(aliceText, want) {
			t.Errorf("exported alice missing %q:\n%s", want, aliceText)
		}
	}

	exportedEpisodic, err := os.ReadFile(filepath.Join(outDir, "episodic", "2026-06-01.md"))
	if err != nil {
		t.Fatalf("read exported episodic: %v", err)
	}
	if !strings.Contains(string(exportedEpisodic), "Reviewed memstore design") || !strings.Contains(string(exportedEpisodic), "Agreed on SQLite schema.") {
		t.Errorf("exported episodic missing content:\n%s", string(exportedEpisodic))
	}
}

func TestSplitFrontmatterUnquotedAtHandle(t *testing.T) {
	content := "---\ntitle: Ada Lovelace\naliases:\n  - @ada\n  - Ada L.\n  - `ada-dev`\n---\n\n# Ada Lovelace\n"
	fm, body, err := splitFrontmatter(content)
	if err != nil {
		t.Fatalf("splitFrontmatter: %v", err)
	}
	if fm.Title != "Ada Lovelace" {
		t.Fatalf("unexpected title: %q", fm.Title)
	}
	if len(fm.Aliases) != 3 || fm.Aliases[0] != "@ada" || fm.Aliases[1] != "Ada L." || fm.Aliases[2] != "`ada-dev`" {
		t.Fatalf("unexpected aliases: %v", fm.Aliases)
	}
	if !strings.Contains(body, "# Ada Lovelace") {
		t.Fatalf("unexpected body: %q", body)
	}
}

func TestParseEpisodicMarkdownHeadings(t *testing.T) {
	md := `# 2026-06-01

## 09:30 AM — Morning Standup

Reviewed roadmap milestones.
- Confirmed API timeline

## ~14:15 Design Review (2026-06-01)

Discussed SQLite vault isolation.
`
	events := parseEpisodicMarkdown("2026-06-01", "episodic/2026-06-01.md", md)
	if len(events) != 2 {
		t.Fatalf("expected 2 events, got %d: %+v", len(events), events)
	}
	if events[0].Summary != "Morning Standup" || events[0].OccurredAt.Hour() != 9 || events[0].OccurredAt.Minute() != 30 {
		t.Errorf("unexpected event[0]: %+v", events[0])
	}
	if !strings.Contains(events[0].DetailsMD, "Confirmed API timeline") {
		t.Errorf("event[0] details missing bullet: %q", events[0].DetailsMD)
	}
	if events[1].Summary != "Design Review (2026-06-01)" || events[1].OccurredAt.Hour() != 14 || events[1].OccurredAt.Minute() != 15 {
		t.Errorf("unexpected event[1]: %+v", events[1])
	}
	if !strings.Contains(events[1].DetailsMD, "Discussed SQLite vault isolation.") {
		t.Errorf("event[1] details missing prose: %q", events[1].DetailsMD)
	}
}
