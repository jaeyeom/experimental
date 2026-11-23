package memstore

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

func TestSQLiteRetrieverSearchAcrossDocKinds(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "test-agent", Reason: "seed"}

	svc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer svc.Close()

	// 1. Entity title + alias.
	if _, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada Lovelace",
		Aliases:            []string{"Countess of Lovelace", "Enchantress of Numbers"},
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Summary:            "Pioneer of analytical engine programming",
	}, actor); err != nil {
		t.Fatalf("PutEntity: %v", err)
	}

	// Update entity without Aliases — existing aliases must remain searchable.
	if _, err := svc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "ada",
		Title:              "Ada Lovelace",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
		Summary:            "Pioneer of analytical engine programming",
	}, actor); err != nil {
		t.Fatalf("PutEntity update: %v", err)
	}

	// 2. Fact (and supersede it to verify old value is removed from FTS5).
	if _, err := svc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "favorite_language",
		ValueJSON:   `"obsolete_fortran"`,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, actor); err != nil {
		t.Fatalf("SetFact old: %v", err)
	}
	if _, err := svc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:ada",
		Key:         "favorite_language",
		ValueJSON:   `"analytical_assembly"`,
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, actor); err != nil {
		t.Fatalf("SetFact new: %v", err)
	}

	// 3. Note block.
	if _, err := svc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "person:ada",
		SectionKey:  "architecture",
		Heading:     "Babbage Engine Notes",
		BodyMD:      "Discussed bernoulli_algorithm optimization.",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
	}, actor); err != nil {
		t.Fatalf("SetNoteBlock: %v", err)
	}

	// 4. Episodic event.
	if _, err := svc.AddEvent(ctx, "work", EventInput{
		OccurredAt:  time.Date(2026, 5, 1, 10, 0, 0, 0, time.UTC),
		EntityID:    "person:ada",
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Channel:     "slack",
		SourceRef:   "msg-1",
		Summary:     "Reviewed punchcard_compiler design",
		DetailsMD:   "Approved rollout for next quarter.",
		Reporter:    "test-agent",
	}, actor); err != nil {
		t.Fatalf("AddEvent: %v", err)
	}

	var r Retriever = NewSQLiteRetriever(svc)

	tests := []struct {
		name      string
		query     SearchQuery
		wantCount int
		wantKind  string
	}{
		{
			name:      "entity title",
			query:     SearchQuery{Query: "Lovelace", Actor: actor},
			wantCount: 1,
			wantKind:  DocKindEntity,
		},
		{
			name:      "entity alias preserved across update",
			query:     SearchQuery{Query: "Enchantress", Actor: actor},
			wantCount: 1,
			wantKind:  DocKindEntity,
		},
		{
			name:      "active fact",
			query:     SearchQuery{Query: "analytical_assembly", Actor: actor},
			wantCount: 1,
			wantKind:  DocKindFact,
		},
		{
			name:      "superseded fact excluded",
			query:     SearchQuery{Query: "obsolete_fortran", Actor: actor},
			wantCount: 0,
		},
		{
			name:      "note block",
			query:     SearchQuery{Query: "bernoulli_algorithm", Actor: actor},
			wantCount: 1,
			wantKind:  DocKindNote,
		},
		{
			name:      "episodic event",
			query:     SearchQuery{Query: "punchcard_compiler", Actor: actor},
			wantCount: 1,
			wantKind:  DocKindEvent,
		},
		{
			name:      "kind filter",
			query:     SearchQuery{Query: "analytical", Kinds: []string{DocKindFact}, Actor: actor},
			wantCount: 1,
			wantKind:  DocKindFact,
		},
		{
			name: "since/until matches event",
			query: SearchQuery{
				Query: "punchcard_compiler",
				Since: time.Date(2026, 4, 30, 0, 0, 0, 0, time.UTC),
				Until: time.Date(2026, 5, 2, 0, 0, 0, 0, time.UTC),
				Actor: actor,
			},
			wantCount: 1,
			wantKind:  DocKindEvent,
		},
		{
			name: "since excludes older event",
			query: SearchQuery{
				Query: "punchcard_compiler",
				Since: time.Date(2026, 5, 2, 0, 0, 0, 0, time.UTC),
				Actor: actor,
			},
			wantCount: 0,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			hits, err := r.Search(ctx, tc.query)
			if err != nil {
				t.Fatalf("Search: %v", err)
			}
			if len(hits) != tc.wantCount {
				t.Fatalf("got %d hits, want %d: %+v", len(hits), tc.wantCount, hits)
			}
			if tc.wantCount > 0 && hits[0].DocKind != tc.wantKind {
				t.Errorf("got DocKind %q, want %q", hits[0].DocKind, tc.wantKind)
			}
		})
	}
}

func TestSQLiteRetrieverLeakageAndAccessLog(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	adminActor := Actor{Caller: "admin-cli", Reason: "seed"}

	adminSvc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer adminSvc.Close()

	// Seed work vault with an allowed doc and a restricted-compartment / high-sensitivity doc.
	if _, err := adminSvc.PutEntity(ctx, "work", EntityInput{
		Kind:               "person",
		Slug:               "bob",
		Title:              "Bob Engineer",
		DefaultCompartment: "engineering",
		Sensitivity:        SensitivityNormal,
	}, adminActor); err != nil {
		t.Fatalf("PutEntity bob: %v", err)
	}
	if _, err := adminSvc.SetFact(ctx, "work", FactInput{
		EntityID:    "person:bob",
		Key:         "salary",
		ValueJSON:   `"secret_compensation_token"`,
		Compartment: "finance",
		Sensitivity: SensitivityNormal,
		SourceType:  "manual",
	}, adminActor); err != nil {
		t.Fatalf("SetFact finance: %v", err)
	}
	if _, err := adminSvc.SetNoteBlock(ctx, "work", NoteBlockInput{
		EntityID:    "person:bob",
		SectionKey:  "private-note",
		Heading:     "Private",
		BodyMD:      "restricted_sensitivity_token",
		Compartment: "engineering",
		Sensitivity: SensitivityRestricted,
	}, adminActor); err != nil {
		t.Fatalf("SetNoteBlock restricted: %v", err)
	}

	// Seed personal vault (unpermitted for "work" profile).
	if _, err := adminSvc.PutEntity(ctx, "personal", EntityInput{
		Kind:               "person",
		Slug:               "carol",
		Title:              "Carol Personal",
		DefaultCompartment: "personal",
		Sensitivity:        SensitivityNormal,
		Summary:            "personal_vault_token",
	}, adminActor); err != nil {
		t.Fatalf("PutEntity personal: %v", err)
	}

	workSvc, err := OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	defer workSvc.Close()

	workActor := Actor{Caller: "work-agent", Reason: "lookup"}
	for _, secretTerm := range []string{
		"secret_compensation_token",
		"restricted_sensitivity_token",
		"personal_vault_token",
	} {
		hits, err := workSvc.Search(ctx, SearchQuery{Query: secretTerm, Actor: workActor})
		if err != nil {
			t.Fatalf("Search(%q): %v", secretTerm, err)
		}
		if len(hits) != 0 {
			t.Errorf("work profile leaked %q: got %d hits (%+v)", secretTerm, len(hits), hits)
		}
	}

	// Verify access_log recorded the searches without storing any query text.
	logs, err := adminSvc.QueryAccessLog(ctx, "work", AccessLogFilter{Caller: "work-agent"})
	if err != nil {
		t.Fatalf("QueryAccessLog: %v", err)
	}
	if len(logs) != 3 {
		t.Fatalf("got %d access_log entries in work vault, want 3", len(logs))
	}
	for _, entry := range logs {
		if entry.Command != "search" {
			t.Errorf("got command %q, want search", entry.Command)
		}
		if strings.Contains(entry.Reason, "token") || strings.Contains(entry.EntityID, "token") {
			t.Errorf("access_log leaked query text: %+v", entry)
		}
	}

	// Personal vault must have 0 access_log entries from work-agent.
	personalLogs, err := adminSvc.QueryAccessLog(ctx, "personal", AccessLogFilter{Caller: "work-agent"})
	if err != nil {
		t.Fatalf("QueryAccessLog(personal): %v", err)
	}
	if len(personalLogs) != 0 {
		t.Errorf("expected 0 access_log entries in personal vault, got %d", len(personalLogs))
	}

	// Profile without read capability must get ErrForbidden.
	epSvc, err := OpenService(cfg, "episodic-only")
	if err != nil {
		t.Fatalf("OpenService(episodic-only): %v", err)
	}
	defer epSvc.Close()
	if _, err := epSvc.Search(ctx, SearchQuery{Query: "Bob", Actor: workActor}); !errors.Is(err, ErrForbidden) {
		t.Errorf("episodic-only Search = %v, want ErrForbidden", err)
	}
}

func TestSQLiteRetrieverMultiVaultMergeAndLimit(t *testing.T) {
	cfg := testMultiVaultConfig(t)
	initAllVaults(t, cfg)

	ctx := context.Background()
	actor := Actor{Caller: "admin-cli", Reason: "merge-test"}

	svc, err := OpenService(cfg, "admin")
	if err != nil {
		t.Fatalf("OpenService(admin): %v", err)
	}
	defer svc.Close()

	for _, tc := range []struct {
		vault string
		slug  string
		title string
		sum   string
	}{
		{"work", "p1", "Deployment Guide", "deployment runbook"},
		{"work", "p2", "Other Doc", "deployment"},
		{"personal", "p3", "Home Deployment", "deployment checklist deployment"},
	} {
		if _, err := svc.PutEntity(ctx, tc.vault, EntityInput{
			Kind:               "project",
			Slug:               tc.slug,
			Title:              tc.title,
			DefaultCompartment: "general",
			Sensitivity:        SensitivityNormal,
			Summary:            tc.sum,
		}, actor); err != nil {
			t.Fatalf("PutEntity(%s): %v", tc.slug, err)
		}
	}

	hits, err := svc.Search(ctx, SearchQuery{Query: "deployment", Limit: 2, Actor: actor})
	if err != nil {
		t.Fatalf("Search: %v", err)
	}
	if len(hits) != 2 {
		t.Fatalf("got %d hits, want 2", len(hits))
	}
	if hits[0].Score < hits[1].Score {
		t.Errorf("hits not sorted by score descending: %v < %v", hits[0].Score, hits[1].Score)
	}
}
