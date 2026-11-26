package syncer_test

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

func TestRefreshUpdatesRowThatStillMatches(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{SourceName: "jira:acme", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
	}}
	if err := st.UpsertItems(ctx, "jira:acme", []source.Item{src.Items["A"]}); err != nil {
		t.Fatal(err)
	}
	a := src.Items["A"]
	a.State = "Done"
	a.Done = true
	src.Items["A"] = a

	removed, err := syncer.Refresh(ctx, st, "jira:acme", src, "A")
	if err != nil || removed {
		t.Fatalf("Refresh = %v, %v, want kept", removed, err)
	}
	got, err := st.Item(ctx, "jira:acme", "A")
	if err != nil || !got.Done || got.State != "Done" {
		t.Fatalf("A = %+v, %v, want the refreshed row", got, err)
	}
}

func TestRefreshRemovesRowThatLeftQuery(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{SourceName: "jira:acme", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
		"B": workItem("jira:acme", "B", t1),
	}}
	if err := st.UpsertItems(ctx, "jira:acme", []source.Item{src.Items["A"], src.Items["B"]}); err != nil {
		t.Fatal(err)
	}
	src.Query = func(it source.Item) bool { return it.ID != "A" }

	removed, err := syncer.Refresh(ctx, st, "jira:acme", src, "A")
	if err != nil || !removed {
		t.Fatalf("Refresh = %v, %v, want removed", removed, err)
	}
	if _, err := st.Item(ctx, "jira:acme", "A"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("A error = %v, want ErrNotFound", err)
	}
	if _, err := st.Item(ctx, "jira:acme", "B"); err != nil {
		t.Fatalf("B: %v", err)
	}
}

func TestRefreshRemovesRowWhenQueryIsEmpty(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{SourceName: "jira:acme", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
	}}
	if err := st.UpsertItems(ctx, "jira:acme", []source.Item{src.Items["A"]}); err != nil {
		t.Fatal(err)
	}
	src.Query = func(source.Item) bool { return false }

	removed, err := syncer.Refresh(ctx, st, "jira:acme", src, "A")
	if err != nil || !removed {
		t.Fatalf("Refresh = %v, %v, want removed", removed, err)
	}
}

func TestRefreshIDsFailureUpdatesRowAndReturnsError(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{SourceName: "jira:acme", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
	}}
	if err := st.UpsertItems(ctx, "jira:acme", []source.Item{src.Items["A"]}); err != nil {
		t.Fatal(err)
	}
	a := src.Items["A"]
	a.Title = "renamed"
	src.Items["A"] = a
	src.FailOn = map[string]error{"IDs": errors.New("search down")}

	removed, err := syncer.Refresh(ctx, st, "jira:acme", src, "A")
	if removed || err == nil || !strings.Contains(err.Error(), "membership check: search down") {
		t.Fatalf("Refresh = %v, %v", removed, err)
	}
	got, err := st.Item(ctx, "jira:acme", "A")
	if err != nil || got.Title != "renamed" {
		t.Fatalf("A = %+v, %v, want the refreshed row", got, err)
	}
}

func TestRefreshGetFailureChangesNothing(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{SourceName: "jira:acme", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
	}}
	if err := st.UpsertItems(ctx, "jira:acme", []source.Item{src.Items["A"]}); err != nil {
		t.Fatal(err)
	}
	src.Query = func(source.Item) bool { return false }
	src.FailOn = map[string]error{"Get": errors.New("get down")}

	if _, err := syncer.Refresh(ctx, st, "jira:acme", src, "A"); err == nil || !strings.Contains(err.Error(), "get down") {
		t.Fatalf("Refresh error = %v", err)
	}
	if _, err := st.Item(ctx, "jira:acme", "A"); err != nil {
		t.Fatalf("A removed after a Get failure: %v", err)
	}
	if called(src, "IDs") {
		t.Fatalf("calls = %+v, want no IDs after a Get failure", src.Calls)
	}
}

func TestRefreshRejectsWrongSource(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	src := &fake.Source{SourceName: "jira:acme", Items: map[string]source.Item{
		"A": workItem("jira:other", "A", time.Time{}),
	}}
	if _, err := syncer.Refresh(ctx, st, "jira:acme", src, "A"); err == nil || !strings.Contains(err.Error(), `expected source "jira:acme", found "jira:other"`) {
		t.Fatalf("Refresh error = %v", err)
	}
}
