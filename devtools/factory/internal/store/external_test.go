package store_test

import (
	"context"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestReplaceExternalBlockers(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	ctx := context.Background()
	first := []store.ExternalBlocker{
		{ID: "X-1", Status: store.ExternalFound, Done: true, State: "Done", Assignee: "bob", Title: "one", FetchedAt: histT0},
		{ID: "X-2", Status: store.ExternalNotFound, FetchedAt: histT0},
	}
	if err := s.ReplaceExternalBlockers(ctx, "jira:acme", first); err != nil {
		t.Fatal(err)
	}
	if err := s.ReplaceExternalBlockers(ctx, "jira:other", []store.ExternalBlocker{{ID: "Y-1", Status: store.ExternalFound, FetchedAt: histT0}}); err != nil {
		t.Fatal(err)
	}
	got, err := s.ExternalBlockers(ctx, "jira:acme")
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]store.ExternalBlocker{"X-1": first[0], "X-2": first[1]}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got %#v\nwant %#v", got, want)
	}
	second := []store.ExternalBlocker{{ID: "X-2", Status: store.ExternalUnchecked, Reason: "over external_blocker_lookups (1)", FetchedAt: histT0}}
	if err := s.ReplaceExternalBlockers(ctx, "jira:acme", second); err != nil {
		t.Fatal(err)
	}
	got, err = s.ExternalBlockers(ctx, "jira:acme")
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(got, map[string]store.ExternalBlocker{"X-2": second[0]}) {
		t.Fatalf("after replace = %#v", got)
	}
	other, err := s.ExternalBlockers(ctx, "jira:other")
	if err != nil {
		t.Fatal(err)
	}
	if len(other) != 1 {
		t.Fatalf("other source = %#v", other)
	}
	items, err := s.Items(ctx, "jira:acme")
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 0 {
		t.Fatalf("external records became items: %#v", items)
	}
}

func TestReplaceExternalBlockersRejectsBadStatus(t *testing.T) {
	s := openAt(t, filepath.Join(t.TempDir(), "factory.db"))
	err := s.ReplaceExternalBlockers(context.Background(), "jira:acme", []store.ExternalBlocker{{ID: "X-1", Status: "maybe", FetchedAt: histT0}})
	if err == nil || !strings.Contains(err.Error(), `found "maybe"`) {
		t.Fatalf("err = %v", err)
	}
}
