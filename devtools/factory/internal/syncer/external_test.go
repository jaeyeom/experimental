package syncer_test

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

var extT0 = time.Date(2026, 11, 1, 9, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

func blocked(id string, blockers ...string) source.Item {
	it := workItem("jira:acme", id, extT0)
	for _, b := range blockers {
		it.BlockedBy = append(it.BlockedBy, source.BlockRef{ID: b})
	}
	return it
}

// externalFake mirrors PRJ-* only; X-* answer Get but are outside the query.
func externalFake(items ...source.Item) *fake.Source {
	all := map[string]source.Item{
		"X-DONE": {Source: "jira:acme", ID: "X-DONE", State: "Done", Done: true, Assignee: "bob", Title: "done one", Updated: extT0},
		"X-OPEN": {Source: "jira:acme", ID: "X-OPEN", State: "In Review", Assignee: "bob", Updated: extT0},
	}
	for _, it := range items {
		all[it.ID] = it
	}
	return &fake.Source{Account: "acct", Items: all, Query: func(it source.Item) bool { return it.ID[:2] != "X-" }}
}

func gets(src *fake.Source) []string {
	var out []string
	for _, call := range src.Calls {
		if call.Method == "Get" {
			out = append(out, call.Args[0])
		}
	}
	return out
}

func mustExternal(t *testing.T, st *store.Store) map[string]store.ExternalBlocker {
	t.Helper()
	recs, err := st.ExternalBlockers(context.Background(), "jira:acme")
	if err != nil {
		t.Fatal(err)
	}
	return recs
}

func TestSyncFetchesOutsideBlockersOnce(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	src := externalFake(
		blocked("PRJ-1", "X-DONE", "X-OPEN"),
		blocked("PRJ-2", "X-OPEN", "X-GONE", "PRJ-1"),
	)
	opts := syncer.Options{FullSyncEvery: every, ExternalLookups: map[string]int{"jira:acme": 10}}
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), extT0, opts)
	if err != nil {
		t.Fatal(err)
	}
	if got := gets(src); !slices.Equal(got, []string{"X-DONE", "X-GONE", "X-OPEN"}) {
		t.Fatalf("Get calls = %q", got)
	}
	if want := (syncer.External{Referenced: 3, Fetched: 3, Done: 1}); *results[0].External != want {
		t.Fatalf("external = %+v, want %+v", results[0].External, want)
	}
	recs := mustExternal(t, st)
	if r := recs["X-DONE"]; r.Status != store.ExternalFound || !r.Done || r.Title != "done one" || !r.FetchedAt.Equal(extT0) {
		t.Fatalf("X-DONE = %+v", r)
	}
	if r := recs["X-OPEN"]; r.Status != store.ExternalFound || r.Done || r.State != "In Review" || r.Assignee != "bob" {
		t.Fatalf("X-OPEN = %+v", r)
	}
	if r := recs["X-GONE"]; r.Status != store.ExternalNotFound {
		t.Fatalf("X-GONE = %+v", r)
	}
	if _, ok := recs["PRJ-1"]; ok || len(recs) != 3 {
		t.Fatalf("records = %+v", recs)
	}
	items, err := st.Items(ctx, "jira:acme")
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 2 {
		t.Fatalf("external blockers became items: %+v", items)
	}

	// PRJ-2 drops its links: X-GONE is no longer referenced and is dropped.
	src.Items["PRJ-2"] = workItem("jira:acme", "PRJ-2", extT0.Add(time.Hour))
	src.Calls = nil
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), extT0.Add(2*time.Hour), opts); err != nil {
		t.Fatal(err)
	}
	if got := gets(src); !slices.Equal(got, []string{"X-DONE", "X-OPEN"}) {
		t.Fatalf("second Get calls = %q", got)
	}
	if recs := mustExternal(t, st); len(recs) != 2 || recs["X-GONE"].ID != "" {
		t.Fatalf("after unlink = %+v", recs)
	}
}

func TestSyncExternalBoundKeepsPrevious(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	src := externalFake(blocked("PRJ-1", "X-DONE", "X-OPEN"))
	sync := func(bound int, at time.Time) syncer.Result {
		t.Helper()
		src.Calls = nil
		results, err := syncer.Sync(ctx, st, one("jira:acme", src), at, syncer.Options{
			FullSyncEvery: every, ExternalLookups: map[string]int{"jira:acme": bound},
		})
		if err != nil {
			t.Fatal(err)
		}
		return results[0]
	}
	res := sync(1, extT0)
	if got := gets(src); !slices.Equal(got, []string{"X-DONE"}) {
		t.Fatalf("Get calls = %q", got)
	}
	if want := (syncer.External{Referenced: 2, Fetched: 1, Done: 1, Skipped: 1}); *res.External != want {
		t.Fatalf("external = %+v", res.External)
	}
	if r := mustExternal(t, st)["X-OPEN"]; r.Status != store.ExternalUnchecked || r.Reason != "over external_blocker_lookups (1)" {
		t.Fatalf("X-OPEN over bound = %+v", r)
	}

	sync(2, extT0.Add(time.Hour))
	sync(0, extT0.Add(2*time.Hour))
	if got := gets(src); len(got) != 0 {
		t.Fatalf("bound 0 fetched %q", got)
	}
	recs := mustExternal(t, st)
	if r := recs["X-OPEN"]; r.Status != store.ExternalFound || r.State != "In Review" || !r.FetchedAt.Equal(extT0.Add(time.Hour)) {
		t.Fatalf("X-OPEN kept = %+v", r)
	}
	if r := recs["X-DONE"]; r.Status != store.ExternalFound || !r.Done {
		t.Fatalf("X-DONE kept = %+v", r)
	}
}

func TestSyncExternalFetchFailureNeverDone(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	src := externalFake(blocked("PRJ-1", "X-DONE"))
	opts := syncer.Options{FullSyncEvery: every, ExternalLookups: map[string]int{"jira:acme": 5}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), extT0, opts); err != nil {
		t.Fatal(err)
	}
	src.FailOn = map[string]error{"Get": errors.New("jira GET: 500\nboom")}
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), extT0.Add(time.Hour), opts)
	if err != nil || results[0].Err != nil {
		t.Fatalf("sync = %v %+v", err, results)
	}
	r := mustExternal(t, st)["X-DONE"]
	if r.Status != store.ExternalUnchecked || r.Done || r.Reason != "lookup failed: jira GET: 500 boom" {
		t.Fatalf("X-DONE after failed fetch = %+v", r)
	}
}

func TestSyncNoExternalLookupsLeavesRecords(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	if err := st.ReplaceExternalBlockers(ctx, "jira:acme", []store.ExternalBlocker{{ID: "X-OLD", Status: store.ExternalNotFound, FetchedAt: extT0}}); err != nil {
		t.Fatal(err)
	}
	src := externalFake(blocked("PRJ-1", "X-DONE"))
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), extT0, syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatal(err)
	}
	if got := gets(src); len(got) != 0 || results[0].External != nil {
		t.Fatalf("Get calls = %q, external = %+v", got, results[0].External)
	}
	if recs := mustExternal(t, st); len(recs) != 1 {
		t.Fatalf("records = %+v", recs)
	}
}
