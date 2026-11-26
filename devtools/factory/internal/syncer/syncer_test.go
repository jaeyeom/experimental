package syncer_test

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

const every = 24 * time.Hour

func TestSyncFirstFullThenIncremental(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	t2 := time.Date(2026, 10, 30, 1, 2, 3, 0, time.UTC)
	now := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": workItem("jira:acme", "PRJ-1", t1),
		"PRJ-2": workItem("jira:acme", "PRJ-2", t2),
	}}

	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("first Sync error = %v", err)
	}
	if len(results) != 1 || !results[0].Full || results[0].Items != 2 || results[0].Removed != 0 {
		t.Fatalf("first result = %+v", results[0])
	}
	if got := syncArgs(src); len(got) != 1 || got[0] != sinceArg(time.Time{}) {
		t.Fatalf("first since = %#v, want zero", got)
	}
	seen, full := mustState(t, st, "jira:acme")
	if !seen.Equal(t2) {
		t.Fatalf("lastSeen = %s, want %s (not %s)", seen, t2, now)
	}
	if !full.Equal(now) {
		t.Fatalf("lastFull = %s, want %s", full, now)
	}
	gotMe, err := st.Me(ctx, "jira:acme")
	if err != nil || gotMe != "acct" {
		t.Fatalf("Me = %q, %v", gotMe, err)
	}
	last, err := st.LastSync(ctx, "jira:acme")
	if err != nil || !last.Equal(now) {
		t.Fatalf("LastSync = %s, %v, want %s", last, err, now)
	}

	now2 := now.Add(time.Minute)
	results, err = syncer.Sync(ctx, st, one("jira:acme", src), now2, syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("second Sync error = %v", err)
	}
	if len(results) != 1 || results[0].Full || results[0].Items != 1 {
		t.Fatalf("second result = %+v", results[0])
	}
	if !results[0].Since.Equal(t2) {
		t.Fatalf("second Since = %s, want %s", results[0].Since, t2)
	}
	got := syncArgs(src)
	if len(got) != 2 || got[1] != sinceArg(t2) {
		t.Fatalf("second since = %#v, want %s", got, sinceArg(t2))
	}
	seen, full = mustState(t, st, "jira:acme")
	if !seen.Equal(t2) || !full.Equal(now) {
		t.Fatalf("cursor after incremental = seen %s full %s, want seen %s full %s", seen, full, t2, now)
	}
	last, err = st.LastSync(ctx, "jira:acme")
	if err != nil || !last.Equal(now2) {
		t.Fatalf("LastSync after incremental = %s, %v, want %s", last, err, now2)
	}

	delete(src.Items, "PRJ-1")
	now3 := now.Add(2 * time.Minute)
	results, err = syncer.Sync(ctx, st, one("jira:acme", src), now3, syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("third Sync error = %v", err)
	}
	if results[0].Full || results[0].Removed != 1 || results[0].Warning != "" {
		t.Fatalf("third result = %+v, want incremental with 1 removed", results[0])
	}
	if _, err := st.Item(ctx, "jira:acme", "PRJ-1"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("PRJ-1 after incremental sync error = %v, want ErrNotFound", err)
	}
}

func TestSyncIncrementalRemovesItemThatLeftQuery(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	now := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
		"B": workItem("jira:acme", "B", t1),
	}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every}); err != nil {
		t.Fatalf("first Sync error = %v", err)
	}

	// B leaves the query without a newer Updated, so Sync(since) skips it.
	src.Query = func(it source.Item) bool { return it.ID == "A" }
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now.Add(time.Minute), syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("incremental Sync error = %v", err)
	}
	if len(results) != 1 || results[0].Full || results[0].Removed != 1 || results[0].Warning != "" || results[0].Err != nil {
		t.Fatalf("result = %+v, want incremental with 1 removed", results[0])
	}
	if _, err := st.Item(ctx, "jira:acme", "B"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("B error = %v, want ErrNotFound", err)
	}
	if _, err := st.Item(ctx, "jira:acme", "A"); err != nil {
		t.Fatalf("A: %v", err)
	}
	if !called(src, "IDs") {
		t.Fatalf("calls = %+v, want IDs", src.Calls)
	}
}

func TestSyncIncrementalIDsFailureWarns(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	t2 := t1.Add(time.Hour)
	now := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
		"B": workItem("jira:acme", "B", t1),
	}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every}); err != nil {
		t.Fatalf("first Sync error = %v", err)
	}

	src.Query = func(it source.Item) bool { return it.ID == "A" }
	a := src.Items["A"]
	a.Title = "renamed"
	a.Updated = t2
	src.Items["A"] = a
	src.FailOn = map[string]error{"IDs": errors.New("search timed out")}
	now2 := now.Add(time.Minute)
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now2, syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("incremental Sync error = %v", err)
	}
	res := results[0]
	if res.Err != nil || res.Full || res.Items != 1 || res.Removed != 0 {
		t.Fatalf("result = %+v, want 1 upserted and none removed", res)
	}
	if !strings.Contains(res.Warning, "search timed out") || !strings.Contains(res.Warning, "removed nothing") {
		t.Fatalf("Warning = %q", res.Warning)
	}
	got, err := st.Item(ctx, "jira:acme", "A")
	if err != nil || got.Title != "renamed" {
		t.Fatalf("A = %+v, %v, want the upserted title", got, err)
	}
	if _, err := st.Item(ctx, "jira:acme", "B"); err != nil {
		t.Fatalf("B removed after an IDs failure: %v", err)
	}
	seen, _ := mustState(t, st, "jira:acme")
	if !seen.Equal(t2) {
		t.Fatalf("lastSeen = %s, want %s", seen, t2)
	}
	if last, err := st.LastSync(ctx, "jira:acme"); err != nil || !last.Equal(now2) {
		t.Fatalf("LastSync = %s, %v, want %s", last, err, now2)
	}
}

func TestSyncIncrementalEmptyIDsWarns(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	now := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", t1),
		"B": workItem("jira:acme", "B", t1),
	}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every}); err != nil {
		t.Fatalf("first Sync error = %v", err)
	}

	src.Query = func(source.Item) bool { return false }
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now.Add(time.Minute), syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("incremental Sync error = %v", err)
	}
	res := results[0]
	if res.Err != nil || res.Full || res.Removed != 0 {
		t.Fatalf("result = %+v, want none removed", res)
	}
	if !strings.Contains(res.Warning, "found none while the mirror has 2 items") {
		t.Fatalf("Warning = %q", res.Warning)
	}
	items, err := st.Items(ctx, "jira:acme")
	if err != nil || len(items) != 2 {
		t.Fatalf("items = %d, %v, want 2", len(items), err)
	}
}

func TestSyncIncrementalEmptyIDsOnEmptyMirrorIsQuiet(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	now := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every}); err != nil {
		t.Fatalf("first Sync error = %v", err)
	}
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now.Add(time.Minute), syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("incremental Sync error = %v", err)
	}
	if res := results[0]; res.Full || res.Warning != "" || res.Removed != 0 {
		t.Fatalf("result = %+v, want a quiet incremental sync", res)
	}
}

func TestSyncFullDoesNotQueryIDs(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	now := time.Date(2026, 10, 31, 12, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"A": workItem("jira:acme", "A", now),
	}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{Full: true, FullSyncEvery: every}); err != nil {
		t.Fatalf("Sync error = %v", err)
	}
	if called(src, "IDs") {
		t.Fatalf("full sync called IDs: %+v", src.Calls)
	}
}

func called(src *fake.Source, method string) bool {
	for _, call := range src.Calls {
		if call.Method == method {
			return true
		}
	}
	return false
}

func TestSyncFailureDoesNotRecordLastSync(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	src := &fake.Source{
		Account: "acct",
		FailOn:  map[string]error{"Sync": errors.New("down")},
		Items:   map[string]source.Item{"PRJ-1": workItem("jira:acme", "PRJ-1", now)},
	}
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every})
	if err == nil || len(results) != 1 || results[0].Err == nil {
		t.Fatalf("Sync error = %v, results = %+v", err, results)
	}
	last, err := st.LastSync(ctx, "jira:acme")
	if err != nil || !last.IsZero() {
		t.Fatalf("LastSync = %s, %v, want zero", last, err)
	}
}

func TestSyncFullAfterFullSyncEvery(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	now := time.Date(2026, 10, 31, 0, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": workItem("jira:acme", "PRJ-1", updated),
	}}
	opts := syncer.Options{FullSyncEvery: every}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, opts); err != nil {
		t.Fatalf("first Sync error = %v", err)
	}

	almost := now.Add(every - time.Nanosecond)
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), almost, opts)
	if err != nil {
		t.Fatalf("boundary Sync error = %v", err)
	}
	if results[0].Full {
		t.Fatalf("sync %s before the interval was full", every-time.Nanosecond)
	}
	if results[0].Since.IsZero() {
		t.Fatal("incremental since is zero")
	}

	at := now.Add(every)
	results, err = syncer.Sync(ctx, st, one("jira:acme", src), at, opts)
	if err != nil {
		t.Fatalf("elapsed Sync error = %v", err)
	}
	if !results[0].Full || !results[0].Since.IsZero() {
		t.Fatalf("elapsed result = %+v, want full with zero since", results[0])
	}
	got := syncArgs(src)
	if len(got) != 3 || got[2] != sinceArg(time.Time{}) {
		t.Fatalf("elapsed since args = %#v", got)
	}
	_, full := mustState(t, st, "jira:acme")
	if !full.Equal(at) {
		t.Fatalf("lastFull = %s, want %s", full, at)
	}
}

func TestSyncFullRemovesDroppedItem(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	t1 := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	t2 := time.Date(2026, 10, 30, 4, 5, 6, 0, time.UTC)
	now := time.Date(2026, 10, 31, 8, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": workItem("jira:acme", "PRJ-1", t1),
		"PRJ-2": workItem("jira:acme", "PRJ-2", t2),
	}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every}); err != nil {
		t.Fatalf("seed Sync error = %v", err)
	}
	delete(src.Items, "PRJ-2")

	now2 := now.Add(time.Minute)
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now2, syncer.Options{Full: true, FullSyncEvery: every})
	if err != nil {
		t.Fatalf("full Sync error = %v", err)
	}
	if len(results) != 1 || !results[0].Full || results[0].Items != 1 || results[0].Removed != 1 {
		t.Fatalf("result = %+v", results[0])
	}
	if _, err := st.Item(ctx, "jira:acme", "PRJ-2"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("dropped item error = %v, want not found", err)
	}
	if _, err := st.Item(ctx, "jira:acme", "PRJ-1"); err != nil {
		t.Fatalf("kept item: %v", err)
	}
	got := syncArgs(src)
	if len(got) != 2 || got[1] != sinceArg(time.Time{}) {
		t.Fatalf("full since args = %#v", got)
	}
}

func TestSyncEmptyFullKeepsCursor(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	now := time.Date(2026, 10, 31, 8, 0, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": workItem("jira:acme", "PRJ-1", updated),
	}}
	if _, err := syncer.Sync(ctx, st, one("jira:acme", src), now, syncer.Options{FullSyncEvery: every}); err != nil {
		t.Fatalf("seed Sync error = %v", err)
	}
	src.Items = map[string]source.Item{}
	now2 := now.Add(time.Hour)
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), now2, syncer.Options{Full: true, FullSyncEvery: every})
	if err != nil {
		t.Fatalf("empty Sync error = %v", err)
	}
	if results[0].Items != 0 || results[0].Removed != 1 {
		t.Fatalf("result = %+v", results[0])
	}
	seen, full := mustState(t, st, "jira:acme")
	if !seen.Equal(updated) {
		t.Fatalf("lastSeen = %s, want %s", seen, updated)
	}
	if !full.Equal(now2) {
		t.Fatalf("lastFull = %s, want %s", full, now2)
	}
}

func TestSyncOneFailureContinues(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	now := time.Date(2026, 10, 31, 8, 0, 0, 0, time.UTC)
	boom := errors.New("jira down")
	bad := &fake.Source{Account: "acct", FailOn: map[string]error{"Sync": boom}}
	ok := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": workItem("jira:ok", "PRJ-1", updated),
	}}
	results, err := syncer.Sync(ctx, st, map[string]source.Source{
		"jira:bad": bad,
		"jira:ok":  ok,
	}, now, syncer.Options{FullSyncEvery: every})
	if err != nil {
		t.Fatalf("Sync error = %v, want nil", err)
	}
	if len(results) != 2 {
		t.Fatalf("results = %d, want 2", len(results))
	}
	if results[0].Source != "jira:bad" || !errors.Is(results[0].Err, boom) {
		t.Fatalf("bad result = %+v", results[0])
	}
	if results[1].Source != "jira:ok" || results[1].Err != nil || results[1].Items != 1 {
		t.Fatalf("ok result = %+v", results[1])
	}
	if _, err := st.Item(ctx, "jira:ok", "PRJ-1"); err != nil {
		t.Fatalf("ok item: %v", err)
	}
	seen, full := mustState(t, st, "jira:bad")
	if !seen.IsZero() || !full.IsZero() {
		t.Fatalf("failed cursor = seen %s full %s, want zero", seen, full)
	}
	if len(syncArgs(ok)) != 1 {
		t.Fatal("ok source was not synced")
	}
}

func TestSyncEverySourceFailed(t *testing.T) {
	st := openStore(t)
	boom := errors.New("down")
	bad := &fake.Source{Account: "acct", FailOn: map[string]error{"Sync": boom}}
	also := &fake.Source{Account: "acct", FailOn: map[string]error{"Sync": boom}}
	results, err := syncer.Sync(context.Background(), st, map[string]source.Source{
		"jira:a": bad,
		"jira:b": also,
	}, time.Date(2026, 10, 31, 0, 0, 0, 0, time.UTC), syncer.Options{FullSyncEvery: every})
	if err == nil || !strings.Contains(err.Error(), "every source failed") {
		t.Fatalf("Sync error = %v", err)
	}
	if len(results) != 2 || results[0].Err == nil || results[1].Err == nil {
		t.Fatalf("results = %+v", results)
	}
}

func TestSyncOnlyFilter(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	now := time.Date(2026, 10, 31, 0, 0, 0, 0, time.UTC)
	want := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": workItem("jira:acme", "PRJ-1", updated),
	}}
	other := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-2": workItem("jira:cub", "PRJ-2", updated),
	}}
	results, err := syncer.Sync(ctx, st, map[string]source.Source{
		"jira:acme": want,
		"jira:cub":  other,
	}, now, syncer.Options{Only: "jira:acme", FullSyncEvery: every})
	if err != nil {
		t.Fatalf("Sync error = %v", err)
	}
	if len(results) != 1 || results[0].Source != "jira:acme" || results[0].Items != 1 {
		t.Fatalf("results = %+v", results)
	}
	if len(syncArgs(other)) != 0 {
		t.Fatalf("filtered source was called: %#v", other.Calls)
	}
	if _, err := st.Item(ctx, "jira:cub", "PRJ-2"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("filtered item error = %v, want not found", err)
	}
}

func TestSyncOnlyUnknown(t *testing.T) {
	st := openStore(t)
	src := &fake.Source{}
	_, err := syncer.Sync(context.Background(), st, one("jira:acme", src), time.Now(), syncer.Options{Only: "acme"})
	if err == nil || !strings.Contains(err.Error(), "jira:acme") || !strings.Contains(err.Error(), `found "acme"`) {
		t.Fatalf("Sync error = %v", err)
	}
	if len(syncArgs(src)) != 0 {
		t.Fatalf("unknown filter still synced: %#v", src.Calls)
	}
}

func TestSyncRejectsBadInput(t *testing.T) {
	st := openStore(t)
	srcs := one("jira:acme", &fake.Source{})
	now := time.Date(2026, 10, 31, 0, 0, 0, 0, time.UTC)
	tests := []struct {
		name string
		ctx  context.Context
		st   *store.Store
		srcs map[string]source.Source
		want string
	}{
		{name: "nil context", st: st, srcs: srcs, want: "expected a context, found nil"},
		{name: "nil store", ctx: context.Background(), srcs: srcs, want: "expected a store, found nil"},
		{name: "nil sources", ctx: context.Background(), st: st, want: "expected a source map, found nil"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := syncer.Sync(tc.ctx, tc.st, tc.srcs, now, syncer.Options{})
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Sync error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestSyncMeErrorSkipsItems(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{
		Account: "acct",
		FailOn:  map[string]error{"Me": errors.New("whoami failed")},
		Items:   map[string]source.Item{"PRJ-1": workItem("jira:acme", "PRJ-1", updated)},
	}
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), updated, syncer.Options{FullSyncEvery: every})
	if err == nil || !strings.Contains(err.Error(), "whoami failed") {
		t.Fatalf("Sync error = %v", err)
	}
	if len(results) != 1 || results[0].Err == nil || !strings.Contains(results[0].Err.Error(), "whoami failed") {
		t.Fatalf("result = %+v", results)
	}
	if len(syncArgs(src)) != 0 {
		t.Fatalf("Sync ran after Me failed: %#v", src.Calls)
	}
	if _, err := st.Item(ctx, "jira:acme", "PRJ-1"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("item error = %v, want not found", err)
	}
	if me, err := st.Me(ctx, "jira:acme"); err != nil || me != "" {
		t.Fatalf("Me = %q, %v", me, err)
	}
}

func TestSyncRejectsEmptyMe(t *testing.T) {
	st := openStore(t)
	ctx := context.Background()
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{Items: map[string]source.Item{
		"PRJ-1": workItem("jira:acme", "PRJ-1", updated),
	}}
	results, err := syncer.Sync(ctx, st, one("jira:acme", src), updated, syncer.Options{FullSyncEvery: every})
	if err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("Sync error = %v", err)
	}
	if len(results) != 1 || results[0].Err == nil || !strings.Contains(results[0].Err.Error(), `found empty`) {
		t.Fatalf("result = %+v", results)
	}
	if len(syncArgs(src)) != 0 {
		t.Fatalf("Sync ran with an empty me: %#v", src.Calls)
	}
}

func openStore(t *testing.T) *store.Store {
	t.Helper()
	st, err := store.Open(context.Background(), filepath.Join(t.TempDir(), "factory.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	})
	return st
}

func one(name string, src source.Source) map[string]source.Source {
	return map[string]source.Source{name: src}
}

func workItem(src, id string, updated time.Time) source.Item {
	return source.Item{Source: src, ID: id, Title: id, Created: updated, Updated: updated}
}

func syncArgs(src *fake.Source) []string {
	var out []string
	for _, call := range src.Calls {
		if call.Method == "Sync" && len(call.Args) > 0 {
			out = append(out, call.Args[0])
		}
	}
	return out
}

func sinceArg(t time.Time) string {
	return t.UTC().Format(time.RFC3339Nano)
}

func mustState(t *testing.T, st *store.Store, name string) (time.Time, time.Time) {
	t.Helper()
	seen, full, err := st.SyncState(context.Background(), name)
	if err != nil {
		t.Fatalf("SyncState(%s): %v", name, err)
	}
	return seen, full
}
