package backlog_test

import (
	"context"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/backlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

const key = "jira:acme"

var t0 = time.Date(2026, 11, 1, 9, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

func TestSyncWritesOneSnapshotPerHour(t *testing.T) {
	ctx := context.Background()
	st := openStore(t)
	cfg := testConfig()
	src := &fake.Source{SourceName: key, Account: "me", Items: map[string]source.Item{
		"PRJ-1": item("PRJ-1", "To Do"),
		"PRJ-2": blockedItem("PRJ-2", "PRJ-1"),
		"PRJ-3": withLabels(item("PRJ-3", "To Do"), "needs-human"),
		"PRJ-4": item("PRJ-4", "In Progress"),
	}}
	if err := st.InsertClaim(ctx, store.Claim{Source: key, ID: "PRJ-4", ClaimedAt: t0, State: store.Started, UpdatedAt: t0}); err != nil {
		t.Fatal(err)
	}
	opts := syncer.Options{FullSyncEvery: 24 * time.Hour, AfterSync: backlog.Hook(st, cfg)}
	for _, at := range []time.Time{t0, t0.Add(30 * time.Minute)} {
		results, err := syncer.Sync(ctx, st, map[string]source.Source{key: src}, at, opts)
		if err != nil || results[0].Err != nil {
			t.Fatalf("Sync at %s = %+v, %v", at, results, err)
		}
	}
	snaps, err := st.BacklogSnapshots(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(snaps) != 1 {
		t.Fatalf("snapshots = %+v, want one", snaps)
	}
	got := snaps[0]
	if got.Ready != 1 || got.Blocked != 1 || got.NeedsHuman != 1 || got.InFlight != 1 ||
		got.ByStatus["To Do"] != 3 || got.ByStatus["In Progress"] != 1 || !got.TakenAt.Equal(t0) {
		t.Fatalf("snapshot = %+v", got)
	}
	if _, err := syncer.Sync(ctx, st, map[string]source.Source{key: src}, t0.Add(time.Hour), opts); err != nil {
		t.Fatal(err)
	}
	if snaps, _ = st.BacklogSnapshots(ctx); len(snaps) != 2 {
		t.Fatalf("snapshots after an hour = %d, want 2", len(snaps))
	}
}

// TestFinishedSurvivesFullSync: a sync sees an item become done, then a
// full sync no longer returns it and deletes it from items.
func TestFinishedSurvivesFullSync(t *testing.T) {
	ctx := context.Background()
	st := openStore(t)
	src := &fake.Source{SourceName: key, Account: "me", Items: map[string]source.Item{"PRJ-1": item("PRJ-1", "To Do")}}
	srcs := map[string]source.Source{key: src}
	opts := syncer.Options{FullSyncEvery: 24 * time.Hour}
	if _, err := syncer.Sync(ctx, st, srcs, t0, opts); err != nil {
		t.Fatal(err)
	}
	done := src.Items["PRJ-1"]
	done.Done, done.State, done.Updated = true, "Done", t0.Add(time.Hour)
	src.Items["PRJ-1"] = done
	if _, err := syncer.Sync(ctx, st, srcs, t0.Add(2*time.Hour), opts); err != nil {
		t.Fatal(err)
	}
	delete(src.Items, "PRJ-1")
	opts.Full = true
	if _, err := syncer.Sync(ctx, st, srcs, t0.Add(3*time.Hour), opts); err != nil {
		t.Fatal(err)
	}
	if items, _ := st.Items(ctx, key); len(items) != 0 {
		t.Fatalf("items = %+v, want none", items)
	}
	rows, err := st.FinishedItems(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 || rows[0].ID != "PRJ-1" || !rows[0].FinishedAt.Equal(done.Updated) {
		t.Fatalf("finished = %+v", rows)
	}
}

func TestHookUnknownSourceFails(t *testing.T) {
	st := openStore(t)
	err := backlog.Hook(st, testConfig())(context.Background(), "jira:other", t0)
	if err == nil {
		t.Fatal("Hook error = nil, want an unknown source error")
	}
}

func testConfig() *config.Config {
	return &config.Config{Sources: []config.Source{{
		Name: "acme", Kind: "jira", AssigneeOK: "any",
		ExcludeLabels: []string{"needs-human"}, HumanLabels: []string{"needs-human"},
		ReadyStatuses: []string{"To Do"}, PRTitlePattern: `\[{id}\]`,
	}}}
}

func item(id, state string) source.Item {
	return source.Item{Source: key, ID: id, Title: id, Type: "Task", State: state, Created: t0, Updated: t0}
}

func blockedItem(id, blocker string) source.Item {
	it := item(id, "To Do")
	it.BlockedBy = []source.BlockRef{{ID: blocker}}
	return it
}

func withLabels(it source.Item, labels ...string) source.Item {
	it.Labels = labels
	return it
}

func openStore(t *testing.T) *store.Store {
	t.Helper()
	st, err := store.Open(context.Background(), filepath.Join(t.TempDir(), "factory.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Error(err)
		}
	})
	return st
}
