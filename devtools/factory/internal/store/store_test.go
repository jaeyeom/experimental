package store_test

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestDefaultPath(t *testing.T) {
	homeCalled := false
	tests := []struct {
		name      string
		lookup    func(string) (string, bool)
		home      func() (string, error)
		want      string
		wantPanic string
	}{
		{
			name: "xdg state home",
			lookup: func(key string) (string, bool) {
				if key == "XDG_STATE_HOME" {
					return "/var/state", true
				}
				return "", false
			},
			home: func() (string, error) {
				homeCalled = true
				return "/home/me", nil
			},
			want: "/var/state/factory/factory.db",
		},
		{
			name: "unset xdg falls back to home",
			lookup: func(string) (string, bool) {
				return "", false
			},
			home: func() (string, error) {
				return "/home/me", nil
			},
			want: "/home/me/.local/state/factory/factory.db",
		},
		{
			name: "empty xdg state home",
			lookup: func(string) (string, bool) {
				return "", true
			},
			home: func() (string, error) {
				return "/home/me", nil
			},
			wantPanic: "XDG_STATE_HOME: expected a non-empty path, found empty",
		},
		{
			name:      "nil lookup",
			wantPanic: "expected LookupEnv, found nil",
		},
		{
			name: "empty home",
			lookup: func(string) (string, bool) {
				return "", false
			},
			home: func() (string, error) {
				return "", nil
			},
			wantPanic: "home directory: expected a non-empty path, found empty",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			homeCalled = false
			env := config.Env{LookupEnv: tc.lookup, HomeDir: tc.home}
			if tc.wantPanic != "" {
				defer func() {
					rec := recover()
					if rec == nil {
						t.Fatal("expected a panic")
					}
					if !strings.Contains(fmt.Sprint(rec), tc.wantPanic) {
						t.Fatalf("panic = %v, want substring %q", rec, tc.wantPanic)
					}
				}()
			}
			got := store.DefaultPath(env)
			if tc.wantPanic != "" {
				t.Fatalf("DefaultPath returned %q", got)
			}
			if got != tc.want {
				t.Fatalf("DefaultPath = %q, want %q", got, tc.want)
			}
			if tc.name == "xdg state home" && homeCalled {
				t.Fatal("HomeDir was called when XDG_STATE_HOME was set")
			}
		})
	}
}

func TestOpenCreatesPrivateParent(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state dir", "factory.db")
	openAt(t, path)
	info, err := os.Stat(filepath.Dir(path))
	if err != nil {
		t.Fatalf("stat parent: %v", err)
	}
	if info.Mode().Perm() != 0o700 {
		t.Fatalf("parent mode = %o, want 700", info.Mode().Perm())
	}
}

func TestOpenRejectsEmptyPath(t *testing.T) {
	_, err := store.Open(context.Background(), "")
	if err == nil || !strings.Contains(err.Error(), "expected a non-empty path, found empty") {
		t.Fatalf("Open empty path error = %v", err)
	}
}

func TestMigrationsAreIdempotent(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory", "factory.db")
	ctx := context.Background()
	s := openAt(t, path)
	item := fullItem("PRJ-1")
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	reopened, err := store.Open(ctx, path)
	if err != nil {
		t.Fatalf("second Open: %v", err)
	}
	t.Cleanup(func() {
		if err := reopened.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	})
	got, err := reopened.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item after reopen: %v", err)
	}
	if !reflect.DeepEqual(got, item) {
		t.Fatalf("item after reopen = %#v\nwant %#v", got, item)
	}
	again := item
	again.Title = "still open"
	if err := reopened.UpsertItems(ctx, again.Source, []source.Item{again}); err != nil {
		t.Fatalf("upsert after second open: %v", err)
	}
}

func TestUpsertRoundTripAndDroppedLink(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	item := fullItem("PRJ-1")
	// A link where PRJ-1 is the blocker must survive a rewrite of PRJ-1's own blockers.
	other := fullItem("PRJ-4")
	other.BlockedBy = []source.BlockRef{{ID: "PRJ-1", LinkID: "keep-me"}}
	other.Labels = []string{}
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item, other}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	got, err := s.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item: %v", err)
	}
	if !reflect.DeepEqual(got, item) {
		t.Fatalf("item = %#v\nwant %#v", got, item)
	}
	if got.Created != item.Created || got.Updated != item.Updated {
		t.Fatalf("times = %s %s, want %s %s", got.Created, got.Updated, item.Created, item.Updated)
	}

	item.BlockedBy = []source.BlockRef{{ID: "PRJ-3", LinkID: "101"}}
	item.Title = "rewritten"
	item.Done = true
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("rewrite: %v", err)
	}
	got, err = s.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item after rewrite: %v", err)
	}
	if !reflect.DeepEqual(got.BlockedBy, item.BlockedBy) {
		t.Fatalf("BlockedBy = %#v, want %#v", got.BlockedBy, item.BlockedBy)
	}
	if !got.Done || got.Title != "rewritten" {
		t.Fatalf("rewritten item = %#v", got)
	}
	blocks, err := s.Blocks(ctx, item.Source, "PRJ-1")
	if err != nil {
		t.Fatalf("Blocks: %v", err)
	}
	if !reflect.DeepEqual(blocks, []string{"PRJ-4"}) {
		t.Fatalf("Blocks(PRJ-1) = %#v, want [PRJ-4]", blocks)
	}
}

func TestUpsertRejectsMismatchedSource(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	good := fullItem("PRJ-1")
	bad := fullItem("PRJ-2")
	bad.Source = "other"
	err := s.UpsertItems(ctx, good.Source, []source.Item{good, bad})
	if err == nil || !strings.Contains(err.Error(), `expected source "jira:acme"`) || !strings.Contains(err.Error(), `found "other"`) {
		t.Fatalf("UpsertItems error = %v", err)
	}
	_, err = s.Item(ctx, good.Source, good.ID)
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("partial upsert persisted, Item error = %v", err)
	}
}

func TestDeleteItemsNotInRemovesBothLinkDirections(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	src := "jira:acme"
	a := fullItem("A")
	a.BlockedBy = []source.BlockRef{{ID: "EXT", LinkID: "ext"}}
	b := fullItem("B")
	b.BlockedBy = []source.BlockRef{{ID: "A", LinkID: "ab"}}
	c := fullItem("C")
	c.BlockedBy = []source.BlockRef{{ID: "B", LinkID: "bc"}}
	other := fullItem("Z")
	other.Source = "github:repo"
	other.BlockedBy = []source.BlockRef{{ID: "B", LinkID: "zb"}}
	if err := s.UpsertItems(ctx, src, []source.Item{a, b, c}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	if err := s.UpsertItems(ctx, other.Source, []source.Item{other}); err != nil {
		t.Fatalf("UpsertItems other: %v", err)
	}

	n, err := s.DeleteItemsNotIn(ctx, src, map[string]bool{"A": true, "C": true, "gone": false})
	if err != nil {
		t.Fatalf("DeleteItemsNotIn: %v", err)
	}
	if n != 1 {
		t.Fatalf("deleted = %d, want 1", n)
	}
	if _, err := s.Item(ctx, src, "B"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("B error = %v, want ErrNotFound", err)
	}
	for _, id := range []string{"A", "C"} {
		if _, err := s.Item(ctx, src, id); err != nil {
			t.Fatalf("Item %s: %v", id, err)
		}
	}
	blocksA, err := s.Blocks(ctx, src, "A")
	if err != nil {
		t.Fatalf("Blocks A: %v", err)
	}
	if len(blocksA) != 0 {
		t.Fatalf("Blocks(A) = %#v, want none", blocksA)
	}
	blocksB, err := s.Blocks(ctx, src, "B")
	if err != nil {
		t.Fatalf("Blocks B: %v", err)
	}
	if len(blocksB) != 0 {
		t.Fatalf("Blocks(B) = %#v, want none", blocksB)
	}
	gotA, err := s.Item(ctx, src, "A")
	if err != nil {
		t.Fatalf("Item A: %v", err)
	}
	if !reflect.DeepEqual(gotA.BlockedBy, a.BlockedBy) {
		t.Fatalf("A.BlockedBy = %#v, want the external blocker kept", gotA.BlockedBy)
	}
	if _, err := s.Item(ctx, other.Source, "Z"); err != nil {
		t.Fatalf("other source item: %v", err)
	}

	n, err = s.DeleteItemsNotIn(ctx, src, nil)
	if err != nil {
		t.Fatalf("delete all: %v", err)
	}
	if n != 2 {
		t.Fatalf("deleted = %d, want 2", n)
	}
	left, err := s.Items(ctx, src)
	if err != nil {
		t.Fatalf("Items: %v", err)
	}
	if len(left) != 0 {
		t.Fatalf("items left = %#v", left)
	}
	if _, err := s.Item(ctx, other.Source, "Z"); err != nil {
		t.Fatalf("other source after wipe: %v", err)
	}
}

func TestDeleteItemRemovesRowAndLinks(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	src := "jira:acme"
	a := fullItem("A")
	a.BlockedBy = nil
	b := fullItem("B")
	b.BlockedBy = []source.BlockRef{{ID: "A", LinkID: "ab"}}
	c := fullItem("C")
	c.BlockedBy = []source.BlockRef{{ID: "B", LinkID: "bc"}}
	other := fullItem("B")
	other.Source = "github:repo"
	other.BlockedBy = nil
	if err := s.UpsertItems(ctx, src, []source.Item{a, b, c}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	if err := s.UpsertItems(ctx, other.Source, []source.Item{other}); err != nil {
		t.Fatalf("UpsertItems other: %v", err)
	}
	if n, err := s.ItemCount(ctx, src); err != nil || n != 3 {
		t.Fatalf("ItemCount = %d, %v, want 3", n, err)
	}

	deleted, err := s.DeleteItem(ctx, src, "B")
	if err != nil || !deleted {
		t.Fatalf("DeleteItem(B) = %v, %v, want true", deleted, err)
	}
	if _, err := s.Item(ctx, src, "B"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("B error = %v, want ErrNotFound", err)
	}
	if blocks, err := s.Blocks(ctx, src, "A"); err != nil || len(blocks) != 0 {
		t.Fatalf("Blocks(A) = %v, %v, want none", blocks, err)
	}
	gotC, err := s.Item(ctx, src, "C")
	if err != nil {
		t.Fatalf("Item C: %v", err)
	}
	if len(gotC.BlockedBy) != 0 {
		t.Fatalf("C.BlockedBy = %+v, want none", gotC.BlockedBy)
	}
	if _, err := s.Item(ctx, other.Source, "B"); err != nil {
		t.Fatalf("other source B: %v", err)
	}
	if n, err := s.ItemCount(ctx, src); err != nil || n != 2 {
		t.Fatalf("ItemCount = %d, %v, want 2", n, err)
	}

	deleted, err = s.DeleteItem(ctx, src, "B")
	if err != nil || deleted {
		t.Fatalf("DeleteItem(B) again = %v, %v, want false", deleted, err)
	}
	if _, err := s.DeleteItem(ctx, src, ""); err == nil {
		t.Fatal("DeleteItem with empty id: want error")
	}
}

func TestItemsAndFindItem(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	a := fullItem("PRJ-2")
	b := fullItem("PRJ-1")
	b.Labels = nil
	gh := fullItem("PRJ-1")
	gh.Source = "github:exp"
	gh.BlockedBy = nil
	if err := s.UpsertItems(ctx, a.Source, []source.Item{a, b}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	if err := s.UpsertItems(ctx, gh.Source, []source.Item{gh}); err != nil {
		t.Fatalf("UpsertItems github: %v", err)
	}

	oneSource, err := s.Items(ctx, a.Source)
	if err != nil {
		t.Fatalf("Items source: %v", err)
	}
	if len(oneSource) != 2 || oneSource[0].ID != "PRJ-1" || oneSource[1].ID != "PRJ-2" {
		t.Fatalf("Items(source) = %#v", ids(oneSource))
	}
	if oneSource[0].Labels != nil {
		t.Fatalf("nil labels = %#v", oneSource[0].Labels)
	}
	all, err := s.Items(ctx, "")
	if err != nil {
		t.Fatalf("Items all: %v", err)
	}
	if len(all) != 3 || all[0].Source != "github:exp" || all[2].ID != "PRJ-2" {
		t.Fatalf("Items(\"\") = %#v", ids(all))
	}
	found, err := s.FindItem(ctx, "PRJ-1")
	if err != nil {
		t.Fatalf("FindItem: %v", err)
	}
	if len(found) != 2 || found[0].Source != "github:exp" || found[1].Source != a.Source {
		t.Fatalf("FindItem = %#v", ids(found))
	}
	if _, err := s.Item(ctx, a.Source, "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("missing item error = %v", err)
	}
}

func TestBlocksReverseLookup(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	a := fullItem("A")
	a.BlockedBy = nil
	b := fullItem("B")
	b.BlockedBy = []source.BlockRef{{ID: "A", LinkID: "1"}}
	c := fullItem("C")
	c.BlockedBy = []source.BlockRef{{ID: "A", LinkID: "2"}}
	if err := s.UpsertItems(ctx, a.Source, []source.Item{a, b, c}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	got, err := s.Blocks(ctx, a.Source, "A")
	if err != nil {
		t.Fatalf("Blocks: %v", err)
	}
	if !reflect.DeepEqual(got, []string{"B", "C"}) {
		t.Fatalf("Blocks(A) = %#v, want [B C]", got)
	}
	none, err := s.Blocks(ctx, a.Source, "B")
	if err != nil {
		t.Fatalf("Blocks B: %v", err)
	}
	if len(none) != 0 {
		t.Fatalf("Blocks(B) = %#v", none)
	}
}

func TestSyncStateRoundTrip(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	seen, full, err := s.SyncState(ctx, "jira:acme")
	if err != nil {
		t.Fatalf("missing SyncState: %v", err)
	}
	if !seen.IsZero() || !full.IsZero() {
		t.Fatalf("missing sync state = %s %s, want zero", seen, full)
	}
	wantSeen := time.Date(2026, 10, 29, 3, 12, 0, 123456789, time.UTC)
	wantFull := time.Date(2026, 10, 31, 1, 2, 3, 4, time.UTC)
	if err := s.SetSyncState(ctx, "jira:acme", wantSeen, wantFull); err != nil {
		t.Fatalf("SetSyncState: %v", err)
	}
	seen, full, err = s.SyncState(ctx, "jira:acme")
	if err != nil {
		t.Fatalf("SyncState: %v", err)
	}
	if seen != wantSeen || full != wantFull {
		t.Fatalf("sync state = %s %s, want %s %s", seen, full, wantSeen, wantFull)
	}
	loc := time.FixedZone("KST", 9*60*60)
	local := time.Date(2026, 11, 1, 9, 0, 0, 987654321, loc)
	if err := s.SetSyncState(ctx, "jira:acme", local, wantFull); err != nil {
		t.Fatalf("SetSyncState local: %v", err)
	}
	seen, _, err = s.SyncState(ctx, "jira:acme")
	if err != nil {
		t.Fatalf("SyncState local: %v", err)
	}
	if !seen.Equal(local) || seen.Location() != time.UTC || seen.Nanosecond() != local.Nanosecond() {
		t.Fatalf("local time stored as %s (%v)", seen, seen.Location())
	}
}

func TestMeRoundTrip(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	if err := s.SetMe(ctx, "", "alice"); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty source error = %v", err)
	}
	if err := s.SetMe(ctx, "jira:acme", ""); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty account error = %v", err)
	}
	if err := s.SetMe(ctx, "jira:acme", "alice"); err == nil || !strings.Contains(err.Error(), "found none") {
		t.Fatalf("missing row error = %v", err)
	}
	if me, err := s.Me(ctx, "jira:acme"); err != nil || me != "" {
		t.Fatalf("missing Me = %q, %v", me, err)
	}
	if _, err := s.Me(ctx, ""); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty Me source error = %v", err)
	}

	seen := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	full := time.Date(2026, 10, 31, 1, 2, 3, 0, time.UTC)
	if err := s.SetSyncState(ctx, "jira:acme", seen, full); err != nil {
		t.Fatalf("SetSyncState: %v", err)
	}
	if err := s.SetMe(ctx, "jira:acme", "alice"); err != nil {
		t.Fatalf("SetMe: %v", err)
	}
	later := seen.Add(time.Hour)
	if err := s.SetSyncState(ctx, "jira:acme", later, full); err != nil {
		t.Fatalf("SetSyncState again: %v", err)
	}
	me, err := s.Me(ctx, "jira:acme")
	if err != nil || me != "alice" {
		t.Fatalf("Me after cursor update = %q, %v", me, err)
	}
}

func TestLastSyncRoundTrip(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	if _, err := s.LastSync(ctx, ""); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty source error = %v", err)
	}
	if err := s.SetLastSync(ctx, "", time.Now()); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty SetLastSync source error = %v", err)
	}
	if err := s.SetLastSync(ctx, "jira:acme", time.Time{}); err == nil || !strings.Contains(err.Error(), "found zero") {
		t.Fatalf("zero time error = %v", err)
	}
	if err := s.SetLastSync(ctx, "jira:acme", time.Now().UTC()); err == nil || !strings.Contains(err.Error(), "found none") {
		t.Fatalf("missing row error = %v", err)
	}
	got, err := s.LastSync(ctx, "jira:acme")
	if err != nil || !got.IsZero() {
		t.Fatalf("missing LastSync = %s, %v", got, err)
	}

	seen := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	full := time.Date(2026, 10, 31, 1, 2, 3, 0, time.UTC)
	if err := s.SetSyncState(ctx, "jira:acme", seen, full); err != nil {
		t.Fatalf("SetSyncState: %v", err)
	}
	want := time.Date(2026, 11, 5, 12, 0, 0, 123456789, time.UTC)
	if err := s.SetLastSync(ctx, "jira:acme", want); err != nil {
		t.Fatalf("SetLastSync: %v", err)
	}
	got, err = s.LastSync(ctx, "jira:acme")
	if err != nil || !got.Equal(want) || got.Location() != time.UTC {
		t.Fatalf("LastSync = %s (%v), want %s", got, got.Location(), want)
	}
	laterSeen := seen.Add(time.Hour)
	if err := s.SetSyncState(ctx, "jira:acme", laterSeen, full); err != nil {
		t.Fatalf("SetSyncState again: %v", err)
	}
	got, err = s.LastSync(ctx, "jira:acme")
	if err != nil || !got.Equal(want) {
		t.Fatalf("LastSync after cursor update = %s, %v", got, err)
	}
}

func TestMigrateV2AddsLastSync(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	v2 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE INDEX links_blocker ON links(source,blocker_id);
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (2);
INSERT INTO sync_state(source, last_updated_seen, last_full_sync, me)
  VALUES ('jira:acme', '2026-10-29T03:12:00.000000000Z', '2026-10-29T03:12:00.000000000Z', 'alice');`
	if _, err := db.Exec(v2); err != nil {
		t.Fatalf("v2 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	s := openAt(t, path)
	ctx := context.Background()
	got, err := s.LastSync(ctx, "jira:acme")
	if err != nil || !got.IsZero() {
		t.Fatalf("migrated LastSync = %s, %v", got, err)
	}
	want := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	if err := s.SetLastSync(ctx, "jira:acme", want); err != nil {
		t.Fatalf("SetLastSync on migrated db: %v", err)
	}
	got, err = s.LastSync(ctx, "jira:acme")
	if err != nil || !got.Equal(want) {
		t.Fatalf("LastSync = %s, %v", got, err)
	}
	me, err := s.Me(ctx, "jira:acme")
	if err != nil || me != "alice" {
		t.Fatalf("Me after migration = %q, %v", me, err)
	}
}

func TestMigrateV1AddsMe(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	v1 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE INDEX links_blocker ON links(source,blocker_id);
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (1);`
	if _, err := db.Exec(v1); err != nil {
		t.Fatalf("v1 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	s := openAt(t, path)
	ctx := context.Background()
	seen := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	if err := s.SetSyncState(ctx, "jira:acme", seen, seen); err != nil {
		t.Fatalf("SetSyncState: %v", err)
	}
	if err := s.SetMe(ctx, "jira:acme", "alice"); err != nil {
		t.Fatalf("SetMe on migrated db: %v", err)
	}
	me, err := s.Me(ctx, "jira:acme")
	if err != nil || me != "alice" {
		t.Fatalf("Me = %q, %v", me, err)
	}
}

func TestClaims(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	startedAt, err := s.LastStartedAt(ctx)
	if err != nil {
		t.Fatalf("LastStartedAt empty: %v", err)
	}
	if !startedAt.IsZero() {
		t.Fatalf("LastStartedAt empty = %s, want zero", startedAt)
	}

	base := time.Date(2026, 10, 31, 12, 0, 0, 123456789, time.UTC)
	rows := []store.Claim{
		claimAt("PRJ-1", base, store.Claimed),
		claimAt("PRJ-2", base.Add(time.Minute), store.Started),
		claimAt("PRJ-3", base.Add(2*time.Minute), store.HandedOff),
		claimAt("PRJ-4", base.Add(3*time.Minute), store.Done),
		claimAt("PRJ-5", base.Add(4*time.Minute), store.Released),
		claimAt("PRJ-6", base.Add(5*time.Minute), store.Failed),
	}
	for _, c := range rows {
		if err := s.InsertClaim(ctx, c); err != nil {
			t.Fatalf("InsertClaim %s: %v", c.ID, err)
		}
	}
	active, err := s.ActiveClaims(ctx)
	if err != nil {
		t.Fatalf("ActiveClaims: %v", err)
	}
	var gotIDs []string
	for _, c := range active {
		gotIDs = append(gotIDs, c.ID)
	}
	if !reflect.DeepEqual(gotIDs, []string{"PRJ-1", "PRJ-2", "PRJ-3"}) {
		t.Fatalf("ActiveClaims = %#v", gotIDs)
	}
	if !reflect.DeepEqual(active[0], rows[0]) {
		t.Fatalf("claim round trip = %#v\nwant %#v", active[0], rows[0])
	}

	latest, err := s.LatestClaim(ctx, "jira:acme", "PRJ-2")
	if err != nil {
		t.Fatalf("LatestClaim: %v", err)
	}
	if latest.State != store.Started || latest.ClaimedAt != rows[1].ClaimedAt {
		t.Fatalf("LatestClaim = %#v", latest)
	}
	if err := s.InsertClaim(ctx, claimAt("PRJ-2", base.Add(10*time.Minute), store.Claimed)); !errors.Is(err, store.ErrActiveClaim) {
		t.Fatalf("InsertClaim duplicate active = %v, want ErrActiveClaim", err)
	}
	newer := claimAt("PRJ-5", base.Add(10*time.Minute), store.Claimed)
	if err := s.InsertClaim(ctx, newer); err != nil {
		t.Fatalf("InsertClaim newer: %v", err)
	}
	latest, err = s.LatestClaim(ctx, "jira:acme", "PRJ-5")
	if err != nil {
		t.Fatalf("LatestClaim newer: %v", err)
	}
	if latest.ClaimedAt != newer.ClaimedAt || latest.State != store.Claimed {
		t.Fatalf("LatestClaim = %#v, want the newer claimed row", latest)
	}
	if _, err := s.LatestClaim(ctx, "jira:acme", "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("missing claim error = %v", err)
	}

	before := time.Now()
	if err := s.UpdateClaim(ctx, "jira:acme", "PRJ-1", base, store.Started, "tab up", "tab-1", "ws-1"); err != nil {
		t.Fatalf("UpdateClaim: %v", err)
	}
	updated, err := s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim updated: %v", err)
	}
	if updated.State != store.Started || updated.Detail != "tab up" || updated.TabID != "tab-1" || updated.WorkspaceID != "ws-1" {
		t.Fatalf("updated claim = %#v", updated)
	}
	if updated.ClaimedAt != base {
		t.Fatalf("claimed_at changed to %s", updated.ClaimedAt)
	}
	if updated.UpdatedAt.Location() != time.UTC || updated.UpdatedAt.Before(before.Add(-time.Second)) || updated.UpdatedAt.After(time.Now().Add(time.Second)) {
		t.Fatalf("updated_at = %s", updated.UpdatedAt)
	}
	if err := s.UpdateClaim(ctx, "jira:acme", "missing", base, store.Failed, "", "", ""); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("update missing error = %v", err)
	}
	if err := s.InsertClaim(ctx, claimAt("PRJ-9", base, store.ClaimState("nope"))); err == nil || !strings.Contains(err.Error(), `found "nope"`) {
		t.Fatalf("bad state error = %v", err)
	}
	releasedLater := claimAt("PRJ-7", base.Add(20*time.Minute), store.Released)
	if err := s.InsertClaim(ctx, releasedLater); err != nil {
		t.Fatalf("InsertClaim released later: %v", err)
	}

	// claimed_at is the start-attempt time and does not move when the row
	// later changes state. A released claim does not count.
	gotStart, err := s.LastStartedAt(ctx)
	if err != nil {
		t.Fatalf("LastStartedAt: %v", err)
	}
	if gotStart != newer.ClaimedAt {
		t.Fatalf("LastStartedAt = %s, want %s", gotStart, newer.ClaimedAt)
	}
}

func TestRunsNewestFirst(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	early := store.Run{
		StartedAt:  time.Date(2026, 10, 31, 1, 0, 0, 1, time.UTC),
		Command:    "sync",
		Outcome:    "ok",
		DetailJSON: `{"summary":"early"}`,
	}
	late := store.Run{
		StartedAt:  time.Date(2026, 10, 31, 2, 0, 0, 999999999, time.UTC),
		Command:    "tick",
		Outcome:    "gated",
		DetailJSON: `{"summary":"late"}`,
	}
	if err := s.AppendRun(ctx, early); err != nil {
		t.Fatalf("AppendRun early: %v", err)
	}
	if err := s.AppendRun(ctx, late); err != nil {
		t.Fatalf("AppendRun late: %v", err)
	}
	got, err := s.Runs(ctx, 1)
	if err != nil {
		t.Fatalf("Runs: %v", err)
	}
	if len(got) != 1 || got[0] != late {
		t.Fatalf("Runs(1) = %#v, want the newer run", got)
	}
	all, err := s.Runs(ctx, 10)
	if err != nil {
		t.Fatalf("Runs 10: %v", err)
	}
	if len(all) != 2 || all[0] != late || all[1] != early {
		t.Fatalf("Runs = %#v", all)
	}
	if all[0].StartedAt != late.StartedAt {
		t.Fatalf("started_at = %s, want %s", all[0].StartedAt, late.StartedAt)
	}
	if _, err := s.Runs(ctx, 0); err == nil || !strings.Contains(err.Error(), "expected limit >= 1, found 0") {
		t.Fatalf("Runs(0) error = %v", err)
	}
}

func TestLastRunAt(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	if _, err := s.LastRunAt(ctx, ""); err == nil || !strings.Contains(err.Error(), "found empty") {
		t.Fatalf("empty command error = %v", err)
	}
	got, err := s.LastRunAt(ctx, "tick")
	if err != nil || !got.IsZero() {
		t.Fatalf("LastRunAt empty = %s, %v", got, err)
	}
	early := time.Date(2026, 10, 31, 1, 0, 0, 0, time.UTC)
	late := time.Date(2026, 10, 31, 2, 0, 0, 0, time.UTC)
	if err := s.AppendRun(ctx, store.Run{StartedAt: early, Command: "tick", Outcome: "idle", DetailJSON: `{"summary":"early"}`}); err != nil {
		t.Fatal(err)
	}
	if err := s.AppendRun(ctx, store.Run{StartedAt: late, Command: "sync", Outcome: "ok", DetailJSON: `{"summary":"sync"}`}); err != nil {
		t.Fatal(err)
	}
	if err := s.AppendRun(ctx, store.Run{StartedAt: late, Command: "tick", Outcome: "gated", DetailJSON: `{"summary":"late"}`}); err != nil {
		t.Fatal(err)
	}
	got, err = s.LastRunAt(ctx, "tick")
	if err != nil {
		t.Fatal(err)
	}
	if !got.Equal(late) {
		t.Fatalf("LastRunAt = %s, want %s", got, late)
	}
}

func openStore(t *testing.T) *store.Store {
	t.Helper()
	return openAt(t, filepath.Join(t.TempDir(), "factory", "factory.db"))
}

func openAt(t *testing.T, path string) *store.Store {
	t.Helper()
	s, err := store.Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open(%s): %v", path, err)
	}
	t.Cleanup(func() {
		if err := s.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	})
	return s
}

func fullItem(id string) source.Item {
	return source.Item{
		Source:   "jira:acme",
		ID:       id,
		Title:    "Fix the gate",
		URL:      "https://example.test/" + id,
		Type:     "Task",
		State:    "To Do",
		Done:     false,
		Parent:   "PRJ-9",
		Labels:   []string{"agent-ready", `quote "here"`},
		Assignee: "me",
		Priority: 0,
		BlockedBy: []source.BlockRef{
			{ID: "PRJ-2", LinkID: "100"},
			{ID: "PRJ-3", LinkID: "101"},
		},
		Created: time.Date(2026, 10, 31, 3, 4, 5, 123456789, time.UTC),
		Updated: time.Date(2026, 10, 31, 4, 5, 6, 1, time.UTC),
	}
}

func TestClaimPRsRoundTrip(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	at := time.Date(2026, 11, 5, 1, 2, 3, 0, time.UTC)
	row := claimAt("PRJ-1", at, store.Claimed)
	row.PRs = []store.ClaimPR{{
		Repo: "acme/widgets", Number: 12, URL: "https://github.com/acme/widgets/pull/12", Title: "elsewhere",
		Head: "feature/PRJ-1",
	}}
	if err := s.InsertClaim(ctx, row); err != nil {
		t.Fatalf("InsertClaim: %v", err)
	}
	got, err := s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim: %v", err)
	}
	if !reflect.DeepEqual(got.PRs, row.PRs) {
		t.Fatalf("prs = %#v, want %#v", got.PRs, row.PRs)
	}
	filledAt := at.Add(time.Minute)
	row.PRs = append(row.PRs, store.ClaimPR{
		Repo: "acme/widgets", Number: 13, URL: "https://github.com/acme/widgets/pull/13", Title: "",
	})
	if err := s.FillClaim(ctx, "jira:acme", "PRJ-1", at, "tab-9", "ws-9", row.PRs, filledAt); err != nil {
		t.Fatalf("FillClaim: %v", err)
	}
	got, err = s.LatestClaim(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim filled: %v", err)
	}
	if got.State != store.Claimed || got.TabID != "tab-9" || got.WorkspaceID != "ws-9" || !got.UpdatedAt.Equal(filledAt) {
		t.Fatalf("filled claim = %#v", got)
	}
	if !reflect.DeepEqual(got.PRs, row.PRs) {
		t.Fatalf("filled prs = %#v", got.PRs)
	}
	bad := claimAt("PRJ-2", at, store.Claimed)
	bad.PRs = []store.ClaimPR{{Repo: "acme/widgets", Number: 0, URL: "https://github.com/acme/widgets/pull/0"}}
	if err := s.InsertClaim(ctx, bad); err == nil || !strings.Contains(err.Error(), "expected owner/repo#N") {
		t.Fatalf("bad pr error = %v", err)
	}
}

func TestMigrateV3AddsClaimPRs(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	v3 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE INDEX links_blocker ON links(source,blocker_id);
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT, last_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (3);
INSERT INTO claims(source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at)
  VALUES ('jira:acme', 'PRJ-1', '2026-10-31T12:00:00.000000000Z', 'laptop', '', '', 'claimed', '', '2026-10-31T12:00:00.000000000Z');`
	if _, err := db.Exec(v3); err != nil {
		t.Fatalf("v3 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	s := openAt(t, path)
	got, err := s.LatestClaim(context.Background(), "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim: %v", err)
	}
	if got.PRs != nil || got.State != store.Claimed {
		t.Fatalf("migrated claim = %#v", got)
	}
}

func TestMigrateV4AddsStashCount(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	v4 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE INDEX links_blocker ON links(source,blocker_id);
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT, last_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, prs_json TEXT NOT NULL DEFAULT '',
  PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (4);
INSERT INTO claims(source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at, prs_json)
  VALUES ('jira:acme', 'PRJ-1', '2026-10-31T12:00:00.000000000Z', 'laptop', '', '', 'claimed', '', '2026-10-31T12:00:00.000000000Z', '');`
	if _, err := db.Exec(v4); err != nil {
		t.Fatalf("v4 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	s := openAt(t, path)
	got, err := s.LatestClaim(context.Background(), "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("LatestClaim: %v", err)
	}
	if got.StashCount != nil {
		t.Fatalf("stash count = %v, want nil", got.StashCount)
	}
	n := 2
	row := claimAt("PRJ-2", time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC), store.Claimed)
	row.StashCount = &n
	if err := s.InsertClaim(context.Background(), row); err != nil {
		t.Fatal(err)
	}
	got, err = s.LatestClaim(context.Background(), "jira:acme", "PRJ-2")
	if err != nil {
		t.Fatal(err)
	}
	if got.StashCount == nil || *got.StashCount != 2 {
		t.Fatalf("stash count = %v", got.StashCount)
	}
}

func TestItemDueRoundTrip(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	item := fullItem("PRJ-1")
	// 2026-11-07 22:00 -0700 is 2026-11-08 05:00 UTC. The column stores that UTC date.
	due := time.Date(2026, 11, 7, 22, 0, 0, 0, time.FixedZone("PDT", -7*3600))
	item.Due = &due
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	got, err := s.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item: %v", err)
	}
	wantDue := time.Date(2026, 11, 8, 0, 0, 0, 0, time.UTC)
	if got.Due == nil || !got.Due.Equal(wantDue) || got.Due.Location() != time.UTC {
		t.Fatalf("Due = %v, want %s", got.Due, wantDue)
	}
	item.Due = nil
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("clear due: %v", err)
	}
	got, err = s.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item after clear: %v", err)
	}
	if got.Due != nil {
		t.Fatalf("Due after clear = %s, want nil", got.Due)
	}
}

func TestMigrateV5AddsDue(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	v5 := `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE INDEX links_blocker ON links(source,blocker_id);
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT, last_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, prs_json TEXT NOT NULL DEFAULT '',
  stash_count INTEGER, PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (5);
INSERT INTO items(source, id, title, url, type, state, done, parent, labels_json, assignee, priority, created, updated)
  VALUES ('jira:acme', 'PRJ-1', 'Old', 'https://example.test/PRJ-1', 'Task', 'To Do', 0, '', 'null', '', 2,
    '2026-10-01T00:00:00.000000000Z', '2026-10-01T00:00:00.000000000Z');`
	if _, err := db.Exec(v5); err != nil {
		t.Fatalf("v5 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	s := openAt(t, path)
	ctx := context.Background()
	got, err := s.Item(ctx, "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatalf("Item: %v", err)
	}
	if got.Due != nil || got.Title != "Old" {
		t.Fatalf("migrated item = %#v", got)
	}
}

func TestDueRejectsBadText(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	ctx := context.Background()
	s, err := store.Open(ctx, path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	item := fullItem("PRJ-1")
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open sqlite: %v", err)
	}
	if _, err := db.Exec(`UPDATE items SET due = 'nope'`); err != nil {
		t.Fatalf("update due: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close sqlite: %v", err)
	}
	s, err = store.Open(ctx, path)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() {
		if err := s.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	})
	_, err = s.Item(ctx, "jira:acme", "PRJ-1")
	if err == nil || !strings.Contains(err.Error(), `expected YYYY-MM-DD, found "nope"`) {
		t.Fatalf("Item error = %v", err)
	}
}

func claimAt(id string, at time.Time, state store.ClaimState) store.Claim {
	return store.Claim{
		Source:      "jira:acme",
		ID:          id,
		ClaimedAt:   at,
		Machine:     "laptop",
		TabID:       "tab",
		WorkspaceID: "ws",
		State:       state,
		Detail:      "detail",
		UpdatedAt:   at.Add(time.Second),
	}
}

func ids(items []source.Item) []string {
	out := make([]string, len(items))
	for i := range items {
		out[i] = items[i].Source + "/" + items[i].ID
	}
	return out
}

func TestItemNotBeforeRoundTrip(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	item := fullItem("PRJ-1")
	date := time.Date(2026, 12, 1, 0, 0, 0, 0, time.UTC)
	item.NotBefore = &date
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	got, err := s.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item: %v", err)
	}
	if got.NotBefore == nil || !got.NotBefore.Equal(date) {
		t.Fatalf("NotBefore = %v, want %s", got.NotBefore, date)
	}
	item.NotBefore = nil
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("clear: %v", err)
	}
	if got, err = s.Item(ctx, item.Source, item.ID); err != nil || got.NotBefore != nil {
		t.Fatalf("after clear NotBefore = %v, err = %v", got.NotBefore, err)
	}
}

func TestItemTrustFieldsRoundTrip(t *testing.T) {
	s := openStore(t)
	ctx := context.Background()
	item := fullItem("acme/widgets#7")
	item.Source = "github:widgets"
	item.Author = "alice"
	item.AuthorAssociation = "NONE"
	item.Locked = true
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("UpsertItems: %v", err)
	}
	got, err := s.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item: %v", err)
	}
	if !reflect.DeepEqual(got, item) {
		t.Fatalf("Item = %#v, want %#v", got, item)
	}
	items, err := s.Items(ctx, item.Source)
	if err != nil {
		t.Fatalf("Items: %v", err)
	}
	if len(items) != 1 || !reflect.DeepEqual(items[0], item) {
		t.Fatalf("Items = %#v, want [%#v]", items, item)
	}
	item.Author = ""
	item.AuthorAssociation = ""
	item.Locked = false
	if err := s.UpsertItems(ctx, item.Source, []source.Item{item}); err != nil {
		t.Fatalf("UpsertItems update: %v", err)
	}
	got, err = s.Item(ctx, item.Source, item.ID)
	if err != nil {
		t.Fatalf("Item after update: %v", err)
	}
	if got.Author != "" || got.AuthorAssociation != "" || got.Locked {
		t.Fatalf("after clear = author:%q assoc:%q locked:%v", got.Author, got.AuthorAssociation, got.Locked)
	}
}

func TestMigrateV14AddsItemTrustColumns(t *testing.T) {
	path := filepath.Join(t.TempDir(), "factory.db")
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("Open sqlite: %v", err)
	}
	const v14 = `
CREATE TABLE items(source TEXT, id TEXT, title TEXT, url TEXT, type TEXT, state TEXT,
  done INTEGER, parent TEXT, labels_json TEXT, assignee TEXT, priority INTEGER,
  created TEXT, updated TEXT, due TEXT, not_before TEXT, PRIMARY KEY(source,id));
CREATE TABLE links(source TEXT, blocker_id TEXT, blocked_id TEXT, link_id TEXT,
  PRIMARY KEY(source,blocker_id,blocked_id));
CREATE TABLE sync_state(source TEXT PRIMARY KEY, last_updated_seen TEXT, last_full_sync TEXT, me TEXT, last_sync TEXT);
CREATE TABLE claims(source TEXT, id TEXT, claimed_at TEXT, machine TEXT, tab_id TEXT,
  workspace_id TEXT, state TEXT, detail TEXT, updated_at TEXT, prs_json TEXT NOT NULL DEFAULT '',
  stash_count INTEGER, lane TEXT NOT NULL DEFAULT '', agent_rule TEXT NOT NULL DEFAULT '',
  model TEXT NOT NULL DEFAULT '', spawner TEXT NOT NULL DEFAULT '', worker_handle TEXT NOT NULL DEFAULT '',
  worktree TEXT NOT NULL DEFAULT '', output_mode TEXT NOT NULL DEFAULT '',
  PRIMARY KEY(source,id,claimed_at));
CREATE TABLE runs(started_at TEXT, command TEXT, outcome TEXT, detail_json TEXT);
CREATE TABLE prs(repo TEXT NOT NULL, number INTEGER NOT NULL, source TEXT NOT NULL DEFAULT '',
  item_id TEXT NOT NULL DEFAULT '', opened_at TEXT, merged_at TEXT, closed_at TEXT,
  draft INTEGER NOT NULL DEFAULT 0, last_seen TEXT NOT NULL, PRIMARY KEY(repo,number));
CREATE TABLE finished(source TEXT NOT NULL, id TEXT NOT NULL, title TEXT NOT NULL DEFAULT '',
  parent TEXT NOT NULL DEFAULT '', labels_json TEXT NOT NULL DEFAULT 'null',
  finished_at TEXT NOT NULL, claimed_at TEXT, PRIMARY KEY(source,id));
CREATE TABLE backlog_snapshots(source TEXT NOT NULL, taken_at TEXT NOT NULL,
  ready INTEGER NOT NULL, blocked INTEGER NOT NULL, in_flight INTEGER NOT NULL,
  needs_human INTEGER NOT NULL, by_status_json TEXT NOT NULL, PRIMARY KEY(source,taken_at));
CREATE TABLE external_blockers(source TEXT NOT NULL, id TEXT NOT NULL, status TEXT NOT NULL,
  done INTEGER NOT NULL DEFAULT 0, state TEXT NOT NULL DEFAULT '', assignee TEXT NOT NULL DEFAULT '',
  title TEXT NOT NULL DEFAULT '', reason TEXT NOT NULL DEFAULT '', fetched_at TEXT NOT NULL,
  PRIMARY KEY(source,id));
CREATE TABLE schema_version(v INTEGER);
INSERT INTO schema_version(v) VALUES (14);
INSERT INTO items(source, id, title, url, type, state, done, parent, labels_json, assignee, priority, created, updated)
  VALUES ('github:widgets', 'acme/widgets#1', 'Legacy', 'https://github.com/acme/widgets/issues/1', 'Issue', 'OPEN', 0, '', 'null', '', 2,
    '2026-10-01T00:00:00Z', '2026-10-01T00:00:00Z');`
	if _, err := db.Exec(v14); err != nil {
		t.Fatalf("v14 schema: %v", err)
	}
	if err := db.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	s := openAt(t, path)
	got, err := s.Item(context.Background(), "github:widgets", "acme/widgets#1")
	if err != nil {
		t.Fatalf("Item: %v", err)
	}
	if got.Author != "" || got.AuthorAssociation != "" || got.Locked || got.Title != "Legacy" {
		t.Fatalf("migrated item = %#v", got)
	}
}
