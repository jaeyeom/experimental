package decide

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func openTempDraftStore(t *testing.T) (*DraftStore, string) {
	t.Helper()
	dir := t.TempDir()
	ds, err := OpenDraftStore(dir)
	if err != nil {
		t.Fatalf("OpenDraftStore(%q) error = %v", dir, err)
	}
	now := time.Date(2026, 11, 21, 20, 0, 0, 0, time.UTC)
	ds.SetClock(func() time.Time { return now })
	return ds, dir
}

func TestDraftStoreCRUDAndPersistence(t *testing.T) {
	ds, dir := openTempDraftStore(t)

	seq, list, err := ds.List()
	if err != nil {
		t.Fatalf("List() error = %v", err)
	}
	if seq != 0 || len(list) != 0 {
		t.Fatalf("initial List() = (%d, %d items), want (0, 0)", seq, len(list))
	}

	// Create reply draft.
	saved, conflict, err := ds.Put(Draft{
		Key:       ReplyDraftKey("D-001"),
		Text:      "first draft",
		UpdatedBy: "dragon",
	}, 0)
	if err != nil {
		t.Fatalf("Put(create) error = %v", err)
	}
	if conflict {
		t.Fatal("Put(create) reported conflict = true")
	}
	if saved.Rev != 1 || saved.Kind != DraftKindReply || saved.ItemID != "D-001" {
		t.Fatalf("saved = %+v, want Rev=1 Kind=reply ItemID=D-001", saved)
	}

	// Create compose draft.
	comp, conflict, err := ds.Put(Draft{
		Key:       DraftKeyCompose,
		To:        "worker",
		Bus:       true,
		Forward:   "D-001",
		Text:      "status check",
		UpdatedBy: "dragon",
	}, 0)
	if err != nil {
		t.Fatalf("Put(compose) error = %v", err)
	}
	if conflict || comp.Rev != 1 || comp.Kind != DraftKindCompose {
		t.Fatalf("compose = %+v, conflict=%v", comp, conflict)
	}

	// Update reply draft with matching baseRev.
	updated, conflict, err := ds.Put(Draft{
		Key:       ReplyDraftKey("D-001"),
		Text:      "second draft",
		UpdatedBy: "dragon",
	}, 1)
	if err != nil {
		t.Fatalf("Put(update) error = %v", err)
	}
	if conflict || updated.Rev != 2 || updated.Text != "second draft" {
		t.Fatalf("updated = %+v, conflict=%v", updated, conflict)
	}

	// Reopen store and verify persistence and file permissions.
	info, err := os.Stat(filepath.Join(dir, draftsFile))
	if err != nil {
		t.Fatalf("Stat(drafts.json) error = %v", err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Fatalf("drafts.json perm = %o, want 0600", perm)
	}

	reopened, err := OpenDraftStore(dir)
	if err != nil {
		t.Fatalf("reopen OpenDraftStore() error = %v", err)
	}
	seq, list, err = reopened.List()
	if err != nil {
		t.Fatalf("reopened.List() error = %v", err)
	}
	if seq != 3 || len(list) != 2 {
		t.Fatalf("reopened.List() = (seq=%d, len=%d), want (3, 2)", seq, len(list))
	}

	got, ok, err := reopened.Get(ReplyDraftKey("D-001"))
	if err != nil || !ok || got.Text != "second draft" || got.Rev != 2 {
		t.Fatalf("reopened.Get() = (%+v, %v, %v)", got, ok, err)
	}

	// Delete compose draft.
	if err := reopened.Delete(DraftKeyCompose, ""); err != nil {
		t.Fatalf("Delete(compose) error = %v", err)
	}
	if _, ok, err := reopened.Get(DraftKeyCompose); err != nil || ok {
		t.Fatalf("Get(compose) after delete = (%v, %v), want false, nil", ok, err)
	}
}

func TestDraftStoreConflictAndNoOp(t *testing.T) {
	ds, _ := openTempDraftStore(t)

	_, _, err := ds.Put(Draft{
		Key:       ReplyDraftKey("D-001"),
		Text:      "v1",
		UpdatedBy: "dragon",
	}, 0)
	if err != nil {
		t.Fatalf("Put(v1) error = %v", err)
	}
	_, _, err = ds.Put(Draft{
		Key:       ReplyDraftKey("D-001"),
		Text:      "v2",
		UpdatedBy: "dragon",
	}, 1)
	if err != nil {
		t.Fatalf("Put(v2) error = %v", err)
	}

	// Identical content with stale baseRev is a no-op.
	seqBefore, _, err := ds.List()
	if err != nil {
		t.Fatalf("List() error = %v", err)
	}
	got, conflict, err := ds.Put(Draft{
		Key:       ReplyDraftKey("D-001"),
		Text:      "v2",
		UpdatedBy: "laptop",
	}, 1)
	if err != nil {
		t.Fatalf("Put(identical stale) error = %v", err)
	}
	if conflict {
		t.Fatal("Put(identical stale) reported conflict = true, want false")
	}
	if got.Rev != 2 || len(got.Conflicts) != 0 {
		t.Fatalf("got = %+v, want Rev=2 and 0 conflicts", got)
	}
	seqAfter, _, err := ds.List()
	if err != nil {
		t.Fatalf("List() error = %v", err)
	}
	if seqAfter != seqBefore {
		t.Fatalf("seq changed on no-op: %d -> %d", seqBefore, seqAfter)
	}

	// Different content with stale baseRev preserves existing draft and records conflict.
	got, conflict, err = ds.Put(Draft{
		Key:       ReplyDraftKey("D-001"),
		Text:      "v2-from-laptop",
		UpdatedBy: "laptop",
	}, 1)
	if err != nil {
		t.Fatalf("Put(stale conflict) error = %v", err)
	}
	if !conflict {
		t.Fatal("Put(stale conflict) reported conflict = false, want true")
	}
	if got.Text != "v2" || got.Rev != 3 || len(got.Conflicts) != 1 {
		t.Fatalf("got = %+v, want Text=v2 Rev=3 1 conflict", got)
	}
	c1 := got.Conflicts[0]
	if c1.Text != "v2-from-laptop" || c1.BaseRev != 1 || c1.UpdatedBy != "laptop" || c1.ID == "" {
		t.Fatalf("conflict[0] = %+v", c1)
	}

	// Record a second conflict, then delete only the first conflictID.
	got, conflict, err = ds.Put(Draft{
		Key:       ReplyDraftKey("D-001"),
		Text:      "v2-from-phone",
		UpdatedBy: "phone",
	}, 1)
	if err != nil || !conflict || len(got.Conflicts) != 2 {
		t.Fatalf("second conflict = (%+v, %v, %v)", got, conflict, err)
	}
	c2 := got.Conflicts[1]

	if err := ds.Delete(ReplyDraftKey("D-001"), c1.ID); err != nil {
		t.Fatalf("Delete(c1.ID) error = %v", err)
	}
	afterDel, ok, err := ds.Get(ReplyDraftKey("D-001"))
	if err != nil || !ok {
		t.Fatalf("Get() after Delete(c1) = (%v, %v)", ok, err)
	}
	if afterDel.Text != "v2" || afterDel.Rev != 5 || len(afterDel.Conflicts) != 1 || afterDel.Conflicts[0].ID != c2.ID {
		t.Fatalf("afterDel = %+v, want only %s remaining", afterDel, c2.ID)
	}
}

func TestDraftStoreLimits(t *testing.T) {
	ds, _ := openTempDraftStore(t)

	// 64 KiB limit: exact limit succeeds, 64 KiB + 1 fails.
	exact := strings.Repeat("a", MaxDraftTextBytes)
	if _, _, err := ds.Put(Draft{Key: ReplyDraftKey("D-001"), Text: exact}, 0); err != nil {
		t.Fatalf("Put(64KiB) error = %v", err)
	}
	tooBig := strings.Repeat("a", MaxDraftTextBytes+1)
	if _, _, err := ds.Put(Draft{Key: ReplyDraftKey("D-002"), Text: tooBig}, 0); !errors.Is(err, ErrDraftTooLarge) {
		t.Fatalf("Put(64KiB+1) error = %v, want ErrDraftTooLarge", err)
	}

	// Max 10 conflicts cap.
	for i := 0; i < MaxDraftConflicts; i++ {
		_, conflict, err := ds.Put(Draft{
			Key:  ReplyDraftKey("D-001"),
			Text: strings.Repeat("b", i+1),
		}, 0)
		if err != nil {
			t.Fatalf("Put(conflict %d) error = %v", i, err)
		}
		if !conflict {
			t.Fatalf("Put(conflict %d) conflict = false", i)
		}
	}
	if _, _, err := ds.Put(Draft{Key: ReplyDraftKey("D-001"), Text: "overflow"}, 0); !errors.Is(err, ErrTooManyConflicts) {
		t.Fatalf("Put(11th conflict) error = %v, want ErrTooManyConflicts", err)
	}
}

func TestLedgerTransitionsClearDrafts(t *testing.T) {
	led, dir := openTemp(t)
	ds, err := OpenDraftStore(dir)
	if err != nil {
		t.Fatalf("OpenDraftStore() error = %v", err)
	}

	seedDrafts := func(id string) {
		t.Helper()
		if _, _, err := ds.Put(Draft{Key: ReplyDraftKey(id), Text: "reply " + id}, 0); err != nil {
			t.Fatalf("Put(reply %s) error = %v", id, err)
		}
		if _, _, err := ds.Put(Draft{Key: AnswerDraftKey(id), Text: "answer " + id}, 0); err != nil {
			t.Fatalf("Put(answer %s) error = %v", id, err)
		}
	}
	assertCleared := func(id string) {
		t.Helper()
		if _, ok, err := ds.Get(ReplyDraftKey(id)); err != nil || ok {
			t.Fatalf("reply draft for %s still exists: ok=%v err=%v", id, ok, err)
		}
		if _, ok, err := ds.Get(AnswerDraftKey(id)); err != nil || ok {
			t.Fatalf("answer draft for %s still exists: ok=%v err=%v", id, ok, err)
		}
	}

	// Compose draft must survive all item transitions.
	if _, _, err := ds.Put(Draft{Key: DraftKeyCompose, Text: "keep me"}, 0); err != nil {
		t.Fatalf("Put(compose) error = %v", err)
	}

	// 1. Archive
	it1, err := led.Create(&Item{Question: "q1", Kind: KindRelay, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	seedDrafts(it1.ID)
	if _, err := led.Archive(it1.ID, time.Now().Add(time.Minute)); err != nil {
		t.Fatalf("Archive() error = %v", err)
	}
	assertCleared(it1.ID)

	// 2. Done / DoneVia
	it2, err := led.Create(&Item{Question: "q2", Kind: KindRelay, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	seedDrafts(it2.ID)
	if _, err := led.Done(it2.ID, "finished"); err != nil {
		t.Fatalf("Done() error = %v", err)
	}
	assertCleared(it2.ID)

	// 3. Withdraw
	it3, err := led.Create(&Item{Question: "q3", Kind: KindRelay, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	seedDrafts(it3.ID)
	if _, err := led.Withdraw(it3.ID, "no longer needed"); err != nil {
		t.Fatalf("Withdraw() error = %v", err)
	}
	assertCleared(it3.ID)

	// 4. Delete
	it4, err := led.Create(&Item{Question: "q4", Kind: KindRelay, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	seedDrafts(it4.ID)
	if err := led.Delete(it4.ID); err != nil {
		t.Fatalf("Delete() error = %v", err)
	}
	assertCleared(it4.ID)

	if got, ok, err := ds.Get(DraftKeyCompose); err != nil || !ok || got.Text != "keep me" {
		t.Fatalf("compose draft = (%+v, %v, %v), want preserved", got, ok, err)
	}
}

func TestDraftStorePrune(t *testing.T) {
	ds, _ := openTempDraftStore(t)

	for _, d := range []Draft{
		{Key: ReplyDraftKey("D-001"), Text: "active reply"},
		{Key: AnswerDraftKey("D-001"), Text: "active answer"},
		{Key: ForwardDraftKey("D-001"), Text: "active forward"},
		{Key: ReplyDraftKey("D-999"), Text: "orphan reply"},
		{Key: AnswerDraftKey("D-999"), Text: "orphan answer"},
		{Key: ForwardDraftKey("D-999"), Text: "orphan forward"},
		{Key: DraftKeyCompose, Text: "compose note"},
	} {
		if _, _, err := ds.Put(d, 0); err != nil {
			t.Fatalf("Put(%s) error = %v", d.Key, err)
		}
	}

	if err := ds.Prune(map[string]bool{"D-001": true}); err != nil {
		t.Fatalf("Prune() error = %v", err)
	}

	for _, keep := range []string{ReplyDraftKey("D-001"), AnswerDraftKey("D-001"), ForwardDraftKey("D-001"), DraftKeyCompose} {
		if _, ok, err := ds.Get(keep); err != nil || !ok {
			t.Fatalf("expected %s to remain after Prune, ok=%v err=%v", keep, ok, err)
		}
	}
	for _, drop := range []string{ReplyDraftKey("D-999"), AnswerDraftKey("D-999"), ForwardDraftKey("D-999")} {
		if _, ok, err := ds.Get(drop); err != nil || ok {
			t.Fatalf("expected %s to be pruned, ok=%v err=%v", drop, ok, err)
		}
	}
}
