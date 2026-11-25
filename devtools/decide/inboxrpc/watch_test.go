package inboxrpc

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func TestDetectConflict(t *testing.T) {
	base := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	for _, tc := range []struct {
		name         string
		op           string
		baseState    string
		baseAnswered bool
		cur          decide.Item
		want         string
	}{
		{"withdrawn", OpAnswer, decide.StateOpen, false, decide.Item{State: decide.StateWithdrawn}, "item was withdrawn"},
		{"done", OpReply, decide.StateOpen, false, decide.Item{State: decide.StateDone}, "item was marked done"},
		{"answer on done", OpAnswer, decide.StateOpen, false, decide.Item{State: decide.StateDone, Owner: "ada"}, "already closed by ada"},
		{"answer on done-unread", OpAnswer, decide.StateOpen, false, decide.Item{State: decide.StateDoneUnread, Owner: "ada"}, "already closed by ada"},
		{"already answered", OpAnswer, decide.StateOpen, false, decide.Item{State: decide.StateAnswered}, "item was already answered"},
		{"answered but base knew", OpAnswer, decide.StateOpen, true, decide.Item{State: decide.StateAnswered}, ""},
		{"new message", OpReply, decide.StateOpen, false, decide.Item{State: decide.StateOpen, Messages: []decide.Message{{Time: base}}}, "new message arrived"},
		{"none", OpReply, decide.StateOpen, false, decide.Item{State: decide.StateOpen}, ""},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := DetectConflict(tc.op, tc.baseState, 0, time.Time{}, tc.baseAnswered, tc.cur); got != tc.want {
				t.Fatalf("DetectConflict() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestWatcherSnapshotAndUpdates(t *testing.T) {
	h := newHub(t)
	first := h.create(t, &decide.Item{Question: "one"})
	spec := filepath.Join(t.TempDir(), "org.pkl")
	if err := os.WriteFile(spec, []byte("a"), 0o600); err != nil {
		t.Fatal(err)
	}
	w := &Watcher{
		Backend:   h.server.Backend,
		OrgConfig: func() (bool, string, error) { return true, spec, nil },
	}
	snap, err := w.Start(0)
	if err != nil {
		t.Fatal(err)
	}
	if !snap.Resync || len(snap.Items) != 1 || snap.Items[0].ID != first.ID || snap.Seq == 0 || snap.Status == nil || snap.OrgSpecModTime == nil {
		t.Fatalf("snapshot = %+v", snap)
	}
	if _, ok := w.Poll(); ok {
		t.Fatal("Poll() reported a change with nothing new")
	}

	second := h.create(t, &decide.Item{Question: "two"})
	ev, ok := w.Poll()
	if !ok || ev.Resync || len(ev.Changes) == 0 || ev.Seq <= snap.Seq {
		t.Fatalf("ledger poll = %+v, %v (second %s)", ev, ok, second.ID)
	}

	if _, _, err := h.server.Backend.PutDraft(decide.Draft{Key: decide.ReplyDraftKey(first.ID), Text: "hi"}, 0); err != nil {
		t.Fatal(err)
	}
	ev, ok = w.Poll()
	if !ok || len(ev.Drafts) != 1 || ev.DraftSeq == 0 {
		t.Fatalf("drafts poll = %+v, %v", ev, ok)
	}

	later := time.Now().Add(time.Hour)
	if err := os.Chtimes(spec, later, later); err != nil {
		t.Fatal(err)
	}
	ev, ok = w.Poll()
	if !ok || ev.OrgSpecModTime == nil || !ev.OrgSpecModTime.Equal(later) {
		t.Fatalf("org spec poll = %+v, %v", ev, ok)
	}
}

func TestWatcherStartSinceDelta(t *testing.T) {
	h := newHub(t)
	h.create(t, &decide.Item{Question: "one"})
	w := &Watcher{Backend: h.server.Backend}
	snap, err := w.Start(0)
	if err != nil {
		t.Fatal(err)
	}
	h.create(t, &decide.Item{Question: "two"})
	w2 := &Watcher{Backend: h.server.Backend}
	delta, err := w2.Start(snap.Seq)
	if err != nil {
		t.Fatal(err)
	}
	if delta.Resync || len(delta.Changes) == 0 {
		t.Fatalf("delta = %+v", delta)
	}
	gap, err := (&Watcher{Backend: h.server.Backend}).Start(snap.Seq + 1000)
	if err != nil {
		t.Fatal(err)
	}
	if !gap.Resync {
		t.Fatalf("gap = %+v, want resync", gap)
	}
}

func TestWatcherRunDeliversOnSignal(t *testing.T) {
	h := newHub(t)
	h.create(t, &decide.Item{Question: "zero"})
	w := &Watcher{Backend: h.server.Backend, Every: time.Hour}
	if _, err := w.Start(0); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	events := make(chan WatchEvent, 1)
	go w.Run(ctx, func(ev WatchEvent) { events <- ev })
	h.create(t, &decide.Item{Question: "one"})
	w.Signal()
	select {
	case ev := <-events:
		if len(ev.Changes) == 0 {
			t.Fatalf("event = %+v", ev)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no event after Signal")
	}
}

func TestWatcherNoChangePollSkipsStoreLoad(t *testing.T) {
	h := newHub(t)
	first := h.create(t, &decide.Item{Question: "one"})
	spec := filepath.Join(t.TempDir(), "org.pkl")
	if err := os.WriteFile(spec, []byte("a"), 0o600); err != nil {
		t.Fatal(err)
	}
	w := &Watcher{
		Backend:   h.server.Backend,
		OrgConfig: func() (bool, string, error) { return true, spec, nil },
	}
	if _, err := w.Start(0); err != nil {
		t.Fatal(err)
	}

	for range 10 {
		if _, ok := w.Poll(); ok {
			t.Fatal("Poll() reported a change with nothing new")
		}
	}
	if w.ledgerPolls != 0 || w.statusPolls != 0 || w.draftsPolls != 0 {
		t.Fatalf("no-change polls loaded files: ledger=%d status=%d drafts=%d", w.ledgerPolls, w.statusPolls, w.draftsPolls)
	}

	// Ledger change loads the ledger once, then subsequent idle polls skip it.
	h.create(t, &decide.Item{Question: "two"})
	ev, ok := w.Poll()
	if !ok || ev.Resync || len(ev.Changes) != 1 {
		t.Fatalf("ledger poll = %+v, %v", ev, ok)
	}
	if w.ledgerPolls != 1 {
		t.Fatalf("ledgerPolls = %d, want 1", w.ledgerPolls)
	}
	if _, ok := w.Poll(); ok {
		t.Fatal("second Poll() reported a change with nothing new")
	}
	if w.ledgerPolls != 1 {
		t.Fatalf("idle poll after change loaded ledger: ledgerPolls = %d", w.ledgerPolls)
	}

	// Status change loads status without touching the ledger.
	st, err := decide.OpenStatus(h.server.Backend.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := st.SetAgent("alpha", decide.StatusWorking, "busy"); err != nil {
		t.Fatal(err)
	}
	ev, ok = w.Poll()
	if !ok || ev.Status == nil || len(ev.Status.Agents) != 1 {
		t.Fatalf("status poll = %+v, %v", ev, ok)
	}
	if w.statusPolls != 1 || w.ledgerPolls != 1 {
		t.Fatalf("after status change: statusPolls=%d ledgerPolls=%d", w.statusPolls, w.ledgerPolls)
	}

	// Draft change loads drafts without touching the ledger.
	if _, _, err := h.server.Backend.PutDraft(decide.Draft{Key: decide.ReplyDraftKey(first.ID), Text: "draft"}, 0); err != nil {
		t.Fatal(err)
	}
	ev, ok = w.Poll()
	if !ok || len(ev.Drafts) != 1 || ev.DraftSeq == 0 {
		t.Fatalf("drafts poll = %+v, %v", ev, ok)
	}
	if w.draftsPolls != 1 || w.ledgerPolls != 1 {
		t.Fatalf("after draft change: draftsPolls=%d ledgerPolls=%d", w.draftsPolls, w.ledgerPolls)
	}

	// Org spec change does not touch the ledger.
	later := time.Now().Add(time.Hour)
	if err := os.Chtimes(spec, later, later); err != nil {
		t.Fatal(err)
	}
	ev, ok = w.Poll()
	if !ok || ev.OrgSpecModTime == nil || !ev.OrgSpecModTime.Equal(later) {
		t.Fatalf("org spec poll = %+v, %v", ev, ok)
	}
	if w.ledgerPolls != 1 {
		t.Fatalf("org spec change loaded ledger: ledgerPolls = %d", w.ledgerPolls)
	}
}

func TestWatcherResyncOnChangesReset(t *testing.T) {
	h := newHub(t)
	h.create(t, &decide.Item{Question: "one"})
	w := &Watcher{Backend: h.server.Backend}
	snap, err := w.Start(0)
	if err != nil {
		t.Fatal(err)
	}
	if !snap.Resync || len(snap.Items) != 1 {
		t.Fatalf("snapshot = %+v", snap)
	}
	// Removing changes.json resets seq to 0 < lastSeq, forcing a resync.
	if err := os.Remove(filepath.Join(h.server.Backend.Dir, "changes.json")); err != nil {
		t.Fatal(err)
	}
	ev, ok := w.Poll()
	if !ok || !ev.Resync || len(ev.Items) != 1 {
		t.Fatalf("resync poll = %+v, %v", ev, ok)
	}
}

func TestWatcherSnoozeWakeDetected(t *testing.T) {
	h := newHub(t)
	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	h.server.Backend.Now = func() time.Time { return now }
	led, err := decide.Open(h.server.Backend.Dir)
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(h.server.Backend.Now)
	item, err := led.Create(&decide.Item{Question: "snooze me", Kind: decide.KindRelay, Owner: "alpha"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Snooze(item.ID, now.Add(time.Hour)); err != nil {
		t.Fatal(err)
	}

	w := &Watcher{Backend: h.server.Backend}
	if _, err := w.Start(0); err != nil {
		t.Fatal(err)
	}
	if _, ok := w.Poll(); ok {
		t.Fatal("Poll() woke item before snooze expired")
	}
	if w.ledgerPolls != 0 {
		t.Fatalf("ledgerPolls before wake = %d, want 0", w.ledgerPolls)
	}

	now = now.Add(time.Hour + time.Second)
	ev, ok := w.Poll()
	if !ok || len(ev.Changes) != 1 || ev.Changes[0].Item == nil || ev.Changes[0].Item.Snoozed() {
		t.Fatalf("wake poll = %+v, %v", ev, ok)
	}
	if _, ok := w.Poll(); ok {
		t.Fatal("Poll() reported change after wake settled")
	}
	settled := w.ledgerPolls
	for range 5 {
		if _, ok := w.Poll(); ok {
			t.Fatal("Poll() reported change after wake settled")
		}
	}
	if w.ledgerPolls != settled {
		t.Fatalf("idle poll after wake loaded ledger: ledgerPolls = %d, want %d", w.ledgerPolls, settled)
	}
}

func TestWatcherRunWakesOnExternalWrite(t *testing.T) {
	h := newHub(t)
	h.create(t, &decide.Item{Question: "zero"})
	// Long fallback interval proves delivery comes from fsnotify, not the tick.
	w := &Watcher{Backend: h.server.Backend, Every: time.Hour}
	if _, err := w.Start(0); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	events := make(chan WatchEvent, 4)
	go w.Run(ctx, func(ev WatchEvent) { events <- ev })

	// Give Run a moment to register the fsnotify watch before writing.
	time.Sleep(50 * time.Millisecond)

	// External ledger write without calling w.Signal().
	h.create(t, &decide.Item{Question: "external"})
	select {
	case ev := <-events:
		if len(ev.Changes) == 0 {
			t.Fatalf("event = %+v", ev)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no event after external ledger write")
	}

	// External status write without calling w.Signal().
	st, err := decide.OpenStatus(h.server.Backend.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := st.SetAgent("beta", decide.StatusIdle, "waiting"); err != nil {
		t.Fatal(err)
	}
	select {
	case ev := <-events:
		if ev.Status == nil || len(ev.Status.Agents) != 1 {
			t.Fatalf("status event = %+v", ev)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("no event after external status write")
	}
}

func BenchmarkWatcherIdlePoll1000Items(b *testing.B) {
	dir := b.TempDir()
	items := make([]decide.Item, 1000)
	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	for i := range items {
		items[i] = decide.Item{
			ID:        decide.FormatID(decide.ClassDecision, i+1),
			Question:  "benchmark item",
			Kind:      decide.KindRelay,
			Owner:     "alpha",
			State:     decide.StateOpen,
			CreatedAt: now,
			Messages:  []decide.Message{{Author: "alpha", Time: now, Text: "benchmark item"}},
		}
	}
	raw, err := json.Marshal(struct {
		Next  int           `json:"next"`
		Items []decide.Item `json:"items"`
	}{Next: 1001, Items: items})
	if err != nil {
		b.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "ledger.json"), raw, 0o600); err != nil {
		b.Fatal(err)
	}
	led, err := decide.Open(dir)
	if err != nil {
		b.Fatal(err)
	}
	if _, err := led.Create(&decide.Item{Question: "one more", Kind: decide.KindRelay, Owner: "alpha"}); err != nil {
		b.Fatal(err)
	}
	w := &Watcher{Backend: &tui.LocalBackend{Dir: dir}}
	if _, err := w.Start(0); err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for b.Loop() {
		if _, ok := w.Poll(); ok {
			b.Fatal("unexpected change during idle benchmark")
		}
	}
	b.StopTimer()
	if w.ledgerPolls != 0 {
		b.Fatalf("idle benchmark loaded ledger %d times, want 0", w.ledgerPolls)
	}
}
