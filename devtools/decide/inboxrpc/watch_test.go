package inboxrpc

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
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
