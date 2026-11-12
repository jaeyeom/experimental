package outbox

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestEnqueueIsDurableAndOrdered(t *testing.T) {
	box := Open(t.TempDir())
	now := time.Date(2026, 11, 12, 1, 2, 3, 0, time.UTC)
	first := Entry{
		Kind: KindSend, Args: []string{"send", "ops/x", "one"}, Stdin: "body",
		Handle: "herdr:linux2/default/w1:p1", ClientID: "linux2:01FIRST",
		EnqueuedAt: now, TTL: "30m", State: StatePending, Target: "ops/x", Body: "one",
	}
	if _, err := box.Enqueue(first); err != nil {
		t.Fatal(err)
	}
	second := first
	second.ClientID = "linux2:02SECOND"
	second.EnqueuedAt = now.Add(time.Second)
	second.Body = "two"
	if _, err := box.Enqueue(second); err != nil {
		t.Fatal(err)
	}
	got, err := box.PendingHandle(first.Handle)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 2 || got[0].ClientID != first.ClientID || got[1].Body != "two" || got[0].Stdin != "body" {
		t.Fatalf("pending = %+v", got)
	}
	if _, err := os.Stat(filepath.Join(box.pendingDir(), "linux2_01FIRST.json")); err != nil {
		t.Fatal(err)
	}
	again, err := box.Enqueue(first)
	if err != nil || again.Body != "one" {
		t.Fatalf("re-enqueue = %+v, %v", again, err)
	}
	if pending, _ := box.Pending(); len(pending) != 2 {
		t.Fatalf("duplicate enqueue wrote a second file: %d", len(pending))
	}
}

func TestSecondFlusherSkips(t *testing.T) {
	box := Open(t.TempDir())
	unlock, ok, err := box.TryLock()
	if err != nil || !ok {
		t.Fatalf("first lock = %v, %v", ok, err)
	}
	defer unlock()
	_, ok, err = box.TryLock()
	if err != nil || ok {
		t.Fatalf("second lock = %v, %v", ok, err)
	}
}

func TestStaleAndStatus(t *testing.T) {
	now := time.Date(2026, 11, 12, 12, 0, 0, 0, time.UTC)
	fresh := Entry{State: StatePending, EnqueuedAt: now.Add(-time.Hour), ClientID: "m:1"}
	if Stale(fresh, now) {
		t.Fatal("an hour pending is not stale")
	}
	old := fresh
	old.EnqueuedAt = now.Add(-25 * time.Hour)
	if !Stale(old, now) {
		t.Fatal("25h pending should be flagged")
	}
	if got := StatusLine(Entry{State: StatePending}, now); got != "queued" {
		t.Fatalf("queued = %q", got)
	}
	retry := Entry{State: StatePending, Attempts: 2, LastError: "down", NextAt: now.Add(4 * time.Second)}
	if got := StatusLine(retry, now); got != "retrying (attempt 2, next in 4s, last error down)" {
		t.Fatalf("retrying = %q", got)
	}
	if got := StatusLine(Entry{State: StateDelivered, MsgID: "m-ab"}, now); got != "delivered → m-ab" {
		t.Fatalf("delivered = %q", got)
	}
	if got := StatusLine(Entry{State: StateExpired}, now); got != "expired" {
		t.Fatalf("expired = %q", got)
	}
}

func TestFinishMovesToDone(t *testing.T) {
	box := Open(t.TempDir())
	e := Entry{Kind: KindSend, ClientID: "linux2:01DONE", State: StatePending, EnqueuedAt: time.Now().UTC()}
	if _, err := box.Enqueue(e); err != nil {
		t.Fatal(err)
	}
	e.State = StateDelivered
	e.MsgID = "m-1"
	if err := box.Finish(e); err != nil {
		t.Fatal(err)
	}
	if pending, _ := box.Pending(); len(pending) != 0 {
		t.Fatalf("still pending: %+v", pending)
	}
	got, ok, err := box.Get(e.ClientID)
	if err != nil || !ok || got.MsgID != "m-1" {
		t.Fatalf("done = %+v, %v, %v", got, ok, err)
	}
}

func TestWriteFailureIsAnError(t *testing.T) {
	parent := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(parent, []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}
	box := Open(parent)
	_, err := box.Enqueue(Entry{ClientID: "linux2:01NOWRITE", State: StatePending})
	if err == nil {
		t.Fatal("enqueue into a file succeeded")
	}
	if errors.Is(err, os.ErrNotExist) {
		t.Fatal("expected a write error, not a missing file")
	}
}
