package inboxrpc

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func waitUntil(t *testing.T, timeout time.Duration, cond func() bool, msg string) {
	t.Helper()
	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		if cond() {
			return
		}
		time.Sleep(10 * time.Millisecond)
	}
	t.Fatalf("timed out waiting for: %s", msg)
}

func TestOutboxWriteReturnsAtOnceWhenHubSlowAndDeliversLater(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{
		Question: "ship it?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})

	gate := make(chan struct{})
	dial := func() (*Conn, error) {
		reqR, reqW := io.Pipe()
		respR, respW := io.Pipe()
		srvR, srvW := io.Pipe()
		// Forward requests from client to server, blocking OpAnswer until gate closes.
		go func() {
			defer srvW.Close()
			sc := bufio.NewScanner(reqR)
			sc.Buffer(make([]byte, 64<<10), maxLine)
			for sc.Scan() {
				line := append([]byte(nil), sc.Bytes()...)
				var req Request
				if json.Unmarshal(line, &req) == nil && req.Op == OpAnswer {
					<-gate
				}
				_, _ = srvW.Write(append(line, '\n'))
			}
		}()
		h.wg.Add(1)
		go func() {
			defer h.wg.Done()
			err := h.server.Serve(context.Background(), srvR, respW)
			respW.CloseWithError(err)
		}()
		conn := &Conn{W: reqW, R: respR, Close: func() {
			_ = reqW.Close()
			_ = respR.Close()
			_ = srvR.Close()
		}}
		h.mu.Lock()
		h.conns = append(h.conns, conn)
		h.mu.Unlock()
		return conn, nil
	}

	c := &Client{
		Dial:      dial,
		Label:     "slow@hub",
		CacheDir:  t.TempDir(),
		UseOutbox: true,
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	start := time.Now()
	if _, err := c.Answer(it.ID, "y"); err != nil {
		t.Fatalf("Answer: %v", err)
	}
	if elapsed := time.Since(start); elapsed > 200*time.Millisecond {
		t.Fatalf("Answer blocked for %s, expected immediate return", elapsed)
	}
	if got := len(c.Outbox()); got != 1 {
		t.Fatalf("outbox len = %d, want 1", got)
	}

	// Unblock hub; outbox should deliver and drain.
	close(gate)
	waitUntil(t, 2*time.Second, func() bool {
		return len(c.Outbox()) == 0
	}, "outbox to drain after hub unblocks")

	got, err := h.server.Backend.Get(it.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "y" {
		t.Fatalf("hub item = %+v, want answered y", got)
	}
}

func TestOutboxPreservesOrderAcrossWritesAndItems(t *testing.T) {
	h := newHub(t)
	it1 := h.create(t, &decide.Item{
		Question: "first?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})
	it2 := h.create(t, &decide.Item{
		Question: "second?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})

	var mu sync.Mutex
	var order []string
	down := true
	dial := func() (*Conn, error) {
		mu.Lock()
		isDown := down
		mu.Unlock()
		if isDown {
			return nil, context.DeadlineExceeded
		}
		reqR, reqW := io.Pipe()
		respR, respW := io.Pipe()
		srvR, srvW := io.Pipe()
		go func() {
			defer srvW.Close()
			sc := bufio.NewScanner(reqR)
			sc.Buffer(make([]byte, 64<<10), maxLine)
			for sc.Scan() {
				line := append([]byte(nil), sc.Bytes()...)
				var req Request
				if json.Unmarshal(line, &req) == nil && (req.Op == OpReply || req.Op == OpAnswer) {
					mu.Lock()
					order = append(order, req.Op+":"+req.Item+":"+req.Text)
					mu.Unlock()
				}
				_, _ = srvW.Write(append(line, '\n'))
			}
		}()
		h.wg.Add(1)
		go func() {
			defer h.wg.Done()
			err := h.server.Serve(context.Background(), srvR, respW)
			respW.CloseWithError(err)
		}()
		conn := &Conn{W: reqW, R: respR, Close: func() {
			_ = reqW.Close()
			_ = respR.Close()
			_ = srvR.Close()
		}}
		h.mu.Lock()
		h.conns = append(h.conns, conn)
		h.mu.Unlock()
		return conn, nil
	}

	c := &Client{
		Dial:      dial,
		Label:     "order@hub",
		CacheDir:  t.TempDir(),
		UseOutbox: true,
	}
	// Enqueue while hub is down.
	if _, err := c.Reply(it1.ID, "r1", ""); err != nil {
		t.Fatal(err)
	}
	if _, err := c.Answer(it2.ID, "y"); err != nil {
		t.Fatal(err)
	}
	if _, err := c.Reply(it1.ID, "r2", ""); err != nil {
		t.Fatal(err)
	}
	if len(c.Outbox()) != 3 {
		t.Fatalf("outbox len = %d, want 3", len(c.Outbox()))
	}

	// Bring hub up and connect.
	mu.Lock()
	down = false
	mu.Unlock()
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	waitUntil(t, 2*time.Second, func() bool {
		return len(c.Outbox()) == 0
	}, "all ordered writes to drain")

	want := []string{
		"reply:" + it1.ID + ":r1",
		"answer:" + it2.ID + ":y",
		"reply:" + it1.ID + ":r2",
	}
	mu.Lock()
	defer mu.Unlock()
	if len(order) != len(want) {
		t.Fatalf("order = %v, want %v", order, want)
	}
	for i := range want {
		if order[i] != want[i] {
			t.Fatalf("order[%d] = %q, want %q (full: %v)", i, order[i], want[i], order)
		}
	}
}

func TestOutboxSurvivesRestartAndResumesInOrder(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{
		Question: "persist?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})
	cacheDir := t.TempDir()

	// First client queues while hub is down, then closes.
	c1 := &Client{
		Dial:      func() (*Conn, error) { return nil, context.DeadlineExceeded },
		Label:     "restart@hub",
		CacheDir:  cacheDir,
		UseOutbox: true,
	}
	if _, err := c1.Reply(it.ID, "first", ""); err != nil {
		t.Fatal(err)
	}
	if _, err := c1.Answer(it.ID, "y"); err != nil {
		t.Fatal(err)
	}
	c1.Close()

	// Second client restarts against the same cacheDir with hub up.
	c2 := &Client{
		Dial:      h.dial,
		Label:     "restart@hub",
		CacheDir:  cacheDir,
		UseOutbox: true,
	}
	if got := len(c2.Outbox()); got != 2 {
		t.Fatalf("restarted outbox len = %d, want 2", got)
	}
	if err := c2.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c2.Close()

	waitUntil(t, 2*time.Second, func() bool {
		return len(c2.Outbox()) == 0
	}, "restarted outbox to drain")

	got, err := h.server.Backend.Get(it.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || len(got.Messages) != 3 {
		t.Fatalf("hub item = %+v, want 3 messages and answered", got)
	}
	if got.Messages[1].Text != "first" || got.Messages[2].OptionKey != "y" {
		t.Fatalf("messages = %+v", got.Messages)
	}
}

func TestOutboxEditAndCancelPendingEntry(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{Question: "edit me?"})
	c := &Client{
		Dial:      func() (*Conn, error) { return nil, context.DeadlineExceeded },
		Label:     "edit@hub",
		CacheDir:  t.TempDir(),
		UseOutbox: true,
	}
	defer c.Close()

	if _, err := c.Reply(it.ID, "draft one", ""); err != nil {
		t.Fatal(err)
	}
	if _, err := c.Reply(it.ID, "draft two", ""); err != nil {
		t.Fatal(err)
	}
	entries := c.Outbox()
	if len(entries) != 2 {
		t.Fatalf("outbox len = %d, want 2", len(entries))
	}
	if err := c.EditOutbox(entries[0].ID, "edited one"); err != nil {
		t.Fatal(err)
	}
	if err := c.CancelOutbox(entries[1].ID); err != nil {
		t.Fatal(err)
	}
	entries = c.Outbox()
	if len(entries) != 1 || entries[0].Text != "edited one" {
		t.Fatalf("outbox after edit+cancel = %+v", entries)
	}
}

func TestOutboxConflictHoldAllCases(t *testing.T) {
	cases := []struct {
		name   string
		mutate func(t *testing.T, h *hub, id string)
		op     func(c *Client, id string) error
		want   string
	}{
		{
			name: "new message arrived",
			mutate: func(t *testing.T, h *hub, id string) {
				led, _ := decide.Open(h.dir)
				if _, err := led.Reply(id, "ada", "new update", nil, false); err != nil {
					t.Fatal(err)
				}
			},
			op:   func(c *Client, id string) error { _, err := c.Reply(id, "my reply", ""); return err },
			want: "new message arrived",
		},
		{
			name: "item was withdrawn",
			mutate: func(t *testing.T, h *hub, id string) {
				if _, err := h.server.Backend.Withdraw(id, "no longer needed"); err != nil {
					t.Fatal(err)
				}
			},
			op:   func(c *Client, id string) error { _, err := c.Reply(id, "my reply", ""); return err },
			want: "item was withdrawn",
		},
		{
			name: "item was marked done",
			mutate: func(t *testing.T, h *hub, id string) {
				if _, err := h.server.Backend.Done(id, "finished"); err != nil {
					t.Fatal(err)
				}
			},
			op:   func(c *Client, id string) error { _, err := c.Reply(id, "my reply", ""); return err },
			want: "item was marked done",
		},
		{
			name: "item was already answered",
			mutate: func(t *testing.T, h *hub, id string) {
				if _, err := h.server.Backend.Answer(id, "n"); err != nil {
					t.Fatal(err)
				}
			},
			op:   func(c *Client, id string) error { _, err := c.Answer(id, "y"); return err },
			want: "item was already answered",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			h := newHub(t)
			it := h.create(t, &decide.Item{
				Question: "conflict?",
				Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
			})
			var mu sync.Mutex
			down := false
			dial := func() (*Conn, error) {
				mu.Lock()
				isDown := down
				mu.Unlock()
				if isDown {
					return nil, context.DeadlineExceeded
				}
				return h.dial()
			}
			c := &Client{
				Dial:      dial,
				Label:     "conflict@hub",
				CacheDir:  t.TempDir(),
				UseOutbox: true,
			}
			if err := c.Connect(); err != nil {
				t.Fatal(err)
			}
			defer c.Close()

			// Drop connection so write stays in outbox while we mutate the hub.
			mu.Lock()
			down = true
			mu.Unlock()
			c.mu.Lock()
			if c.sess != nil {
				c.sess.close(context.DeadlineExceeded)
				c.sess = nil
			}
			c.mu.Unlock()

			if err := tc.op(c, it.ID); err != nil {
				t.Fatal(err)
			}
			tc.mutate(t, h, it.ID)

			// Reconnect and let outbox attempt delivery.
			mu.Lock()
			down = false
			mu.Unlock()
			if err := c.Connect(); err != nil {
				t.Fatal(err)
			}

			waitUntil(t, 2*time.Second, func() bool {
				entries := c.Outbox()
				return len(entries) == 1 && entries[0].Status == tui.OutboxHeld
			}, "entry to be held on conflict")

			got := c.Outbox()[0]
			if got.HoldReason != tc.want {
				t.Fatalf("HoldReason = %q, want %q", got.HoldReason, tc.want)
			}
		})
	}
}

func TestOutboxPermanentFailureMarksFailed(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{
		Question: "pick one?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})
	c := &Client{
		Dial:      h.dial,
		Label:     "fail@hub",
		CacheDir:  t.TempDir(),
		UseOutbox: true,
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	if _, err := c.Answer(it.ID, "invalid-option"); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, 2*time.Second, func() bool {
		entries := c.Outbox()
		return len(entries) == 1 && entries[0].Status == tui.OutboxFailed
	}, "invalid option to fail permanently")

	got := c.Outbox()[0]
	if got.Error == "" {
		t.Fatal("expected non-empty permanent error")
	}
}

func TestOutboxProvenanceRecordsHumanViaUIAndBothTimestamps(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{
		Question: "timestamps?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})
	writtenAt := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)
	receivedAt := writtenAt.Add(30 * time.Second)
	h.server.Backend.Now = func() time.Time { return receivedAt }

	c := &Client{
		Dial:      h.dial,
		Label:     "prov@hub",
		CacheDir:  t.TempDir(),
		UseOutbox: true,
		Now:       func() time.Time { return writtenAt },
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	if _, err := c.Answer(it.ID, "y"); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, 2*time.Second, func() bool {
		return len(c.Outbox()) == 0
	}, "answer to deliver")

	got, err := h.server.Backend.Get(it.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.AnsweredBy != "human" || got.Answer.Via != decide.ViaUI {
		t.Fatalf("Answer provenance = %+v", got.Answer)
	}
	if got.Answer.WrittenAt == nil || !got.Answer.WrittenAt.Equal(writtenAt) {
		t.Fatalf("Answer.WrittenAt = %v, want %v", got.Answer.WrittenAt, writtenAt)
	}
	if got.Answer.ReceivedAt == nil || !got.Answer.ReceivedAt.Equal(receivedAt) {
		t.Fatalf("Answer.ReceivedAt = %v, want %v", got.Answer.ReceivedAt, receivedAt)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.WrittenAt == nil || !last.WrittenAt.Equal(writtenAt) || last.ReceivedAt == nil || !last.ReceivedAt.Equal(receivedAt) {
		t.Fatalf("Message timestamps = written:%v received:%v", last.WrittenAt, last.ReceivedAt)
	}
}

func TestOutboxArchiveRespectsSeenGuard(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{Question: "fyi"})
	c := &Client{
		Dial:      h.dial,
		Label:     "archive@hub",
		CacheDir:  t.TempDir(),
		UseOutbox: true,
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	// Send an archive with a seen timestamp older than a newly arriving message.
	staleSeen := it.LastMessageAt().Add(-time.Second)
	if _, err := c.Archive(it.ID, staleSeen); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, 2*time.Second, func() bool {
		entries := c.Outbox()
		return len(entries) == 1 && entries[0].Status == tui.OutboxFailed
	}, "archive with stale seen to be rejected")

	got, err := h.server.Backend.Get(it.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateOpen {
		t.Fatalf("item state = %q, want open (seen guard should prevent archive)", got.State)
	}
}

func newDoneItemClient(t *testing.T) (*hub, *Client, decide.Item) {
	t.Helper()
	h := newHub(t)
	it := h.create(t, &decide.Item{
		Question: "pick one?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})
	if _, err := h.server.Backend.Done(it.ID, "finished"); err != nil {
		t.Fatal(err)
	}
	c := &Client{Dial: h.dial, Label: "refused@hub", CacheDir: t.TempDir(), UseOutbox: true}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { c.Close() })
	return h, c, it
}

func TestOutboxFailedEntryDoesNotBlockLaterEntries(t *testing.T) {
	_, c, it := newDoneItemClient(t)
	if _, err := c.Answer(it.ID, "y"); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, 2*time.Second, func() bool {
		e := c.Outbox()
		return len(e) == 1 && e[0].Status == tui.OutboxFailed
	}, "answer on a done item to fail")
	// done and archive on a done item are no-op successes, and a reply is
	// accepted, all behind the failed entry and in order.
	if _, err := c.Done(it.ID, "again"); err != nil {
		t.Fatal(err)
	}
	if _, err := c.Archive(it.ID, time.Now()); err != nil {
		t.Fatal(err)
	}
	if _, err := c.Reply(it.ID, "thanks", ""); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, 2*time.Second, func() bool {
		e := c.Outbox()
		return len(e) == 1 && e[0].Status == tui.OutboxFailed && e[0].Op == OpAnswer
	}, "later entries to drain past the failed one")
}

func TestOutboxStuckEntriesAreCleanedUpOnStart(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{
		Question: "pick one?",
		Options:  []decide.Option{{Key: "y", Label: "Yes", Recommended: true}, {Key: "n", Label: "No"}},
	})
	if _, err := h.server.Backend.Answer(it.ID, "y"); err != nil {
		t.Fatal(err)
	}
	if _, err := h.server.Backend.Done(it.ID, "finished"); err != nil {
		t.Fatal(err)
	}
	cache := t.TempDir()
	path, err := OutboxPath(cache, "stuck@hub")
	if err != nil {
		t.Fatal(err)
	}
	now := time.Now().UTC()
	data, err := json.Marshal(outboxData{Hub: "stuck@hub", NextID: 2, Entries: []tui.OutboxEntry{
		{ID: "OB-001", Op: OpAnswer, ItemID: it.ID, Text: "y", WrittenAt: now, Status: tui.OutboxFailed, Error: "decide item " + it.ID + " is done"},
		{ID: "OB-002", Op: OpDone, ItemID: it.ID, Text: "finished", WrittenAt: now, Status: tui.OutboxPending},
	}})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, data, 0o600); err != nil {
		t.Fatal(err)
	}
	c := &Client{Dial: h.dial, Label: "stuck@hub", CacheDir: cache, UseOutbox: true}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	waitUntil(t, 2*time.Second, func() bool {
		e := c.Outbox()
		return len(e) == 1 && e[0].ID == "OB-001" && e[0].Status == tui.OutboxFailed
	}, "pending done to drain behind the failed answer")
}

func TestIsDoneRefusalOnlyForDoneAndArchive(t *testing.T) {
	refusal := errors.New("decide item D-001 is done")
	if !isDoneRefusal(OpDone, refusal) || !isDoneRefusal(OpArchive, refusal) {
		t.Fatal("done or archive refused as done is not a no-op success")
	}
	if isDoneRefusal(OpAnswer, refusal) {
		t.Fatal("answer refused as done counted as a no-op success")
	}
	if isDoneRefusal(OpDone, errors.New("decide item D-001 is withdrawn")) {
		t.Fatal("done on a withdrawn item counted as a no-op success")
	}
}

func TestOutboxSendFailedEntryAsMessage(t *testing.T) {
	h, c, it := newDoneItemClient(t)
	if _, err := c.Answer(it.ID, "thanks anyway"); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, 2*time.Second, func() bool {
		e := c.Outbox()
		return len(e) == 1 && e[0].Status == tui.OutboxFailed
	}, "answer to fail")
	id := c.Outbox()[0].ID
	if err := c.SendOutboxAsMessage(id); err != nil {
		t.Fatal(err)
	}
	waitUntil(t, 2*time.Second, func() bool { return len(c.Outbox()) == 0 }, "message to deliver")
	got, err := h.server.Backend.Get(it.ID)
	if err != nil {
		t.Fatal(err)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Author != "human" || last.Text != "thanks anyway" {
		t.Fatalf("last message = %+v", last)
	}
}

func TestOutboxDiscardRemovesPendingAndFailed(t *testing.T) {
	c := &Client{Label: "discard@hub", CacheDir: t.TempDir(), UseOutbox: true}
	ob, err := c.ensureOutbox()
	if err != nil {
		t.Fatal(err)
	}
	a := ob.append(tui.OutboxEntry{Op: OpReply, ItemID: "D-001", Text: "a"})
	b := ob.append(tui.OutboxEntry{Op: OpReply, ItemID: "D-002", Text: "b"})
	ob.markFailed(b.ID, "decide item D-002 is done")
	for _, id := range []string{a.ID, b.ID} {
		if err := c.CancelOutbox(id); err != nil {
			t.Fatal(err)
		}
	}
	if got := c.Outbox(); len(got) != 0 {
		t.Fatalf("outbox = %+v, want empty", got)
	}
}
