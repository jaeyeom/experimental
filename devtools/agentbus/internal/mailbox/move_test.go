package mailbox

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

type moveWorld struct {
	dir       string
	ad        *fake.Adapter
	reg       *registry.Registry
	box       *Mailbox
	clock     *fakeNow
	oldHandle string
	newHandle string
	fromPane  string
}

func newMoveWorld(t *testing.T) *moveWorld {
	t.Helper()
	dir := t.TempDir()
	oldHandle := "herdr:local/s1/w1:p1"
	newHandle := "herdr:local/s3/w1:p3"
	fromHandle := "herdr:local/s2/w1:p2"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			oldHandle:  adapter.StateIdle,
			newHandle:  adapter.StateIdle,
			fromHandle: adapter.StateIdle,
		},
		Result: map[string]adapter.Result{
			oldHandle:  adapter.ResultAccepted,
			newHandle:  adapter.ResultAccepted,
			fromHandle: adapter.ResultAccepted,
		},
		PaneHandles: map[string]string{
			"p1": oldHandle,
			"p2": fromHandle,
			"p3": newHandle,
		},
	}
	ads := []adapter.Adapter{ad}
	lg := eventlog.Open(dir)
	reg, err := registry.Open(dir, ads, lg)
	if err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=p1"}, "ops/x", "reviews"); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=p2"}, "ops/y", "builds"); err != nil {
		t.Fatal(err)
	}
	box, err := Open(dir, reg, ads, lg, 0)
	if err != nil {
		t.Fatal(err)
	}
	clock := &fakeNow{t: time.Date(2026, 11, 22, 12, 0, 0, 0, time.UTC)}
	box.now = clock.now
	return &moveWorld{
		dir:       dir,
		ad:        ad,
		reg:       reg,
		box:       box,
		clock:     clock,
		oldHandle: oldHandle,
		newHandle: newHandle,
		fromPane:  "p2",
	}
}

func (w *moveWorld) send(t *testing.T, target, body string) Result {
	t.Helper()
	w.clock.add(time.Second)
	res, err := w.box.Send(context.Background(), []string{"HERDR_PANE_ID=" + w.fromPane}, 0, target, body, Options{})
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func (w *moveWorld) events(t *testing.T, name string) []eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(w.dir).Read(eventlog.Filter{Event: name})
	if err != nil {
		t.Fatal(err)
	}
	out := make([]eventlog.Event, 0, len(lines))
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev)
	}
	return out
}

func (w *moveWorld) deliveredTexts() []string {
	return w.ad.Texts
}

func TestClaimAfterGoneMovesQueuedMessagesToNewHolder(t *testing.T) {
	w := newMoveWorld(t)
	delete(w.ad.Reach, w.oldHandle)

	res := w.send(t, "ops/x", "follow the name")
	if res.State != "held: gone" {
		t.Fatalf("initial state = %q, want held: gone", res.State)
	}

	if _, err := w.reg.Register(context.Background(), []string{"HERDR_PANE_ID=p3"}, "ops/x", "new holder"); err != nil {
		t.Fatal(err)
	}

	status, err := w.box.Status(context.Background(), res.ID)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(status, "held: gone") {
		t.Fatalf("status after move still reports held: gone:\n%s", status)
	}
	if !strings.Contains(status, "state: delivered") {
		t.Fatalf("status after move = %q, want delivered", status)
	}
	if got := w.deliveredTexts(); len(got) != 1 || !strings.Contains(got[0], "follow the name") {
		t.Fatalf("deliveries = %q", got)
	}
}

func TestStatusAfterMoveReportsNewHolderHoldNotGone(t *testing.T) {
	w := newMoveWorld(t)
	delete(w.ad.Reach, w.oldHandle)
	res := w.send(t, "ops/x", "wait on new holder")
	if res.State != "held: gone" {
		t.Fatalf("initial state = %q, want held: gone", res.State)
	}

	// New holder is blocked (not gone), so status after move should show
	// held: blocked, never held: gone.
	w.ad.Reach[w.newHandle] = adapter.StateBlocked
	if _, err := w.reg.Register(context.Background(), []string{"HERDR_PANE_ID=p3"}, "ops/x", "blocked holder"); err != nil {
		t.Fatal(err)
	}

	status, err := w.box.Status(context.Background(), res.ID)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(status, "held: gone") {
		t.Fatalf("status after move still reports held: gone:\n%s", status)
	}
	if !strings.Contains(status, "state: held: blocked") {
		t.Fatalf("status after move = %q, want state: held: blocked", status)
	}
}

func TestReleaseToMovesQueuedMessagesFromLiveHolder(t *testing.T) {
	w := newMoveWorld(t)
	w.ad.Reach[w.oldHandle] = adapter.StateBlocked

	res := w.send(t, "ops/x", "handoff mail")
	if res.State != "held: blocked" {
		t.Fatalf("initial state = %q, want held: blocked", res.State)
	}

	if _, err := w.reg.Release(context.Background(), []string{"HERDR_PANE_ID=p1"}, "ops/x", w.newHandle); err != nil {
		t.Fatal(err)
	}

	if err := w.box.flushHandle(context.Background(), w.newHandle); err != nil {
		t.Fatal(err)
	}
	report := w.box.Report(context.Background(), res.ID)
	if report.State != stateDelivered {
		t.Fatalf("report = %+v, want delivered", report)
	}
	if got := w.deliveredTexts(); len(got) != 1 || !strings.Contains(got[0], "handoff mail") {
		t.Fatalf("deliveries = %q", got)
	}
}

func TestMovePreservesSendOrderAndMergesBySendTime(t *testing.T) {
	w := newMoveWorld(t)
	// Hold both handles while queueing messages at interleaved send times.
	w.ad.Reach[w.oldHandle] = adapter.StateBlocked
	w.ad.Reach[w.newHandle] = adapter.StateBlocked

	m1 := w.send(t, "ops/x", "first-to-name")
	m2 := w.send(t, w.newHandle, "second-to-new-handle")
	m3 := w.send(t, "ops/x", "third-to-name")
	m4 := w.send(t, w.newHandle, "fourth-to-new-handle")

	if _, err := w.reg.Release(context.Background(), []string{"HERDR_PANE_ID=p1"}, "ops/x", w.newHandle); err != nil {
		t.Fatal(err)
	}

	// Logs one move event for the 2 moved messages.
	evs := w.events(t, eventlog.EventMoved)
	if len(evs) != 1 {
		t.Fatalf("moved events = %+v, want 1", evs)
	}
	if evs[0].Name != "ops/x" ||
		!strings.Contains(evs[0].Detail, "2") ||
		!strings.Contains(evs[0].Detail, w.oldHandle) ||
		!strings.Contains(evs[0].Detail, w.newHandle) {
		t.Fatalf("moved event = %+v", evs[0])
	}

	w.ad.Reach[w.newHandle] = adapter.StateIdle
	if err := w.box.flushHandle(context.Background(), w.newHandle); err != nil {
		t.Fatal(err)
	}

	got := w.deliveredTexts()
	wantOrder := []string{m1.ID, m2.ID, m3.ID, m4.ID}
	if len(got) != len(wantOrder) {
		t.Fatalf("delivered %d messages (%q), want %d", len(got), got, len(wantOrder))
	}
	for i, id := range wantOrder {
		if !strings.Contains(got[i], id) {
			t.Fatalf("delivery[%d] = %q, want message %s", i, got[i], id)
		}
	}
}

func TestMoveLeavesRawHandleMessagesAloneToExpire(t *testing.T) {
	w := newMoveWorld(t)
	delete(w.ad.Reach, w.oldHandle)

	named := w.send(t, "ops/x", "named message")
	raw := w.send(t, w.oldHandle, "raw handle message")

	if _, err := w.reg.Register(context.Background(), []string{"HERDR_PANE_ID=p3"}, "ops/x", "replacement"); err != nil {
		t.Fatal(err)
	}

	if err := w.box.flushHandle(context.Background(), w.newHandle); err != nil {
		t.Fatal(err)
	}
	if got := w.box.Report(context.Background(), named.ID); got.State != stateDelivered {
		t.Fatalf("named report = %+v, want delivered", got)
	}

	// Only the named message moved; the raw-handle message stayed on oldHandle
	// and expires after gone_after.
	evs := w.events(t, eventlog.EventMoved)
	if len(evs) != 1 || !strings.Contains(evs[0].Detail, "1") {
		t.Fatalf("moved events = %+v, want count 1", evs)
	}

	if err := w.box.flushHandle(context.Background(), w.oldHandle); err != nil {
		t.Fatal(err)
	}
	if got := w.box.Report(context.Background(), raw.ID); got.State != "held" || got.HeldReason != HeldTargetNotReady {
		t.Fatalf("raw report before gone_after = %+v, want held/target not ready", got)
	}

	w.clock.add(defaultGoneAfter)
	if err := w.box.flushHandle(context.Background(), w.oldHandle); err != nil {
		t.Fatal(err)
	}
	if got := w.box.Report(context.Background(), raw.ID); got.State != stateExpired || got.Reason != ExpireGone {
		t.Fatalf("raw report = %+v, want expired/gone", got)
	}
	for _, text := range w.deliveredTexts() {
		if strings.Contains(text, "[agentbus "+raw.ID) {
			t.Fatalf("raw message %s was unexpectedly delivered: %q", raw.ID, text)
		}
	}
}
