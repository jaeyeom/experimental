package mailbox

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

const (
	sleeper = "herdr:local/s1/w1:p1"
	sender  = "herdr:local/s2/w1:p2"
)

// fakeWaker relaunches by marking the sleeper's pane idle again. With
// hold set it waits for release first. With err set it fails.
type fakeWaker struct {
	mu      sync.Mutex
	ad      *fake.Adapter
	recs    []hibernate.Record
	err     error
	hold    bool
	started chan struct{}
	release chan struct{}
}

func (w *fakeWaker) Wake(ctx context.Context, rec hibernate.Record) error {
	w.mu.Lock()
	w.recs = append(w.recs, rec)
	hold, err := w.hold, w.err
	w.mu.Unlock()
	if hold {
		w.started <- struct{}{}
		select {
		case <-w.release:
		case <-ctx.Done():
			return context.Cause(ctx) //nolint:wrapcheck // the fake passes the cancellation through
		}
	}
	if err != nil {
		return err
	}
	w.ad.Reach[sleeper] = adapter.StateIdle
	return nil
}

func (w *fakeWaker) calls() int {
	w.mu.Lock()
	defer w.mu.Unlock()
	return len(w.recs)
}

// fakeNow is a clock tests move by hand.
type fakeNow struct {
	mu sync.Mutex
	t  time.Time
}

func (c *fakeNow) now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.t
}

func (c *fakeNow) add(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.t = c.t.Add(d)
}

type wakeWorld struct {
	dir   string
	ad    *fake.Adapter
	reg   *registry.Registry
	box   *Mailbox
	waker *fakeWaker
	clock *fakeNow
}

// newWakeWorld has ops/x hibernated in w1:p1, whose pane runs a shell,
// and ops/y, an idle agent in w1:p2 that sends.
func newWakeWorld(t *testing.T) *wakeWorld {
	t.Helper()
	dir := t.TempDir()
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sleeper: adapter.StateNoAgent, sender: adapter.StateIdle},
		Result:      map[string]adapter.Result{sleeper: adapter.ResultAccepted, sender: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sleeper, "p2": sender},
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
	rec := hibernate.Record{Handle: sleeper, Pane: "w1:p1", Kind: "claude", Session: "s1", Cwd: "/src", Argv: []string{"claude"}}
	if _, err := reg.BeginHibernation(rec); err != nil {
		t.Fatal(err)
	}
	if _, err := reg.EndHibernation(sleeper, hibernate.StatusHibernated, ""); err != nil {
		t.Fatal(err)
	}
	box, err := Open(dir, reg, ads, lg, 0)
	if err != nil {
		t.Fatal(err)
	}
	clock := &fakeNow{t: time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)}
	box.now = clock.now
	waker := &fakeWaker{ad: ad, started: make(chan struct{}, 1), release: make(chan struct{})}
	box.SetWaker(waker)
	return &wakeWorld{dir: dir, ad: ad, reg: reg, box: box, waker: waker, clock: clock}
}

func (w *wakeWorld) send(t *testing.T, body string) Result {
	t.Helper()
	res, err := w.box.Send(context.Background(), []string{"HERDR_PANE_ID=p2"}, 0, "ops/x", body, Options{})
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func (w *wakeWorld) state(t *testing.T, id string) string {
	t.Helper()
	res, err := w.box.resultOf(id)
	if err != nil {
		t.Fatal(err)
	}
	return res.State
}

func (w *wakeWorld) events(t *testing.T, name string) []eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(w.dir).Read(eventlog.Filter{Event: name})
	if err != nil {
		t.Fatal(err)
	}
	var out []eventlog.Event
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev)
	}
	return out
}

func textsWith(texts []string, part string) int {
	n := 0
	for _, text := range texts {
		if strings.Contains(text, part) {
			n++
		}
	}
	return n
}

func TestSendToAHibernatedNameWakesItThenDelivers(t *testing.T) {
	w := newWakeWorld(t)
	res := w.send(t, "wake up")
	if res.State != "held: "+HoldWaking {
		t.Fatalf("state = %q, want held: %s", res.State, HoldWaking)
	}
	w.box.wakes.Wait()
	if got := w.state(t, res.ID); got != stateDelivered {
		t.Fatalf("state after the wake = %q", got)
	}
	if w.waker.calls() != 1 || w.waker.recs[0].Session != "s1" {
		t.Fatalf("wakes = %+v", w.waker.recs)
	}
	if textsWith(w.ad.Texts, "wake up") != 1 {
		t.Fatalf("pastes = %q", w.ad.Texts)
	}
	if _, ok := w.reg.Hibernation(sleeper); ok {
		t.Fatal("the record is still there after the wake")
	}
	rows, err := w.reg.List(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	for _, row := range rows {
		if row.Name == "ops/x" && row.Liveness != "alive" {
			t.Fatalf("ops/x liveness = %q, want alive", row.Liveness)
		}
	}
	if evs := w.events(t, eventlog.EventWake); len(evs) != 1 || evs[0].Name != "ops/x" {
		t.Fatalf("agent_wake events = %+v", evs)
	}
}

func TestMessagesSentDuringAWakeGoOutInOrderAfterOneRelaunch(t *testing.T) {
	w := newWakeWorld(t)
	w.waker.hold = true
	first := w.send(t, "first")
	<-w.waker.started
	second := w.send(t, "second")
	if got := w.state(t, second.ID); !strings.HasPrefix(got, "held:") {
		t.Fatalf("second state during the wake = %q", got)
	}
	close(w.waker.release)
	w.box.wakes.Wait()
	if w.waker.calls() != 1 {
		t.Fatalf("relaunched %d times, want once", w.waker.calls())
	}
	for _, id := range []string{first.ID, second.ID} {
		if got := w.state(t, id); got != stateDelivered {
			t.Fatalf("%s state = %q", id, got)
		}
	}
	if len(w.ad.Texts) != 2 || !strings.Contains(w.ad.Texts[0], "first") || !strings.Contains(w.ad.Texts[1], "second") {
		t.Fatalf("pastes = %q, want first then second", w.ad.Texts)
	}
}

func TestAFailedWakeKeepsTheMessageTellsTheSenderAndBacksOff(t *testing.T) {
	w := newWakeWorld(t)
	w.waker.err = errors.New("relaunched agent not ready within 1m0s: agent is working")
	res := w.send(t, "wake up")
	w.box.wakes.Wait()
	if got := w.state(t, res.ID); got != "held: "+HoldWakeFailed {
		t.Fatalf("state = %q, want held: %s", got, HoldWakeFailed)
	}
	rec, ok := w.reg.Hibernation(sleeper)
	if !ok || rec.Status != hibernate.StatusWakeFailed || rec.Failures != 1 || !strings.Contains(rec.Reason, "not ready") {
		t.Fatalf("record = %+v, %v", rec, ok)
	}
	want := "[agentbus] ops/x is hibernated and failed to wake: relaunched agent not ready within 1m0s: agent is working; your message is queued"
	if textsWith(w.ad.Texts, want) != 1 || textsWith(w.ad.Texts, "wake up") != 0 {
		t.Fatalf("pastes = %q, want only the notice to the sender", w.ad.Texts)
	}
	if evs := w.events(t, eventlog.EventWakeFailed); len(evs) != 1 || !strings.Contains(evs[0].Reason, "not ready") {
		t.Fatalf("agent_wake_failed events = %+v", evs)
	}
	status, err := w.box.Status(context.Background(), res.ID)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(status, "waiting: wake failed") {
		t.Fatalf("status = %q, want waiting: wake failed", status)
	}

	// Before the backoff ends, deliveries do not relaunch.
	w.clock.add(30 * time.Second)
	if err := w.box.flushHandle(context.Background(), sleeper); err != nil {
		t.Fatal(err)
	}
	w.box.wakes.Wait()
	if w.waker.calls() != 1 {
		t.Fatalf("relaunched %d times inside the backoff", w.waker.calls())
	}
	// After it, the next delivery attempt tries again, and the next
	// backoff is longer.
	w.clock.add(31 * time.Second)
	if err := w.box.flushHandle(context.Background(), sleeper); err != nil {
		t.Fatal(err)
	}
	w.box.wakes.Wait()
	if w.waker.calls() != 2 {
		t.Fatalf("relaunched %d times after the backoff, want 2", w.waker.calls())
	}
	rec, _ = w.reg.Hibernation(sleeper)
	if rec.Failures != 2 || rec.RetryAt.Sub(w.clock.now()) != 2*time.Minute {
		t.Fatalf("record after the second failure = %+v", rec)
	}
}

func TestAReusedPaneIsNotRelaunched(t *testing.T) {
	w := newWakeWorld(t)
	w.waker.err = &hibernate.PaneReusedError{Handle: sleeper, Command: "vim notes.md"}
	res := w.send(t, "wake up")
	w.box.wakes.Wait()
	if _, ok := w.reg.Hibernation(sleeper); ok {
		t.Fatal("the record of a reused pane was kept")
	}
	evs := w.events(t, eventlog.EventWakeSkipped)
	if len(evs) != 1 || !strings.Contains(evs[0].Reason, "vim notes.md") {
		t.Fatalf("agent_wake_skipped events = %+v", evs)
	}
	if textsWith(w.ad.Texts, "wake up") != 0 {
		t.Fatalf("pasted into a reused pane: %q", w.ad.Texts)
	}
	if got := w.state(t, res.ID); !strings.HasPrefix(got, "held:") {
		t.Fatalf("state = %q, want held", got)
	}
	if err := w.box.flushHandle(context.Background(), sleeper); err != nil {
		t.Fatal(err)
	}
	w.box.wakes.Wait()
	if w.waker.calls() != 1 {
		t.Fatalf("relaunched %d times, want one try", w.waker.calls())
	}
}

func TestWakeWithNoMessageRelaunchesAndClearsTheRecord(t *testing.T) {
	w := newWakeWorld(t)
	handle, err := w.box.Wake(context.Background(), "ops/x")
	if err != nil || handle != sleeper {
		t.Fatalf("Wake() = %q, %v", handle, err)
	}
	if w.waker.calls() != 1 {
		t.Fatalf("relaunched %d times", w.waker.calls())
	}
	if _, ok := w.reg.Hibernation(sleeper); ok {
		t.Fatal("the record is still there after the wake")
	}
	if _, err := w.box.Wake(context.Background(), "ops/x"); err == nil || !strings.Contains(err.Error(), "not hibernated") {
		t.Fatalf("waking an awake agent = %v", err)
	}
}

func TestWakeIgnoresTheBackoff(t *testing.T) {
	w := newWakeWorld(t)
	w.waker.err = errors.New("boom")
	if _, err := w.box.Wake(context.Background(), "ops/x"); err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("Wake() = %v, want boom", err)
	}
	w.waker.mu.Lock()
	w.waker.err = nil
	w.waker.mu.Unlock()
	if _, err := w.box.Wake(context.Background(), "ops/x"); err != nil {
		t.Fatal(err)
	}
	if w.waker.calls() != 2 {
		t.Fatalf("relaunched %d times, want 2", w.waker.calls())
	}
}
