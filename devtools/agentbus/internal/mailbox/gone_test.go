package mailbox

import (
	"context"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// goneTo is a raw handle no name holds and no adapter lists.
const goneTo = "herdr:local/s9/w1:p9"

func (w *holdWorld) sendTo(t *testing.T, target, body string) Result {
	t.Helper()
	res, err := w.box.Send(context.Background(), []string{"HERDR_PANE_ID=p2"}, 0, target, body, Options{})
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func (w *holdWorld) flushTo(t *testing.T, handle string) {
	t.Helper()
	if err := w.box.flushHandle(context.Background(), handle); err != nil {
		t.Fatal(err)
	}
}

func (w *holdWorld) state(t *testing.T, id string) string {
	t.Helper()
	res, err := w.box.resultOf(id)
	if err != nil {
		t.Fatal(err)
	}
	return res.State
}

func goneNotice(id string) string {
	return "agentbus: message " + id + " to " + goneTo + " not delivered: target pane is gone\n"
}

func textsEqual(texts []string, want string) int {
	n := 0
	for _, text := range texts {
		if text == want {
			n++
		}
	}
	return n
}

func TestRawHandleGoneExpiresAfterGoneAfter(t *testing.T) {
	w := newHoldWorld(t)
	res := w.sendTo(t, goneTo, "nobody home")
	if res.State != "held: gone" {
		t.Fatalf("state %q", res.State)
	}
	w.clock.add(defaultGoneAfter - time.Nanosecond)
	w.flushTo(t, goneTo)
	if got := w.state(t, res.ID); got != "held: gone" {
		t.Fatalf("state before gone_after = %q", got)
	}
	w.clock.add(time.Nanosecond)
	w.flushTo(t, goneTo)
	if got := w.state(t, res.ID); got != stateExpired {
		t.Fatalf("state after gone_after = %q", got)
	}
	if ids := w.box.queue[goneTo]; len(ids) != 0 {
		t.Fatalf("queue = %q", ids)
	}
	evs := w.events(t, eventlog.EventExpired)
	if len(evs) != 1 || evs[0].MsgID != res.ID || evs[0].Reason != ExpireGone {
		t.Fatalf("expired = %+v", evs)
	}
	w.clock.add(time.Hour)
	w.flushTo(t, goneTo)
	if n := textsEqual(w.ad.Texts, goneNotice(res.ID)); n != 1 {
		t.Fatalf("notices = %d, texts %q", n, w.ad.Texts)
	}
	report := w.box.Report(context.Background(), res.ID)
	if report.State != stateExpired || report.Reason != ExpireGone {
		t.Fatalf("report = %+v", report)
	}
}

func TestGoneAfterZeroExpiresAtTheFirstGoneCheck(t *testing.T) {
	w := newHoldWorld(t)
	zero := time.Duration(0)
	w.box.SetGoneAfter(&zero)
	res := w.sendTo(t, goneTo, "now or never")
	w.flushTo(t, goneTo)
	if got := w.state(t, res.ID); got != stateExpired {
		t.Fatalf("state = %q", got)
	}
	if n := textsEqual(w.ad.Texts, goneNotice(res.ID)); n != 1 {
		t.Fatalf("notices = %d, texts %q", n, w.ad.Texts)
	}
}

func TestRawHandleNotPositivelyGoneStaysHeld(t *testing.T) {
	for _, tc := range []struct {
		name  string
		setup func(w *holdWorld)
	}{
		{"unknown", func(w *holdWorld) { w.ad.Reach[goneTo] = adapter.StateUnknown }},
		{"unreachable", func(w *holdWorld) {
			w.ad.StateErr = map[string]error{goneTo: &remotecall.UnreachableError{Machine: "linux2", Cause: "host unreachable"}}
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			w := newHoldWorld(t)
			zero := time.Duration(0)
			w.box.SetGoneAfter(&zero)
			tc.setup(w)
			res := w.sendTo(t, goneTo, "wait for me")
			w.clock.add(24 * time.Hour)
			w.flushTo(t, goneTo)
			if got := w.state(t, res.ID); got == stateExpired || got == stateDelivered {
				t.Fatalf("state = %q", got)
			}
			if evs := w.events(t, eventlog.EventExpired); len(evs) != 0 {
				t.Fatalf("expired = %+v", evs)
			}
		})
	}
}

func TestRawHandleBackBeforeGoneAfterResetsTheClock(t *testing.T) {
	w := newHoldWorld(t)
	res := w.sendTo(t, goneTo, "come back")
	w.clock.add(45 * time.Minute)
	w.flushTo(t, goneTo)
	w.ad.Reach[goneTo] = adapter.StateBlocked
	w.flushTo(t, goneTo)
	delete(w.ad.Reach, goneTo)
	w.flushTo(t, goneTo)
	w.clock.add(defaultGoneAfter - time.Nanosecond)
	w.flushTo(t, goneTo)
	if got := w.state(t, res.ID); got != "held: gone" {
		t.Fatalf("state after the reset = %q", got)
	}
	w.ad.Reach[goneTo] = adapter.StateIdle
	w.ad.Result[goneTo] = adapter.ResultAccepted
	w.flushTo(t, goneTo)
	if got := w.state(t, res.ID); got != stateDelivered {
		t.Fatalf("state = %q", got)
	}
}

func TestRegisteredNameWhosePaneIsGoneStaysHeld(t *testing.T) {
	w := newHoldWorld(t)
	zero := time.Duration(0)
	w.box.SetGoneAfter(&zero)
	delete(w.ad.Reach, holdTo)
	res := w.send(t, "to a name")
	w.clock.add(24 * time.Hour)
	w.flush(t)
	if got := w.state(t, res.ID); got != "held: gone" {
		t.Fatalf("state = %q", got)
	}
}

func TestRepeatedHoldLogsOneHeldEvent(t *testing.T) {
	w := newHoldWorld(t)
	delete(w.ad.Reach, holdTo)
	res := w.send(t, "quiet hold")
	for range 5 {
		w.clock.add(time.Minute)
		w.flush(t)
	}
	held := func() []eventlog.Event {
		var out []eventlog.Event
		for _, ev := range w.events(t, eventlog.EventHeld) {
			if ev.MsgID == res.ID {
				out = append(out, ev)
			}
		}
		return out
	}
	if evs := held(); len(evs) != 1 || evs[0].Reason != string(adapter.StateGone) {
		t.Fatalf("held = %+v", evs)
	}
	w.ad.Reach[holdTo] = adapter.StateUnknown
	w.flush(t)
	w.flush(t)
	if evs := held(); len(evs) != 2 || evs[1].Reason != string(adapter.StateUnknown) {
		t.Fatalf("held after a reason change = %+v", evs)
	}
}
