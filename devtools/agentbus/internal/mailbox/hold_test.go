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

const (
	holdTo   = "herdr:local/s1/w1:p1"
	holdFrom = "herdr:local/s2/w1:p2"
)

type holdWorld struct {
	dir   string
	ad    *fake.Adapter
	reg   *registry.Registry
	box   *Mailbox
	clock *fakeNow
}

// newHoldWorld has ops/x idle in w1:p1 and ops/y, an idle sender in w1:p2.
func newHoldWorld(t *testing.T) *holdWorld {
	t.Helper()
	dir := t.TempDir()
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{holdTo: adapter.StateIdle, holdFrom: adapter.StateIdle},
		Result:      map[string]adapter.Result{holdTo: adapter.ResultAccepted, holdFrom: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": holdTo, "p2": holdFrom},
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
	clock := &fakeNow{t: time.Date(2026, 11, 13, 12, 0, 0, 0, time.UTC)}
	box.now = clock.now
	return &holdWorld{dir: dir, ad: ad, reg: reg, box: box, clock: clock}
}

func (w *holdWorld) send(t *testing.T, body string) Result {
	t.Helper()
	return w.sendFrom(t, []string{"HERDR_PANE_ID=p2"}, 0, body)
}

func (w *holdWorld) sendFrom(t *testing.T, env []string, pid int, body string) Result {
	t.Helper()
	res, err := w.box.Send(context.Background(), env, pid, "ops/x", body, Options{})
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func (w *holdWorld) flush(t *testing.T) {
	t.Helper()
	if err := w.box.flushHandle(context.Background(), holdTo); err != nil {
		t.Fatal(err)
	}
}

func (w *holdWorld) events(t *testing.T, name string) []eventlog.Event {
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

func noticeTexts(texts []string) []string {
	var out []string
	for _, text := range texts {
		if strings.Contains(text, "agentbus status ") {
			out = append(out, text)
		}
	}
	return out
}

func (w *holdWorld) holdScrolled(t *testing.T) {
	t.Helper()
	w.ad.Composer = func(handle string) (bool, string, error) {
		if handle == holdTo {
			return false, adapter.HoldScrolled, nil
		}
		return true, "", nil
	}
}

func TestScrolledHoldIsNotDelivered(t *testing.T) {
	w := newHoldWorld(t)
	w.holdScrolled(t)
	res := w.send(t, "behind the scroll")
	if res.State != "held: "+adapter.HoldScrolled || len(w.ad.Texts) != 0 {
		t.Fatalf("state %q texts %q", res.State, w.ad.Texts)
	}
}

func TestQueuedMessagesKeepTheWaitingReasonWhenTheHeadIsReleased(t *testing.T) {
	w := newHoldWorld(t)
	reason := adapter.HoldDraft
	w.ad.Composer = func(handle string) (bool, string, error) {
		if handle != holdTo || reason == "" {
			return true, "", nil
		}
		return false, reason, nil
	}
	head := w.send(t, "one")
	second := w.send(t, "two")
	third := w.send(t, "three")
	want := "held: waiting for an earlier message " + head.ID
	if second.State != want || third.State != want {
		t.Fatalf("queued %q / %q, want %q", second.State, third.State, want)
	}
	waiting := "waiting for an earlier message " + head.ID
	for _, ev := range w.events(t, eventlog.EventHeld) {
		if ev.MsgID != second.ID && ev.MsgID != third.ID {
			continue
		}
		if ev.Reason != waiting {
			t.Fatalf("held %s reason %q, want %q", ev.MsgID, ev.Reason, waiting)
		}
	}
	reason = ""
	w.flush(t)
	for _, id := range []string{head.ID, second.ID, third.ID} {
		res, err := w.box.resultOf(id)
		if err != nil {
			t.Fatal(err)
		}
		if res.State != stateDelivered {
			t.Fatalf("%s state %q", id, res.State)
		}
	}
	for _, ev := range w.events(t, eventlog.EventHeld) {
		if (ev.MsgID == second.ID || ev.MsgID == third.ID) && ev.Reason == adapter.HoldDraft {
			t.Fatalf("queued message logged as a draft: %+v", ev)
		}
	}
	if len(w.ad.Texts) != 3 {
		t.Fatalf("texts = %q", w.ad.Texts)
	}
}

func TestHoldNoticeOnceAfterTheThreshold(t *testing.T) {
	w := newHoldWorld(t)
	w.holdScrolled(t)
	minute := time.Minute
	w.box.SetHoldNoticeAfter(&minute)
	head := w.send(t, "held body")
	second := w.send(t, "queued body")
	if len(noticeTexts(w.ad.Texts)) != 0 {
		t.Fatalf("notice before the threshold: %q", w.ad.Texts)
	}
	w.clock.add(time.Minute)
	w.flush(t)
	notices := noticeTexts(w.ad.Texts)
	if len(notices) != 2 {
		t.Fatalf("notices = %q", notices)
	}
	headText := "agentbus: message " + head.ID + " to ops/x held: " + adapter.HoldScrolled + "\nagentbus status " + head.ID + "\n"
	tailText := "agentbus: message " + second.ID + " to ops/x held: waiting for an earlier message " + head.ID + "\nagentbus status " + second.ID + "\n"
	if notices[0] != headText || notices[1] != tailText {
		t.Fatalf("notices = %q", notices)
	}
	if textsWith(w.ad.Texts, "held body") != 0 || textsWith(w.ad.Texts, "queued body") != 0 {
		t.Fatalf("delivered while scrolled: %q", w.ad.Texts)
	}
	w.clock.add(time.Hour)
	w.flush(t)
	if again := noticeTexts(w.ad.Texts); len(again) != 2 {
		t.Fatalf("notices after another poll = %q", again)
	}
}

func TestHoldNoticeDefaultIsTenMinutes(t *testing.T) {
	w := newHoldWorld(t)
	w.holdScrolled(t)
	res := w.send(t, "wait")
	w.clock.add(defaultHoldNotice - time.Nanosecond)
	w.flush(t)
	if len(noticeTexts(w.ad.Texts)) != 0 {
		t.Fatalf("notice before 10m: %q", w.ad.Texts)
	}
	w.clock.add(time.Nanosecond)
	w.flush(t)
	notices := noticeTexts(w.ad.Texts)
	if len(notices) != 1 || !strings.Contains(notices[0], res.ID) || !strings.Contains(notices[0], "agentbus status "+res.ID) || !strings.Contains(notices[0], "to ops/x") || !strings.Contains(notices[0], adapter.HoldScrolled) {
		t.Fatalf("notice = %q", notices)
	}
}

func TestHoldNoticeSurvivesRestart(t *testing.T) {
	w := newHoldWorld(t)
	w.holdScrolled(t)
	minute := time.Minute
	w.box.SetHoldNoticeAfter(&minute)
	w.send(t, "once")
	w.clock.add(time.Minute)
	w.flush(t)
	if len(noticeTexts(w.ad.Texts)) != 1 {
		t.Fatalf("notices = %q", w.ad.Texts)
	}
	box, err := Open(w.dir, w.reg, []adapter.Adapter{w.ad}, eventlog.Open(w.dir), 0)
	if err != nil {
		t.Fatal(err)
	}
	box.now = w.clock.now
	box.SetHoldNoticeAfter(&minute)
	w.clock.add(time.Hour)
	if err := box.flushHandle(context.Background(), holdTo); err != nil {
		t.Fatal(err)
	}
	if len(noticeTexts(w.ad.Texts)) != 1 {
		t.Fatalf("notices after restart = %q", w.ad.Texts)
	}
}

func TestAnonymousHoldNoticeIsOnlyLogged(t *testing.T) {
	w := newHoldWorld(t)
	w.holdScrolled(t)
	minute := time.Minute
	w.box.SetHoldNoticeAfter(&minute)
	res := w.sendFrom(t, nil, 7, "from nowhere")
	if !strings.HasPrefix(res.State, "held:") {
		t.Fatalf("state %q", res.State)
	}
	w.clock.add(time.Minute)
	w.flush(t)
	if len(noticeTexts(w.ad.Texts)) != 0 || len(w.ad.Texts) != 0 {
		t.Fatalf("texts = %q", w.ad.Texts)
	}
	evs := w.events(t, eventlog.EventHoldNoticeSkipped)
	if len(evs) != 1 || evs[0].MsgID != res.ID || evs[0].Name != "ops/x" || evs[0].Detail != "anonymous sender" || evs[0].Reason != adapter.HoldScrolled {
		t.Fatalf("skipped = %+v", evs)
	}
	w.clock.add(time.Hour)
	w.flush(t)
	if again := w.events(t, eventlog.EventHoldNoticeSkipped); len(again) != 1 {
		t.Fatalf("skipped again = %+v", again)
	}
}

func TestHoldNoticeDisabledSendsNothing(t *testing.T) {
	w := newHoldWorld(t)
	w.holdScrolled(t)
	zero := time.Duration(0)
	w.box.SetHoldNoticeAfter(&zero)
	w.send(t, "named")
	w.sendFrom(t, nil, 9, "anon")
	w.clock.add(time.Hour)
	w.flush(t)
	if len(w.ad.Texts) != 0 {
		t.Fatalf("texts = %q", w.ad.Texts)
	}
	if evs := w.events(t, eventlog.EventHoldNoticeSkipped); len(evs) != 0 {
		t.Fatalf("skipped = %+v", evs)
	}
}
