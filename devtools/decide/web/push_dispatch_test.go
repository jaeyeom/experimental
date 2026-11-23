package web

import (
	"net/http"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
)

type fakeTimers struct {
	fns []func()
	ds  []time.Duration
}

func (f *fakeTimers) after(d time.Duration, fn func()) func() bool {
	i := len(f.fns)
	f.fns = append(f.fns, fn)
	f.ds = append(f.ds, d)
	return func() bool {
		alive := f.fns[i] != nil
		f.fns[i] = nil
		return alive
	}
}

func (f *fakeTimers) fireAll() {
	for i := 0; i < len(f.fns); i++ {
		if fn := f.fns[i]; fn != nil {
			f.fns[i] = nil
			fn()
		}
	}
}

type dispatchEnv struct {
	t      *testing.T
	s      *Server
	d      *pushDispatcher
	svc    *fakePushService
	timers *fakeTimers
	sub    testBrowser
	items  map[string]decide.Item
	ep     string
}

func newDispatchEnv(t *testing.T) *dispatchEnv {
	t.Helper()
	svc := newFakePushService(t)
	s, _ := pushServer(t)
	s.PushClient = svc.Client()
	e := &dispatchEnv{t: t, s: s, svc: svc, timers: &fakeTimers{}, sub: newTestBrowser(t), items: map[string]decide.Item{}}
	s.PushAfter = e.timers.after
	st, err := s.pushStore()
	if err != nil {
		t.Fatal(err)
	}
	e.ep = svc.URL + "/sub"
	if _, err := st.Upsert(e.ep, e.sub.keys(), testNow()); err != nil {
		t.Fatal(err)
	}
	e.d = newPushDispatcher(s)
	e.d.get = func(id string) (decide.Item, error) {
		it, ok := e.items[id]
		if !ok {
			return decide.Item{}, decide.ErrNotFound
		}
		return it, nil
	}
	return e
}

func (e *dispatchEnv) put(it decide.Item) {
	e.items[it.ID] = it
}

func (e *dispatchEnv) change(it decide.Item) {
	e.put(it)
	e.d.handle(inboxrpc.WatchEvent{Seq: 1, Changes: []decide.LedgerChange{{ID: it.ID, Item: &it}}}, false)
}

func approval(id string) decide.Item {
	t0 := testNow().Add(-time.Minute)
	return decide.Item{
		ID: id, Class: decide.ClassApproval, Owner: "slackwatch", State: decide.StateOpen,
		Question: "Post reply to #ops as written?",
		Messages: []decide.Message{{Author: "slackwatch", Time: t0, Text: "Post reply to #ops as written?"}},
	}
}

func TestPushGraceThenSend(t *testing.T) {
	e := newDispatchEnv(t)
	e.change(approval("A-870"))
	if e.svc.count() != 0 {
		t.Fatal("pushed before the grace delay")
	}
	if len(e.timers.ds) != 1 || e.timers.ds[0] != 5*time.Second {
		t.Fatalf("timers = %v", e.timers.ds)
	}
	e.timers.fireAll()
	if e.svc.count() != 1 {
		t.Fatalf("sent %d", e.svc.count())
	}
	m := e.sub.decrypt(t, e.svc.bodies[0])
	if m["itemId"] != "A-870" || m["class"] != "approval" || m["owner"] != "slackwatch" ||
		m["title"] != "[A-870] Approval · slackwatch" || m["body"] != "Post reply to #ops as written?" ||
		m["url"] != "/#/item/A-870" || m["tag"] != "A-870" {
		t.Fatalf("payload = %v", m)
	}
	acts := m["actions"].([]any)
	if len(acts) != 1 || acts[0].(map[string]any)["action"] != "snooze-1h" || acts[0].(map[string]any)["title"] != "Snooze 1h" {
		t.Fatalf("actions = %v", acts)
	}
	data := m["data"].(map[string]any)
	if data["itemId"] != "A-870" || data["url"] != "/#/item/A-870" {
		t.Fatalf("data = %v", data)
	}
}

func TestPushNeverNotes(t *testing.T) {
	e := newDispatchEnv(t)
	n := approval("N-1")
	n.Class = decide.ClassNote
	e.change(n)
	// A subscription that asks for notes still gets none.
	st, _ := e.s.pushStore()
	_, _, _ = st.Update(e.ep, func(s *Subscription) { s.Classes = append(s.Classes, "note") })
	e.change(n)
	e.timers.fireAll()
	if e.svc.count() != 0 || len(e.timers.fns) != 0 {
		t.Fatalf("note pushed: %d sent, %d timers", e.svc.count(), len(e.timers.fns))
	}
}

func TestPushSkipsItemHandledInGrace(t *testing.T) {
	for name, mutate := range map[string]func(*decide.Item){
		"read": func(it *decide.Item) {
			now := testNow()
			it.ReadAt = &now
		},
		"answered":  func(it *decide.Item) { it.State = decide.StateAnswered },
		"done":      func(it *decide.Item) { it.State = decide.StateDone },
		"withdrawn": func(it *decide.Item) { it.State = decide.StateWithdrawn },
		"snoozed": func(it *decide.Item) {
			u := testNow().Add(time.Hour)
			it.SnoozedUntil = &u
		},
	} {
		t.Run(name, func(t *testing.T) {
			e := newDispatchEnv(t)
			it := approval("A-1")
			e.change(it)
			mutate(&it)
			e.put(it)
			e.timers.fireAll()
			if e.svc.count() != 0 {
				t.Fatal("pushed an item handled during the grace window")
			}
		})
	}
}

func TestPushSuppressedByActivePresence(t *testing.T) {
	e := newDispatchEnv(t)
	e.s.state().mu.Lock()
	e.s.state().clients = map[string]time.Time{"tab": testNow().Add(time.Minute)}
	e.s.state().mu.Unlock()
	e.change(approval("A-1"))
	e.timers.fireAll()
	if e.svc.count() != 0 {
		t.Fatal("pushed while a web client is active")
	}
}

func TestPushQuietHoursAndPreferences(t *testing.T) {
	e := newDispatchEnv(t)
	st, _ := e.s.pushStore()
	// testNow is 03:04 UTC; 22:00-08:00 in UTC wraps midnight.
	_, _, _ = st.Update(e.ep, func(s *Subscription) { s.QuietStart, s.QuietEnd, s.TZ = "22:00", "08:00", "UTC" })
	e.change(approval("A-1"))
	e.timers.fireAll()
	if e.svc.count() != 0 {
		t.Fatal("pushed during quiet hours")
	}
	// Seoul is 12:04: outside quiet hours.
	_, _, _ = st.Update(e.ep, func(s *Subscription) { s.TZ = "Asia/Seoul" })
	e.change(approval("A-2"))
	e.timers.fireAll()
	if e.svc.count() != 1 {
		t.Fatalf("sent %d outside quiet hours", e.svc.count())
	}
	_, _, _ = st.Update(e.ep, func(s *Subscription) { s.Enabled = false })
	e.change(approval("A-3"))
	e.timers.fireAll()
	_, _, _ = st.Update(e.ep, func(s *Subscription) { s.Enabled, s.Classes = true, []string{"task"} })
	e.change(approval("A-4"))
	e.timers.fireAll()
	if e.svc.count() != 1 {
		t.Fatalf("disabled or filtered subscription was pushed: %d", e.svc.count())
	}
}

func TestInQuietHours(t *testing.T) {
	at := func(h, m int) time.Time { return time.Date(2026, 1, 2, h, m, 0, 0, time.UTC) }
	cases := []struct {
		start, end string
		now        time.Time
		want       bool
	}{
		{"22:00", "08:00", at(23, 0), true},
		{"22:00", "08:00", at(2, 0), true},
		{"22:00", "08:00", at(8, 0), false},
		{"22:00", "08:00", at(21, 59), false},
		{"22:00", "08:00", at(22, 0), true},
		{"09:00", "17:00", at(12, 0), true},
		{"09:00", "17:00", at(17, 0), false},
		{"", "", at(3, 0), false},
		{"10:00", "10:00", at(10, 0), false},
	}
	for _, c := range cases {
		sub := Subscription{QuietStart: c.start, QuietEnd: c.end, TZ: "UTC"}
		if got := inQuietHours(sub, c.now); got != c.want {
			t.Errorf("%s-%s at %s = %v", c.start, c.end, c.now.Format("15:04"), got)
		}
	}
}

func TestPushNewHumanMessageDoesNotPush(t *testing.T) {
	e := newDispatchEnv(t)
	it := approval("A-1")
	e.change(it)
	e.timers.fireAll()
	if e.svc.count() != 1 {
		t.Fatal("setup push missing")
	}
	// The user's own reply is not pushed back at them.
	it.Messages = append(it.Messages, decide.Message{Author: "human", Time: testNow(), Text: "ok"})
	e.change(it)
	e.timers.fireAll()
	if e.svc.count() != 1 {
		t.Fatal("human message pushed")
	}
	// A new agent message is.
	it.Messages = append(it.Messages, decide.Message{Author: "slackwatch", Time: testNow().Add(time.Second), Text: "more detail"})
	e.change(it)
	e.timers.fireAll()
	if e.svc.count() != 2 {
		t.Fatalf("agent message not pushed: %d", e.svc.count())
	}
	if m := e.sub.decrypt(t, e.svc.bodies[1]); m["body"] != "more detail" {
		t.Fatalf("payload = %v", m)
	}
}

func TestPushSnoozeWake(t *testing.T) {
	e := newDispatchEnv(t)
	it := approval("A-1")
	until := testNow().Add(time.Hour)
	it.SnoozedUntil = &until
	e.put(it)
	e.d.handle(inboxrpc.WatchEvent{Seq: 1, Resync: true, Items: []decide.Item{it}}, true)
	if len(e.timers.ds) != 1 || e.timers.ds[0] != time.Hour {
		t.Fatalf("wake timers = %v", e.timers.ds)
	}
	// The snooze ends: the ledger clears it by the time the timer fires.
	woken := it
	woken.SnoozedUntil = nil
	e.put(woken)
	e.timers.fireAll()
	if e.svc.count() != 1 {
		t.Fatalf("wake-up sent %d", e.svc.count())
	}
}

func TestPushUnsnoozeCancelsWake(t *testing.T) {
	e := newDispatchEnv(t)
	it := approval("A-1")
	until := testNow().Add(time.Hour)
	it.SnoozedUntil = &until
	e.d.handle(inboxrpc.WatchEvent{Seq: 1, Resync: true, Items: []decide.Item{it}}, true)
	it.SnoozedUntil = nil
	now := testNow()
	it.ReadAt = &now
	e.change(it)
	e.timers.fireAll()
	if e.svc.count() != 0 {
		t.Fatal("cancelled wake still pushed")
	}
}

func TestPushGonePrunesSubscription(t *testing.T) {
	e := newDispatchEnv(t)
	e.svc.setStatus(http.StatusGone)
	e.change(approval("A-1"))
	e.timers.fireAll()
	st, _ := e.s.pushStore()
	if len(st.List()) != 0 {
		t.Fatal("410 did not prune the subscription")
	}
}

func TestPushSeedingDoesNotNotify(t *testing.T) {
	e := newDispatchEnv(t)
	it := approval("A-1")
	e.d.handle(inboxrpc.WatchEvent{Seq: 1, Resync: true, Items: []decide.Item{it}}, true)
	e.timers.fireAll()
	if e.svc.count() != 0 {
		t.Fatal("startup snapshot pushed")
	}
}

func TestPushCoalescesPerItem(t *testing.T) {
	e := newDispatchEnv(t)
	it := approval("A-1")
	e.change(it)
	it.Messages = append(it.Messages, decide.Message{Author: "slackwatch", Time: testNow(), Text: "again"})
	e.change(it)
	e.timers.fireAll()
	if e.svc.count() != 1 {
		t.Fatalf("sent %d for one item in one grace window", e.svc.count())
	}
}
