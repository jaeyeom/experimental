package web

import (
	"context"
	"crypto/ecdh"
	"encoding/json"
	"errors"
	"fmt"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
)

func ecdhP256() ecdh.Curve { return ecdh.P256() }

// maxPushBody bounds the notification body text.
const maxPushBody = 140

// pushDispatcher turns ledger changes into Web Push notifications. A
// push waits out the grace delay, then re-reads the item and is dropped
// if the user already handled it or is looking at the web app.
type pushDispatcher struct {
	s   *Server
	get func(id string) (decide.Item, error)

	mu      sync.Mutex
	ctx     context.Context
	msgs    map[string]int         // messages seen per item
	pending map[string]func() bool // grace timers
	wake    map[string]func() bool // snooze wake timers
	pushed  map[string]time.Time   // latest message time already pushed
}

func newPushDispatcher(s *Server) *pushDispatcher {
	d := &pushDispatcher{
		s: s, ctx: context.Background(),
		msgs: map[string]int{}, pending: map[string]func() bool{},
		wake: map[string]func() bool{}, pushed: map[string]time.Time{},
	}
	if s.Backend != nil {
		d.get = s.Backend.Get
	}
	return d
}

func (s *Server) after(d time.Duration, f func()) func() bool {
	if s.PushAfter != nil {
		return s.PushAfter(d, f)
	}
	return time.AfterFunc(d, f).Stop
}

// run follows the ledger on its own watcher until ctx ends. The items
// present at start are only recorded, not pushed.
func (d *pushDispatcher) run(ctx context.Context) {
	d.mu.Lock()
	d.ctx = ctx
	d.mu.Unlock()
	w := d.s.newWatcher()
	ev, err := w.Start(0)
	if err != nil {
		d.s.report(fmt.Errorf("web push: %w", err))
		return
	}
	d.handle(ev, true)
	w.Run(ctx, func(ev inboxrpc.WatchEvent) { d.handle(ev, false) })
	d.stop()
}

func (d *pushDispatcher) stop() {
	d.mu.Lock()
	defer d.mu.Unlock()
	for _, m := range []map[string]func() bool{d.pending, d.wake} {
		for id, stop := range m {
			stop()
			delete(m, id)
		}
	}
}

// handle observes the items one watch event carries. With seeding it
// records them without pushing.
func (d *pushDispatcher) handle(ev inboxrpc.WatchEvent, seeding bool) {
	items := ev.Items
	if !ev.Resync {
		items = nil
		for _, c := range ev.Changes {
			if !c.Deleted && c.Item != nil {
				items = append(items, *c.Item)
			}
		}
	}
	for _, it := range items {
		d.observe(it, seeding)
	}
}

func pushableClass(class string) bool {
	return slices.Contains(DefaultPushClasses, class)
}

func lastAuthor(it decide.Item) string {
	if n := len(it.Messages); n > 0 {
		return it.Messages[n-1].Author
	}
	return it.Owner
}

func (d *pushDispatcher) observe(it decide.Item, seeding bool) {
	d.mu.Lock()
	defer d.mu.Unlock()
	prev, seen := d.msgs[it.ID]
	d.msgs[it.ID] = len(it.Messages)
	d.armWake(it)
	if seeding || it.State != decide.StateOpen || it.Snoozed() || !pushableClass(it.EffectiveClass()) {
		return
	}
	author := lastAuthor(it)
	if !seen || (len(it.Messages) > prev && author != "human" && author != "system") {
		d.schedule(it.ID, false)
	}
}

// armWake (re)schedules the snooze wake-up of it, or cancels it when it
// is no longer snoozed.
func (d *pushDispatcher) armWake(it decide.Item) {
	if stop, ok := d.wake[it.ID]; ok {
		stop()
		delete(d.wake, it.ID)
	}
	if it.SnoozedUntil == nil {
		return
	}
	id := it.ID
	d.wake[id] = d.s.after(it.SnoozedUntil.Sub(d.s.now()), func() { d.fire(id, true) })
}

// schedule starts the grace timer of id unless one is already running.
func (d *pushDispatcher) schedule(id string, wake bool) {
	if _, ok := d.pending[id]; ok {
		return
	}
	d.pending[id] = d.s.after(d.s.Config.WebPushGrace(), func() { d.fire(id, wake) })
}

func latestMessageTime(it decide.Item) time.Time {
	if n := len(it.Messages); n > 0 {
		return it.Messages[n-1].Time
	}
	return it.CreatedAt
}

// shouldPush re-checks the item after the grace delay.
func (d *pushDispatcher) shouldPush(it decide.Item, wake bool) bool {
	latest := latestMessageTime(it)
	switch {
	case it.State != decide.StateOpen, it.Snoozed(), !pushableClass(it.EffectiveClass()):
		return false
	case it.ReadAt != nil && !it.ReadAt.Before(latest):
		return false
	case !wake && !latest.After(d.pushed[it.ID]):
		return false
	}
	return true
}

func (d *pushDispatcher) fire(id string, wake bool) {
	d.mu.Lock()
	delete(d.pending, id)
	if wake {
		delete(d.wake, id)
	}
	ctx := d.ctx
	d.mu.Unlock()
	if d.get == nil {
		return
	}
	it, err := d.get(id)
	if err != nil {
		return
	}
	d.mu.Lock()
	ok := d.shouldPush(it, wake)
	if ok {
		d.pushed[id] = latestMessageTime(it)
	}
	d.mu.Unlock()
	if !ok || d.s.HasActiveClient() {
		return
	}
	st, err := d.s.pushStore()
	if err != nil {
		d.s.report(err)
		return
	}
	payload, err := json.Marshal(pushPayload(it))
	if err != nil {
		return
	}
	now := d.s.now()
	for _, sub := range st.List() {
		if !subscriptionWants(sub, it.EffectiveClass(), now) {
			continue
		}
		if err := d.s.sendPush(ctx, st, sub, payload); err != nil && !isGone(err) {
			d.s.report(fmt.Errorf("web push: %w", err))
		}
	}
}

// subscriptionWants reports whether sub should be pushed for class now.
func subscriptionWants(sub Subscription, class string, now time.Time) bool {
	return sub.Enabled && class != decide.ClassNote && slices.Contains(sub.Classes, class) && !inQuietHours(sub, now)
}

// inQuietHours reports whether now, in sub's time zone, falls in
// [QuietStart, QuietEnd), wrapping past midnight when start > end.
func inQuietHours(sub Subscription, now time.Time) bool {
	if sub.QuietStart == "" || sub.QuietEnd == "" {
		return false
	}
	start, err1 := parseClock(sub.QuietStart)
	end, err2 := parseClock(sub.QuietEnd)
	if err1 != nil || err2 != nil || start == end {
		return false
	}
	loc := time.UTC
	if sub.TZ != "" {
		if l, err := time.LoadLocation(sub.TZ); err == nil {
			loc = l
		}
	}
	local := now.In(loc)
	m := local.Hour()*60 + local.Minute()
	if start < end {
		return m >= start && m < end
	}
	return m >= start || m < end
}

func pushPayload(it decide.Item) map[string]any {
	class := it.EffectiveClass()
	body := it.Question
	if n := len(it.Messages); n > 1 && it.Messages[n-1].Text != "" {
		body = it.Messages[n-1].Text
	}
	if r := []rune(body); len(r) > maxPushBody {
		body = string(r[:maxPushBody-1]) + "…"
	}
	url := "/#/item/" + it.ID
	return map[string]any{
		"itemId": it.ID,
		"class":  class,
		"owner":  it.Owner,
		"title":  fmt.Sprintf("[%s] %s · %s", it.ID, strings.ToUpper(class[:1])+class[1:], it.Owner),
		"body":   body,
		"url":    url,
		"tag":    it.ID,
		"actions": []map[string]string{
			{"action": "snooze-1h", "title": "Snooze 1h"},
		},
		// The service worker reads these from data.
		"data": map[string]string{"itemId": it.ID, "url": url},
	}
}

func isGone(err error) bool { return errors.Is(err, ErrSubscriptionGone) }
