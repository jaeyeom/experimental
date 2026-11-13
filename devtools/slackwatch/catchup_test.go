package slackwatch

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"time"
)

// fakeSlack serves canned messages. Replies are keyed by channel/thread
// and history by channel. errs holds errors to return, in order, before
// the canned messages for a key.
type fakeSlack struct {
	replies map[string][]Fetched
	history map[string][]Fetched
	search  []Fetched
	errs    map[string][]error
	calls   []string
}

func newFakeSlack() *fakeSlack {
	return &fakeSlack{replies: map[string][]Fetched{}, history: map[string][]Fetched{}, errs: map[string][]error{}}
}

func (f *fakeSlack) fail(key string) error {
	if errs := f.errs[key]; len(errs) > 0 {
		f.errs[key] = errs[1:]
		return errs[0]
	}
	return nil
}

func (f *fakeSlack) Replies(_ context.Context, channel, thread, oldest string) ([]Fetched, error) {
	key := channel + "/" + thread
	f.calls = append(f.calls, "replies "+key+" "+oldest)
	if err := f.fail(key); err != nil {
		return nil, err
	}
	return f.replies[key], nil
}

func (f *fakeSlack) History(_ context.Context, channel, oldest string) ([]Fetched, error) {
	f.calls = append(f.calls, "history "+channel+" "+oldest)
	if err := f.fail(channel); err != nil {
		return nil, err
	}
	return f.history[channel], nil
}

func (f *fakeSlack) Search(_ context.Context, query string) ([]Fetched, error) {
	f.calls = append(f.calls, "search "+query)
	if err := f.fail("search"); err != nil {
		return nil, err
	}
	return f.search, nil
}

type catchUpFixture struct {
	*eventFixture
	slack  *fakeSlack
	c      *CatchUp
	sleeps []time.Duration
}

func newCatchUpFixture(t *testing.T) *catchUpFixture {
	t.Helper()
	f := &catchUpFixture{eventFixture: newEventFixture(t), slack: newFakeSlack()}
	f.proc.Config.QuietExpiry = Duration{DefaultQuietExpiry}
	f.c = &CatchUp{
		Slack: f.slack,
		Proc:  f.proc,
		Sleep: func(_ context.Context, d time.Duration) error {
			f.sleeps = append(f.sleeps, d)
			return nil
		},
	}
	return f
}

func fetched(m Message, reactions ...Reaction) Fetched {
	return Fetched{Message: m, Reactions: reactions}
}

// tsAt is a Slack ts for t.
func tsAt(t time.Time) string {
	return strconv.FormatInt(t.Unix(), 10) + ".000100"
}

func TestCatchUpWatchStoresRepliesAfterCursor(t *testing.T) {
	f := newCatchUpFixture(t)
	cursor := "1700000005.000000"
	if err := f.store.setCursor(watchScope(f.thread.ID), cursor); err != nil {
		t.Fatal(err)
	}
	f.slack.replies["C1/"+threadTS] = []Fetched{
		fetched(threadReply("U9", "1700000009.000300", "after")),
		fetched(Message{Channel: "C1", TS: threadTS, ThreadTS: threadTS, User: selfID, Text: "the post"},
			Reaction{TS: threadTS, User: "U8", Name: "eyes"}),
		fetched(threadReply("U9", "1700000003.000300", "before")),
	}
	if err := f.c.CatchUpWatch(context.Background(), f.thread); err != nil {
		t.Fatal(err)
	}
	if want := []string{"replies C1/" + threadTS + " " + cursor}; !reflect.DeepEqual(f.slack.calls, want) {
		t.Errorf("calls = %q, want %q", f.slack.calls, want)
	}
	if got := f.events(); !reflect.DeepEqual(got, []string{EventReply}) || f.rec.notes[0].Text != "after" {
		t.Errorf("notes = %+v, want one reply for %q", f.rec.notes, "after")
	}
	if got := f.count(t, `SELECT COUNT(*) FROM replies`); got != 1 {
		t.Errorf("replies = %d, want 1", got)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM reactions WHERE ts = ? AND name = 'eyes'`, threadTS); got != 1 {
		t.Errorf("reactions on the watched post = %d, want 1", got)
	}
	if got, _ := f.store.cursor(watchScope(f.thread.ID), ""); got != "1700000009.000300" {
		t.Errorf("cursor = %q, want the latest fetched ts", got)
	}
}

func TestCatchUpWatchStartsAtSinceTS(t *testing.T) {
	f := newCatchUpFixture(t)
	if err := f.c.CatchUpWatch(context.Background(), f.thread); err != nil {
		t.Fatal(err)
	}
	if err := f.c.CatchUpWatch(context.Background(), f.dm); err != nil {
		t.Fatal(err)
	}
	want := []string{"replies C1/" + threadTS + " " + threadTS, "history D1 " + dmTS}
	if !reflect.DeepEqual(f.slack.calls, want) {
		t.Errorf("calls = %q, want %q", f.slack.calls, want)
	}
}

func TestCatchUpDMHistory(t *testing.T) {
	f := newCatchUpFixture(t)
	f.slack.history["D1"] = []Fetched{fetched(Message{Channel: "D1", TS: "1700000009.000400", User: "U9", Text: "sure"})}
	if err := f.c.CatchUpWatch(context.Background(), f.dm); err != nil {
		t.Fatal(err)
	}
	if got := f.events(); !reflect.DeepEqual(got, []string{EventReply}) || f.rec.notes[0].WatchID != f.dm.ID {
		t.Errorf("notes = %+v, want one reply on the dm watch", f.rec.notes)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM inbound`); got != 0 {
		t.Errorf("inbound = %d, want 0", got)
	}
}

func TestCatchUpTwiceNotifiesOnce(t *testing.T) {
	f := newCatchUpFixture(t)
	f.slack.replies["C1/"+threadTS] = []Fetched{fetched(threadReply("U9", "1700000009.000300", "hi"))}
	for range 2 {
		if err := f.c.Run(context.Background()); err != nil {
			t.Fatal(err)
		}
		// Forget the cursor so the same reply is fetched again.
		if _, err := f.store.DB().Exec(`DELETE FROM cursors`); err != nil {
			t.Fatal(err)
		}
	}
	if len(f.rec.notes) != 1 {
		t.Errorf("notes = %+v, want one", f.rec.notes)
	}
}

func TestEventThenCatchUpNotifiesOnce(t *testing.T) {
	f := newCatchUpFixture(t)
	reply := threadReply("U9", "1700000009.000300", "hi")
	if err := f.proc.HandleMessage(context.Background(), reply); err != nil {
		t.Fatal(err)
	}
	reply.ChannelType = "" // Fetched messages carry no channel type.
	f.slack.replies["C1/"+threadTS] = []Fetched{fetched(reply)}
	if err := f.c.CatchUpWatch(context.Background(), f.thread); err != nil {
		t.Fatal(err)
	}
	if len(f.rec.notes) != 1 {
		t.Errorf("notes = %+v, want one", f.rec.notes)
	}
}

func TestCatchUpHonorsRetryAfter(t *testing.T) {
	f := newCatchUpFixture(t)
	key := "C1/" + threadTS
	f.slack.errs[key] = []error{&RateLimitError{RetryAfter: 7 * time.Second}}
	f.slack.replies[key] = []Fetched{fetched(threadReply("U9", "1700000009.000300", "hi"))}
	if err := f.c.CatchUpWatch(context.Background(), f.thread); err != nil {
		t.Fatal(err)
	}
	if want := []time.Duration{7 * time.Second}; !reflect.DeepEqual(f.sleeps, want) {
		t.Errorf("sleeps = %v, want %v", f.sleeps, want)
	}
	if len(f.rec.notes) != 1 {
		t.Errorf("notes = %+v, want one after the retry", f.rec.notes)
	}
}

func TestCatchUpGivesUpAfterRepeatedRateLimits(t *testing.T) {
	f := newCatchUpFixture(t)
	key := "C1/" + threadTS
	for range maxRateLimitRetries + 1 {
		f.slack.errs[key] = append(f.slack.errs[key], &RateLimitError{RetryAfter: time.Second})
	}
	var rl *RateLimitError
	if err := f.c.CatchUpWatch(context.Background(), f.thread); !errors.As(err, &rl) {
		t.Errorf("err = %v, want a rate limit error", err)
	}
	if len(f.sleeps) != maxRateLimitRetries {
		t.Errorf("sleeps = %d, want %d", len(f.sleeps), maxRateLimitRetries)
	}
}

func TestRunContinuesPastAFailingWatch(t *testing.T) {
	f := newCatchUpFixture(t)
	bad, _, err := f.store.AddWatch(PostRef{Channel: "C0", TS: "1700000000.000050"}, WatchUpdate{Owner: strp("agent-3")}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	f.slack.errs["C0/"+bad.ThreadTS] = []error{errors.New("channel_not_found")}
	f.slack.replies["C1/"+threadTS] = []Fetched{fetched(threadReply("U9", "1700000009.000300", "hi"))}
	f.slack.history["D1"] = []Fetched{fetched(Message{Channel: "D1", TS: "1700000009.000400", User: "U9", Text: "sure"})}
	err = f.c.Run(context.Background())
	if err == nil || !strings.Contains(err.Error(), fmt.Sprintf("watch %d", bad.ID)) {
		t.Errorf("err = %v, want the failing watch named", err)
	}
	if got := f.events(); len(got) != 2 {
		t.Errorf("notes = %+v, want replies on both healthy watches", f.rec.notes)
	}
	if got, _ := f.store.cursor(watchScope(bad.ID), ""); got != "" {
		t.Errorf("failing watch cursor = %q, want unset", got)
	}
}

func TestRunSkipsLongClosedWatches(t *testing.T) {
	f := newCatchUpFixture(t)
	if _, err := f.store.CloseWatch(f.thread.ID, "", f.now.Add(-time.Hour)); err != nil {
		t.Fatal(err)
	}
	if _, err := f.store.CloseWatch(f.dm.ID, "", f.now.Add(-DefaultReopenWindow-time.Hour)); err != nil {
		t.Fatal(err)
	}
	f.slack.replies["C1/"+threadTS] = []Fetched{fetched(threadReply("U9", "1700000009.000300", "back"))}
	if err := f.c.Run(context.Background()); err != nil {
		t.Fatal(err)
	}
	for _, c := range f.slack.calls {
		if strings.HasPrefix(c, "history D1") {
			t.Errorf("caught up a watch closed past the reopen window: %q", c)
		}
	}
	if got := f.events(); !reflect.DeepEqual(got, []string{EventReopened}) {
		t.Errorf("events = %v, want a reopen of the recently closed watch", got)
	}
}

func TestRunSearchRegistersFooterPost(t *testing.T) {
	f := newCatchUpFixture(t)
	ts := tsAt(f.now.Add(-time.Hour))
	f.slack.search = []Fetched{
		fetched(Message{Channel: "C5", TS: ts, User: selfID, Text: "done, sent by agent"}),
		fetched(Message{Channel: "C6", TS: ts, User: "U9", Text: "hey <@" + selfID + ">"}),
		// Older than the search cursor: ignored.
		fetched(Message{Channel: "C7", TS: tsAt(f.now.Add(-48 * time.Hour)), User: "U9", Text: "old <@" + selfID + ">"}),
	}
	if err := f.c.Run(context.Background()); err != nil {
		t.Fatal(err)
	}
	w, err := f.store.FindWatch(PostRef{Channel: "C5", TS: ts})
	if err != nil {
		t.Fatal(err)
	}
	if w.State != StateUnowned || w.Source != SourceFooter {
		t.Errorf("footer watch = %+v, want unowned from footer", w)
	}
	if got := f.events(); !reflect.DeepEqual(got, []string{EventUnowned}) {
		t.Errorf("events = %v, want one unowned", got)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM inbound WHERE channel = 'C6' AND kind = 'mention'`); got != 1 {
		t.Errorf("mention inbound = %d, want 1", got)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM inbound WHERE channel = 'C7'`); got != 0 {
		t.Errorf("message before the search cursor was stored")
	}
	var queries []string
	for _, c := range f.slack.calls {
		if q, ok := strings.CutPrefix(c, "search "); ok {
			queries = append(queries, q)
		}
	}
	if len(queries) == 0 || !strings.Contains(strings.Join(queries, "|"), "from:me") {
		t.Errorf("search queries = %q, want one for the token owner's posts", queries)
	}
	if got, _ := f.store.cursor(searchScope, ""); got != ts {
		t.Errorf("search cursor = %q, want %q", got, ts)
	}
}

func TestCatchUpInboundReplayNotifiesOnce(t *testing.T) {
	f := newCatchUpFixture(t)
	f.proc.Config.Inbound = &InboundConfig{Target: "agent-in"}
	ts := tsAt(f.now.Add(-time.Hour))
	f.slack.search = []Fetched{fetched(Message{Channel: "D9", ChannelType: "im", TS: ts, User: "U9", Text: "ping"})}
	if err := f.c.Run(context.Background()); err != nil {
		t.Fatal(err)
	}
	if err := f.store.setCursor(searchScope, "1.000000"); err != nil {
		t.Fatal(err)
	}
	if err := f.c.Run(context.Background()); err != nil {
		t.Fatal(err)
	}
	if got := f.events(); !reflect.DeepEqual(got, []string{EventInbound}) {
		t.Errorf("events = %v, want one inbound", got)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM inbound WHERE channel = 'D9' AND notified = 1`); got != 1 {
		t.Errorf("notified inbound = %d, want 1", got)
	}
}

func TestExpireWatches(t *testing.T) {
	f := newCatchUpFixture(t)
	now := testNow()
	add := func(ts string, created time.Time, u WatchUpdate) Watch {
		t.Helper()
		u.Owner = strp("agent")
		w, _, err := f.store.AddWatch(PostRef{Channel: "C9", TS: ts}, u, created)
		if err != nil {
			t.Fatal(err)
		}
		return w
	}
	reply := func(w Watch, at time.Time, self bool) {
		t.Helper()
		if _, err := f.store.AddReply(Reply{Channel: "C9", TS: tsAt(at), WatchID: w.ID, ThreadTS: w.ThreadTS, IsSelf: self}); err != nil {
			t.Fatal(err)
		}
	}
	quiet := add("1.1", now.Add(-15*24*time.Hour), WatchUpdate{})
	recent := add("2.1", now.Add(-15*24*time.Hour), WatchUpdate{})
	reply(recent, now.Add(-2*24*time.Hour), false)
	selfOnly := add("3.1", now.Add(-15*24*time.Hour), WatchUpdate{})
	reply(selfOnly, now.Add(-24*time.Hour), true)
	// A self reply sets last_activity, which must not keep the watch open.
	if _, err := f.store.DB().Exec(`UPDATE watches SET last_activity = ? WHERE id = ?`,
		now.Add(-24*time.Hour).Unix(), selfOnly.ID); err != nil {
		t.Fatal(err)
	}
	pastUntil := add("4.1", now, WatchUpdate{Until: ptr(now.Add(-time.Hour))})
	futureUntil := add("5.1", now.Add(-30*24*time.Hour), WatchUpdate{Until: ptr(now.Add(time.Hour))})
	fresh := add("6.1", now.Add(-24*time.Hour), WatchUpdate{})

	n, err := f.store.ExpireWatches(now, DefaultQuietExpiry)
	if err != nil {
		t.Fatal(err)
	}
	if n != 3 {
		t.Errorf("closed %d, want 3", n)
	}
	for _, tc := range []struct {
		w     Watch
		state string
	}{
		{quiet, StateClosed},
		{recent, StateOpen},
		{selfOnly, StateClosed},
		{pastUntil, StateClosed},
		{futureUntil, StateOpen},
		{fresh, StateOpen},
	} {
		got := f.watch(t, tc.w.ID)
		if got.State != tc.state {
			t.Errorf("watch %s state = %s, want %s", tc.w.ThreadTS, got.State, tc.state)
		}
		if tc.state == StateClosed && (!got.ClosedAt.Equal(now) || got.CloseReason == "") {
			t.Errorf("watch %s closed_at = %v reason %q", tc.w.ThreadTS, got.ClosedAt, got.CloseReason)
		}
	}
	if len(f.rec.notes) != 0 {
		t.Errorf("expiry notified: %+v", f.rec.notes)
	}
}

func TestSweepExpiresWithoutNotify(t *testing.T) {
	f := newCatchUpFixture(t)
	f.now = testNow().Add(15 * 24 * time.Hour)
	if err := f.c.Sweep(context.Background()); err != nil {
		t.Fatal(err)
	}
	for _, w := range []Watch{f.thread, f.dm} {
		if got := f.watch(t, w.ID); got.State != StateClosed {
			t.Errorf("watch %d state = %s, want closed", w.ID, got.State)
		}
	}
	if len(f.rec.notes) != 0 {
		t.Errorf("sweep notified: %+v", f.rec.notes)
	}
}
