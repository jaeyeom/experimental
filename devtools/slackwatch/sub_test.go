package slackwatch

import (
	"context"
	"reflect"
	"testing"
	"time"
)

func (f *eventFixture) sub(t *testing.T, s Sub) Sub {
	t.Helper()
	s, _, err := f.store.AddSub(s, testNow())
	if err != nil {
		t.Fatal(err)
	}
	return s
}

func (f *eventFixture) handle(t *testing.T, msgs ...Message) {
	t.Helper()
	for _, m := range msgs {
		if err := f.proc.HandleMessage(context.Background(), m); err != nil {
			t.Fatal(err)
		}
	}
}

func channelMsg(user, ts, threadTS, text string) Message {
	return Message{Channel: "C2", ChannelType: "channel", TS: ts, ThreadTS: threadTS, User: user, Text: text}
}

func TestAddSubUpsertsPerChannelAndOwner(t *testing.T) {
	s := openTestStore(t)
	until := testNow().Add(time.Hour).UTC()
	first, created, err := s.AddSub(Sub{Channel: "C2", Owner: "agent-1", Purpose: "triage"}, testNow())
	if err != nil || !created {
		t.Fatalf("AddSub = %+v, %v, %v", first, created, err)
	}
	again, created, err := s.AddSub(Sub{Channel: "C2", Owner: "agent-1", Until: until, IncludeSelf: true}, testNow())
	if err != nil || created || again.ID != first.ID {
		t.Fatalf("second AddSub = %+v, %v, %v; want an update of %d", again, created, err, first.ID)
	}
	if _, _, err := s.AddSub(Sub{Channel: "C2", Owner: "agent-2"}, testNow()); err != nil {
		t.Fatal(err)
	}
	subs, err := s.ListSubs()
	if err != nil {
		t.Fatal(err)
	}
	want := []Sub{
		{ID: first.ID, Channel: "C2", Owner: "agent-1", Until: until, IncludeSelf: true},
		{ID: first.ID + 1, Channel: "C2", Owner: "agent-2"},
	}
	if !reflect.DeepEqual(subs, want) {
		t.Errorf("ListSubs = %+v, want %+v", subs, want)
	}
	if got, _ := s.cursor(subScope(first.ID), ""); got != unixTS(testNow()) {
		t.Errorf("cursor = %q, want the subscribe time", got)
	}
}

func TestRemoveSubs(t *testing.T) {
	s := openTestStore(t)
	for _, owner := range []string{"agent-1", "agent-2"} {
		if _, _, err := s.AddSub(Sub{Channel: "C2", Owner: owner}, testNow()); err != nil {
			t.Fatal(err)
		}
	}
	if n, err := s.RemoveSubs("C2", "agent-1"); err != nil || n != 1 {
		t.Fatalf("RemoveSubs one owner = %d, %v", n, err)
	}
	if n, err := s.RemoveSubs("C2", ""); err != nil || n != 1 {
		t.Fatalf("RemoveSubs all = %d, %v", n, err)
	}
	var cursors int
	if err := s.DB().QueryRow(`SELECT COUNT(*) FROM cursors`).Scan(&cursors); err != nil {
		t.Fatal(err)
	}
	if cursors != 0 {
		t.Errorf("cursors = %d, want removed with the subs", cursors)
	}
}

func TestSubscribedChannelNotifiesTopLevelAndReplies(t *testing.T) {
	f := newEventFixture(t)
	f.sub(t, Sub{Channel: "C2", Owner: "agent-9", Purpose: "triage"})
	top := channelMsg("U9", "1700000040.000100", "", "deploy failed")
	reply := channelMsg("U8", "1700000041.000100", "1600000000.000100", "in an old thread")
	f.handle(t, top, top, reply, reply)
	if got := f.events(); !reflect.DeepEqual(got, []string{EventChannel, EventChannel}) {
		t.Fatalf("events = %v, want two channel notes", got)
	}
	n := f.rec.notes[0]
	if n.Owner != "agent-9" || n.WatchID != 0 || n.Purpose != "triage" || n.Author != "U9" || n.Text != "deploy failed" ||
		n.ReplyTS != top.TS || n.ReplyLink != "https://x.slack.com/archives/C2/p1700000040000100" {
		t.Errorf("top-level note = %+v", n)
	}
	if n := f.rec.notes[1]; n.PostLink != "https://x.slack.com/archives/C2/p1600000000000100" ||
		n.ReplyLink != "https://x.slack.com/archives/C2/p1700000041000100?thread_ts=1600000000.000100&cid=C2" {
		t.Errorf("reply note = %+v", n)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM replies WHERE channel = 'C2' AND watch_id IS NULL`); got != 2 {
		t.Errorf("stored channel messages = %d, want 2", got)
	}
}

func TestSubscribedChannelSkipsSelfUnlessIncludeSelf(t *testing.T) {
	f := newEventFixture(t)
	f.sub(t, Sub{Channel: "C2", Owner: "agent-9"})
	f.handle(t, channelMsg(selfID, "1700000040.000100", "", "mine"))
	if len(f.rec.notes) != 0 {
		t.Errorf("notes = %+v, want none for a self message", f.rec.notes)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM replies`); got != 0 {
		t.Errorf("replies = %d, want the self message skipped", got)
	}
	f.sub(t, Sub{Channel: "C2", Owner: "agent-9", IncludeSelf: true})
	f.handle(t, channelMsg(selfID, "1700000041.000100", "", "mine again"))
	if got := f.events(); !reflect.DeepEqual(got, []string{EventChannel}) {
		t.Errorf("events = %v, want one channel note with include-self", got)
	}
}

func TestWatchReplyInSubscribedChannelNotifiesSameTargetOnce(t *testing.T) {
	f := newEventFixture(t)
	f.proc.Config.Targets = map[string]string{"agent-1": "pane:1", "alias": "pane:1"}
	f.sub(t, Sub{Channel: "C1", Owner: "alias"})
	f.handle(t, threadReply("U9", "1700000009.000300", "hi"))
	if got := f.events(); !reflect.DeepEqual(got, []string{EventReply}) {
		t.Errorf("events = %v, want one reply", got)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM replies WHERE watch_id = ?`, f.thread.ID); got != 1 {
		t.Errorf("watch replies = %d, want 1", got)
	}
}

func TestWatchReplyInSubscribedChannelNotifiesDifferentTargets(t *testing.T) {
	f := newEventFixture(t)
	f.sub(t, Sub{Channel: "C1", Owner: "agent-9"})
	f.handle(t, threadReply("U9", "1700000009.000300", "hi"))
	if got := f.events(); !reflect.DeepEqual(got, []string{EventReply, EventChannel}) {
		t.Errorf("events = %v, want reply then channel", got)
	}
}

func TestMentionInSubscribedChannelIsInboundAndNotified(t *testing.T) {
	f := newEventFixture(t)
	f.sub(t, Sub{Channel: "C2", Owner: "agent-9"})
	f.handle(t, channelMsg("U9", "1700000040.000100", "", "hey <@"+selfID+">"))
	if got := f.events(); !reflect.DeepEqual(got, []string{EventChannel}) {
		t.Errorf("events = %v, want one channel note", got)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM inbound WHERE kind = 'mention'`); got != 1 {
		t.Errorf("mention inbound = %d, want 1", got)
	}
}

func TestUnsubAndExpiredSubNotifyNothing(t *testing.T) {
	f := newEventFixture(t)
	f.sub(t, Sub{Channel: "C2", Owner: "agent-9"})
	f.sub(t, Sub{Channel: "C3", Owner: "agent-9", Until: f.now.Add(-time.Minute)})
	if _, err := f.store.RemoveSubs("C2", ""); err != nil {
		t.Fatal(err)
	}
	f.handle(t, channelMsg("U9", "1700000040.000100", "", "anyone?"),
		Message{Channel: "C3", ChannelType: "channel", TS: "1700000040.000200", User: "U9", Text: "late"})
	if len(f.rec.notes) != 0 {
		t.Errorf("notes = %+v, want none", f.rec.notes)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM replies`); got != 0 {
		t.Errorf("replies = %d, want 0", got)
	}
}

func TestCatchUpSubPicksUpTopLevelAndOldThreadReplies(t *testing.T) {
	f := newCatchUpFixture(t)
	s := f.sub(t, Sub{Channel: "C2", Owner: "agent-9"})
	cursor := unixTS(testNow())
	oldThread := tsAt(testNow().Add(-48 * time.Hour))
	newTop := tsAt(testNow().Add(time.Minute))
	newReply := tsAt(testNow().Add(2 * time.Minute))
	f.slack.history["C2"] = []Fetched{
		{Message: Message{Channel: "C2", TS: oldThread, ThreadTS: oldThread, User: "U8", Text: "old"}, LatestReply: newReply},
		{Message: Message{Channel: "C2", TS: tsAt(testNow().Add(-time.Hour)), User: "U8", Text: "before"}},
		fetched(Message{Channel: "C2", TS: newTop, User: "U9", Text: "new top"}),
	}
	f.slack.replies["C2/"+oldThread] = []Fetched{
		fetched(Message{Channel: "C2", TS: oldThread, ThreadTS: oldThread, User: "U8", Text: "old"}),
		fetched(Message{Channel: "C2", TS: newReply, ThreadTS: oldThread, User: "U7", Text: "new reply"}),
	}
	if err := f.c.CatchUpSub(context.Background(), s); err != nil {
		t.Fatal(err)
	}
	wantCalls := []string{
		"history C2 " + unixTS(testNow().Add(-subThreadLookback)),
		"replies C2/" + oldThread + " " + cursor,
	}
	if !reflect.DeepEqual(f.slack.calls, wantCalls) {
		t.Errorf("calls = %q, want %q", f.slack.calls, wantCalls)
	}
	var texts []string
	for _, n := range f.rec.notes {
		if n.Event != EventChannel {
			t.Errorf("note %+v, want channel", n)
		}
		texts = append(texts, n.Text)
	}
	if want := []string{"new top", "new reply"}; !reflect.DeepEqual(texts, want) {
		t.Errorf("notified %q, want %q", texts, want)
	}
	if got, _ := f.store.cursor(subScope(s.ID), ""); got != newReply {
		t.Errorf("cursor = %q, want %q", got, newReply)
	}
}

func TestRunCatchesUpActiveSubs(t *testing.T) {
	f := newCatchUpFixture(t)
	f.sub(t, Sub{Channel: "C2", Owner: "agent-9"})
	f.sub(t, Sub{Channel: "C3", Owner: "agent-9", Until: f.now.Add(-time.Minute)})
	f.slack.history["C2"] = []Fetched{fetched(Message{Channel: "C2", TS: tsAt(testNow().Add(time.Minute)), User: "U9", Text: "hi"})}
	if err := f.c.Run(context.Background()); err != nil {
		t.Fatal(err)
	}
	for _, c := range f.slack.calls {
		if c[:min(len(c), 10)] == "history C3" {
			t.Errorf("caught up an expired sub: %q", c)
		}
	}
	if got := f.events(); !reflect.DeepEqual(got, []string{EventChannel}) {
		t.Errorf("events = %v, want one channel note", got)
	}
}
