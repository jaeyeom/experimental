package slackwatch

import (
	"context"
	"regexp"
	"testing"
	"time"
)

const (
	selfID   = "USELF"
	threadTS = "1700000000.000100"
	dmTS     = "1700000000.000200"
)

type recordingNotifier struct {
	notes []Notification
}

func (r *recordingNotifier) Notify(_ context.Context, n Notification) error {
	r.notes = append(r.notes, n)
	return nil
}

type eventFixture struct {
	store  *Store
	rec    *recordingNotifier
	proc   *Processor
	thread Watch
	dm     Watch
	now    time.Time
}

func newEventFixture(t *testing.T) *eventFixture {
	t.Helper()
	s := openTestStore(t)
	thread, _, err := s.AddWatch(PostRef{Channel: "C1", TS: threadTS},
		WatchUpdate{Owner: strp("agent-1"), Purpose: strp("ask")}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	dm, _, err := s.AddWatch(PostRef{Channel: "D1", TS: dmTS}, WatchUpdate{Owner: strp("agent-2")}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	f := &eventFixture{store: s, rec: &recordingNotifier{}, thread: thread, dm: dm, now: testNow().Add(time.Hour)}
	f.proc = &Processor{
		Store:    s,
		Notifier: f.rec,
		Config: Config{
			FooterPattern: Regexp{regexp.MustCompile(`sent by agent`)},
			ReopenWindow:  Duration{DefaultReopenWindow},
		},
		Self:    selfID,
		TeamURL: "https://x.slack.com/",
		Now:     func() time.Time { return f.now },
	}
	return f
}

func (f *eventFixture) watch(t *testing.T, id int64) Watch {
	t.Helper()
	w, err := f.store.FindWatch(PostRef{WatchID: id})
	if err != nil {
		t.Fatal(err)
	}
	return w
}

func (f *eventFixture) count(t *testing.T, query string, args ...any) int {
	t.Helper()
	var n int
	if err := f.store.DB().QueryRow(query, args...).Scan(&n); err != nil {
		t.Fatal(err)
	}
	return n
}

func (f *eventFixture) events() []string {
	var out []string
	for _, n := range f.rec.notes {
		out = append(out, n.Event)
	}
	return out
}

func threadReply(user, ts, text string) Message {
	return Message{Channel: "C1", ChannelType: "channel", TS: ts, ThreadTS: threadTS, User: user, Text: text}
}

func TestHandleMessage(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, f *eventFixture)
		msgs  []Message
		check func(t *testing.T, f *eventFixture)
	}{
		{
			name: "thread reply by another user notifies once",
			msgs: []Message{threadReply("U9", "1700000009.000300", "looks good"), threadReply("U9", "1700000009.000300", "looks good")},
			check: func(t *testing.T, f *eventFixture) {
				if len(f.rec.notes) != 1 {
					t.Fatalf("notes = %+v, want one", f.rec.notes)
				}
				n := f.rec.notes[0]
				if n.Event != EventReply || n.Owner != "agent-1" || n.WatchID != f.thread.ID || n.ReplyTS != "1700000009.000300" ||
					n.Author != "U9" || n.Text != "looks good" || n.Purpose != "ask" ||
					n.PostLink != "https://x.slack.com/archives/C1/p1700000000000100" ||
					n.ReplyLink != "https://x.slack.com/archives/C1/p1700000009000300?thread_ts=1700000000.000100" {
					t.Errorf("note = %+v", n)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM replies WHERE watch_id = ? AND is_self = 0`, f.thread.ID); got != 1 {
					t.Errorf("replies = %d, want 1", got)
				}
				if w := f.watch(t, f.thread.ID); !w.LastActivity.Equal(f.now) {
					t.Errorf("last_activity = %v, want %v", w.LastActivity, f.now)
				}
			},
		},
		{
			name: "self reply is stored without notify",
			msgs: []Message{threadReply(selfID, "1700000009.000300", "follow-up")},
			check: func(t *testing.T, f *eventFixture) {
				if len(f.rec.notes) != 0 {
					t.Errorf("notes = %+v, want none", f.rec.notes)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM replies WHERE watch_id = ? AND is_self = 1`, f.thread.ID); got != 1 {
					t.Errorf("self replies = %d, want 1", got)
				}
				if w := f.watch(t, f.thread.ID); !w.LastActivity.Equal(f.now) {
					t.Errorf("last_activity = %v, want %v", w.LastActivity, f.now)
				}
			},
		},
		{
			name: "self reply notifies with notifySelf",
			setup: func(_ *testing.T, f *eventFixture) {
				f.proc.Config.NotifySelf = true
			},
			msgs: []Message{threadReply(selfID, "1700000009.000300", "follow-up")},
			check: func(t *testing.T, f *eventFixture) {
				if got := f.events(); len(got) != 1 || got[0] != EventReply {
					t.Errorf("events = %v, want [reply]", got)
				}
			},
		},
		{
			name: "reply older than since_ts is ignored",
			msgs: []Message{threadReply("U9", "1699999999.000100", "old")},
			check: func(t *testing.T, f *eventFixture) {
				if len(f.rec.notes) != 0 {
					t.Errorf("notes = %+v, want none", f.rec.notes)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM replies`); got != 0 {
					t.Errorf("replies = %d, want 0", got)
				}
			},
		},
		{
			name: "reply on an unowned watch goes to the fallback",
			setup: func(t *testing.T, f *eventFixture) {
				if _, err := f.store.DB().Exec(`UPDATE watches SET owner = '', state = ? WHERE id = ?`, StateUnowned, f.thread.ID); err != nil {
					t.Fatal(err)
				}
			},
			msgs: []Message{threadReply("U9", "1700000009.000300", "hi")},
			check: func(t *testing.T, f *eventFixture) {
				if len(f.rec.notes) != 1 || f.rec.notes[0].Event != EventUnowned || f.rec.notes[0].Owner != "" {
					t.Errorf("notes = %+v, want one unowned with no owner", f.rec.notes)
				}
			},
		},
		{
			name: "top-level IM message after a DM watch notifies without inbound",
			msgs: []Message{{Channel: "D1", ChannelType: "im", TS: "1700000010.000100", User: "U9", Text: "answer"}},
			check: func(t *testing.T, f *eventFixture) {
				if len(f.rec.notes) != 1 || f.rec.notes[0].Owner != "agent-2" || f.rec.notes[0].WatchID != f.dm.ID {
					t.Errorf("notes = %+v, want one for agent-2", f.rec.notes)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM inbound`); got != 0 {
					t.Errorf("inbound = %d, want 0", got)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM replies WHERE watch_id = ?`, f.dm.ID); got != 1 {
					t.Errorf("dm replies = %d, want 1", got)
				}
			},
		},
		{
			name: "IM message before the DM watch is inbound",
			msgs: []Message{{Channel: "D1", ChannelType: "im", TS: "1600000000.000100", User: "U9", Text: "old"}},
			check: func(t *testing.T, f *eventFixture) {
				if len(f.rec.notes) != 0 {
					t.Errorf("notes = %+v, want none", f.rec.notes)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM inbound WHERE kind = 'dm'`); got != 1 {
					t.Errorf("dm inbound = %d, want 1", got)
				}
			},
		},
		{
			name: "self post matching the footer registers an unowned watch",
			msgs: []Message{
				{Channel: "C2", ChannelType: "channel", TS: "1700000020.000100", User: selfID, Text: "status update\n_sent by agent_"},
				{Channel: "C2", ChannelType: "channel", TS: "1700000020.000100", User: selfID, Text: "status update\n_sent by agent_"},
			},
			check: func(t *testing.T, f *eventFixture) {
				w, err := f.store.FindWatch(PostRef{Channel: "C2", TS: "1700000020.000100"})
				if err != nil {
					t.Fatal(err)
				}
				if w.State != StateUnowned || w.Source != SourceFooter || w.ThreadTS != "1700000020.000100" {
					t.Errorf("watch = %+v", w)
				}
				if len(f.rec.notes) != 1 || f.rec.notes[0].Event != EventUnowned || f.rec.notes[0].Owner != "" || f.rec.notes[0].WatchID != w.ID {
					t.Errorf("notes = %+v, want one unowned for the fallback", f.rec.notes)
				}
			},
		},
		{
			name: "self thread reply matching the footer watches the parent",
			msgs: []Message{{Channel: "C2", ChannelType: "channel", TS: "1700000021.000100", ThreadTS: "1700000020.000100", User: selfID, Text: "sent by agent"}},
			check: func(t *testing.T, f *eventFixture) {
				w, err := f.store.FindWatch(PostRef{Channel: "C2", TS: "1700000021.000100", ThreadTS: "1700000020.000100"})
				if err != nil {
					t.Fatal(err)
				}
				if w.ThreadTS != "1700000020.000100" || w.SinceTS != "1700000021.000100" {
					t.Errorf("watch = %+v", w)
				}
			},
		},
		{
			name: "self post without the footer is dropped",
			msgs: []Message{{Channel: "C2", ChannelType: "channel", TS: "1700000020.000100", User: selfID, Text: "hello"}},
			check: func(t *testing.T, f *eventFixture) {
				if got := f.count(t, `SELECT COUNT(*) FROM watches`); got != 2 {
					t.Errorf("watches = %d, want 2", got)
				}
			},
		},
		{
			name: "mention is inbound without notify",
			msgs: []Message{{Channel: "C2", ChannelType: "channel", TS: "1700000030.000100", User: "U9", Text: "hey <@" + selfID + "> look"}},
			check: func(t *testing.T, f *eventFixture) {
				if len(f.rec.notes) != 0 {
					t.Errorf("notes = %+v, want none", f.rec.notes)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM inbound WHERE kind = 'mention' AND author = 'U9' AND text LIKE 'hey%'`); got != 1 {
					t.Errorf("mention inbound = %d, want 1", got)
				}
			},
		},
		{
			name: "general channel traffic is dropped",
			msgs: []Message{{Channel: "C2", ChannelType: "channel", TS: "1700000030.000100", User: "U9", Text: "lunch?"}},
			check: func(t *testing.T, f *eventFixture) {
				if got := f.count(t, `SELECT (SELECT COUNT(*) FROM replies) + (SELECT COUNT(*) FROM inbound)`); got != 0 {
					t.Errorf("stored rows = %d, want 0", got)
				}
			},
		},
		{
			name: "reply within the reopen window reopens the watch",
			setup: func(t *testing.T, f *eventFixture) {
				if _, err := f.store.CloseWatch(f.thread.ID, "done", f.now.Add(-24*time.Hour)); err != nil {
					t.Fatal(err)
				}
			},
			msgs: []Message{threadReply("U9", "1700000009.000300", "one more thing")},
			check: func(t *testing.T, f *eventFixture) {
				w := f.watch(t, f.thread.ID)
				if w.State != StateOpen || !w.ClosedAt.IsZero() || w.CloseReason != "" {
					t.Errorf("watch = %+v, want reopened", w)
				}
				if got := f.events(); len(got) != 1 || got[0] != EventReopened {
					t.Errorf("events = %v, want [reopened]", got)
				}
				if got := f.count(t, `SELECT COUNT(*) FROM replies WHERE watch_id = ?`, f.thread.ID); got != 1 {
					t.Errorf("replies = %d, want 1", got)
				}
			},
		},
		{
			name: "reply after the reopen window leaves the watch closed",
			setup: func(t *testing.T, f *eventFixture) {
				if _, err := f.store.CloseWatch(f.thread.ID, "done", f.now.Add(-DefaultReopenWindow-time.Hour)); err != nil {
					t.Fatal(err)
				}
			},
			msgs: []Message{threadReply("U9", "1700000009.000300", "late")},
			check: func(t *testing.T, f *eventFixture) {
				if w := f.watch(t, f.thread.ID); w.State != StateClosed {
					t.Errorf("state = %s, want closed", w.State)
				}
				if len(f.rec.notes) != 0 {
					t.Errorf("notes = %+v, want none", f.rec.notes)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			f := newEventFixture(t)
			if tt.setup != nil {
				tt.setup(t, f)
			}
			for _, m := range tt.msgs {
				if err := f.proc.HandleMessage(context.Background(), m); err != nil {
					t.Fatal(err)
				}
			}
			tt.check(t, f)
		})
	}
}

func TestHandleEdit(t *testing.T) {
	f := newEventFixture(t)
	ctx := context.Background()
	if err := f.proc.HandleMessage(ctx, threadReply("U9", "1700000009.000300", "frist")); err != nil {
		t.Fatal(err)
	}
	f.rec.notes = nil
	if err := f.proc.HandleEdit(Message{Channel: "C1", TS: "1700000009.000300", Text: "first", EditedTS: "1700000011.000000"}); err != nil {
		t.Fatal(err)
	}
	d, err := f.store.ShowWatch(f.thread.ID)
	if err != nil {
		t.Fatal(err)
	}
	if len(d.Replies) != 1 || d.Replies[0].Text != "first" || d.Replies[0].EditedAt != "1700000011.000000" {
		t.Errorf("replies = %+v", d.Replies)
	}
	if len(f.rec.notes) != 0 {
		t.Errorf("notes = %+v, want none", f.rec.notes)
	}
}

func TestHandleDelete(t *testing.T) {
	f := newEventFixture(t)
	ctx := context.Background()
	if err := f.proc.HandleMessage(ctx, threadReply("U9", "1700000009.000300", "oops")); err != nil {
		t.Fatal(err)
	}
	f.rec.notes = nil
	if err := f.proc.HandleDelete("C1", "1700000009.000300"); err != nil {
		t.Fatal(err)
	}
	d, err := f.store.ShowWatch(f.thread.ID)
	if err != nil {
		t.Fatal(err)
	}
	if len(d.Replies) != 1 || !d.Replies[0].Deleted {
		t.Errorf("replies = %+v, want one deleted", d.Replies)
	}
	if len(f.rec.notes) != 0 {
		t.Errorf("notes = %+v, want none", f.rec.notes)
	}
}

func TestHandleReaction(t *testing.T) {
	f := newEventFixture(t)
	ctx := context.Background()
	if err := f.proc.HandleMessage(ctx, threadReply("U9", "1700000009.000300", "hi")); err != nil {
		t.Fatal(err)
	}
	f.rec.notes = nil
	for _, r := range []struct{ channel, ts string }{
		{"C1", threadTS},            // watched post
		{"C1", "1700000009.000300"}, // stored reply
		{"C1", threadTS},            // duplicate
		{"C2", "1700000050.000100"}, // unwatched
	} {
		if err := f.proc.HandleReaction(r.channel, Reaction{TS: r.ts, User: "U9", Name: "eyes"}); err != nil {
			t.Fatal(err)
		}
	}
	d, err := f.store.ShowWatch(f.thread.ID)
	if err != nil {
		t.Fatal(err)
	}
	if len(d.Reactions) != 2 {
		t.Errorf("reactions = %+v, want 2", d.Reactions)
	}
	if got := f.count(t, `SELECT COUNT(*) FROM reactions`); got != 2 {
		t.Errorf("stored reactions = %d, want 2", got)
	}
	if len(f.rec.notes) != 0 {
		t.Errorf("notes = %+v, want none", f.rec.notes)
	}
}
