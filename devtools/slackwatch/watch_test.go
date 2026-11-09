package slackwatch

import (
	"errors"
	"path/filepath"
	"reflect"
	"testing"
	"time"
)

func testNow() time.Time { return time.Date(2026, 11, 9, 12, 0, 0, 0, time.UTC) }

func openTestStore(t *testing.T) *Store {
	t.Helper()
	s, err := OpenStore(filepath.Join(t.TempDir(), "slackwatch.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { s.Close() })
	return s
}

func strp(s string) *string { return &s }

func TestAddWatchThreadAndReply(t *testing.T) {
	s := openTestStore(t)
	top := PostRef{Channel: "C1", TS: "1700000000.000100"}
	w, created, err := s.AddWatch(top, WatchUpdate{Owner: strp("agent-1"), Purpose: strp("ask")}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if !created {
		t.Error("first add did not create a watch")
	}
	if w.Kind != KindThread || w.State != StateOpen || w.ThreadTS != top.TS || w.SinceTS != top.TS || w.Source != SourceManual {
		t.Errorf("watch = %+v", w)
	}

	reply := PostRef{Channel: "C1", TS: "1700000005.000200", ThreadTS: top.TS}
	w2, created, err := s.AddWatch(reply, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if created || w2.ID != w.ID {
		t.Errorf("reply add created=%v id=%d, want existing watch %d", created, w2.ID, w.ID)
	}
	if w2.SinceTS != top.TS {
		t.Errorf("since_ts = %q, want earliest post %q", w2.SinceTS, top.TS)
	}

	d, err := s.ShowWatch(w.ID)
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{top.TS, reply.TS}; !reflect.DeepEqual(d.Posts, want) {
		t.Errorf("posts = %q, want %q", d.Posts, want)
	}
}

func TestAddWatchReplyFirstMovesSinceBack(t *testing.T) {
	s := openTestStore(t)
	reply := PostRef{Channel: "C1", TS: "1700000005.000200", ThreadTS: "1700000000.000100"}
	w, _, err := s.AddWatch(reply, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if w.ThreadTS != reply.ThreadTS || w.SinceTS != reply.TS || w.State != StateUnowned {
		t.Errorf("watch = %+v", w)
	}
	w, _, err = s.AddWatch(PostRef{Channel: "C1", TS: reply.ThreadTS}, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if w.SinceTS != reply.ThreadTS {
		t.Errorf("since_ts = %q, want %q", w.SinceTS, reply.ThreadTS)
	}
}

func TestAddWatchIdempotentUpdatesGivenFields(t *testing.T) {
	s := openTestStore(t)
	p := PostRef{Channel: "C1", TS: "1700000000.000100"}
	if _, _, err := s.AddWatch(p, WatchUpdate{Owner: strp("a"), Purpose: strp("old"), Refs: []string{"PR#1"}}, testNow()); err != nil {
		t.Fatal(err)
	}
	w, created, err := s.AddWatch(p, WatchUpdate{Purpose: strp("new")}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if created {
		t.Error("second add created a watch")
	}
	if w.Purpose != "new" || w.Owner != "a" || !reflect.DeepEqual(w.Refs, []string{"PR#1"}) {
		t.Errorf("watch = %+v", w)
	}
	var n int
	if err := s.DB().QueryRow(`SELECT (SELECT COUNT(*) FROM watches) + (SELECT COUNT(*) FROM watch_posts)`).Scan(&n); err != nil {
		t.Fatal(err)
	}
	if n != 2 {
		t.Errorf("watches + posts = %d, want 2", n)
	}
}

func TestAddWatchChannelTSOfKnownReply(t *testing.T) {
	s := openTestStore(t)
	reply := PostRef{Channel: "C1", TS: "1700000005.000200", ThreadTS: "1700000000.000100"}
	w, _, err := s.AddWatch(reply, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	// Re-adding the reply by channel and ts finds its thread.
	w2, created, err := s.AddWatch(PostRef{Channel: "C1", TS: reply.TS}, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if created || w2.ID != w.ID {
		t.Errorf("created=%v id=%d, want %d", created, w2.ID, w.ID)
	}
	found, err := s.FindWatch(PostRef{Channel: "C1", TS: reply.TS})
	if err != nil || found.ID != w.ID {
		t.Errorf("FindWatch by reply = %+v, %v", found, err)
	}
}

func TestAddWatchDM(t *testing.T) {
	s := openTestStore(t)
	w, _, err := s.AddWatch(PostRef{Channel: "D1", TS: "1700000000.000100"}, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if w.Kind != KindDM || w.SinceTS != "1700000000.000100" {
		t.Errorf("watch = %+v", w)
	}
}

func TestClaimDoneSnooze(t *testing.T) {
	s := openTestStore(t)
	w, _, err := s.AddWatch(PostRef{Channel: "C1", TS: "1700000000.000100"}, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if w.State != StateUnowned {
		t.Fatalf("state = %q, want unowned", w.State)
	}
	w, err = s.ClaimWatch(w.ID, "agent-2", WatchUpdate{Purpose: strp("p")})
	if err != nil {
		t.Fatal(err)
	}
	if w.State != StateOpen || w.Owner != "agent-2" || w.Purpose != "p" {
		t.Errorf("after claim = %+v", w)
	}
	if _, err := s.ClaimWatch(w.ID, "agent-3", WatchUpdate{}); err == nil {
		t.Error("claim of an open watch succeeded")
	}

	until := testNow().Add(72 * time.Hour)
	w, err = s.SnoozeWatch(w.ID, until)
	if err != nil {
		t.Fatal(err)
	}
	if !w.Until.Equal(until) {
		t.Errorf("until = %v, want %v", w.Until, until)
	}

	w, err = s.CloseWatch(w.ID, "answered", testNow())
	if err != nil {
		t.Fatal(err)
	}
	if w.State != StateClosed || w.CloseReason != "answered" || !w.ClosedAt.Equal(testNow()) {
		t.Errorf("after done = %+v", w)
	}

	if _, err := s.db.Exec(`UPDATE watches SET state = 'orphaned' WHERE id = ?`, w.ID); err != nil {
		t.Fatal(err)
	}
	if w, err = s.ClaimWatch(w.ID, "agent-4", WatchUpdate{}); err != nil || w.State != StateOpen {
		t.Errorf("claim orphaned = %+v, %v", w, err)
	}
}

func TestFindWatchMissing(t *testing.T) {
	s := openTestStore(t)
	if _, err := s.FindWatch(PostRef{WatchID: 9}); !errors.Is(err, ErrNoWatch) {
		t.Errorf("err = %v, want ErrNoWatch", err)
	}
	if _, err := s.FindWatch(PostRef{Channel: "C1", TS: "1.000001"}); !errors.Is(err, ErrNoWatch) {
		t.Errorf("err = %v, want ErrNoWatch", err)
	}
}

func TestListAndAck(t *testing.T) {
	s := openTestStore(t)
	w1, _, err := s.AddWatch(PostRef{Channel: "C1", TS: "1700000000.000100"}, WatchUpdate{Owner: strp("a")}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	w2, _, err := s.AddWatch(PostRef{Channel: "C2", TS: "1700000000.000100"}, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	for _, r := range []Reply{
		{Channel: "C1", TS: "1700000001.000000", ThreadTS: "1700000000.000100", WatchID: w1.ID, Author: "U2", Text: "one"},
		{Channel: "C1", TS: "1700000002.000000", ThreadTS: "1700000000.000100", WatchID: w1.ID, Author: "U3", Text: "two"},
		{Channel: "C1", TS: "1700000003.000000", ThreadTS: "1700000000.000100", WatchID: w1.ID, Author: "U1", Text: "me", IsSelf: true},
	} {
		if _, err := s.AddReply(r); err != nil {
			t.Fatal(err)
		}
	}
	if _, err := s.db.Exec(`INSERT INTO deliveries (event, target, delivered, at, watch_id) VALUES ('reply', 't', 0, 1, ?)`, w2.ID); err != nil {
		t.Fatal(err)
	}

	all, err := s.ListWatches(WatchFilter{})
	if err != nil {
		t.Fatal(err)
	}
	if len(all) != 2 {
		t.Fatalf("ls = %d rows, want 2", len(all))
	}
	got := all[0]
	if got.ID != w1.ID || got.Replies != 3 || got.Unacked != 2 || got.LastReplyAuthor != "U1" || got.LastReplyTS != "1700000003.000000" || got.Undelivered {
		t.Errorf("row 1 = %+v", got)
	}
	if !all[1].Undelivered {
		t.Errorf("row 2 = %+v, want undelivered", all[1])
	}

	for _, tt := range []struct {
		f    WatchFilter
		want []int64
	}{
		{WatchFilter{Owner: "a"}, []int64{w1.ID}},
		{WatchFilter{State: StateUnowned}, []int64{w2.ID}},
		{WatchFilter{Unacked: true}, []int64{w1.ID}},
	} {
		rows, err := s.ListWatches(tt.f)
		if err != nil {
			t.Fatal(err)
		}
		var ids []int64
		for _, r := range rows {
			ids = append(ids, r.ID)
		}
		if !reflect.DeepEqual(ids, tt.want) {
			t.Errorf("ls %+v = %v, want %v", tt.f, ids, tt.want)
		}
	}

	n, err := s.AckWatch(w1.ID)
	if err != nil {
		t.Fatal(err)
	}
	if n != 3 {
		t.Errorf("acked %d, want 3", n)
	}
	rows, err := s.ListWatches(WatchFilter{Unacked: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 0 {
		t.Errorf("unacked rows after ack = %+v", rows)
	}
}

func TestShowWatchRepliesAndReactions(t *testing.T) {
	s := openTestStore(t)
	w, _, err := s.AddWatch(PostRef{Channel: "C1", TS: "1700000000.000100"}, WatchUpdate{}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	if _, err := s.AddReply(Reply{Channel: "C1", TS: "1700000001.000000", ThreadTS: w.ThreadTS, WatchID: w.ID, Author: "U2", Text: "hi"}); err != nil {
		t.Fatal(err)
	}
	if _, err := s.db.Exec(`INSERT INTO reactions (channel, ts, user, name) VALUES
('C1', '1700000000.000100', 'U2', 'eyes'),
('C1', '1700000001.000000', 'U1', '+1'),
('C1', '1800000000.000000', 'U1', 'unrelated')`); err != nil {
		t.Fatal(err)
	}
	d, err := s.ShowWatch(w.ID)
	if err != nil {
		t.Fatal(err)
	}
	if len(d.Replies) != 1 || d.Replies[0].Text != "hi" {
		t.Errorf("replies = %+v", d.Replies)
	}
	want := []Reaction{
		{TS: "1700000000.000100", User: "U2", Name: "eyes"},
		{TS: "1700000001.000000", User: "U1", Name: "+1"},
	}
	if !reflect.DeepEqual(d.Reactions, want) {
		t.Errorf("reactions = %+v, want %+v", d.Reactions, want)
	}
}
