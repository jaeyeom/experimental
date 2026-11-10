package slackwatch

import (
	"reflect"
	"testing"
	"time"
)

func TestReadStatusNeverStarted(t *testing.T) {
	s := openTestStore(t)
	st, err := s.ReadStatus(testNow())
	if err != nil {
		t.Fatal(err)
	}
	if st.State != ConnDown || st.Healthy() {
		t.Errorf("status = %+v, want down and unhealthy", st)
	}
}

func TestWriteStatusRoundTrip(t *testing.T) {
	s := openTestStore(t)
	now := testNow()
	want := Status{
		PID:               42,
		StartedAt:         now.Add(-time.Hour),
		HeartbeatAt:       now.Add(-time.Minute),
		HeartbeatInterval: Duration{time.Minute},
		Connection:        ConnConnected,
		LastEventAt:       now.Add(-2 * time.Minute),
		LastCatchUpAt:     now.Add(-time.Hour),
		LastCatchUpError:  "",
		LastSweepAt:       now.Add(-30 * time.Minute),
		LastSweepError:    "boom",
	}
	if err := s.WriteStatus(want); err != nil {
		t.Fatal(err)
	}
	got, err := s.ReadStatus(now)
	if err != nil {
		t.Fatal(err)
	}
	want.State = ConnConnected
	if !reflect.DeepEqual(got, want) {
		t.Errorf("status = %+v\nwant %+v", got, want)
	}
	if !got.Healthy() {
		t.Error("fresh connected heartbeat is not healthy")
	}
}

func TestReadStatusStaleAfterTwoIntervals(t *testing.T) {
	s := openTestStore(t)
	now := testNow()
	if err := s.WriteStatus(Status{
		PID: 1, StartedAt: now.Add(-time.Hour), HeartbeatAt: now.Add(-3 * time.Minute),
		HeartbeatInterval: Duration{time.Minute}, Connection: ConnConnected,
	}); err != nil {
		t.Fatal(err)
	}
	st, err := s.ReadStatus(now)
	if err != nil {
		t.Fatal(err)
	}
	if st.State != ConnStale || st.Healthy() {
		t.Errorf("status = %+v, want stale and unhealthy", st)
	}
	// Just inside 2x the interval is still fresh.
	st, err = s.ReadStatus(now.Add(-time.Minute - time.Second))
	if err != nil {
		t.Fatal(err)
	}
	if st.State != ConnConnected {
		t.Errorf("state = %q, want connected", st.State)
	}
}

func TestReadStatusDownIsUnhealthy(t *testing.T) {
	s := openTestStore(t)
	now := testNow()
	if err := s.WriteStatus(Status{PID: 1, HeartbeatAt: now, HeartbeatInterval: Duration{time.Minute}, Connection: ConnDown}); err != nil {
		t.Fatal(err)
	}
	st, err := s.ReadStatus(now)
	if err != nil {
		t.Fatal(err)
	}
	if st.State != ConnDown || st.Healthy() {
		t.Errorf("status = %+v, want down and unhealthy", st)
	}
}

func TestReconnectingIsHealthy(t *testing.T) {
	st := Status{State: ConnReconnecting}
	if !st.Healthy() {
		t.Error("a reconnecting server with nothing undelivered is unhealthy")
	}
}

func addDelivery(t *testing.T, s *Store, event, target, replyTS string, watchID any, attempt int, delivered bool) {
	t.Helper()
	if _, err := s.DB().Exec(`INSERT INTO deliveries (event, target, attempt, delivered, at, watch_id, reply_ts)
VALUES (?, ?, ?, ?, 0, ?, ?)`, event, target, attempt, delivered, watchID, replyTS); err != nil {
		t.Fatal(err)
	}
}

func TestWriteStatusCountsUndelivered(t *testing.T) {
	s := openTestStore(t)
	w, _, err := s.AddWatch(PostRef{Channel: "C1", TS: "1.0"}, WatchUpdate{Owner: strp("a")}, testNow())
	if err != nil {
		t.Fatal(err)
	}
	// Owner failed, fallback delivered: not undelivered.
	addDelivery(t, s, EventReply, "a", "2.0", w.ID, 1, false)
	addDelivery(t, s, EventOrphaned, "fb", "2.0", w.ID, 2, true)
	// Owner and fallback both failed: one undelivered notification.
	addDelivery(t, s, EventReply, "a", "3.0", w.ID, 1, false)
	addDelivery(t, s, EventOrphaned, "fb", "3.0", w.ID, 2, false)
	// Subscription notification with no fallback: undelivered.
	addDelivery(t, s, EventChannel, "b", "4.0", nil, 1, false)
	// Another subscriber's success does not cover a failed one.
	addDelivery(t, s, EventChannel, "c", "4.0", nil, 1, true)
	// A retry to the same target that succeeded covers the failure.
	addDelivery(t, s, EventChannel, "d", "5.0", nil, 1, false)
	addDelivery(t, s, EventChannel, "d", "5.0", nil, 1, true)

	now := testNow()
	if err := s.WriteStatus(Status{PID: 1, HeartbeatAt: now, HeartbeatInterval: Duration{time.Minute}, Connection: ConnConnected}); err != nil {
		t.Fatal(err)
	}
	st, err := s.ReadStatus(now)
	if err != nil {
		t.Fatal(err)
	}
	if st.Undelivered != 2 {
		t.Errorf("undelivered = %d, want 2", st.Undelivered)
	}
	if st.Healthy() {
		t.Error("status with undelivered notifications is healthy")
	}
}
