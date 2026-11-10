package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack/socketmode"
)

// testStore opens the store that cfg points at.
func testStore(t *testing.T, cfg string) *slackwatch.Store {
	t.Helper()
	_, store, err := openStore(cfg)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { store.Close() })
	return store
}

func writeStatus(t *testing.T, store *slackwatch.Store, heartbeat time.Time) {
	t.Helper()
	if err := store.WriteStatus(slackwatch.Status{
		PID: 4242, StartedAt: heartbeat.Add(-time.Hour), HeartbeatAt: heartbeat,
		HeartbeatInterval: slackwatch.Duration{Duration: time.Minute}, Connection: slackwatch.ConnConnected,
	}); err != nil {
		t.Fatal(err)
	}
}

func runSW(cfg string, args ...string) (int, string, string) {
	var stdout, stderr bytes.Buffer
	code := run(append([]string{"--config", cfg}, args...), &stdout, &stderr)
	return code, stdout.String(), stderr.String()
}

func TestStatusFresh(t *testing.T) {
	cfg := noTokenConfig(t)
	writeStatus(t, testStore(t, cfg), time.Now())
	code, out, errOut := runSW(cfg, "status")
	if code != 0 {
		t.Fatalf("status = %d, stdout %q stderr %q", code, out, errOut)
	}
	if !strings.Contains(out, "connected") || !strings.Contains(out, "4242") {
		t.Errorf("stdout = %q", out)
	}
}

func TestStatusStale(t *testing.T) {
	cfg := noTokenConfig(t)
	writeStatus(t, testStore(t, cfg), time.Now().Add(-5*time.Minute))
	code, out, _ := runSW(cfg, "status")
	if code != 1 {
		t.Errorf("status = %d, want 1", code)
	}
	if !strings.Contains(out, "stale") {
		t.Errorf("stdout = %q, want stale", out)
	}
}

func TestStatusNeverStarted(t *testing.T) {
	cfg := noTokenConfig(t)
	code, out, _ := runSW(cfg, "status")
	if code != 1 || !strings.Contains(out, "down") {
		t.Errorf("status = %d %q, want 1 and down", code, out)
	}
}

func TestStatusUndelivered(t *testing.T) {
	cfg := noTokenConfig(t)
	store := testStore(t, cfg)
	if _, err := store.DB().Exec(`INSERT INTO deliveries (event, target, attempt, delivered, at, reply_ts)
VALUES ('channel', 'b', 1, 0, 0, '1.0')`); err != nil {
		t.Fatal(err)
	}
	writeStatus(t, store, time.Now())
	code, out, _ := runSW(cfg, "status")
	if code != 1 {
		t.Errorf("status = %d, want 1", code)
	}
	if !strings.Contains(out, "undelivered: 1") {
		t.Errorf("stdout = %q, want undelivered: 1", out)
	}

	code, out, _ = runSW(cfg, "status", "--json")
	if code != 1 {
		t.Errorf("status --json = %d, want 1", code)
	}
	var st slackwatch.Status
	if err := json.Unmarshal([]byte(out), &st); err != nil {
		t.Fatalf("decode %q: %v", out, err)
	}
	if st.Undelivered != 1 || st.State != slackwatch.ConnConnected || st.PID != 4242 {
		t.Errorf("status = %+v", st)
	}
}

func addInbound(t *testing.T, store *slackwatch.Store, channel string, at time.Time, author, text, kind string) {
	t.Helper()
	ts := strconv.FormatInt(at.Unix(), 10) + ".000100"
	if _, err := store.DB().Exec(`INSERT INTO inbound (channel, ts, author, text, kind) VALUES (?, ?, ?, ?, ?)`,
		channel, ts, author, text, kind); err != nil {
		t.Fatal(err)
	}
}

func TestInboundSince(t *testing.T) {
	cfg := noTokenConfig(t)
	store := testStore(t, cfg)
	now := time.Now()
	addInbound(t, store, "D1", now.Add(-48*time.Hour), "U1", "old", "dm")
	addInbound(t, store, "C1", now.Add(-10*time.Minute), "U2", "hey <@U0>", "mention")

	out := sw(t, cfg, "inbound")
	if !strings.Contains(out, "old") || !strings.Contains(out, "hey <@U0>") {
		t.Errorf("inbound = %q, want both", out)
	}
	out = sw(t, cfg, "inbound", "--since", "1h")
	if strings.Contains(out, "old") || !strings.Contains(out, "hey <@U0>") {
		t.Errorf("inbound --since 1h = %q, want only the recent one", out)
	}
	out = sw(t, cfg, "inbound", "--since", now.Add(-time.Hour).Format(time.RFC3339))
	if strings.Contains(out, "old") {
		t.Errorf("inbound --since RFC 3339 = %q", out)
	}
	if code, _, _ := runSW(cfg, "inbound", "--since", "yesterday-ish"); code != 2 {
		t.Errorf("bad --since code = %d, want 2", code)
	}
}

func TestInboundJSONIsStable(t *testing.T) {
	cfg := noTokenConfig(t)
	store := testStore(t, cfg)
	if _, err := store.DB().Exec(`INSERT INTO inbound (channel, ts, author, text, kind)
VALUES ('D1', '1700000000.000100', 'U1', 'hi', 'dm')`); err != nil {
		t.Fatal(err)
	}
	out := sw(t, cfg, "inbound", "--json")
	want := `[
  {
    "channel": "D1",
    "ts": "1700000000.000100",
    "author": "U1",
    "text": "hi",
    "kind": "dm"
  }
]
`
	if out != want {
		t.Errorf("inbound --json = %s\nwant %s", out, want)
	}
	if out := sw(t, cfg, "inbound", "--json", "--since", "1h"); out != "[]\n" {
		t.Errorf("empty inbound --json = %q, want []", out)
	}
}

func TestParseSince(t *testing.T) {
	now := time.Date(2026, 11, 9, 12, 0, 0, 0, time.Local)
	tests := map[string]time.Time{
		"2h":                        now.Add(-2 * time.Hour),
		"2026-11-08":                time.Date(2026, 11, 8, 0, 0, 0, 0, time.Local),
		"2026-11-08T10:00:00+00:00": time.Date(2026, 11, 8, 10, 0, 0, 0, time.UTC),
	}
	for in, want := range tests {
		got, err := parseSince(in, now)
		if err != nil || !got.Equal(want) {
			t.Errorf("parseSince(%q) = %v, %v, want %v", in, got, err, want)
		}
	}
}

func TestHeartbeatTracksServe(t *testing.T) {
	cfg := noTokenConfig(t)
	store := testStore(t, cfg)
	now := time.Date(2026, 11, 9, 12, 0, 0, 0, time.UTC)
	hb := newHeartbeat(store, time.Minute, discardLogger(), func() time.Time { return now })

	read := func() slackwatch.Status {
		t.Helper()
		st, err := store.ReadStatus(now)
		if err != nil {
			t.Fatal(err)
		}
		return st
	}
	st := read()
	if st.PID != os.Getpid() || !st.StartedAt.Equal(now) || st.Connection != slackwatch.ConnReconnecting {
		t.Errorf("after start = %+v", st)
	}

	now = now.Add(time.Second)
	hb.socketEvent(socketmode.EventTypeConnected)
	if st := read(); st.Connection != slackwatch.ConnConnected || !st.HeartbeatAt.Equal(now) {
		t.Errorf("after connect = %+v", st)
	}

	now = now.Add(time.Second)
	hb.socketEvent(socketmode.EventTypeEventsAPI)
	if st := read(); !st.LastEventAt.Equal(now) {
		t.Errorf("after event = %+v", st)
	}

	hb.socketEvent(socketmode.EventTypeDisconnect)
	if st := read(); st.Connection != slackwatch.ConnReconnecting {
		t.Errorf("after disconnect = %+v", st)
	}

	now = now.Add(time.Second)
	hb.swept(true, nil)
	hb.swept(false, errors.New("rate limited"))
	st = read()
	if !st.LastCatchUpAt.Equal(now) || st.LastCatchUpError != "" || !st.LastSweepAt.Equal(now) || st.LastSweepError != "rate limited" {
		t.Errorf("after sweeps = %+v", st)
	}

	hb.stop()
	if st := read(); st.Connection != slackwatch.ConnDown || st.State != slackwatch.ConnDown {
		t.Errorf("after stop = %+v", st)
	}
}
