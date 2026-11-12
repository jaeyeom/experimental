package main

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

const missingDecide = `decide show: exec: "decide": executable file not found in $PATH`

func TestStatusOutboxPollFailing(t *testing.T) {
	cfg := noTokenConfig(t)
	store := testStore(t, cfg)
	now := time.Now()
	st := slackwatch.Status{
		PID: 4242, StartedAt: now.Add(-time.Hour), HeartbeatAt: now,
		HeartbeatInterval: slackwatch.Duration{Duration: time.Minute}, Connection: slackwatch.ConnConnected,
	}
	st.OutboxPoll.Record(now.Add(-time.Minute), errors.New(missingDecide))
	if err := store.WriteStatus(st); err != nil {
		t.Fatal(err)
	}
	code, out, _ := runSW(cfg, "status")
	if code != 1 || !strings.Contains(out, "outbox poll: failing since ") || !strings.Contains(out, missingDecide) {
		t.Errorf("status = %d %q, want 1 and the poll failure", code, out)
	}
	code, out, _ = runSW(cfg, "status", "--json")
	if code != 1 {
		t.Errorf("status --json = %d, want 1", code)
	}
	var got struct {
		OutboxPoll map[string]any `json:"outboxPoll"`
	}
	if err := json.Unmarshal([]byte(out), &got); err != nil {
		t.Fatalf("decode %q: %v", out, err)
	}
	if got.OutboxPoll["ok"] != false || got.OutboxPoll["lastError"] != missingDecide || got.OutboxPoll["failingSince"] == nil {
		t.Errorf("outboxPoll = %v", got.OutboxPoll)
	}

	st.OutboxPoll.Record(now, nil)
	if err := store.WriteStatus(st); err != nil {
		t.Fatal(err)
	}
	code, out, _ = runSW(cfg, "status")
	if code != 0 || strings.Contains(out, "failing") || !strings.Contains(out, "outbox poll: ok ") {
		t.Errorf("status after recovery = %d %q, want 0 and ok", code, out)
	}
}

// handoffConfig writes a handoff-mode config whose decideCommand is
// decide.
func handoffConfig(t *testing.T, decide string) string {
	t.Helper()
	dir := t.TempDir()
	cfg := filepath.Join(dir, "config.json")
	body, err := json.Marshal(map[string]any{
		"dbPath":        filepath.Join(dir, "slackwatch.db"),
		"decideCommand": decide,
		"post":          map[string]any{"mode": "handoff", "agent": "poster", "expectUser": "U1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfg, body, 0o600); err != nil {
		t.Fatal(err)
	}
	return cfg
}

// serveOutbox starts posting as serve does, with a heartbeat, until the
// test ends.
func serveOutbox(t *testing.T, cfgPath string) {
	t.Helper()
	cfg, store, err := openStore(cfgPath)
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(func() {
		cancel()
		store.Close()
	})
	hb := newHeartbeat(store, time.Minute, discardLogger(), time.Now)
	hb.update(func(st *slackwatch.Status) { st.Connection = slackwatch.ConnConnected })
	startOutbox(ctx, cfgPath, cfg, store, discardLogger(), hb.outboxPolled)
}

// waitStatus runs status until want accepts its output.
func waitStatus(t *testing.T, cfg string, want func(code int, out string) bool) (int, string) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for {
		code, out, _ := runSW(cfg, "status", "--json")
		if want(code, out) || time.Now().After(deadline) {
			return code, out
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func TestServeOutboxDecideNotFound(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "fake-decide")
	cfg := handoffConfig(t, missing)
	serveOutbox(t, cfg)
	code, out := waitStatus(t, cfg, func(code int, _ string) bool { return code == 1 })
	var st slackwatch.Status
	if err := json.Unmarshal([]byte(out), &st); err != nil {
		t.Fatalf("decode %q: %v", out, err)
	}
	if code != 1 || st.OutboxPoll.OK || !strings.Contains(st.OutboxPoll.LastError, missing) || st.OutboxPoll.FailingSince.IsZero() {
		t.Errorf("status = %d %+v, want 1 and a failing poll naming %s", code, st.OutboxPoll, missing)
	}
}

func TestServeOutboxDecideFound(t *testing.T) {
	fake := filepath.Join(t.TempDir(), "fake-decide")
	// Never run: there are no pending records.
	if err := os.WriteFile(fake, []byte("#!/bin/sh\nexit 1\n"), 0o700); err != nil { //nolint:gosec // G306: the fake decide must be executable.
		t.Fatal(err)
	}
	cfg := handoffConfig(t, fake)
	serveOutbox(t, cfg)
	code, out := waitStatus(t, cfg, func(code int, out string) bool { return code == 0 && strings.Contains(out, `"ok": true`) })
	if code != 0 || !strings.Contains(out, `"ok": true`) {
		t.Errorf("status = %d %q, want 0 and an ok poll", code, out)
	}
}
