package main

import (
	"bytes"
	"context"
	"io"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

func TestRunFailsLoudlyWithoutRuntimeDir(t *testing.T) {
	var stderr bytes.Buffer
	code := run(context.Background(), nil, []string{"HOME=/Users/me"}, strings.NewReader(""), io.Discard, &stderr)
	if code == 0 {
		t.Fatal("run exited 0")
	}
	if !strings.Contains(stderr.String(), "XDG_RUNTIME_DIR") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestRunRejectsCommands(t *testing.T) {
	var stderr bytes.Buffer
	code := run(context.Background(), []string{"send"}, []string{"XDG_RUNTIME_DIR=/tmp", "HOME=/Users/me"}, strings.NewReader(""), io.Discard, &stderr)
	if code != 2 {
		t.Fatalf("run = %d, stderr %q", code, stderr.String())
	}
}

func TestLogReadsTheFileWithoutADaemon(t *testing.T) {
	stateHome := t.TempDir()
	lg := eventlog.Open(filepath.Join(stateHome, "agentbus"))
	when := time.Date(2026, 11, 9, 3, 4, 5, 0, time.UTC)
	if err := lg.Append(eventlog.Event{
		Time: when, Event: eventlog.EventSend, Actor: "cli", Name: "alpha", MsgID: "m1", Detail: "hello",
	}); err != nil {
		t.Fatal(err)
	}
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"log", "--json", "--name", "alpha"}, []string{"XDG_STATE_HOME=" + stateHome}, strings.NewReader(""), &stdout, &stderr)
	if code != 0 {
		t.Fatalf("run = %d, stderr %q", code, stderr.String())
	}
	if strings.Contains(stderr.String(), "agentbusd ready") {
		t.Fatalf("log started the daemon: %q", stderr.String())
	}
	if !strings.Contains(stdout.String(), `"event":"send"`) || !strings.Contains(stdout.String(), `"msg_id":"m1"`) {
		t.Fatalf("stdout = %q", stdout.String())
	}

	stdout.Reset()
	code = run(context.Background(), []string{"log", "--since", "2026-11-09T03:04:06Z"}, []string{"XDG_STATE_HOME=" + stateHome}, strings.NewReader(""), &stdout, &stderr)
	if code != 0 {
		t.Fatalf("filtered run = %d, stderr %q", code, stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("since filter stdout = %q", stdout.String())
	}
}

func TestParseSince(t *testing.T) {
	now := time.Date(2026, 11, 10, 12, 0, 0, 0, time.UTC)
	tests := []struct {
		in   string
		want time.Time
	}{
		{"30m", now.Add(-30 * time.Minute)},
		{"6h", now.Add(-6 * time.Hour)},
		{"1h30m", now.Add(-90 * time.Minute)},
		{"2d", now.Add(-48 * time.Hour)},
		{"1d12h", now.Add(-36 * time.Hour)},
		{"2026-11-09T13:00:00Z", time.Date(2026, 11, 9, 13, 0, 0, 0, time.UTC)},
		{"2026-11-09T13:00:00+09:00", time.Date(2026, 11, 9, 4, 0, 0, 0, time.UTC)},
		{"2026-11-09", time.Date(2026, 11, 9, 0, 0, 0, 0, time.Local)},
	}
	for _, tt := range tests {
		got, err := parseSince(tt.in, now)
		if err != nil {
			t.Errorf("parseSince(%q) error: %v", tt.in, err)
			continue
		}
		if !got.Equal(tt.want) {
			t.Errorf("parseSince(%q) = %v, want %v", tt.in, got, tt.want)
		}
	}
	for _, in := range []string{"0", "0d", "-1h", "-1d", "6x", "", "d", "1d-1h", "2026-13-01"} {
		_, err := parseSince(in, now)
		if err == nil {
			t.Errorf("parseSince(%q) succeeded", in)
			continue
		}
		if msg := err.Error(); !strings.Contains(msg, "30m, 6h, 2d") || !strings.Contains(msg, "RFC3339") {
			t.Errorf("parseSince(%q) error %q does not name the formats", in, msg)
		}
	}
}

func TestLogSinceAcceptsADuration(t *testing.T) {
	now := time.Date(2026, 11, 10, 12, 0, 0, 0, time.UTC)
	orig := logNow
	logNow = func() time.Time { return now }
	t.Cleanup(func() { logNow = orig })

	stateHome := t.TempDir()
	lg := eventlog.Open(filepath.Join(stateHome, "agentbus"))
	for id, when := range map[string]time.Time{"old": now.Add(-7 * time.Hour), "new": now.Add(-5 * time.Hour)} {
		if err := lg.Append(eventlog.Event{Time: when, Event: eventlog.EventSend, Actor: "cli", Name: "alpha", MsgID: id, Detail: "hello"}); err != nil {
			t.Fatal(err)
		}
	}
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), []string{"log", "--json", "--since", "6h"}, []string{"XDG_STATE_HOME=" + stateHome}, strings.NewReader(""), &stdout, &stderr)
	if code != 0 {
		t.Fatalf("run = %d, stderr %q", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), `"msg_id":"new"`) || strings.Contains(stdout.String(), `"msg_id":"old"`) {
		t.Fatalf("stdout = %q", stdout.String())
	}

	stdout.Reset()
	stderr.Reset()
	code = run(context.Background(), []string{"log", "--since", "6x"}, []string{"XDG_STATE_HOME=" + stateHome}, strings.NewReader(""), &stdout, &stderr)
	want := `agentbus log: --since "6x": want a duration like 30m, 6h, 2d, or an RFC3339 time like 2026-11-09T13:00:00Z`
	if code != 2 || !strings.Contains(stderr.String(), want) {
		t.Fatalf("run = %d, stderr %q", code, stderr.String())
	}
}
