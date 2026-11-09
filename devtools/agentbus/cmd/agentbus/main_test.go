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
