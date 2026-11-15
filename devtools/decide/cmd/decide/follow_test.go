package main

import (
	"bytes"
	"errors"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"golang.org/x/sys/unix"
)

// setupStaleAnswer stores a relay answer an hour old for an idle owner
// in pane w3:p7 and returns the notifier's delivery key log.
func setupStaleAnswer(t *testing.T) (decide.Item, string) {
	t.Helper()
	state := t.TempDir()
	cfg := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", cfg)
	if err := os.MkdirAll(filepath.Join(cfg, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	body := `{"notifyTemplate":"notifier","ui":{"staleAnswerAfter":"30m"}}`
	if err := os.WriteFile(filepath.Join(cfg, "decide", "config.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	logPath := filepath.Join(state, "keys")
	installNotifierScript(t, "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\n")
	herdr := herdrtest.Install(t)
	herdr.Write("local.panes", `{"result":{"panes":[{"pane_id":"w3:p7","tab_id":"w3:t2","workspace_id":"w3"}]}}`)
	herdr.Write("local.tabs", `{"result":{"tabs":[{"tab_id":"w3:t2","label":"review","workspace_id":"w3"}]}}`)
	herdr.Write("local.workspaces", `{"result":{"workspaces":[{"workspace_id":"w3","label":"experimental"}]}}`)
	herdr.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)

	led, err := decide.Open(filepath.Join(state, "decide"))
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return time.Now().Add(-time.Hour) })
	item, err := led.Create(&decide.Item{
		Question:   "ship",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		NotifyPane: "w3:p7",
		Options:    []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	item, err = led.Answer(item.ID, "a", decide.ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	return item, logPath
}

func deliveryKeys(t *testing.T, path string) []string {
	t.Helper()
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		t.Fatal(err)
	}
	text := strings.TrimSpace(string(data))
	if text == "" {
		return nil
	}
	return strings.Split(text, "\n")
}

func TestFollowOnceResendsAStaleAnswerOnce(t *testing.T) {
	item, logPath := setupStaleAnswer(t)
	for range 2 {
		var stderr bytes.Buffer
		if code := run([]string{"follow", "--once"}, &bytes.Buffer{}, &stderr); code != 0 {
			t.Fatalf("follow --once = %d, stderr %q", code, stderr.String())
		}
	}
	key := decide.DeliveryKey(&item)
	if keys := deliveryKeys(t, logPath); len(keys) != 1 || keys[0] != key {
		t.Fatalf("keys = %#v, want %q once", keys, key)
	}
	got := showJSON(t, item.ID)
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != decide.OutcomeResend {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
}

func TestFollowRejectsBadFlags(t *testing.T) {
	for _, args := range [][]string{
		{"follow", "--every", "0s"},
		{"follow", "extra"},
	} {
		var stderr bytes.Buffer
		if code := run(args, &bytes.Buffer{}, &stderr); code != 2 {
			t.Fatalf("%q = %d, stderr %q", args, code, stderr.String())
		}
	}
}

func TestFollowStopsOnSIGTERM(t *testing.T) {
	_, logPath := setupStaleAnswer(t)
	cmd := exec.Command(os.Args[0], "follow", "--every", "20ms") //nolint:gosec // G204: the test re-execs itself; no shell.
	cmd.Env = append(os.Environ(), "DECIDE_TEST_CHILD=1")
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	done := make(chan error, 1)
	go func() { done <- cmd.Wait() }()
	// The first pass runs after the signal handler is set, so a resend
	// means SIGTERM reaches the loop.
	deadline := time.Now().Add(5 * time.Second)
	for len(deliveryKeys(t, logPath)) == 0 {
		if time.Now().After(deadline) {
			_ = cmd.Process.Kill()
			t.Fatalf("no follow pass ran; stderr %q", stderr.String())
		}
		time.Sleep(10 * time.Millisecond)
	}
	if err := cmd.Process.Signal(unix.SIGTERM); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-done:
		if err != nil {
			t.Fatalf("follow exit = %v, stderr %q", err, stderr.String())
		}
	case <-time.After(5 * time.Second):
		_ = cmd.Process.Kill()
		t.Fatal("follow did not stop on SIGTERM")
	}
}
