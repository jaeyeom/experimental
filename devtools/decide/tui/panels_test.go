package tui

import (
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestStatusViewRendersPanelsAndUnreadableSafe(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	store, err := decide.OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	store.SetClock(func() time.Time { return now.Add(-10 * time.Minute) })
	if _, err := store.Set(decide.Panel{
		ID:         "ci",
		Title:      "CI Build",
		Kind:       "kv",
		Data:       json.RawMessage(`{"ok":true}`),
		StaleAfter: "5m",
	}); err != nil {
		t.Fatal(err)
	}
	store.SetClock(func() time.Time { return now })
	if _, err := store.Set(decide.Panel{
		ID:    "deploy",
		Title: "Deployments",
		Kind:  "table",
		Data:  json.RawMessage(`{"rows":[]}`),
	}); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(decide.PanelsDir(dir), "broken.json"), []byte("{bad"), 0o600); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return now }
	m.width = 100
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "gs")
	view := m.View()
	if !strings.Contains(view, "CI Build") || !strings.Contains(view, "Deployments") {
		t.Fatalf("missing panels in view:\n%s", view)
	}
	if !strings.Contains(view, "panel unreadable: broken.json") {
		t.Fatalf("missing unreadable panel message in view:\n%s", view)
	}
	ciLine := lineContaining(view, "CI Build")
	if !strings.Contains(ciLine, "stale") || !strings.Contains(ciLine, "\x1b[2m") {
		t.Fatalf("stale panel line not marked/dimmed: %q", ciLine)
	}
}

func TestRunWatchesPanelsWithoutPolling(t *testing.T) {
	dir := t.TempDir()
	if _, err := decide.Open(dir); err != nil {
		t.Fatal(err)
	}
	r, w := io.Pipe()
	defer w.Close()
	out := &lockedBuffer{}
	done := make(chan error, 1)
	go func() {
		// Hour-long ledger interval proves panel updates arrive via fsnotify, not polling.
		done <- Run(Options{Dir: dir, In: r, Out: out, Interval: time.Hour})
	}()
	waitOutput(t, out, "(empty)")
	if _, err := w.Write([]byte{'g'}); err != nil {
		t.Fatal(err)
	}
	waitOutput(t, out, "go to:")
	if _, err := w.Write([]byte{'s'}); err != nil {
		t.Fatal(err)
	}
	waitOutput(t, out, "no status")

	store, err := decide.OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.Set(decide.Panel{
		ID:    "live",
		Title: "Live Panel",
		Kind:  "kv",
		Data:  json.RawMessage(`{"ok":true}`),
	}); err != nil {
		t.Fatal(err)
	}
	waitOutput(t, out, "Live Panel")

	if _, err := w.Write([]byte{'q'}); err != nil {
		t.Fatal(err)
	}
	waitRun(t, done)
}
