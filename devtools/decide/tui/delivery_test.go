package tui

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestInboxShowsNotDeliveredAndRetriesWithTheSameKey(t *testing.T) {
	dir := t.TempDir()
	cfgDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfgDir)
	logPath := filepath.Join(dir, "keys")
	installExitNotifier(t, logPath)
	writeTUINotifyConfig(t, cfgDir, "notifier", "pane-9")

	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question:   "keep me",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		State:      decide.StateOpen,
		NotifyPane: "pane-9",
		Options:    []decide.Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}},
		CreatedAt:  clock,
	})
	m := New(dir)
	m.Now = func() time.Time { return clock }
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r1")
	press(t, m, "ga")
	row := visibleRow(t, m.View(), item.ID)
	if !strings.Contains(row, "not delivered") || !strings.Contains(row, "exit status 1") {
		t.Fatalf("list row = %q", row)
	}
	press(t, m, "\r")
	header := visibleLine(m.View())
	if !strings.Contains(header, "not delivered") || !strings.Contains(header, "exit status 1") {
		t.Fatalf("header = %q", header)
	}
	keys := notifyLog(t, logPath)
	if len(keys) != 1 || keys[0] == "" {
		t.Fatalf("keys = %#v", keys)
	}

	clock = clock.Add(decide.DeliveryBackoff(1))
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	again := notifyLog(t, logPath)
	if len(again) != 2 || again[0] != again[1] {
		t.Fatalf("retry keys = %#v", again)
	}
}

func installExitNotifier(t *testing.T, logPath string) {
	t.Helper()
	script := "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '" + logPath + "'\nexit 1\n"
	bin := filepath.Join(t.TempDir(), "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(bin, "notifier"), []byte(script), 0o755); err != nil { //nolint:gosec // test sentinel script
		t.Fatal(err)
	}
	t.Setenv("PATH", bin+string(os.PathListSeparator)+os.Getenv("PATH"))
}
