package tui

import (
	"context"
	"encoding/json"
	"errors"
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
		done <- Run(Options{
			Dir:       dir,
			In:        r,
			Out:       out,
			Interval:  time.Hour,
			BusRunner: func(context.Context, []string) ([]byte, error) { return nil, errors.New("no bus") },
		})
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

func TestStatusPanelKindsGoldenAndUnknownAndStale(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	if _, err := decide.Open(dir); err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 26, 15, 0, 0, 0, time.UTC)
	store, err := decide.OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}

	store.SetClock(func() time.Time { return now.Add(-2 * time.Minute) })
	fixtures := []decide.Panel{
		{
			ID:    "p1-text",
			Title: "Deploy Note",
			Kind:  decide.PanelKindText,
			Data:  json.RawMessage(`{"text":"All services healthy on main."}`),
		},
		{
			ID:    "p2-list",
			Title: "Checks",
			Kind:  decide.PanelKindList,
			Data:  json.RawMessage(`{"rows":[{"label":"lint","value":"passed","state":"ok"},{"label":"canary","value":"slow","state":"warn"},{"label":"smoke","value":"timeout","state":"fail"}]}`),
		},
		{
			ID:    "p3-gauge",
			Title: "Disk Usage",
			Kind:  decide.PanelKindGauge,
			Data:  json.RawMessage(`{"value":82,"max":100,"warn":75,"fail":95,"unit":"%"}`),
		},
		{
			ID:    "p4-chart",
			Title: "Quota Burn",
			Kind:  decide.PanelKindChart,
			Data:  json.RawMessage(`{"series":[{"t":"2026-11-26T12:00:00Z","v":20},{"t":"2026-11-26T13:00:00Z","v":45},{"t":"2026-11-26T14:00:00Z","v":70}],"unit":"%","limit":100,"projection":{"to":100,"at":"2026-11-26T16:30:00Z"}}`),
		},
		{
			ID:    "p5-table",
			Title: "Recent Runs",
			Kind:  decide.PanelKindTable,
			Data:  json.RawMessage(`{"columns":["Job","Status"],"rows":[["build-1","ok"],["build-2","ok"],["build-3","ok"],["build-4","ok"],["build-5","ok"],["build-6","fail"],["build-7","ok"]]}`),
		},
		{
			ID:    "p6-unknown",
			Title: "Custom Widget",
			Kind:  "SparkGrid",
			Data:  json.RawMessage(`{"alpha":1,"beta":"two"}`),
		},
	}
	for _, p := range fixtures {
		if _, err := store.Set(p); err != nil {
			t.Fatalf("set %s: %v", p.ID, err)
		}
	}

	store.SetClock(func() time.Time { return now.Add(-3 * time.Hour) })
	if _, err := store.Set(decide.Panel{
		ID:         "p7-stale",
		Title:      "Stale Feed",
		Kind:       decide.PanelKindText,
		Data:       json.RawMessage(`{"text":"Old snapshot"}`),
		StaleAfter: "1h",
	}); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return now }
	m.UTC = true
	m.width = 100
	m.height = 42
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "gs")
	view := m.View()

	checkGolden(t, "status_panels_100.golden", view)

	vis := visibleLine(view)
	for _, want := range []string{
		"All services healthy on main.",
		"[ok]", "[warn]", "[fail]",
		"82/100%",
		"latest 70%",
		"limit 100%",
		"projected 100% at",
		"+2 more",
		`"alpha": 1`,
		"updated 3h ago",
	} {
		if !strings.Contains(vis, want) {
			t.Errorf("status view missing %q:\n%s", want, vis)
		}
	}
	staleHeader := lineContaining(view, "Stale Feed")
	if !strings.Contains(staleHeader, "\x1b[2m") || !strings.Contains(staleHeader, "updated 3h ago") {
		t.Fatalf("stale panel not dimmed with age: %q", staleHeader)
	}
}

func TestStatusBuiltinPanelsPresentByDefault(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	now := time.Date(2026, 11, 26, 15, 0, 0, 0, time.UTC)
	board, err := decide.OpenStatus(dir)
	if err != nil {
		t.Fatal(err)
	}
	board.SetClock(func() time.Time { return now })
	if _, err := board.SetAgent("alpha", "working", "building"); err != nil {
		t.Fatal(err)
	}
	if _, err := board.SetResource("repo", decide.ResourceBusy, "held by alpha"); err != nil {
		t.Fatal(err)
	}

	ob := &fakeOutboxBackend{
		LocalBackend: &LocalBackend{Dir: dir},
		entries: []OutboxEntry{
			{ID: "ob-1", Op: "send", To: "alpha", Status: OutboxHeld, HoldReason: "bus down", WrittenAt: now},
		},
	}
	m := NewWithBackend(ob)
	m.Now = func() time.Time { return now }
	m.busHealth = busHealthReport{Daemon: "up", Transport: "up"}
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	press(t, m, "gs")
	view := visibleLine(m.View())
	for _, want := range []string{
		"Agents", "alpha", "working", "building",
		"Resources", "repo", "busy", "held by alpha",
		"Bus Health", "daemon", "up", "transport",
		"Outbox", "send", "alpha", "held: bus down",
	} {
		if !strings.Contains(view, want) {
			t.Errorf("status missing %q:\n%s", want, view)
		}
	}

	// Settings lists all 4 built-in panels enabled in default order.
	press(t, m, "g,")
	settingsView := visibleLine(m.View())
	for _, want := range []string{
		"Dashboard panels",
		"1. agents",
		"2. resources",
		"3. bus-health",
		"4. outbox",
		"enabled",
	} {
		if !strings.Contains(settingsView, want) {
			t.Errorf("settings missing %q:\n%s", want, settingsView)
		}
	}
}

func TestStatusSpaceJKToggleReorderPersistAndSettings(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	xdg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", xdg)
	dir := t.TempDir()
	now := time.Date(2026, 11, 26, 15, 0, 0, 0, time.UTC)

	board, err := decide.OpenStatus(dir)
	if err != nil {
		t.Fatal(err)
	}
	board.SetClock(func() time.Time { return now })
	if _, err := board.SetAgent("alpha", "working", "coding"); err != nil {
		t.Fatal(err)
	}
	store, err := decide.OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	store.SetClock(func() time.Time { return now })
	if _, err := store.Set(decide.Panel{
		ID:    "ci",
		Title: "CI Status",
		Kind:  decide.PanelKindText,
		Data:  json.RawMessage(`{"text":"green"}`),
	}); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return now }
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	press(t, m, "gs")
	// Initially: agents (0), ci (1). Move down to ci, move it up with K, then toggle off with space.
	press(t, m, "j")
	press(t, m, "K")
	view := visibleLine(m.View())
	if strings.Index(view, "CI Status") > strings.Index(view, "Agents") {
		t.Fatalf("K did not move CI Status above Agents:\n%s", view)
	}

	// Move back down to Agents (J moves CI Status down -> Agents above CI Status).
	press(t, m, "J")
	view = visibleLine(m.View())
	if strings.Index(view, "Agents") > strings.Index(view, "CI Status") {
		t.Fatalf("J did not move CI Status below Agents:\n%s", view)
	}

	// Move CI Status back to top and toggle it off with space.
	press(t, m, "K")
	press(t, m, " ")
	view = visibleLine(m.View())
	if !strings.Contains(view, "CI Status  (off)") || strings.Contains(view, "green") {
		t.Fatalf("space did not disable CI Status:\n%s", view)
	}

	// Simulate restart: new Model reading persisted config.json.
	m2 := New(dir)
	m2.Now = func() time.Time { return now }
	m2.width = 120
	m2.height = 30
	if err := m2.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m2, "gs")
	view2 := visibleLine(m2.View())
	if strings.Index(view2, "CI Status") > strings.Index(view2, "Agents") {
		t.Fatalf("reordered panel order did not persist across restart:\n%s", view2)
	}
	if !strings.Contains(view2, "CI Status  (off)") || strings.Contains(view2, "green") {
		t.Fatalf("disabled panel state did not persist across restart:\n%s", view2)
	}

	// Settings lists the panels with their persisted state and order.
	press(t, m2, "g,")
	sView := visibleLine(m2.View())
	ciRow := lineContaining(sView, "1. ci")
	agentsRow := lineContaining(sView, "2. agents")
	if !strings.Contains(ciRow, "1.") || !strings.Contains(ciRow, "disabled") {
		t.Fatalf("settings ci row = %q\n%s", ciRow, sView)
	}
	if !strings.Contains(agentsRow, "2.") || !strings.Contains(agentsRow, "enabled") {
		t.Fatalf("settings agents row = %q\n%s", agentsRow, sView)
	}
}
