package decide

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestKnownPanelKindValidators(t *testing.T) {
	dir := t.TempDir()
	store, err := OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	store.SetClock(func() time.Time { return now })

	validCases := []struct {
		id   string
		kind string
		data string
	}{
		{"t1", PanelKindText, `{"text":"hello world"}`},
		{"l1", PanelKindList, `{"rows":[{"label":"API","value":"200 OK","state":"ok"},{"label":"Queue","value":"12","state":"warn"}]}`},
		{"g1", PanelKindGauge, `{"value":72,"max":100,"warn":75,"fail":90,"unit":"%"}`},
		{"c1", PanelKindChart, `{"series":[{"t":"2026-11-26T10:00:00Z","v":20},{"t":"2026-11-26T11:00:00Z","v":45}],"unit":"%","limit":80,"projection":{"to":80,"at":"2026-11-26T14:00:00Z"}}`},
		{"tb1", PanelKindTable, `{"columns":["Service","Latency"],"rows":[["auth","12ms"],["web",42]]}`},
		{"u1", "CustomWidget", `{"anything":[1,2,3]}`},
	}
	for _, tc := range validCases {
		if _, err := store.Set(Panel{
			ID:    tc.id,
			Title: "Title " + tc.id,
			Kind:  tc.kind,
			Data:  json.RawMessage(tc.data),
		}); err != nil {
			t.Fatalf("Set(%s, %s) failed: %v", tc.kind, tc.data, err)
		}
	}

	invalidCases := []struct {
		kind string
		data string
	}{
		{PanelKindText, `{}`},
		{PanelKindText, `{"text":123}`},
		{PanelKindList, `{}`},
		{PanelKindList, `{"rows":[{"label":"a","value":"b","state":"bogus"}]}`},
		{PanelKindList, `{"rows":[{"label":"a"}]}`},
		{PanelKindGauge, `{"value":10}`},
		{PanelKindGauge, `{"value":10,"max":0}`},
		{PanelKindGauge, `{"value":"10","max":100}`},
		{PanelKindChart, `{}`},
		{PanelKindChart, `{"series":[{"t":"","v":1}]}`},
		{PanelKindChart, `{"series":[{"t":"2026-11-26T10:00:00Z","v":1}],"projection":{"to":100,"at":"not-rfc3339"}}`},
		{PanelKindTable, `{"columns":["a"]}`},
		{PanelKindTable, `{"columns":["a"],"rows":[[{"nested":1}]]}`},
	}
	for _, tc := range invalidCases {
		if _, err := store.Set(Panel{
			ID:    "bad-case",
			Title: "Bad",
			Kind:  tc.kind,
			Data:  json.RawMessage(tc.data),
		}); err == nil {
			t.Fatalf("Set(%s, %s) succeeded, want validation error", tc.kind, tc.data)
		}
	}
}

func TestDashboardConfigValidationAndPersistence(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("{\"keepMe\": true}\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	wantPanels := []DashboardPanelConfig{
		NewDashboardPanelConfig("ci", true, 0),
		NewDashboardPanelConfig("agents", false, 1),
	}
	cfg, err := SetConfigValueAt(path, "ui.dashboard.panels", wantPanels)
	if err != nil {
		t.Fatalf("SetConfigValueAt: %v", err)
	}
	if len(cfg.UI.Dashboard.Panels) != 2 || cfg.UI.Dashboard.Panels[0].ID != "ci" || cfg.UI.Dashboard.Panels[1].IsEnabled() {
		t.Fatalf("saved panels = %+v", cfg.UI.Dashboard.Panels)
	}

	reloaded, err := LoadConfigAt(path)
	if err != nil {
		t.Fatalf("LoadConfigAt: %v", err)
	}
	if len(reloaded.UI.Dashboard.Panels) != 2 || reloaded.UI.Dashboard.Panels[1].ID != "agents" || reloaded.UI.Dashboard.Panels[1].IsEnabled() {
		t.Fatalf("reloaded panels = %+v", reloaded.UI.Dashboard.Panels)
	}

	// Reject bad ID and duplicate IDs.
	if _, err := SetConfigValueAt(path, "ui.dashboard.panels", []DashboardPanelConfig{
		NewDashboardPanelConfig("Bad_ID", true, 0),
	}); err == nil {
		t.Fatal("expected error for invalid panel id")
	}
	if _, err := SetConfigValueAt(path, "ui.dashboard.panels", []DashboardPanelConfig{
		NewDashboardPanelConfig("agents", true, 0),
		NewDashboardPanelConfig("agents", false, 1),
	}); err == nil || !strings.Contains(err.Error(), "duplicate") {
		t.Fatalf("expected duplicate error, got %v", err)
	}
}

func TestResolveToggleAndMoveDashboardPanels(t *testing.T) {
	available := []string{"agents", "resources", "bus-health", "outbox", "ci", "deploy"}
	configured := []DashboardPanelConfig{
		NewDashboardPanelConfig("deploy", true, 10),
		NewDashboardPanelConfig("agents", false, 2),
	}
	resolved := ResolveDashboardPanels(configured, available)
	wantOrder := []string{"agents", "deploy", "resources", "bus-health", "outbox", "ci"}
	if len(resolved) != len(wantOrder) {
		t.Fatalf("len = %d, want %d: %+v", len(resolved), len(wantOrder), resolved)
	}
	for i, wantID := range wantOrder {
		if resolved[i].ID != wantID {
			t.Errorf("resolved[%d].ID = %q, want %q", i, resolved[i].ID, wantID)
		}
		wantEnabled := wantID != "agents"
		if resolved[i].Enabled != wantEnabled {
			t.Errorf("resolved[%d].Enabled (%s) = %v, want %v", i, wantID, resolved[i].Enabled, wantEnabled)
		}
	}

	toggled := ToggleDashboardPanel(configured, available, "ci")
	res2 := ResolveDashboardPanels(toggled, available)
	if res2[5].ID != "ci" || res2[5].Enabled {
		t.Fatalf("after toggle ci = %+v", res2[5])
	}

	// Move deploy down among visible ["agents", "deploy", "ci"] -> shifts deploy past ci.
	moved, ok := MoveDashboardPanel(toggled, available, []string{"agents", "deploy", "ci"}, "deploy", 1)
	if !ok {
		t.Fatal("MoveDashboardPanel failed")
	}
	res3 := ResolveDashboardPanels(moved, available)
	if res3[4].ID != "ci" || res3[5].ID != "deploy" {
		t.Fatalf("after move deploy down = %+v", res3)
	}
}
