package claudehud

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestResolvePluginDirPicksHighestSemver(t *testing.T) {
	t.Parallel()

	root := t.TempDir()
	configDir := filepath.Join(root, ".claude")
	base := filepath.Join(configDir, "plugins", "cache", "market", "claude-hud")
	for _, ver := range []string{"0.1.0", "0.2.0", "0.1.5"} {
		dir := filepath.Join(base, ver, "dist")
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, "usage-api.js"), []byte("// stub"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	// Non-semver dir should be ignored.
	junk := filepath.Join(base, "not-a-version", "dist")
	if err := os.MkdirAll(junk, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(junk, "usage-api.js"), []byte("// junk"), 0o600); err != nil {
		t.Fatal(err)
	}

	f := &Fetcher{ConfigDir: configDir}
	got, err := f.resolvePluginDir()
	if err != nil {
		t.Fatalf("resolvePluginDir: %v", err)
	}
	want := filepath.Join(base, "0.2.0")
	if got != want {
		t.Fatalf("got %q, want %q", got, want)
	}
}

func TestResolvePluginDirNotFound(t *testing.T) {
	t.Parallel()

	f := &Fetcher{ConfigDir: t.TempDir()}
	_, err := f.resolvePluginDir()
	if !errors.Is(err, ErrPluginNotFound) {
		t.Fatalf("want ErrPluginNotFound, got %v", err)
	}
}

func TestParseUsageJSON(t *testing.T) {
	t.Parallel()

	raw := []byte(`{
  "planName": "Team",
  "fiveHour": 2,
  "sevenDay": 12,
  "fiveHourResetAt": "2026-07-24T03:29:59.312Z",
  "sevenDayResetAt": "2026-07-27T22:59:59.312Z"
}`)
	d, err := parseUsageJSON(raw)
	if err != nil {
		t.Fatalf("parseUsageJSON: %v", err)
	}
	if d.PlanName != "Team" {
		t.Errorf("PlanName = %q", d.PlanName)
	}
	if d.FiveHour == nil || *d.FiveHour != 2 {
		t.Errorf("FiveHour = %v", d.FiveHour)
	}
	if d.SevenDay == nil || *d.SevenDay != 12 {
		t.Errorf("SevenDay = %v", d.SevenDay)
	}
	want5 := time.Date(2026, 7, 24, 3, 29, 59, 312000000, time.UTC)
	if d.FiveHourResetAt == nil || !d.FiveHourResetAt.Equal(want5) {
		t.Errorf("FiveHourResetAt = %v, want %v", d.FiveHourResetAt, want5)
	}
	want7 := time.Date(2026, 7, 27, 22, 59, 59, 312000000, time.UTC)
	if d.SevenDayResetAt == nil || !d.SevenDayResetAt.Equal(want7) {
		t.Errorf("SevenDayResetAt = %v, want %v", d.SevenDayResetAt, want7)
	}
}

func TestParseUsageJSONNulls(t *testing.T) {
	t.Parallel()

	d, err := parseUsageJSON([]byte(`{"planName":"Pro","fiveHour":null,"sevenDay":null}`))
	if err != nil {
		t.Fatalf("parseUsageJSON: %v", err)
	}
	if d.FiveHour != nil || d.SevenDay != nil {
		t.Fatalf("expected nil percents, got five=%v seven=%v", d.FiveHour, d.SevenDay)
	}
}
