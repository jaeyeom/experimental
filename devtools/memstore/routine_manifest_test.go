package memstore

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestParseRoutineManifestDefaultsAndOverrides(t *testing.T) {
	minimal := `
id = "daily-digest"
vault = "personal"
profile = "automation"
schedule = "FREQ=DAILY;BYHOUR=8;BYMINUTE=0"

[handler]
command = ["/usr/local/bin/digest", "--send"]
`
	m, err := ParseRoutineManifest([]byte(minimal))
	if err != nil {
		t.Fatalf("ParseRoutineManifest minimal: %v", err)
	}
	if m.Name != "daily-digest" || m.Residency != "local" || m.Timezone != "UTC" || m.Timeout != "2m" || m.Idempotency != "date" || !m.Enabled || m.Handler.OutputFormat != "none" {
		t.Errorf("unexpected defaults: %+v", m)
	}
	if d, err := m.TimeoutDuration(); err != nil || d != 2*time.Minute {
		t.Errorf("TimeoutDuration = %v, %v, want 2m", d, err)
	}

	full := `
id = "inbox_sync"
name = "Inbox Sync"
vault = "work"
profile = "worker"
residency = "gcp"
schedule = "@every 15m"
timezone = "America/Los_Angeles"
timeout = "45s"
idempotency = "date-hour"
enabled = false

[handler]
command = ["memstore-sync"]
output_format = "json"
`
	m2, err := ParseRoutineManifest([]byte(full))
	if err != nil {
		t.Fatalf("ParseRoutineManifest full: %v", err)
	}
	if m2.Enabled || m2.Residency != "gcp" || m2.Timezone != "America/Los_Angeles" || m2.Idempotency != "date-hour" || m2.Handler.OutputFormat != "json" {
		t.Errorf("unexpected parsed manifest: %+v", m2)
	}
}

func TestParseRoutineManifestValidationErrors(t *testing.T) {
	cases := []struct {
		name    string
		toml    string
		wantSub string
	}{
		{
			name:    "invalid id uppercase",
			toml:    "id = \"BadID\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "invalid routine id",
		},
		{
			name:    "invalid id spaces",
			toml:    "id = \"bad id\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "invalid routine id",
		},
		{
			name:    "missing vault",
			toml:    "id = \"ok\"\nprofile = \"p\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "vault is required",
		},
		{
			name:    "missing profile",
			toml:    "id = \"ok\"\nvault = \"v\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "profile is required",
		},
		{
			name:    "missing command",
			toml:    "id = \"ok\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"@every 5m\"\n[handler]\ncommand = []\n",
			wantSub: "handler.command",
		},
		{
			name:    "bad timezone",
			toml:    "id = \"ok\"\nvault = \"v\"\nprofile = \"p\"\ntimezone = \"Invalid/Zone\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "load timezone",
		},
		{
			name:    "bad schedule",
			toml:    "id = \"ok\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"not-a-schedule\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "invalid schedule",
		},
		{
			name:    "bad residency",
			toml:    "id = \"ok\"\nvault = \"v\"\nprofile = \"p\"\nresidency = \"aws\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "invalid residency",
		},
		{
			name:    "bad idempotency",
			toml:    "id = \"ok\"\nvault = \"v\"\nprofile = \"p\"\nidempotency = \"weekly\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "invalid idempotency",
		},
		{
			name:    "bad timeout",
			toml:    "id = \"ok\"\nvault = \"v\"\nprofile = \"p\"\ntimeout = \"0s\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\n",
			wantSub: "invalid timeout",
		},
		{
			name:    "bad output format",
			toml:    "id = \"ok\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"echo\"]\noutput_format = \"yaml\"\n",
			wantSub: "invalid handler.output_format",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ParseRoutineManifest([]byte(tc.toml))
			if err == nil || !strings.Contains(err.Error(), tc.wantSub) {
				t.Errorf("ParseRoutineManifest error = %v, want substring %q", err, tc.wantSub)
			}
		})
	}
}

func TestLoadRoutineManifests(t *testing.T) {
	// Missing directory returns empty slice without error.
	empty, err := LoadRoutineManifests(filepath.Join(t.TempDir(), "missing"))
	if err != nil {
		t.Fatalf("LoadRoutineManifests missing dir: %v", err)
	}
	if len(empty) != 0 {
		t.Errorf("expected empty slice, got %d", len(empty))
	}

	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "b.toml"), []byte("id = \"b\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"@every 1h\"\n[handler]\ncommand = [\"b\"]\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "a.toml"), []byte("id = \"a\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"a\"]\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "README.md"), []byte("ignored"), 0o600); err != nil {
		t.Fatal(err)
	}

	list, err := LoadRoutineManifests(dir)
	if err != nil {
		t.Fatalf("LoadRoutineManifests: %v", err)
	}
	if len(list) != 2 || list[0].ID != "a" || list[1].ID != "b" {
		t.Errorf("loaded = %+v, want [a, b]", list)
	}

	// Duplicate IDs across files must fail.
	if err := os.WriteFile(filepath.Join(dir, "c.toml"), []byte("id = \"a\"\nvault = \"v\"\nprofile = \"p\"\nschedule = \"@every 5m\"\n[handler]\ncommand = [\"c\"]\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := LoadRoutineManifests(dir); err == nil || !strings.Contains(err.Error(), "duplicate routine id") {
		t.Errorf("duplicate ID error = %v, want duplicate routine id", err)
	}
}
