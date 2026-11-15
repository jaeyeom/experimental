package crons

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

func TestReadFixtureParsesJobsAndExpiry(t *testing.T) {
	created := time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)
	fired := time.Date(2026, 11, 2, 3, 4, 5, 0, time.UTC)
	longPrompt := strings.Repeat("a", 79) + "é" + "z"
	body := `{
		"tasks": [
			{
				"id": "overnight",
				"cron": "0 9 * * 1-5",
				"prompt": "` + longPrompt + `",
				"createdAt": ` + strconv.FormatInt(created.UnixMilli(), 10) + `,
				"lastFiredAt": ` + strconv.FormatInt(fired.UnixMilli(), 10) + `,
				"createdBySessionId": "sess-1",
				"createdByPid": 42,
				"recurring": true,
				"permanent": true
			},
			{
				"id": "note",
				"cron": "15 8 * * *",
				"prompt": "short",
				"createdAt": ` + strconv.FormatInt(created.UnixMilli(), 10) + `
			}
		]
	}`
	cwd := t.TempDir()
	path := writeTasks(t, cwd, body)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, 0o444); err != nil {
		t.Fatal(err)
	}

	jobs, statuses, warnings := Read(cwd, []Schedule{{ID: "overnight", Durable: true}}, Config{})
	if len(warnings) != 0 {
		t.Fatalf("warnings = %#v, want none", warnings)
	}
	if len(jobs) != 2 {
		t.Fatalf("jobs = %d, want 2", len(jobs))
	}
	got := jobs[0]
	if got.ID != "overnight" || got.Cron != "0 9 * * 1-5" || got.CreatedBySessionID != "sess-1" {
		t.Fatalf("job = %#v", got)
	}
	if got.Prompt != strings.Repeat("a", 79)+"é" {
		t.Fatalf("prompt = %q, want 80 characters", got.Prompt)
	}
	if got.FullPrompt != longPrompt {
		t.Fatalf("full prompt = %q, want the file text", got.FullPrompt)
	}
	if !got.CreatedAt.Equal(created) {
		t.Fatalf("createdAt = %s", got.CreatedAt)
	}
	if got.LastFiredAt == nil || !got.LastFiredAt.Equal(fired) {
		t.Fatalf("lastFiredAt = %v", got.LastFiredAt)
	}
	if !got.ExpiresAt.Equal(created.Add(DefaultLifetime)) {
		t.Fatalf("expiresAt = %s, want %s", got.ExpiresAt, created.Add(DefaultLifetime))
	}
	if jobs[1].ID != "note" || jobs[1].Prompt != "short" || jobs[1].LastFiredAt != nil {
		t.Fatalf("second job = %#v", jobs[1])
	}
	if len(statuses) != 1 || statuses[0].ID != "overnight" || statuses[0].Status != StatusPresent {
		t.Fatalf("statuses = %#v", statuses)
	}

	overridden, _, warnings := Read(cwd, nil, Config{Lifetime: 24 * time.Hour})
	if len(warnings) != 0 || len(overridden) != 2 {
		t.Fatalf("override jobs = %#v warnings = %#v", overridden, warnings)
	}
	if !overridden[0].ExpiresAt.Equal(created.Add(24 * time.Hour)) {
		t.Fatalf("overridden expiry = %s", overridden[0].ExpiresAt)
	}

	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(after) != string(before) {
		t.Fatal("scheduled_tasks.json changed")
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm() != 0o444 {
		t.Fatalf("mode = %o, want 0444", info.Mode().Perm())
	}
}

func TestReadMissingFile(t *testing.T) {
	cwd := t.TempDir()
	jobs, statuses, warnings := Read(cwd, []Schedule{
		{ID: "overnight", Durable: true},
		{ID: "ping", Durable: false},
	}, Config{})
	if len(jobs) != 0 {
		t.Fatalf("jobs = %#v, want empty", jobs)
	}
	if len(warnings) != 0 {
		t.Fatalf("warnings = %#v, want none", warnings)
	}
	if len(statuses) != 2 || statuses[0].Status != StatusMissing || statuses[1].Status != StatusUnverifiable {
		t.Fatalf("statuses = %#v", statuses)
	}
	entries, err := os.ReadDir(cwd)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 0 {
		t.Fatalf("cwd entries = %v, want none", entries)
	}
}

func TestReadMalformedFile(t *testing.T) {
	cwd := t.TempDir()
	writeTasks(t, cwd, `{`)
	jobs, statuses, warnings := Read(cwd, []Schedule{
		{ID: "overnight", Durable: true},
		{ID: "ping", Durable: false},
	}, Config{})
	if len(jobs) != 0 {
		t.Fatalf("jobs = %#v, want none", jobs)
	}
	if len(warnings) != 1 || !strings.Contains(warnings[0], "malformed") {
		t.Fatalf("warnings = %#v, want one malformed warning", warnings)
	}
	if len(statuses) != 2 || statuses[0].Status != StatusUnverifiable || statuses[1].Status != StatusUnverifiable {
		t.Fatalf("statuses = %#v, want unverifiable", statuses)
	}
}

func TestReadScheduleNotFound(t *testing.T) {
	created := time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)
	body := `{"tasks":[{"id":"kept","cron":"0 * * * *","prompt":"stay","createdAt":` + strconv.FormatInt(created.UnixMilli(), 10) + `}]}`
	cwd := t.TempDir()
	writeTasks(t, cwd, body)
	_, statuses, warnings := Read(cwd, []Schedule{
		{ID: "kept", Durable: false},
		{ID: "overnight", Durable: true},
		{ID: "ping", Durable: false},
	}, Config{})
	if len(warnings) != 0 {
		t.Fatalf("warnings = %#v", warnings)
	}
	want := []ScheduleStatus{
		{ID: "kept", Status: StatusPresent},
		{ID: "overnight", Status: StatusMissing},
		{ID: "ping", Status: StatusUnverifiable},
	}
	if len(statuses) != len(want) {
		t.Fatalf("statuses = %#v", statuses)
	}
	for i := range want {
		if statuses[i] != want[i] {
			t.Fatalf("statuses = %#v, want %#v", statuses, want)
		}
	}
}

func writeTasks(t *testing.T, cwd, body string) string {
	t.Helper()
	dir := filepath.Join(cwd, ".claude")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(dir, "scheduled_tasks.json")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}
