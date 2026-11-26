package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestLogsOrderAndLimit(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	exampleAt := time.Date(2026, 10, 29, 14, 3, 0, 0, time.Local)
	older := exampleAt.Add(-time.Hour)
	newer := exampleAt.Add(time.Hour)
	summary := "agent PRJ-2366 (w3:t43) is working"
	seedLogs(t, dbPath,
		store.Run{StartedAt: older, Command: "sync", Outcome: "ok", DetailJSON: `[]`},
		store.Run{StartedAt: exampleAt, Command: "tick", Outcome: "gated", DetailJSON: `{"summary":"` + summary + `"}`},
		store.Run{StartedAt: newer, Command: "start", Outcome: "error", DetailJSON: `{"summary":"refused"}`},
	)
	wantAll := logLine(newer, "start", "error", "refused") +
		logLine(exampleAt, "tick", "gated", summary) +
		logLine(older, "sync", "ok", "ok")
	stdout, stderr, code := runCmd(t, "logs", "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	if stdout != wantAll {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, wantAll)
	}
	if !strings.Contains(stdout, "2026-10-29 14:03  tick   gated   agent PRJ-2366 (w3:t43) is working\n") {
		t.Fatalf("example line missing:\n%s", stdout)
	}

	stdout, stderr, code = runCmd(t, "logs", "-n", "2", "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("n=2 exit = %d, stderr = %q", code, stderr)
	}
	wantTwo := logLine(newer, "start", "error", "refused") + logLine(exampleAt, "tick", "gated", summary)
	if stdout != wantTwo {
		t.Fatalf("n=2 stdout =\n%s\nwant =\n%s", stdout, wantTwo)
	}

	stdout, stderr, code = runCmd(t, "logs", "-n", "1", "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != logLine(newer, "start", "error", "refused") {
		t.Fatalf("n=1 exit = %d stdout =\n%s\nstderr = %q", code, stdout, stderr)
	}
}

func TestLogsDefaultLimit(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	base := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	runs := make([]store.Run, 21)
	for i := range runs {
		runs[i] = store.Run{
			StartedAt:  base.Add(time.Duration(i) * time.Minute),
			Command:    fmt.Sprintf("c%02d", i),
			Outcome:    "ok",
			DetailJSON: `{}`,
		}
	}
	seedLogs(t, dbPath, runs...)
	stdout, stderr, code := runCmd(t, "logs", "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	lines := strings.Split(strings.TrimSuffix(stdout, "\n"), "\n")
	if len(lines) != 20 {
		t.Fatalf("lines = %d, want 20\n%s", len(lines), stdout)
	}
	if !strings.Contains(lines[0], "c20") || !strings.Contains(lines[19], "c01") || strings.Contains(stdout, "c00") {
		t.Fatalf("window =\n%s", stdout)
	}
}

func TestLogsJSON(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	exampleAt := time.Date(2026, 10, 29, 14, 3, 0, 0, time.UTC)
	older := exampleAt.Add(-time.Minute)
	newer := exampleAt.Add(time.Minute)
	details := []string{`[]`, `{"summary":"agent PRJ-2366 (w3:t43) is working"}`, `{"summary":"refused"}`}
	seedLogs(t, dbPath,
		store.Run{StartedAt: older, Command: "sync", Outcome: "ok", DetailJSON: details[0]},
		store.Run{StartedAt: exampleAt, Command: "tick", Outcome: "gated", DetailJSON: details[1]},
		store.Run{StartedAt: newer, Command: "start", Outcome: "error", DetailJSON: details[2]},
	)
	stdout, stderr, code := runCmd(t, "logs", "--json", "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	if !json.Valid([]byte(stdout)) {
		t.Fatalf("invalid json:\n%s", stdout)
	}
	var got []struct {
		StartedAt  time.Time `json:"started_at"` //nolint:tagliatelle // documented factory JSON output field
		Command    string    `json:"command"`
		Outcome    string    `json:"outcome"`
		DetailJSON string    `json:"detail_json"` //nolint:tagliatelle // documented factory JSON output field
	}
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(got) != 3 {
		t.Fatalf("rows = %d\n%s", len(got), stdout)
	}
	wantCmd := []string{"start", "tick", "sync"}
	wantAt := []time.Time{newer, exampleAt, older}
	wantDetail := []string{details[2], details[1], details[0]}
	for i := range got {
		if got[i].Command != wantCmd[i] || got[i].DetailJSON != wantDetail[i] || !got[i].StartedAt.Equal(wantAt[i]) {
			t.Fatalf("row %d = %#v", i, got[i])
		}
	}
	if got[0].Outcome != "error" || got[1].Outcome != "gated" || got[2].Outcome != "ok" {
		t.Fatalf("outcomes = %#v", got)
	}

	stdout, stderr, code = runCmd(t, "logs", "--json", "-n", "1", "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("n=1 exit = %d, stderr = %q", code, stderr)
	}
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("json n=1: %v\n%s", err, stdout)
	}
	if len(got) != 1 || got[0].Command != "start" {
		t.Fatalf("n=1 rows = %#v", got)
	}
}

func TestLogsJSONRawDetail(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedLogs(t, dbPath, store.Run{
		StartedAt:  time.Date(2026, 10, 29, 14, 3, 0, 0, time.UTC),
		Command:    "sync",
		Outcome:    "ok",
		DetailJSON: `not json`,
	})
	stdout, stderr, code := runCmd(t, "logs", "--json", "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	var got []struct {
		DetailJSON string `json:"detail_json"` //nolint:tagliatelle // documented factory JSON output field
	}
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	if len(got) != 1 || got[0].DetailJSON != "not json" {
		t.Fatalf("rows = %#v", got)
	}
}

func TestLogsPath(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	stdout, stderr, code := runCmd(t, "logs", "--path", "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != dbPath+"\n" {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	if _, err := os.Stat(dbPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("path created the database: %v", err)
	}

	stdout, stderr, code = runCmd(t, "logs", "--path", "--json", "-n", "1", "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != dbPath+"\n" {
		t.Fatalf("with json: exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}

	want := store.DefaultPath(config.OSEnv())
	stdout, stderr, code = runCmd(t, "logs", "--path")
	if code != ExitOK || stderr != "" || stdout != want+"\n" {
		t.Fatalf("default path exit = %d stdout = %q stderr = %q, want %q", code, stdout, stderr, want)
	}
}

func TestLogsSummary(t *testing.T) {
	at := time.Date(2026, 10, 29, 14, 3, 0, 0, time.Local)
	tests := []struct {
		name    string
		detail  string
		outcome string
		want    string
		wantErr string
	}{
		{name: "summary", detail: `{"summary":"agent working"}`, outcome: "gated", want: "agent working"},
		{name: "absent", detail: `{"source":"jira:acme"}`, outcome: "ok", want: "ok"},
		{name: "empty detail", detail: "", outcome: "partial", want: "partial"},
		{name: "array", detail: `[{"source":"jira:acme"}]`, outcome: "ok", want: "ok"},
		{name: "null", detail: `null`, outcome: "ok", want: "ok"},
		{name: "not json", detail: `not-json`, outcome: "ok", wantErr: `expected detail_json to be JSON, found "not-json"`},
		{name: "number", detail: `{"summary":1}`, outcome: "ok", wantErr: "expected detail_json summary to be a string, found 1"},
		{name: "bool", detail: `{"summary":true}`, outcome: "ok", wantErr: "expected detail_json summary to be a string, found true"},
		{name: "null summary", detail: `{"summary":null}`, outcome: "ok", wantErr: "expected detail_json summary to be a string, found null"},
		{name: "newline", detail: `{"summary":"a\nb"}`, outcome: "ok", wantErr: "expected a one-line summary"},
		{name: "extra", detail: `{}{}`, outcome: "ok", wantErr: "expected detail_json to be one JSON value, found extra data"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			dbPath := filepath.Join(t.TempDir(), "factory.db")
			seedLogs(t, dbPath, store.Run{
				StartedAt: at, Command: "tick", Outcome: tc.outcome, DetailJSON: tc.detail,
			})
			stdout, stderr, code := runCmd(t, "logs", "--db", dbPath)
			if tc.wantErr != "" {
				if code != ExitError || !strings.Contains(stderr, tc.wantErr) {
					t.Fatalf("exit = %d, stderr = %q", code, stderr)
				}
				return
			}
			if code != ExitOK || stderr != "" || stdout != logLine(at, "tick", tc.outcome, tc.want) {
				t.Fatalf("exit = %d stdout =\n%s\nstderr = %q", code, stdout, stderr)
			}
		})
	}
}

func TestLogsEmpty(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	stdout, stderr, code := runCmd(t, "logs", "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "" {
		t.Fatalf("text exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	stdout, stderr, code = runCmd(t, "logs", "--json", "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "[]\n" {
		t.Fatalf("json exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
}

func TestLogsNoConfig(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	missing := filepath.Join(t.TempDir(), "missing.toml")
	seedLogs(t, dbPath, store.Run{
		StartedAt:  time.Date(2026, 10, 29, 14, 3, 0, 0, time.UTC),
		Command:    "sync",
		Outcome:    "ok",
		DetailJSON: `{"summary":"done"}`,
	})
	stdout, stderr, code := runCmd(t, "logs", "--config", missing, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "done") {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
}

func TestLogsUsage(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "extra", args: []string{"logs", "extra", "--db", dbPath}, want: "found 1"},
		{name: "n zero", args: []string{"logs", "-n", "0", "--db", dbPath}, want: "found 0"},
		{name: "n negative", args: []string{"logs", "-n", "-1", "--db", dbPath}, want: "found -1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, stderr, code := runCmd(t, tc.args...)
			if code != ExitUsage || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, tc.want) {
				t.Fatalf("exit = %d, stderr = %q", code, stderr)
			}
		})
	}
}

func seedLogs(t *testing.T, dbPath string, runs ...store.Run) {
	t.Helper()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		for _, run := range runs {
			if err := st.AppendRun(ctx, run); err != nil {
				t.Fatal(err)
			}
		}
	})
}

func logLine(at time.Time, command, outcome, summary string) string {
	return fmt.Sprintf("%s  %s   %s   %s\n", at.Local().Format(logTimeLayout), command, outcome, summary)
}
