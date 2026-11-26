package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/syncer"
)

func TestFormatResult(t *testing.T) {
	since := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	tests := []struct {
		name string
		res  syncer.Result
		want string
	}{
		{
			name: "full",
			res:  syncer.Result{Source: "jira:acme", Full: true, Items: 312, Removed: 4},
			want: "jira:acme  full  312 items (4 removed)",
		},
		{
			name: "incremental",
			res:  syncer.Result{Source: "jira:acme", Items: 7, Since: since},
			want: "jira:acme  incremental  7 items (0 removed) since 2026-10-29T03:12:00Z",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := formatResult(tc.res)
			if got != tc.want {
				t.Fatalf("format = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestSyncCommandFullThenIncremental(t *testing.T) {
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	newer := updated.Add(time.Hour)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": item("jira:acme", "PRJ-1", updated),
		"PRJ-2": item("jira:acme", "PRJ-2", newer),
	}}
	useSources(t, map[string]*fake.Source{"acme": src})
	cfgPath := writeSyncConfig(t, "acme")
	dbPath := filepath.Join(t.TempDir(), "factory.db")

	stdout, stderr, code := runSyncCmd(t, cfgPath, dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if stdout != "jira:acme  full  2 items (0 removed)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if me := storedMe(t, dbPath, "jira:acme"); me != "acct" {
		t.Fatalf("Me = %q, want acct", me)
	}
	run := latestRun(t, dbPath)
	if run.Command != "sync" || run.Outcome != "ok" {
		t.Fatalf("run = %+v", run)
	}
	details := decodeDetails(t, run.DetailJSON)
	if len(details) != 1 || details[0].Mode != "full" || details[0].Items != 2 || details[0].Source != "jira:acme" {
		t.Fatalf("detail = %+v", details)
	}

	stdout, stderr, code = runSyncCmd(t, cfgPath, dbPath)
	if code != ExitOK {
		t.Fatalf("second exit = %d, stderr = %s", code, stderr)
	}
	want := "jira:acme  incremental  1 items (0 removed) since " + newer.Format(time.RFC3339) + "\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
	got := syncArgs(src)
	if len(got) != 2 || got[0] != sinceArg(time.Time{}) || got[1] != sinceArg(newer) {
		t.Fatalf("since args = %#v", got)
	}
	run = latestRun(t, dbPath)
	if run.Outcome != "ok" {
		t.Fatalf("second outcome = %s", run.Outcome)
	}
	details = decodeDetails(t, run.DetailJSON)
	if len(details) != 1 || details[0].Mode != "incremental" || details[0].Since != newer.Format(time.RFC3339) {
		t.Fatalf("second detail = %+v", details)
	}
}

func TestSyncFullFlagRemovesDroppedItem(t *testing.T) {
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	src := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": item("jira:acme", "PRJ-1", updated),
		"PRJ-2": item("jira:acme", "PRJ-2", updated.Add(time.Minute)),
	}}
	useSources(t, map[string]*fake.Source{"acme": src})
	cfgPath := writeSyncConfig(t, "acme")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	if _, stderr, code := runSyncCmd(t, cfgPath, dbPath); code != ExitOK {
		t.Fatalf("seed exit = %d, stderr = %s", code, stderr)
	}
	delete(src.Items, "PRJ-2")

	stdout, stderr, code := runSyncCmd(t, cfgPath, dbPath, "--full")
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "jira:acme  full  1 items (1 removed)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	st := reopen(t, dbPath)
	if _, err := st.Item(context.Background(), "jira:acme", "PRJ-2"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("dropped item error = %v, want not found", err)
	}
	if _, err := st.Item(context.Background(), "jira:acme", "PRJ-1"); err != nil {
		t.Fatalf("kept item: %v", err)
	}
}

func TestSyncPartialExitsAndRecordsRun(t *testing.T) {
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	acme := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": item("jira:acme", "PRJ-1", updated),
	}}
	cub := &fake.Source{Account: "acct", FailOn: map[string]error{"Sync": errors.New("jira down")}}
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
	cfgPath := writeSyncConfig(t, "acme", "cub")
	dbPath := filepath.Join(t.TempDir(), "factory.db")

	stdout, stderr, code := runSyncCmd(t, cfgPath, dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
	}
	if !strings.Contains(stderr, "jira:cub") || !strings.Contains(stderr, "jira down") {
		t.Fatalf("stderr = %q", stderr)
	}
	want := "jira:acme  full  1 items (0 removed)\njira:cub  error: jira down\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
	run := latestRun(t, dbPath)
	if run.Command != "sync" || run.Outcome != "partial" {
		t.Fatalf("run = %+v", run)
	}
	details := decodeDetails(t, run.DetailJSON)
	if len(details) != 2 || details[0].Error != "" || details[1].Error != "jira down" {
		t.Fatalf("detail = %+v", details)
	}
	st := reopen(t, dbPath)
	if _, err := st.Item(context.Background(), "jira:acme", "PRJ-1"); err != nil {
		t.Fatalf("ok source was not stored: %v", err)
	}
}

func TestSyncSourceFilter(t *testing.T) {
	updated := time.Date(2026, 10, 29, 3, 12, 0, 0, time.UTC)
	acme := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-1": item("jira:acme", "PRJ-1", updated),
	}}
	cub := &fake.Source{Account: "acct", Items: map[string]source.Item{
		"PRJ-2": item("jira:cub", "PRJ-2", updated),
	}}
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
	cfgPath := writeSyncConfig(t, "acme", "cub")
	dbPath := filepath.Join(t.TempDir(), "factory.db")

	stdout, stderr, code := runSyncCmd(t, cfgPath, dbPath, "--source", "jira:acme")
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "jira:acme  full  1 items (0 removed)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if len(cub.Calls) != 0 {
		t.Fatalf("filtered source was called: %#v", cub.Calls)
	}

	stdout, stderr, code = runSyncCmd(t, cfgPath, dbPath, "--source", "acme")
	if code != ExitError {
		t.Fatalf("unknown source exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stderr, "jira:acme") || !strings.Contains(stderr, `found "acme"`) {
		t.Fatalf("stderr = %q", stderr)
	}
	if stdout != "" {
		t.Fatalf("unknown source stdout = %q", stdout)
	}
}

func useSources(t *testing.T, srcs map[string]*fake.Source) {
	t.Helper()
	prev := newSource
	newSource = func(cfg *config.Source, _ Deps) (source.Source, error) {
		if cfg == nil {
			return nil, errors.New("source: expected a config source, found nil")
		}
		src, ok := srcs[cfg.Name]
		if !ok {
			return nil, fmt.Errorf("source: expected one of %s, found %q", sourceNames(srcs), cfg.Name)
		}
		return src, nil
	}
	t.Cleanup(func() { newSource = prev })
}

func sourceNames(srcs map[string]*fake.Source) string {
	names := make([]string, 0, len(srcs))
	for name := range srcs {
		names = append(names, name)
	}
	return strings.Join(names, ", ")
}

func writeSyncConfig(t *testing.T, names ...string) string {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var body strings.Builder
	for _, name := range names {
		fmt.Fprintf(&body, `
[[source]]
name = %q
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = %q
jql = "project = PRJ"
claim_status = "In Progress"
cwd = %q
base = "main"
start_prompt = "work {id}"
`, name, token, cwd)
	}
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body.String()), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func runSyncCmd(t *testing.T, cfgPath, dbPath string, extra ...string) (string, string, int) {
	t.Helper()
	args := []string{"sync", "--config", cfgPath, "--db", dbPath}
	args = append(args, extra...)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), args, &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}

func latestRun(t *testing.T, dbPath string) store.Run {
	t.Helper()
	st := reopen(t, dbPath)
	runs, err := st.Runs(context.Background(), 1)
	if err != nil {
		t.Fatalf("Runs: %v", err)
	}
	if len(runs) != 1 {
		t.Fatalf("runs = %d, want 1", len(runs))
	}
	return runs[0]
}

func reopen(t *testing.T, dbPath string) *store.Store {
	t.Helper()
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	})
	return st
}

func decodeDetails(t *testing.T, raw string) []syncDetail {
	t.Helper()
	var details []syncDetail
	if err := json.Unmarshal([]byte(raw), &details); err != nil {
		t.Fatalf("detail %s: %v", raw, err)
	}
	return details
}

func storedMe(t *testing.T, dbPath, sourceName string) string {
	t.Helper()
	me, err := reopen(t, dbPath).Me(context.Background(), sourceName)
	if err != nil {
		t.Fatalf("Me: %v", err)
	}
	return me
}

func item(src, id string, updated time.Time) source.Item {
	return source.Item{Source: src, ID: id, Title: id, Created: updated, Updated: updated}
}

func syncArgs(src *fake.Source) []string {
	var out []string
	for _, call := range src.Calls {
		if call.Method == "Sync" && len(call.Args) > 0 {
			out = append(out, call.Args[0])
		}
	}
	return out
}

func sinceArg(t time.Time) string {
	return t.UTC().Format(time.RFC3339Nano)
}
