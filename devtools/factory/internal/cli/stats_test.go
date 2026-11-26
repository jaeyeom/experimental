package cli

import (
	"context"
	"encoding/json"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// TestStatsReadsOnlyTheStore runs stats with an executor that fails every
// command: stats must not call herdr, gh, git, or a tracker.
func TestStatsReadsOnlyTheStore(t *testing.T) {
	cfgPath, _ := writeVerifyConfig(t)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	day := time.Now().UTC().Add(-24 * time.Hour)
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := source.Item{Source: "jira:acme", ID: "PRJ-1", Title: "Ship it", Type: "Task", State: "To Do", Parent: "PRJ-9", Created: day, Updated: day}
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day, day); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "alice"); err != nil {
			t.Fatal(err)
		}
		if err := st.SetLastSync(ctx, "jira:acme", day); err != nil {
			t.Fatal(err)
		}
	})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)

	stdout, stderr, code := runCmd(t, "stats", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	for _, want := range []string{"Throughput", "Backlog", "Epics", "Flow", "PRJ-9"} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("stdout has no %q:\n%s", want, stdout)
		}
	}

	if !strings.Contains(stdout, "─ remaining items (end of each day)\n") || strings.Contains(stdout, "pace to reach 0") {
		t.Fatalf("default burndown legend:\n%s", stdout)
	}
	stdout, _, code = runCmd(t, "stats", "--burndown-pace", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "┄ pace to reach 0 by ") {
		t.Fatalf("--burndown-pace exit = %d:\n%s", code, stdout)
	}

	stdout, stderr, code = runCmd(t, "stats", "--json", "--split", "rule", "--since", "2w", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("json exit = %d, stderr = %s", code, stderr)
	}
	var top map[string]json.RawMessage
	if err := json.Unmarshal([]byte(stdout), &top); err != nil {
		t.Fatalf("stdout is not JSON: %v\n%s", err, stdout)
	}
	for _, section := range []string{"throughput", "backlog", "epics", "flow", "split"} {
		if _, ok := top[section]; !ok {
			t.Fatalf("JSON has no %q: %s", section, stdout)
		}
	}
	if calls := mock.GetCallHistory(); len(calls) != 0 {
		t.Fatalf("stats ran %d commands, want none", len(calls))
	}
}

func TestStatsUsageErrors(t *testing.T) {
	for _, args := range [][]string{
		{"stats", "--since", "soon"},
		{"stats", "--by", "month"},
		{"stats", "--split", "lane"},
		{"stats", "--bar-width", "-1"},
		{"stats", "extra"},
	} {
		_, stderr, code := runCmd(t, args...)
		if code != ExitUsage {
			t.Fatalf("%v: exit = %d, want %d; stderr = %s", args, code, ExitUsage, stderr)
		}
	}
}
