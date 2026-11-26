package cli

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func deferredFixture(t *testing.T) (string, string) {
	t.Helper()
	cfgPath, dbPath := rankingFixture(t)
	date := func(y int, m time.Month, d int) *time.Time {
		v := time.Date(y, m, d, 0, 0, 0, 0, time.UTC)
		return &v
	}
	later := rankItem("jira:acme", "PRJ-20", "Later", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"})
	later.NotBefore = date(2099, 1, 2)
	soon := rankItem("jira:acme", "PRJ-21", "Sooner", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"})
	soon.NotBefore = date(2098, 5, 1)
	past := rankItem("jira:acme", "PRJ-22", "Deferral passed", "Task", "To Do", "EPIC-A", "alice", 2, false, day1, []string{"agent-ready"})
	past.NotBefore = date(2000, 1, 1)
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.UpsertItems(context.Background(), "jira:acme", []source.Item{later, soon, past}); err != nil {
			t.Fatal(err)
		}
	})
	return cfgPath, dbPath
}

func TestNextDeferredSummary(t *testing.T) {
	cfgPath, dbPath := deferredFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if strings.Contains(stdout, "PRJ-20") || strings.Contains(stdout, "PRJ-21") {
		t.Fatalf("deferred item listed as ready:\n%s", stdout)
	}
	if !strings.Contains(stdout, "PRJ-22") {
		t.Fatalf("item whose date passed is not ready:\n%s", stdout)
	}
	if !strings.HasSuffix(stdout, "2 deferred (earliest 2098-05-01); factory next --deferred lists them\n") {
		t.Fatalf("missing summary:\n%s", stdout)
	}
}

func TestNextDeferredListing(t *testing.T) {
	cfgPath, dbPath := deferredFixture(t)
	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, stderr, code := runCmd(t, "next", "--deferred", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	lines := dataLines(stdout)
	if len(lines) != 2 || !strings.Contains(lines[0], "PRJ-21") || !strings.Contains(lines[0], "2098-05-01") ||
		!strings.Contains(lines[1], "PRJ-20") || !strings.Contains(lines[1], "2099-01-02") {
		t.Fatalf("listing:\n%s", stdout)
	}

	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, _, code = runCmd(t, "next", "--deferred", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, `"notBefore": "2098-05-01"`) {
		t.Fatalf("json exit = %d:\n%s", code, stdout)
	}

	useHerdrGH(t, rankWorkspaces, rankTabs, rankPRs)
	stdout, _, code = runCmd(t, "next", "--explain", "PRJ-20", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "FAIL not_before  deferred until 2099-01-02\n") {
		t.Fatalf("explain exit = %d:\n%s", code, stdout)
	}
}
