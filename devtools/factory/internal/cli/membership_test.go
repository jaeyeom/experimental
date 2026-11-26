package cli

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

// twoReadyFake returns a source with ready items PRJ-1 and PRJ-2.
func twoReadyFake() *fake.Source {
	src := readyFake("jira:acme", "PRJ-1")
	src.Items["PRJ-2"] = rankItem("jira:acme", "PRJ-2", "Second", "Task", "To Do", "", "alice", 2, false, day1, []string{"agent-ready"})
	return src
}

func TestIncrementalSyncRemovesItemThatLeftQuery(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := twoReadyFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "0s", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")

	if stdout, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath); code != ExitOK {
		t.Fatalf("full sync exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	// PRJ-2 leaves the query without a newer updated time.
	src.Query = func(it source.Item) bool { return it.ID != "PRJ-2" }
	withNow(t, now.Add(time.Minute))

	stdout, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("incremental exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	want := "jira:acme  incremental  1 items (1 removed) since " + day1.Format(time.RFC3339) + "\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
	details := decodeDetails(t, latestRun(t, dbPath).DetailJSON)
	if len(details) != 1 || details[0].Removed != 1 || details[0].Warning != "" {
		t.Fatalf("detail = %+v", details)
	}

	stdout, stderr, code = runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("next exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "PRJ-1") || strings.Contains(stdout, "PRJ-2") {
		t.Fatalf("next stdout = %q, want PRJ-1 and not PRJ-2", stdout)
	}
}

func TestIncrementalSyncWarnsWhenIDsFail(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := twoReadyFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	cfgPath := writeAgeConfig(t, "0s", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	if _, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath); code != ExitOK {
		t.Fatalf("full sync exit = %d, stderr = %q", code, stderr)
	}
	src.Query = func(it source.Item) bool { return it.ID != "PRJ-2" }
	src.FailOn = map[string]error{"IDs": errors.New("search down")}

	stdout, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	if !strings.Contains(stdout, "(0 removed)") {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.HasPrefix(stderr, "warning: sync jira:acme: membership check failed, removed nothing: search down") {
		t.Fatalf("stderr = %q", stderr)
	}
	run := latestRun(t, dbPath)
	details := decodeDetails(t, run.DetailJSON)
	if run.Outcome != "ok" || len(details) != 1 || !strings.Contains(details[0].Warning, "search down") {
		t.Fatalf("run = %+v, detail = %+v", run, details)
	}
	if _, err := reopen(t, dbPath).Item(context.Background(), "jira:acme", "PRJ-2"); err != nil {
		t.Fatalf("PRJ-2 removed after an IDs failure: %v", err)
	}
}

func TestReleaseRemovesItemThatLeftQuery(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := twoReadyFake()
	// The query is assignee = currentUser(). PRJ-2 starts unassigned.
	it := src.Items["PRJ-2"]
	it.Assignee = ""
	src.Items["PRJ-2"] = it
	src.Query = func(it source.Item) bool { return it.Assignee == "alice" }
	if err := src.Claim(context.Background(), "PRJ-2"); err != nil {
		t.Fatal(err)
	}
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "0s", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	if _, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath); code != ExitOK {
		t.Fatalf("sync exit = %d, stderr = %q", code, stderr)
	}

	stdout, stderr, code := runCmd(t, "release", "--config", cfgPath, "--db", dbPath, "PRJ-2")
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("release exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
	if _, err := reopen(t, dbPath).Item(context.Background(), "jira:acme", "PRJ-2"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("PRJ-2 error = %v, want ErrNotFound", err)
	}
	run := latestRun(t, dbPath)
	if run.Command != "release" || run.Outcome != "ok" || !strings.Contains(run.DetailJSON, "removed from the mirror") {
		t.Fatalf("run = %+v", run)
	}

	stdout, stderr, code = runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("next exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "PRJ-1") || strings.Contains(stdout, "PRJ-2") {
		t.Fatalf("next stdout = %q, want PRJ-1 and not PRJ-2", stdout)
	}
}

func TestReleaseKeepsItemThatStillMatches(t *testing.T) {
	src := twoReadyFake()
	if err := src.Claim(context.Background(), "PRJ-2"); err != nil {
		t.Fatal(err)
	}
	useSources(t, map[string]*fake.Source{"acme": src})
	cfgPath := writeAgeConfig(t, "0s", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedItems(t, dbPath, src.Items["PRJ-2"])

	stdout, stderr, code := runCmd(t, "release", "--config", cfgPath, "--db", dbPath, "PRJ-2")
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("release exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
	got, err := reopen(t, dbPath).Item(context.Background(), "jira:acme", "PRJ-2")
	if err != nil || got.State != "To Do" {
		t.Fatalf("PRJ-2 = %+v, %v, want the released row", got, err)
	}
}
