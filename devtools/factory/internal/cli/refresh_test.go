package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestFormatAge(t *testing.T) {
	tests := []struct {
		name string
		age  time.Duration
		want string
	}{
		{name: "zero", age: 0, want: "0s"},
		{name: "negative", age: -time.Second, want: "0s"},
		{name: "seconds", age: 45 * time.Second, want: "45s"},
		{name: "minutes", age: 15 * time.Minute, want: "15m"},
		{name: "hours and minutes", age: 3*time.Hour + 12*time.Minute, want: "3h12m"},
		{name: "exact hours", age: 2 * time.Hour, want: "2h0m"},
		{name: "drops subseconds", age: 3*time.Hour + 12*time.Minute + 400*time.Millisecond, want: "3h12m"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := formatAge(tc.age); got != tc.want {
				t.Fatalf("formatAge(%s) = %q, want %q", tc.age, got, tc.want)
			}
		})
	}
}

func TestNextSyncsStaleSource(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := readyFake("jira:acme", "PRJ-1")
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")

	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.HasPrefix(stdout, "mirror age 0s\n") || !strings.Contains(stdout, "PRJ-1") {
		t.Fatalf("stdout = %q", stdout)
	}
	if syncCount(src) != 1 {
		t.Fatalf("Sync calls = %d, want 1", syncCount(src))
	}
	if got := lastSyncAt(t, dbPath, "jira:acme"); !got.Equal(now) {
		t.Fatalf("LastSync = %s, want %s", got, now)
	}
}

func TestNextSkipsFreshSource(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	acme := readyFake("jira:acme", "PRJ-1")
	cub := readyFake("jira:cub", "CUB-1")
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"}, ageSource{name: "cub"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedAgeDB(t, dbPath, map[string]time.Time{
		"jira:acme": now.Add(-time.Hour),
		"jira:cub":  now.Add(-time.Minute),
	})

	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.HasPrefix(stdout, "mirror age 1m\n") || strings.Contains(stdout, "(stale)") {
		t.Fatalf("stdout = %q", stdout)
	}
	if syncCount(acme) != 1 || syncCount(cub) != 0 {
		t.Fatalf("sync acme=%d cub=%d", syncCount(acme), syncCount(cub))
	}
	if got := lastSyncAt(t, dbPath, "jira:cub"); !got.Equal(now.Add(-time.Minute)) {
		t.Fatalf("fresh LastSync = %s", got)
	}
}

func TestNextMaxAgeBoundary(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := readyFake("jira:acme", "PRJ-1")
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedAgeDB(t, dbPath, map[string]time.Time{"jira:acme": now.Add(-15 * time.Minute)})

	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.HasPrefix(stdout, "mirror age 15m\n") || strings.Contains(stdout, "(stale)") {
		t.Fatalf("stdout = %q", stdout)
	}
	if syncCount(src) != 0 {
		t.Fatalf("Sync calls = %d, want 0", syncCount(src))
	}
}

func TestNextMaxAgeZeroAndNoSync(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	old := now.Add(-(3*time.Hour + 12*time.Minute))
	src := readyFake("jira:acme", "PRJ-1")
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedAgeDB(t, dbPath, map[string]time.Time{"jira:acme": old})

	zeroCfg := writeAgeConfig(t, "0s", ageSource{name: "acme"})
	stdout, stderr, code := runCmd(t, "next", "--config", zeroCfg, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("zero exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.HasPrefix(stdout, "mirror age 3h12m\n") || strings.Contains(stdout, "(stale)") {
		t.Fatalf("zero stdout = %q", stdout)
	}
	if syncCount(src) != 0 {
		t.Fatalf("max_age 0 synced %d times", syncCount(src))
	}

	staleCfg := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	stdout, stderr, code = runCmd(t, "next", "--no-sync", "--config", staleCfg, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("no-sync exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.HasPrefix(stdout, "mirror age 3h12m (stale)\n") {
		t.Fatalf("no-sync stdout = %q", stdout)
	}
	if syncCount(src) != 0 {
		t.Fatalf("--no-sync synced %d times", syncCount(src))
	}

	doctorToolOK(t)
	stdout, stderr, code = runCmd(t, "doctor", "--no-sync", "--config", staleCfg, "--db", dbPath)
	if code != 1 || stderr != "" {
		t.Fatalf("doctor exit = %d, want 1, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.HasPrefix(stdout, "mirror age 3h12m (stale)\n") {
		t.Fatalf("doctor stdout = %q", stdout)
	}
	stdout, stderr, code = runCmd(t, "doctor", "--json", "--no-sync", "--config", staleCfg, "--db", dbPath)
	if code != 1 || stderr != "mirror age 3h12m (stale)\n" || !strings.HasPrefix(stdout, "{") {
		t.Fatalf("doctor json exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
}

func TestNextAutoSyncFailureStillRanks(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	old := now.Add(-(3*time.Hour + 12*time.Minute))
	src := readyFake("jira:acme", "PRJ-1")
	src.FailOn = map[string]error{"Sync": errors.New("jira down")}
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedAgeDB(t, dbPath, map[string]time.Time{"jira:acme": old})

	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "PRJ-1") || !strings.Contains(stdout, "mirror age 3h12m (stale)\n") {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "warning: auto-sync jira:acme failed, mirror age 3h12m: jira down") {
		t.Fatalf("stderr = %q", stderr)
	}
	if syncCount(src) != 1 {
		t.Fatalf("Sync calls = %d", syncCount(src))
	}
	if got := lastSyncAt(t, dbPath, "jira:acme"); !got.Equal(old) {
		t.Fatalf("LastSync = %s, want %s", got, old)
	}
}

func TestNextSourceMaxAgeOverrides(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	acme := readyFake("jira:acme", "PRJ-1")
	cub := readyFake("jira:cub", "CUB-1")
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m",
		ageSource{name: "acme", maxAge: "0s"},
		ageSource{name: "cub", maxAge: "1m"},
	)
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	old := now.Add(-2 * time.Hour)
	seedAgeDB(t, dbPath, map[string]time.Time{"jira:acme": old, "jira:cub": old})

	stdout, stderr, code := runCmd(t, "next", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if syncCount(acme) != 0 || syncCount(cub) != 1 {
		t.Fatalf("sync acme=%d cub=%d", syncCount(acme), syncCount(cub))
	}
	if !strings.HasPrefix(stdout, "mirror age 2h0m\n") || strings.Contains(stdout, "(stale)") {
		t.Fatalf("stdout = %q", stdout)
	}
}

func TestNextSyncFlagsExclusive(t *testing.T) {
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	_, stderr, code := runCmd(t, "next", "--sync", "--no-sync", "--config", cfgPath)
	if code != ExitUsage || !strings.Contains(stderr, "found both") {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
}

func TestShowSyncsStaleSource(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := readyFake("jira:acme", "PRJ-1")
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")

	stdout, stderr, code := runCmd(t, "show", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "PRJ-1") || syncCount(src) != 1 {
		t.Fatalf("stdout = %q syncs = %d", stdout, syncCount(src))
	}

	quiet := readyFake("jira:acme", "PRJ-9")
	useSources(t, map[string]*fake.Source{"acme": quiet})
	dbPath = filepath.Join(t.TempDir(), "factory.db")
	stdout, stderr, code = runCmd(t, "show", "PRJ-9", "--no-sync", "--config", cfgPath, "--db", dbPath)
	if code == ExitOK || syncCount(quiet) != 0 {
		t.Fatalf("no-sync exit = %d syncs = %d stderr = %q\n%s", code, syncCount(quiet), stderr, stdout)
	}
}

func TestShowAutoSyncFailureStillAnswers(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := readyFake("jira:acme", "PRJ-1")
	src.FailOn = map[string]error{"Sync": errors.New("jira down")}
	useSources(t, map[string]*fake.Source{"acme": src})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedAgeDB(t, dbPath, map[string]time.Time{"jira:acme": now.Add(-time.Hour)})

	stdout, stderr, code := runCmd(t, "show", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "PRJ-1") || !strings.Contains(stderr, "warning: auto-sync jira:acme failed, mirror age 1h0m: jira down") {
		t.Fatalf("stdout = %q\nstderr = %q", stdout, stderr)
	}
}

func TestPrioritiesSyncsStaleSource(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	src := readyFake("jira:acme", "PRJ-1")
	useSources(t, map[string]*fake.Source{"acme": src})
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")

	stdout, stderr, code := runCmd(t, "priorities", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if syncCount(src) != 1 {
		t.Fatalf("Sync calls = %d", syncCount(src))
	}

	quiet := readyFake("jira:acme", "PRJ-1")
	quiet.FailOn = map[string]error{"Sync": errors.New("jira down")}
	useSources(t, map[string]*fake.Source{"acme": quiet})
	seedAgeDB(t, dbPath, map[string]time.Time{"jira:acme": now.Add(-time.Hour)})
	stdout, stderr, code = runCmd(t, "priorities", "--no-sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || syncCount(quiet) != 0 {
		t.Fatalf("no-sync exit = %d syncs = %d stderr = %q\n%s", code, syncCount(quiet), stderr, stdout)
	}
}

func TestNextSourceFilterSkipsOtherStaleSource(t *testing.T) {
	now := time.Date(2026, 11, 5, 12, 0, 0, 0, time.UTC)
	withNow(t, now)
	acme := readyFake("jira:acme", "PRJ-1")
	cub := readyFake("jira:cub", "CUB-1")
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	cfgPath := writeAgeConfig(t, "15m", ageSource{name: "acme"}, ageSource{name: "cub"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedAgeDB(t, dbPath, map[string]time.Time{
		"jira:acme": now.Add(-time.Hour),
		"jira:cub":  now.Add(-time.Hour),
	})

	stdout, stderr, code := runCmd(t, "next", "--source", "jira:cub", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	if syncCount(cub) != 1 || syncCount(acme) != 0 {
		t.Fatalf("sync acme=%d cub=%d", syncCount(acme), syncCount(cub))
	}
	if strings.Contains(stdout, "PRJ-1") || !strings.Contains(stdout, "CUB-1") {
		t.Fatalf("stdout = %q", stdout)
	}
}

type ageSource struct {
	name   string
	maxAge string
}

func writeAgeConfig(t *testing.T, top string, sources ...ageSource) string {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var body strings.Builder
	if top != "" {
		fmt.Fprintf(&body, "[sync]\nmax_age = %q\n", top)
	}
	for _, src := range sources {
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
workspace = "factory"
require_labels = ["agent-ready"]
ready_statuses = ["To Do"]
`, src.name, token, cwd)
		if src.maxAge != "" {
			fmt.Fprintf(&body, "max_age = %q\n", src.maxAge)
		}
	}
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body.String()), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func readyFake(sourceName, id string) *fake.Source {
	it := rankItem(sourceName, id, "Ship the parser", "Task", "To Do", "", "alice", 2, false, day1, []string{"agent-ready"})
	return &fake.Source{
		Account: "alice",
		Items:   map[string]source.Item{id: it},
	}
}

func seedAgeDB(t *testing.T, path string, last map[string]time.Time) {
	t.Helper()
	seedDB(t, path, func(st *store.Store) {
		ctx := context.Background()
		for key, at := range last {
			it := rankItem(key, itemID(key), "Ship the parser", "Task", "To Do", "", "alice", 2, false, day1, []string{"agent-ready"})
			if err := st.UpsertItems(ctx, key, []source.Item{it}); err != nil {
				t.Fatal(err)
			}
			if err := st.SetSyncState(ctx, key, day1, day1); err != nil {
				t.Fatal(err)
			}
			if err := st.SetMe(ctx, key, "alice"); err != nil {
				t.Fatal(err)
			}
			if err := st.SetLastSync(ctx, key, at); err != nil {
				t.Fatal(err)
			}
		}
	})
}

func itemID(key string) string {
	if key == "jira:cub" {
		return "CUB-1"
	}
	return "PRJ-1"
}

func syncCount(src *fake.Source) int {
	n := 0
	for _, call := range src.Calls {
		if call.Method == "Sync" {
			n++
		}
	}
	return n
}

func lastSyncAt(t *testing.T, path, key string) time.Time {
	t.Helper()
	st, err := store.Open(context.Background(), path)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := st.Close(); err != nil {
			t.Errorf("Close: %v", err)
		}
	}()
	got, err := st.LastSync(context.Background(), key)
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func withNow(t *testing.T, now time.Time) {
	t.Helper()
	prev := nowUTC
	nowUTC = func() time.Time { return now }
	t.Cleanup(func() { nowUTC = prev })
}
