package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// scopeFixture has two sources: acme at outer, and cub at outer/cub, nested
// inside it. Each has one ready item, and the run log has a sync of each,
// a start of acme, and a tick.
func scopeFixture(t *testing.T) (cfgPath, dbPath, outer, inner string) {
	t.Helper()
	outer = t.TempDir()
	inner = filepath.Join(outer, "cub")
	if err := os.MkdirAll(filepath.Join(inner, "pkg"), 0o750); err != nil {
		t.Fatal(err)
	}
	cfgPath = writeNextConfig(t, nextSource{name: "acme", cwd: outer}, nextSource{name: "cub", cwd: inner})
	dbPath = filepath.Join(t.TempDir(), "factory.db")
	ready := []string{"agent-ready"}
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		for key, it := range map[string]source.Item{
			"jira:acme": rankItem("jira:acme", "PRJ-1", "acme item", "Task", "To Do", "", "", 2, false, day1, ready),
			"jira:cub":  rankItem("jira:cub", "CUB-1", "cub item", "Task", "To Do", "", "", 2, false, day1, ready),
		} {
			if err := st.UpsertItems(ctx, key, []source.Item{it}); err != nil {
				t.Fatal(err)
			}
			if err := st.SetSyncState(ctx, key, day1, day1); err != nil {
				t.Fatal(err)
			}
			if err := st.SetMe(ctx, key, "alice"); err != nil {
				t.Fatal(err)
			}
		}
		for i, run := range []store.Run{
			{Command: "sync", Outcome: "ok", DetailJSON: `[{"source":"jira:acme","items":1},{"source":"jira:cub","items":1}]`},
			{Command: "sync", Outcome: "ok", DetailJSON: `[{"source":"jira:cub","items":1}]`},
			{Command: "start", Outcome: "ok", DetailJSON: `{"summary":"started PRJ-1","source":"acme"}`},
			{Command: "tick", Outcome: "ok", DetailJSON: `{"summary":"nothing ready"}`},
		} {
			run.StartedAt = day1.Add(time.Duration(i) * time.Minute)
			if err := st.AppendRun(ctx, run); err != nil {
				t.Fatal(err)
			}
		}
	})
	return cfgPath, dbPath, outer, inner
}

// inDir makes list commands see dir as the current directory.
func inDir(t *testing.T, dir string) {
	t.Helper()
	prev := processEnv
	processEnv = func() config.Env {
		env := config.OSEnv()
		env.Getwd = func() (string, error) { return dir, nil }
		return env
	}
	t.Cleanup(func() { processEnv = prev })
}

const bearLine = "factory: showing jira:acme (current directory); --all for every source\n"

func TestListScopeFromCurrentDirectory(t *testing.T) {
	cfgPath, dbPath, outer, inner := scopeFixture(t)
	tests := []struct {
		name       string
		dir        string
		args       []string
		wantItems  []string
		noItems    []string
		wantStderr string
	}{
		{name: "source cwd", dir: outer, wantItems: []string{"PRJ-1"}, noItems: []string{"CUB-1"}, wantStderr: bearLine},
		{name: "subdirectory", dir: filepath.Join(outer, "cub", "..", "x"), wantItems: []string{"PRJ-1"}, noItems: []string{"CUB-1"}, wantStderr: bearLine},
		{
			name: "nested picks the deepest", dir: filepath.Join(inner, "pkg"), wantItems: []string{"CUB-1"}, noItems: []string{"PRJ-1"},
			wantStderr: "factory: showing jira:cub (current directory); --all for every source\n",
		},
		{name: "all", dir: inner, args: []string{"--all"}, wantItems: []string{"PRJ-1", "CUB-1"}},
		{name: "source wins", dir: inner, args: []string{"--source", "jira:acme"}, wantItems: []string{"PRJ-1"}, noItems: []string{"CUB-1"}},
		{name: "outside every cwd", dir: t.TempDir(), wantItems: []string{"PRJ-1", "CUB-1"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			inDir(t, tc.dir)
			useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
			args := append([]string{"next", "--config", cfgPath, "--db", dbPath}, tc.args...)
			stdout, stderr, code := runCmd(t, args...)
			if code != ExitOK {
				t.Fatalf("exit = %d, stderr = %s", code, stderr)
			}
			for _, id := range tc.wantItems {
				if !strings.Contains(stdout, id) {
					t.Fatalf("missing %s:\n%s", id, stdout)
				}
			}
			for _, id := range tc.noItems {
				if strings.Contains(stdout, id) {
					t.Fatalf("listed %s:\n%s", id, stdout)
				}
			}
			if got := strings.ReplaceAll(stderr, "mirror age unknown\n", ""); got != tc.wantStderr {
				t.Fatalf("stderr = %q, want %q", got, tc.wantStderr)
			}
		})
	}
}

func TestListScopeOtherCommands(t *testing.T) {
	cfgPath, dbPath, outer, _ := scopeFixture(t)
	inDir(t, outer)

	doctorToolOK(t)
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "doctor", "--config", cfgPath, "--db", dbPath)
	if code != 1 || !strings.Contains(stderr, bearLine) || !strings.Contains(stdout, "PRJ-1 has no parent") || strings.Contains(stdout, "CUB-1") {
		t.Fatalf("doctor exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}

	stdout, stderr, code = runCmd(t, "stats", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != bearLine {
		t.Fatalf("stats exit = %d, stderr = %q", code, stderr)
	}
	var rep struct {
		Source    string `json:"source"`
		Freshness struct {
			LastSync []struct {
				Source string `json:"source"`
			} `json:"last_sync"` //nolint:tagliatelle // documented factory JSON output field
		} `json:"freshness"`
	}
	if err := json.Unmarshal([]byte(stdout), &rep); err != nil {
		t.Fatalf("stats json: %v\n%s", err, stdout)
	}
	if rep.Source != "jira:acme" || len(rep.Freshness.LastSync) != 1 || rep.Freshness.LastSync[0].Source != "jira:acme" {
		t.Fatalf("stats = %+v", rep)
	}
	stdout, stderr, code = runCmd(t, "stats", "--json", "--all", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("stats --all exit = %d, stderr = %q", code, stderr)
	}
	rep.Source, rep.Freshness.LastSync = "", nil
	if err := json.Unmarshal([]byte(stdout), &rep); err != nil || rep.Source != "" || len(rep.Freshness.LastSync) != 2 {
		t.Fatalf("stats --all = %+v, %v", rep, err)
	}

	stdout, stderr, code = runCmd(t, "priorities", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != bearLine {
		t.Fatalf("priorities exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}

	stdout, stderr, code = runCmd(t, "logs", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != bearLine {
		t.Fatalf("logs exit = %d, stderr = %q", code, stderr)
	}
	if strings.Count(stdout, "\n") != 3 || !strings.Contains(stdout, "tick") || !strings.Contains(stdout, "start") {
		t.Fatalf("logs for acme: want the acme sync, the acme start, and the tick:\n%s", stdout)
	}
	stdout, _, code = runCmd(t, "logs", "--source", "jira:cub", "--db", dbPath)
	if code != ExitOK || strings.Count(stdout, "\n") != 3 || strings.Contains(stdout, "start") {
		t.Fatalf("logs for cub: want both syncs and the tick:\n%s", stdout)
	}
	stdout, _, code = runCmd(t, "logs", "--all", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || strings.Count(stdout, "\n") != 4 {
		t.Fatalf("logs --all:\n%s", stdout)
	}
}

func TestScopeAllWithSourceIsUsageError(t *testing.T) {
	cfgPath, dbPath, outer, _ := scopeFixture(t)
	inDir(t, outer)
	for _, tc := range []struct {
		name  string
		extra []string
	}{
		{name: "next"},
		{name: "stats"},
		{name: "doctor"},
		{name: "priorities"},
		{name: "logs"},
		{name: "sync"},
		{name: "gate"},
		{name: "tick"},
		{name: "run", extra: []string{"--interval", "1m"}},
	} {
		args := append([]string{tc.name, "--all", "--source", "jira:acme", "--config", cfgPath, "--db", dbPath}, tc.extra...)
		_, stderr, code := runCmd(t, args...)
		if code != ExitUsage || !strings.Contains(stderr, tc.name+": expected --all or --source, found both") {
			t.Fatalf("%s exit = %d, stderr = %q", tc.name, code, stderr)
		}
	}
}

func TestActionCommandsScopeToCurrentDirectory(t *testing.T) {
	cfgPath, dbPath, outer, inner := scopeFixture(t)
	inDir(t, outer)
	ready := []string{"agent-ready"}
	acme := &fake.Source{SourceName: "jira:acme", Account: "alice", Items: map[string]source.Item{
		"PRJ-1": rankItem("jira:acme", "PRJ-1", "acme item", "Task", "To Do", "", "", 2, false, day1, ready),
	}}
	cub := &fake.Source{SourceName: "jira:cub", Account: "alice", Items: map[string]source.Item{
		"CUB-1": rankItem("jira:cub", "CUB-1", "cub item", "Task", "To Do", "", "", 2, false, day1, ready),
	}}
	useSources(t, map[string]*fake.Source{"acme": acme, "cub": cub})

	// sync scopes to jira:acme when inside outer.
	stdout, stderr, code := runCmd(t, "sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stderr, "factory: scoping to jira:acme (current directory); --all for every source") {
		t.Fatalf("sync exit = %d, stderr = %q", code, stderr)
	}
	if !strings.Contains(stdout, "jira:acme") || strings.Contains(stdout, "jira:cub") {
		t.Fatalf("sync should scope to jira:acme only:\n%s", stdout)
	}

	// sync --all runs every source without scoping stderr line.
	stdout, stderr, code = runCmd(t, "sync", "--all", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("sync --all exit = %d, stderr = %q", code, stderr)
	}
	if !strings.Contains(stdout, "jira:acme") || !strings.Contains(stdout, "jira:cub") {
		t.Fatalf("sync --all did not run every source:\n%s", stdout)
	}

	// gate scopes to jira:acme inside outer (would otherwise fail as ambiguous checkout).
	gateMock(t, tabsJSON(), "", "", nil)
	stdout, stderr, code = runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stderr, "factory: scoping to jira:acme (current directory); --all for every source") {
		t.Fatalf("gate exit = %d, stderr = %q, stdout = %s", code, stderr, stdout)
	}

	// tick scopes to jira:cub when inside inner.
	inDir(t, inner)
	tickMock(t, emptyTabs, false, inner)
	stdout, stderr, code = runCmd(t, "tick", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || !strings.Contains(stderr, "factory: scoping to jira:cub (current directory); --all for every source") {
		t.Fatalf("tick exit = %d, stderr = %q, stdout = %s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "CUB-1") || strings.Contains(stdout, "PRJ-1") {
		t.Fatalf("tick should pick CUB-1 only, got:\n%s", stdout)
	}

	// run also scopes to jira:cub when inside inner.
	tickMock(t, emptyTabs, false, inner)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var outBuf, errBuf bytes.Buffer
	code = Execute(ctx, []string{"run", "--interval", "1m", "--config", cfgPath, "--db", dbPath}, &cancelOnWrite{w: &outBuf, cancel: cancel}, &errBuf)
	if code != ExitOK || !strings.Contains(errBuf.String(), "factory: scoping to jira:cub (current directory); --all for every source") {
		t.Fatalf("run exit = %d, stderr = %q, stdout = %q", code, errBuf.String(), outBuf.String())
	}
	if !strings.Contains(outBuf.String(), "CUB-1") || strings.Contains(outBuf.String(), "PRJ-1") {
		t.Fatalf("run should pick CUB-1 only, got:\n%s", outBuf.String())
	}
}

type cancelOnWrite struct {
	w      *bytes.Buffer
	cancel context.CancelFunc
}

func (c *cancelOnWrite) Write(p []byte) (int, error) {
	c.cancel()
	return c.w.Write(p) //nolint:wrapcheck // test helper delegates directly
}

func TestUnconfiguredGitRepoRefusedAndOverrides(t *testing.T) {
	_, dbPath, outer, _ := scopeFixture(t)
	// Write XDG config pointing at outer (jira:acme only, so gate outside git can share one cwd).
	xdgDir := t.TempDir()
	xdgCfgDir := filepath.Join(xdgDir, "factory")
	if err := os.MkdirAll(xdgCfgDir, 0o750); err != nil {
		t.Fatal(err)
	}
	cfgBody, err := os.ReadFile(writeNextConfig(t, nextSource{name: "acme", cwd: outer}))
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(xdgCfgDir, "factory.toml"), cfgBody, 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("XDG_CONFIG_HOME", xdgDir)

	unconfiguredRepo := t.TempDir()
	if err := os.Mkdir(filepath.Join(unconfiguredRepo, ".git"), 0o750); err != nil {
		t.Fatal(err)
	}
	nested := filepath.Join(unconfiguredRepo, "sub", "dir")
	if err := os.MkdirAll(nested, 0o750); err != nil {
		t.Fatal(err)
	}

	inDir(t, nested)
	useHerdrGH(t, emptyWorkspaces, emptyTabs, emptyPRs)
	acme := &fake.Source{SourceName: "jira:acme", Account: "alice", Items: map[string]source.Item{}}
	useSources(t, map[string]*fake.Source{"acme": acme})

	for _, tc := range []struct {
		name  string
		extra []string
	}{
		{name: "next"},
		{name: "stats"},
		{name: "doctor"},
		{name: "priorities"},
		{name: "logs"},
		{name: "sync"},
		{name: "gate"},
		{name: "tick"},
		{name: "run", extra: []string{"--interval", "1m"}},
	} {
		args := append([]string{tc.name, "--db", dbPath}, tc.extra...)
		_, stderr, code := runCmd(t, args...)
		if code != ExitError {
			t.Fatalf("%s in unconfigured git repo: exit = %d, want %d, stderr = %q", tc.name, code, ExitError, stderr)
		}
		for _, want := range []string{unconfiguredRepo, "factory init", "--all", "--source"} {
			if !strings.Contains(stderr, want) {
				t.Fatalf("%s stderr = %q, want substring %q", tc.name, stderr, want)
			}
		}
	}

	// --all and --source override the unconfigured-repo refusal.
	stdout, stderr, code := runCmd(t, "next", "--all", "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "PRJ-1") {
		t.Fatalf("next --all exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
	stdout, stderr, code = runCmd(t, "next", "--source", "jira:acme", "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "PRJ-1") {
		t.Fatalf("next --source exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}

	// Outside any git repo, XDG config covers all sources without --all.
	inDir(t, t.TempDir())
	stdout, stderr, code = runCmd(t, "next", "--db", dbPath)
	if code != ExitOK || !strings.Contains(stdout, "PRJ-1") {
		t.Fatalf("next outside git exit = %d, stderr = %q\n%s", code, stderr, stdout)
	}
}
