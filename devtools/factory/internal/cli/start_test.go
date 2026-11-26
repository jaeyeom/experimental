package cli

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	startPrompt = "Implement PRJ-1 Fix it"
	startTab    = `{
	  "id": "cli:tab:create",
	  "result": {
	    "type": "tab_created",
	    "tab": {
	      "tab_id": "w1:t1",
	      "workspace_id": "w1",
	      "label": "PRJ-1",
	      "pane_count": 1,
	      "agent_status": "idle"
	    },
	    "root_pane": {"pane_id": "w1:p1", "tab_id": "w1:t1", "workspace_id": "w1"}
	  }
	}`
	startAgent = `{
	  "id": "cli:agent:start",
	  "result": {
	    "type": "agent_started",
	    "agent": {"pane_id": "w1:p1", "agent_status": "idle"},
	    "argv": ["claude"]
	  }
	}`
	agentIdle     = `{"id":"cli:agent:wait","result":{"type":"agent_info","agent":{"pane_id":"w1:p1","agent_status":"idle"}}}`
	startPrompted = `{
	  "id": "cli:agent:prompt",
	  "result": {
	    "type": "agent_prompted",
	    "agent": {"pane_id": "w1:p1", "agent_status": "working"}
	  }
	}`
)

func TestStartUsage(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no id", args: []string{"start"}},
		{name: "two ids", args: []string{"start", "PRJ-1", "PRJ-2"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			stdout, stderr, code := runCmd(t, tc.args...)
			if code != ExitUsage {
				t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitUsage, stderr)
			}
			if stdout != "" {
				t.Fatalf("stdout = %q", stdout)
			}
			if strings.HasPrefix(stderr, "factory:") {
				t.Fatalf("usage printed as a command error: %s", stderr)
			}
		})
	}
}

func TestStartUnknownID(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-404", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "PRJ-404") {
		t.Fatalf("stderr = %q", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartNeedsSync(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", false)
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, "run factory sync first") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartDryRun(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if stdout != startPlan(cwd) {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, startPlan(cwd))
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestStartGateRefused(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	tabs := tabsJSON(tabSpec{id: "w1:t9", label: "PRJ-9", status: "working"})
	panes := panesJSON(paneSpec{id: "w1:p9", tab: "w1:t9", status: "working", cwd: cwd})
	mock := startMockFull(t, rankWorkspaces, tabs, panes, false, cwd, nil)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	wantOut := "factory start PRJ-1: refused, nothing changed: start PRJ-1: gate: agent PRJ-9 (w1:t9) is working\n"
	if stdout != wantOut || !strings.Contains(stderr, "agent PRJ-9 (w1:t9) is working") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestStartWrongBranch(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, `
[gate]
base_branch = "main"
require = "on_base"
`, "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "rev-parse", "--abbrev-ref", "HEAD").WillSucceed("feature\n", 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	wantErr := "gate: " + cwd + ": expected branch main, found feature"
	wantOut := "blocked  yes\n" + startPlan(cwd) + "factory start PRJ-1: refused, nothing changed: start PRJ-1: " + wantErr + "\n"
	if stdout != wantOut || !strings.Contains(stderr, "factory: start PRJ-1: "+wantErr) {
		t.Fatalf("stdout =\n%s\nstderr = %q", stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestStartDryRunGateRefused(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	tabs := tabsJSON(tabSpec{id: "w1:t9", label: "PRJ-9", status: "working"})
	panes := panesJSON(paneSpec{id: "w1:p9", tab: "w1:t9", status: "working", cwd: cwd})
	mock := startMockFull(t, rankWorkspaces, tabs, panes, false, cwd, nil)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := "blocked  yes\n" + startPlan(cwd) + "factory start PRJ-1: refused, nothing changed: start PRJ-1: gate: agent PRJ-9 (w1:t9) is working\n"
	if stdout != want || !strings.Contains(stderr, "factory: start PRJ-1: gate: agent PRJ-9 (w1:t9) is working") {
		t.Fatalf("stdout =\n%s\nstderr = %q", stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestStartOpenPRBlocksDryRun(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[defaults]\nmax_in_flight = 1\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(oneOpenPR, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	want := "blocked  yes\n" + startPlan(cwd) + "factory start PRJ-1: refused, nothing changed: start PRJ-1: gate: in flight: 1 (claims 0, PRs 1, overlap 0) of max 1\n"
	if stdout != want || !strings.Contains(stderr, "factory: start PRJ-1: gate: in flight: 1 (claims 0, PRs 1, overlap 0) of max 1") {
		t.Fatalf("stdout =\n%s\nstderr = %q", stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestStartForceDryRun(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	tabs := tabsJSON(tabSpec{id: "w1:t9", label: "PRJ-9", status: "working"})
	mock := startMock(t, rankWorkspaces, tabs, false, cwd)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--force", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	want := "warning: --force skips the ready checks and the start gate\n" + startPlan(cwd)
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestStartApply(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := startMock(t, rankWorkspaces, emptyTabs, true, cwd)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if !strings.HasPrefix(stdout, startPlan(cwd)+"factory start PRJ-1: started (tab w1:t1, claim ") {
		t.Fatalf("stdout =\n%s\nwant prefix =\n%s", stdout, startPlan(cwd))
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertSourceCalls(t, src, []string{"Get", "ClaimWithOptions", "Get", "PostClaimComment"})
	assertStartedRow(t, dbPath)
	logs, logErr, logCode := runCmd(t, "logs", "--config", cfgPath, "--db", dbPath)
	if logCode != ExitOK || logErr != "" || !strings.Contains(logs, "started PRJ-1") {
		t.Fatalf("logs exit = %d stdout = %q stderr = %q", logCode, logs, logErr)
	}
}

func TestStartCaller(t *testing.T) {
	tests := []struct {
		name     string
		defaults string
		flag     string
		pane     string
		want     string
	}{
		{name: "flag", defaults: "[defaults]\ncaller = \"from-config\"\n", flag: "from-flag", pane: "w1:p9", want: "from-flag"},
		{name: "config", defaults: "[defaults]\ncaller = \"from-config\"\n", pane: "w1:p9", want: "from-config"},
		{name: "empty config uses pane", defaults: "[defaults]\ncaller = \"\"\n", pane: "w1:p9", want: "w1:p9"},
		{name: "pane", pane: "w1:p9", want: "w1:p9"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			clearPaneID(t)
			if tc.pane != "" {
				t.Setenv("HERDR_PANE_ID", tc.pane)
			}
			cfgPath, cwd := writeStartConfig(t, tc.defaults, "report to {caller} about {id}")
			dbPath := filepath.Join(t.TempDir(), "factory.db")
			seedStartItem(t, dbPath, "Fix it", true)
			src := startFake()
			useSources(t, map[string]*fake.Source{"acme": src})
			mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
			args := []string{"start", "PRJ-1", "--config", cfgPath, "--db", dbPath}
			if tc.flag != "" {
				args = append(args, "--caller", tc.flag)
			}
			stdout, stderr, code := runCmd(t, args...)
			if code != ExitOK || stderr != "" {
				t.Fatalf("exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
			}
			want := "prompt   report to " + tc.want + " about PRJ-1…\n"
			if !strings.Contains(stdout, want) || strings.Contains(stdout, "{caller}") {
				t.Fatalf("stdout =\n%s", stdout)
			}
			if err := mock.AssertExpectationsMet(); err != nil {
				t.Fatal(err)
			}
			assertNoClaimCall(t, src)
		})
	}
}

func TestStartCallerEmptyWarns(t *testing.T) {
	clearPaneID(t)
	cfgPath, cwd := writeStartConfig(t, "", "report to {caller} about {id}")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
	}
	want := "prompt   report to  about PRJ-1…\n"
	if !strings.Contains(stdout, want) || strings.Contains(stdout, "{caller}") {
		t.Fatalf("stdout =\n%s", stdout)
	}
	if !strings.Contains(stderr, "level=WARN") || !strings.Contains(stderr, "{caller}") {
		t.Fatalf("stderr = %q", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)
}

func TestEmptyCallerFlag(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "report to {caller}")
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "start", args: []string{"start", "PRJ-1", "--caller", "", "--config", cfgPath}, want: "start: expected a non-empty caller, found empty"},
		{name: "tick", args: []string{"tick", "--caller", "  ", "--config", cfgPath}, want: "tick: expected a non-empty caller, found empty"},
		{name: "run", args: []string{"run", "--interval", "1m", "--caller", "", "--config", cfgPath}, want: "run: expected a non-empty caller, found empty"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			stdout, stderr, code := runCmd(t, tc.args...)
			if code != ExitUsage || stdout != "" || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, tc.want) {
				t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
			}
		})
	}
}

func clearPaneID(t *testing.T) {
	t.Helper()
	prev, ok := os.LookupEnv("HERDR_PANE_ID")
	if err := os.Unsetenv("HERDR_PANE_ID"); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if !ok {
			return
		}
		if err := os.Setenv("HERDR_PANE_ID", prev); err != nil {
			t.Errorf("restore HERDR_PANE_ID: %v", err)
		}
	})
}

func TestStartAgentArgs(t *testing.T) {
	const rules = `
[defaults]
agent_args = ["--model", "opus"]

[[agent_rule]]
labels = ["mechanical"]
agent_args = ["--model", "sonnet"]
`
	tests := []struct {
		name     string
		labels   []string
		extra    []string
		wantArgs []string
		wantLine string
	}{
		{
			name:     "rule",
			labels:   []string{"mechanical"},
			wantArgs: []string{"--model", "sonnet"},
			wantLine: "args     --model sonnet (rule 1 labels mechanical)\n",
		},
		{
			name:     "default",
			wantArgs: []string{"--model", "opus"},
			wantLine: "args     --model opus (default)\n",
		},
		{
			name:     "model flag",
			labels:   []string{"mechanical"},
			extra:    []string{"--model", "haiku"},
			wantArgs: []string{"--model", "haiku"},
			wantLine: "args     --model haiku (flag)\n",
		},
		{
			name:     "agent-arg flag",
			labels:   []string{"mechanical"},
			extra:    []string{"--agent-arg=--model", "--agent-arg=haiku"},
			wantArgs: []string{"--model", "haiku"},
			wantLine: "args     --model haiku (flag)\n",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfgPath, cwd := writeStartConfig(t, rules, "")
			dbPath := filepath.Join(t.TempDir(), "factory.db")
			seedLabeledStart(t, dbPath, tc.labels)
			src := startFake()
			useSources(t, map[string]*fake.Source{"acme": src})
			mock := startMockWithArgs(t, rankWorkspaces, emptyTabs, true, cwd, tc.wantArgs)
			args := []string{"start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath}
			args = append(args, tc.extra...)
			stdout, stderr, code := runCmd(t, args...)
			if code != ExitOK {
				t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
			}
			if !strings.Contains(stdout, tc.wantLine) {
				t.Fatalf("stdout =\n%s\nwant line %q", stdout, tc.wantLine)
			}
			if stderr != "" {
				t.Fatalf("stderr = %q", stderr)
			}
			if err := mock.AssertExpectationsMet(); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestStartAgentArgUsage(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "both", args: []string{"--model", "haiku", "--agent-arg=--model"}, want: "found both"},
		{name: "empty model", args: []string{"--model="}, want: "found empty"},
		{name: "empty agent arg", args: []string{"--agent-arg="}, want: "found empty"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			args := append([]string{"start", "PRJ-1"}, tc.args...)
			stdout, stderr, code := runCmd(t, args...)
			if code != ExitUsage {
				t.Fatalf("exit = %d, stderr = %s", code, stderr)
			}
			if stdout != "" || !strings.Contains(stderr, tc.want) {
				t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
			}
		})
	}
}

func TestNextExplainAgentArgs(t *testing.T) {
	const rules = `
[defaults]
agent_args = ["--model", "opus"]

[[agent_rule]]
labels = ["mechanical"]
agent_args = ["--model", "sonnet"]

[[agent_rule]]
labels = ["report-only"]
agent_args = ["--model", "haiku"]
`
	cfgPath, _ := writeStartConfig(t, rules, "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedLabeledStart(t, dbPath, []string{"report-only"})
	useHerdrGH(t, rankWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--explain", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s\n%s", code, stderr, stdout)
	}
	if !strings.Contains(stdout, "args     --model haiku (rule 2 labels report-only)\n") {
		t.Fatalf("explain =\n%s", stdout)
	}
}

func seedLabeledStart(t *testing.T, dbPath string, labels []string) {
	t.Helper()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := source.Item{
			Source: "jira:acme", ID: "PRJ-1", Title: "Fix it",
			URL: "https://example.test/browse/PRJ-1", Type: "Task", State: "To Do",
			Labels: labels,
		}
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "ada"); err != nil {
			t.Fatal(err)
		}
	})
}

func TestStartImpliedApply(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[defaults]\ndry_run = false\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := startMock(t, rankWorkspaces, emptyTabs, true, cwd)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.HasPrefix(stdout, startPlan(cwd)+"factory start PRJ-1: started (tab w1:t1, claim ") {
		t.Fatalf("stdout =\n%s\nwant prefix =\n%s", stdout, startPlan(cwd))
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertSourceCalls(t, src, []string{"Get", "ClaimWithOptions", "Get", "PostClaimComment"})
	assertStartedRow(t, dbPath)
}

func startPlan(cwd string) string {
	return "claim    PRJ-1 → In Progress (assign to me)\n" +
		"comment  Claimed by claude at <timestamp>.\n" +
		"tab      workspace \"factory\" / tab \"PRJ-1\" (cwd " + cwd + ")\n" +
		"agent    claude\n" +
		"args     (default)\n" +
		"prompt   " + startPrompt + "…\n"
}

func writeStartConfig(t *testing.T, defaults, prompt string) (string, string) {
	t.Helper()
	if prompt == "" {
		prompt = "Implement {id} {title}"
	}
	dir := t.TempDir()
	cwd := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var body strings.Builder
	body.WriteString(leaseOff(defaults))
	if defaults != "" {
		body.WriteString(defaults)
		if !strings.HasSuffix(defaults, "\n") {
			body.WriteByte('\n')
		}
	}
	fmt.Fprintf(&body, `
[[source]]
name = "acme"
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = %q
jql = "project = PRJ"
claim_status = "In Progress"
cwd = %q
base = "main"
start_prompt = %q
`, token, cwd, prompt)
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body.String()), 0o600); err != nil {
		t.Fatal(err)
	}
	return path, cwd
}

func seedStartItem(t *testing.T, dbPath, title string, withMe bool) {
	t.Helper()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		it := source.Item{
			Source: "jira:acme", ID: "PRJ-1", Title: title,
			URL: "https://example.test/browse/PRJ-1", Type: "Task", State: "To Do",
		}
		if err := st.UpsertItems(ctx, "jira:acme", []source.Item{it}); err != nil {
			t.Fatal(err)
		}
		if !withMe {
			return
		}
		if err := st.SetSyncState(ctx, "jira:acme", day1, day1); err != nil {
			t.Fatal(err)
		}
		if err := st.SetMe(ctx, "jira:acme", "ada"); err != nil {
			t.Fatal(err)
		}
	})
}

func startFake() *fake.Source {
	return &fake.Source{
		SourceName: "jira:acme",
		Account:    "ada",
		Items: map[string]source.Item{
			"PRJ-1": {Source: "jira:acme", ID: "PRJ-1", Title: "Fix it", State: "To Do"},
		},
	}
}

func startMock(t *testing.T, workspaces, tabs string, apply bool, cwd string) *executor.MockExecutor {
	t.Helper()
	return startMockWithArgs(t, workspaces, tabs, apply, cwd, nil)
}

func startMockWithArgs(t *testing.T, workspaces, tabs string, apply bool, cwd string, agentArgs []string) *executor.MockExecutor {
	t.Helper()
	return startMockFull(t, workspaces, tabs, emptyPanes, apply, cwd, agentArgs)
}

func startMockFull(t *testing.T, workspaces, tabs, panes string, apply bool, cwd string, agentArgs []string) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(workspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(panes, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	if apply {
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(workspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").
			WillSucceed(startTab, 0).Once().Build()
		startArgs := []string{"agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000"}
		if len(agentArgs) > 0 {
			startArgs = append(startArgs, "--")
			startArgs = append(startArgs, agentArgs...)
		}
		mock.ExpectCommandWithArgs("herdr", startArgs...).WillSucceed(startAgent, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "prompt", "w1:p1", startPrompt, "--wait", "--until", "working", "--timeout", "30000").
			WillSucceed(startPrompted, 0).Once().Build()
	}
	useExec(t, mock)
	return mock
}

func assertNoClaimCall(t *testing.T, src *fake.Source) {
	t.Helper()
	for _, call := range src.Calls {
		if call.Method == "Claim" || call.Method == "Release" || call.Method == "Get" {
			t.Fatalf("source calls = %#v", src.Calls)
		}
	}
}

func assertSourceCalls(t *testing.T, src *fake.Source, want []string) {
	t.Helper()
	var got []string
	for _, call := range src.Calls {
		if call.Method == "Name" {
			continue
		}
		got = append(got, call.Method)
	}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("source calls = %v, want %v", got, want)
	}
}

func assertStartedRow(t *testing.T, dbPath string) {
	t.Helper()
	st, err := store.Open(context.Background(), dbPath)
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := st.Close(); err != nil {
			t.Errorf("close store: %v", err)
		}
	}()
	got, err := st.LatestClaim(context.Background(), "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatal(err)
	}
	host, err := os.Hostname()
	if err != nil {
		t.Fatal(err)
	}
	if got.State != store.Started || got.TabID != "w1:t1" || got.WorkspaceID != "w1" || got.Machine != host || got.StashCount == nil || *got.StashCount != 0 {
		t.Fatalf("claim = %+v, host %q", got, host)
	}
}

func TestStartOutcomesCLIAndJSON(t *testing.T) {
	t.Run("started_json", func(t *testing.T) {
		cfgPath, cwd := writeStartConfig(t, "", "")
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedStartItem(t, dbPath, "Fix it", true)
		useSources(t, map[string]*fake.Source{"acme": startFake()})
		mock := startMock(t, rankWorkspaces, emptyTabs, true, cwd)
		stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--json", "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		if !strings.Contains(stdout, `"status":"started"`) ||
			!strings.Contains(stdout, `"tab_id":"w1:t1"`) ||
			!strings.Contains(stdout, `"output_mode":"pr"`) ||
			!strings.Contains(stdout, `"keep_worker":true`) ||
			!strings.Contains(stdout, `"exit_code":0`) {
			t.Fatalf("stdout = %q", stdout)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("started_merge_json", func(t *testing.T) {
		cfgPath, cwd := writeStartConfig(t, "[output]\nmode = \"merge\"\n", "")
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedStartItem(t, dbPath, "Fix it", true)
		useSources(t, map[string]*fake.Source{"acme": startFake()})
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=merge", "--no-focus").
			WillSucceed(startTab, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").
			WillSucceed(startAgent, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
		mock.ExpectCommand("herdr").WillSucceed(startPrompted, 0).Once().Build()
		useExec(t, mock)
		stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--json", "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("exit = %d, stderr = %s", code, stderr)
		}
		for _, want := range []string{`"status":"started"`, `"output_mode":"merge"`, `"keep_worker":false`} {
			if !strings.Contains(stdout, want) {
				t.Fatalf("stdout missing %s: %q", want, stdout)
			}
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("source_and_lane_output_override_json", func(t *testing.T) {
		cfgPath, cwd := writeStartConfig(t, "[output]\nmode = \"pr\"\n", "")
		raw, err := os.ReadFile(cfgPath) //nolint:gosec // test temp config path
		if err != nil {
			t.Fatal(err)
		}
		extra := string(raw) + "\n[source.output]\nmode = \"merge\"\n\n[lanes]\ndefault = [\"mac\"]\n\n[[lane]]\nname = \"mac\"\nmachine = \"mac1\"\ncheckout = " + fmt.Sprintf("%q", cwd) + "\nmax_workers = 1\n[lane.output]\nmode = \"pr\"\n"
		if err := os.WriteFile(cfgPath, []byte(extra), 0o600); err != nil {
			t.Fatal(err)
		}
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedStartItem(t, dbPath, "Fix it", true)
		useSources(t, map[string]*fake.Source{"acme": startFake()})
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		expectSnapshot(mock)
		expectMacUp(mock)
		mac := func(want string, args ...string) {
			mock.ExpectCommandWithArgs("herdr", append([]string{"--machine", "mac1"}, args...)...).WillSucceed(want, 0).Once().Build()
		}
		mac(rankWorkspaces, "workspace", "list")
		mac(emptyTabs, "tab", "list")
		mac(rankWorkspaces, "workspace", "list")
		mac(startTab, "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus")
		mac(startAgent, "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000")
		mac(agentIdle, "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000")
		mac(startPrompted, "agent", "prompt", "w1:p1", startPrompt, "--wait", "--until", "working", "--timeout", "30000")
		useExec(t, mock)
		stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--json", "--config", cfgPath, "--db", dbPath)
		if code != ExitOK || stderr != "" {
			t.Fatalf("exit = %d, stderr = %s, stdout = %s", code, stderr, stdout)
		}
		for _, want := range []string{`"status":"started"`, `"output_mode":"pr"`, `"keep_worker":true`} {
			if !strings.Contains(stdout, want) {
				t.Fatalf("stdout missing %s: %q", want, stdout)
			}
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("refused_json", func(t *testing.T) {
		cfgPath, cwd := writeStartConfig(t, "", "")
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedStartItem(t, dbPath, "Fix it", true)
		useSources(t, map[string]*fake.Source{"acme": startFake()})
		tabs := tabsJSON(tabSpec{id: "w1:t9", label: "PRJ-9", status: "working"})
		panes := panesJSON(paneSpec{id: "w1:p9", tab: "w1:t9", status: "working", cwd: cwd})
		mock := startMockFull(t, rankWorkspaces, tabs, panes, false, cwd, nil)
		stdout, _, code := runCmd(t, "start", "PRJ-1", "--apply", "--json", "--config", cfgPath, "--db", dbPath)
		if code != ExitStartRefused {
			t.Fatalf("exit = %d, want %d", code, ExitStartRefused)
		}
		if !strings.Contains(stdout, `"status":"refused"`) || !strings.Contains(stdout, `"exit_code":3`) {
			t.Fatalf("stdout = %q", stdout)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("failed_released", func(t *testing.T) {
		cfgPath, cwd := writeStartConfig(t, "", "")
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedStartItem(t, dbPath, "Fix it", true)
		useSources(t, map[string]*fake.Source{"acme": startFake()})
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").
			WillSucceed(startTab, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").
			WillFail("agent down\n", 1).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "close", "w1:t1").WillSucceed(`{"id":"cli:tab:close","result":{"type":"tab_closed","tab_id":"w1:t1","workspace_id":"w1"}}`, 0).Once().Build()
		useExec(t, mock)
		stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
		if code != ExitStartReleased {
			t.Fatalf("exit = %d, want %d, stdout = %q, stderr = %q", code, ExitStartReleased, stdout, stderr)
		}
		if !strings.HasSuffix(stdout, "factory start PRJ-1: failed after claim; claim released: re-run: factory start PRJ-1 --apply\n") {
			t.Fatalf("stdout = %q", stdout)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("failed_claim_kept", func(t *testing.T) {
		cfgPath, cwd := writeStartConfig(t, "", "")
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seedStartItem(t, dbPath, "Fix it", true)
		useSources(t, map[string]*fake.Source{"acme": startFake()})
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").
			WillSucceed(startTab, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").
			WillSucceed(startAgent, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "agent", "prompt", "w1:p1", startPrompt, "--wait", "--until", "working", "--timeout", "30000").
			WillFail("prompt failed\n", 1).Once().Build()
		useExec(t, mock)
		stdout, _, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
		if code != ExitStartClaimKept {
			t.Fatalf("exit = %d, want %d, stdout = %q", code, ExitStartClaimKept, stdout)
		}
		if !strings.HasSuffix(stdout, "factory start PRJ-1: failed after claim; claim kept: fix and re-run with --force, or factory release PRJ-1\n") {
			t.Fatalf("stdout = %q", stdout)
		}
		if err := mock.AssertExpectationsMet(); err != nil {
			t.Fatal(err)
		}
	})
}

func TestStartSubagentAndShowCLI(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[worker]\nspawner = \"subagent\"\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})

	// Dry-run prints subagent plan without claiming.
	dryMock := executor.NewMockExecutor()
	dryMock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	dryMock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	dryMock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	dryMock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	dryMock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	dryMock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	useExec(t, dryMock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("dry-run exit=%d stderr=%q", code, stderr)
	}
	if !strings.Contains(stdout, "spawner  subagent (cwd "+cwd+")") {
		t.Fatalf("dry-run stdout = %q", stdout)
	}
	if err := dryMock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)

	// Apply with --json claims and emits structured subagent outcome with no tab/agent herdr calls.
	applyMock := executor.NewMockExecutor()
	applyMock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	applyMock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	applyMock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	applyMock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	applyMock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	applyMock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	applyMock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	useExec(t, applyMock)
	stdout, stderr, code = runCmd(t, "start", "PRJ-1", "--apply", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("apply exit=%d stderr=%q stdout=%q", code, stderr, stdout)
	}
	for _, want := range []string{
		`"status":"started"`,
		`"spawner":"subagent"`,
		`"output_mode":"pr"`,
		`"keep_worker":true`,
		`"source":"jira:acme"`,
		`"agent":"claude"`,
		`"FACTORY_ITEM_SOURCE":"jira:acme"`,
		`"FACTORY_OUTPUT_MODE":"pr"`,
		`"brief":"Implement PRJ-1 Fix it"`,
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("apply json missing %q: %s", want, stdout)
		}
	}
	if err := applyMock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}

	// Show prints spawner: subagent and omits Tab section.
	showMock := executor.NewMockExecutor()
	showMock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	showMock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	showMock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	showMock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	showMock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	useExec(t, showMock)
	stdout, stderr, code = runCmd(t, "show", "PRJ-1", "--no-sync", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("show exit=%d stderr=%q", code, stderr)
	}
	if !strings.Contains(stdout, "  spawner: subagent\n") {
		t.Fatalf("show missing spawner: subagent:\n%s", stdout)
	}
	if strings.Contains(stdout, "Tab:\n") {
		t.Fatalf("show should omit Tab section for subagent:\n%s", stdout)
	}
	if err := showMock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}
