package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestGateOK(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, tabsJSON(
		tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"},
		tabSpec{id: "w1:t4", label: "PRJ-4", status: "blocked"},
		tabSpec{id: "w1:t5", label: "PRJ-5", status: "done"},
	), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	assertGateCalls(t, mock, cwds["jira:acme"])
}

func TestGateIgnoresReleasedClaim(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[defaults]
max_in_flight = 3
min_start_interval = "10m"
`, gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	now := time.Now().UTC()
	seedDB(t, dbPath, func(st *store.Store) {
		ctx := context.Background()
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-OLD", ClaimedAt: now.Add(-2 * time.Hour),
			Machine: "laptop", State: store.Started, UpdatedAt: now.Add(-2 * time.Hour),
		}); err != nil {
			t.Fatal(err)
		}
		if err := st.InsertClaim(ctx, store.Claim{
			Source: "jira:acme", ID: "PRJ-REL", ClaimedAt: now,
			Machine: "laptop", State: store.Released, UpdatedAt: now,
		}); err != nil {
			t.Fatal(err)
		}
	})
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "OK: 1 in flight (max 3)\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	assertGateCalls(t, mock, cwds["jira:acme"])
}

func TestGateReasons(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[defaults]
max_in_flight = 1
min_start_interval = "10m"
`, gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	now := time.Now().UTC()
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.InsertClaim(context.Background(), store.Claim{
			Source: "jira:acme", ID: "PRJ-9", ClaimedAt: now,
			Machine: "laptop", State: store.Started, UpdatedAt: now,
		}); err != nil {
			t.Fatal(err)
		}
	})
	cwd := cwds["jira:acme"]
	mock := gateMockPanes(t, tabsJSON(tabSpec{id: "w1:t9", label: "PRJ-9", status: "working"}), panesJSON(paneSpec{
		id: "w1:p9", tab: "w1:t9", status: "working", cwd: cwd,
	}), " M a.go\n?? b.go\n", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	lines := strings.Split(strings.TrimSuffix(stdout, "\n"), "\n")
	want := []string{
		"agent PRJ-9 (w1:t9) is working",
		cwd + " has uncommitted changes (2 files)",
		"in flight: 1 (claims 1, PRs 0, overlap 0) of max 1",
	}
	if len(lines) != 4 || !slices.Equal(lines[:3], want) {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.HasPrefix(lines[3], "last start ") || !strings.HasSuffix(lines[3], " ago (min 10m0s)") {
		t.Fatalf("rate line = %q", lines[3])
	}
	assertGateCalls(t, mock, cwd)
}

func TestGateGitError(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "fatal: not a git repository\n", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "git status: fatal: not a git repository\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateHerdrError(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillFail("herdr down", 1).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitError, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "herdr down") {
		t.Fatalf("stderr = %q", stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	if len(mock.Executions()) != 1 {
		t.Fatalf("executions = %d, want 1", len(mock.Executions()))
	}
}

func TestGateSkipsCallerPane(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "w1:p1")
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMockPanes(t, tabsJSON(tabSpec{id: "w1:t1", label: "coord", status: "working"}), panesJSON(paneSpec{
		id: "w1:p1", tab: "w1:t1", status: "working",
	}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "OK: 0 in flight (max 3)\n" || stderr != "" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	assertGateCalls(t, mock, cwds["jira:acme"])
}

func TestGateOtherPaneRefuses(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "w1:p1")
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMockPanes(t, tabsJSON(
		tabSpec{id: "w1:t1", label: "coord", status: "working"},
		tabSpec{id: "w1:t2", label: "PRJ-2", status: "working"},
	), panesJSON(
		paneSpec{id: "w1:p1", tab: "w1:t1", status: "working", cwd: cwds["jira:acme"]},
		paneSpec{id: "w1:p2", tab: "w1:t2", status: "working", cwd: cwds["jira:acme"]},
	), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "agent PRJ-2 (w1:t2) is working\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	assertGateCalls(t, mock, cwds["jira:acme"])
}

func TestGateSameTabMarksThisPane(t *testing.T) {
	t.Setenv("HERDR_PANE_ID", "w1:p1")
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMockPanes(t, tabsJSON(tabSpec{id: "w1:t1", label: "coord", status: "working"}), panesJSON(
		paneSpec{id: "w1:p1", tab: "w1:t1", status: "working", cwd: cwds["jira:acme"]},
		paneSpec{id: "w1:p2", tab: "w1:t1", status: "working", cwd: cwds["jira:acme"]},
	), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "agent coord (w1:t1) is working (this pane)\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGatePaneListError(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "tab", "list").
		WillSucceed(tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillFail("herdr down", 1).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, "herdr down") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	if len(mock.Executions()) != 2 {
		t.Fatalf("executions = %d, want 2", len(mock.Executions()))
	}
}

func TestGateListsEveryWorkingTab(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	cwd := cwds["jira:acme"]
	mock := gateMockPanes(t, tabsJSON(
		tabSpec{id: "w1:t2", label: "PRJ-2", status: "working"},
		tabSpec{id: "w1:t3", label: "PRJ-3", status: "working"},
	), panesJSON(
		paneSpec{id: "w1:p2", tab: "w1:t2", status: "working", cwd: cwd},
		paneSpec{id: "w1:p3", tab: "w1:t3", status: "working", cwd: cwd + "/pkg"},
	), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "agent PRJ-2 (w1:t2) is working\nagent PRJ-3 (w1:t3) is working\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateIgnoreLists(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[gate]
ignore_panes = ["w1:p2"]
ignore_tabs = ["dashboard"]
`, gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMockPanes(t, tabsJSON(
		tabSpec{id: "w1:t8", label: "dashboard", status: "working"},
		tabSpec{id: "w1:t2", label: "worker", status: "working"},
	), panesJSON(
		paneSpec{id: "w1:p8", tab: "w1:t8", status: "working", cwd: cwds["jira:acme"]},
		paneSpec{id: "w1:p2", tab: "w1:t2", status: "working", cwd: cwds["jira:acme"]},
	), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "OK: 0 in flight (max 3)\n" || stderr != "" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	assertGateCalls(t, mock, cwds["jira:acme"])
}

func TestGateSourceCwd(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[defaults]
max_in_flight = 1
min_start_interval = "10m"
`, gateSrc{name: "acme"}, gateSrc{name: "cub"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	old := time.Now().UTC().Add(-2 * time.Hour)
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.InsertClaim(context.Background(), store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: old,
			Machine: "laptop", State: store.Started, UpdatedAt: old,
		}); err != nil {
			t.Fatal(err)
		}
	})
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--source", "jira:cub", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "in flight: 1 (claims 1, PRs 0, overlap 0) of max 1\n" {
		t.Fatalf("stdout = %q", stdout)
	}
	assertGateCalls(t, mock, cwds["jira:cub"])
}

func TestGateSharedCheckout(t *testing.T) {
	shared := t.TempDir()
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme", cwd: shared}, gateSrc{name: "cub", cwd: shared})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if cwds["jira:acme"] != shared || cwds["jira:cub"] != shared {
		t.Fatalf("cwds = %#v", cwds)
	}
	assertGateCalls(t, mock, shared)
}

func TestGateAmbiguousCheckout(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, "", gateSrc{name: "acme"}, gateSrc{name: "cub"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q", stdout)
	}
	want := fmt.Sprintf("gate: expected one checkout, found jira:acme=%s, jira:cub=%s; pass --source", cwds["jira:acme"], cwds["jira:cub"])
	if !strings.Contains(stderr, want) {
		t.Fatalf("stderr = %q, want %q", stderr, want)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestGateUnknownSource(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--source", "acme", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "jira:acme") || !strings.Contains(stderr, `found "acme"`) {
		t.Fatalf("stderr = %q", stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestGateNoSources(t *testing.T) {
	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(cfgPath, []byte(""), 0o600); err != nil {
		t.Fatal(err)
	}
	dbPath := filepath.Join(dir, "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, "expected at least one source, found none") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestGateNilExecutor(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	useExec(t, nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, "expected an executor, found nil") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func TestGateUsage(t *testing.T) {
	stdout, stderr, code := runCmd(t, "gate", "extra")
	if code != ExitUsage {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != "" || strings.HasPrefix(stderr, "factory:") || !strings.Contains(stderr, "expected no arguments") {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
}

func TestGateCwd(t *testing.T) {
	acme := config.Source{Name: "acme", Kind: "jira", Cwd: "/acme"}
	cub := config.Source{Name: "cub", Kind: "jira", Cwd: "/cub"}
	tests := []struct {
		name    string
		cfg     *config.Config
		only    string
		want    string
		wantErr string
	}{
		{name: "nil_config", wantErr: "found nil"},
		{name: "no_sources", cfg: &config.Config{}, wantErr: "expected at least one source, found none"},
		{name: "one_source", cfg: &config.Config{Sources: []config.Source{acme}}, want: "/acme"},
		{
			name: "shared",
			cfg:  &config.Config{Sources: []config.Source{acme, {Name: "cub", Kind: "jira", Cwd: "/acme"}}},
			want: "/acme",
		},
		{
			name:    "different",
			cfg:     &config.Config{Sources: []config.Source{acme, cub}},
			wantErr: "jira:acme=/acme, jira:cub=/cub",
		},
		{
			name: "source_flag",
			cfg:  &config.Config{Sources: []config.Source{acme, cub}},
			only: "jira:cub",
			want: "/cub",
		},
		{
			name:    "unknown",
			cfg:     &config.Config{Sources: []config.Source{acme}},
			only:    "acme",
			wantErr: `found "acme"`,
		},
		{
			name:    "empty_cwd",
			cfg:     &config.Config{Sources: []config.Source{{Name: "acme", Kind: "jira"}}},
			only:    "jira:acme",
			wantErr: "expected a cwd, found empty",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := gateCwd(tc.cfg, tc.only)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("gateCwd() = %q, %v, want error %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("gateCwd() = %q, %v, want %q", got, err, tc.want)
			}
		})
	}
}

type gateSrc struct {
	name    string
	cwd     string
	pattern string
}

type tabSpec struct {
	id     string
	label  string
	status string
}

type paneSpec struct {
	id     string
	tab    string
	status string
	cwd    string
	fg     string
}

func writeGateConfig(t *testing.T, defaults string, sources ...gateSrc) (string, map[string]string) {
	t.Helper()
	dir := t.TempDir()
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte("secret\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cwds := make(map[string]string, len(sources))
	var body strings.Builder
	body.WriteString(leaseOff(defaults))
	if defaults != "" {
		body.WriteString(strings.TrimSpace(defaults))
		body.WriteByte('\n')
	}
	for _, src := range sources {
		cwd := src.cwd
		if cwd == "" {
			cwd = t.TempDir()
		}
		cwds["jira:"+src.name] = cwd
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
`, src.name, token, cwd)
		if src.pattern != "" {
			fmt.Fprintf(&body, "pr_title_pattern = %q\n", src.pattern)
		}
	}
	path := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(path, []byte(body.String()), 0o600); err != nil {
		t.Fatal(err)
	}
	return path, cwds
}

// leaseOffTOML turns the agentbus lease gate off. The config helpers add it
// so the tests written before the lease keep running no agentbus command.
// The lease tests write their own [lease] table.
const leaseOffTOML = "[lease]\ngate = \"off\"\n"

// leaseOff is leaseOffTOML unless text already has a [lease] table.
func leaseOff(text string) string {
	if strings.Contains(text, "[lease]") {
		return ""
	}
	return leaseOffTOML
}

func tabsJSON(tabs ...tabSpec) string {
	var body strings.Builder
	body.WriteString(`{"id":"cli:tab:list","result":{"type":"tab_list","tabs":[`)
	for i, tab := range tabs {
		if i > 0 {
			body.WriteByte(',')
		}
		fmt.Fprintf(&body, `{"agent_status":%q,"label":%q,"pane_count":1,"tab_id":%q,"workspace_id":"w1"}`,
			tab.status, tab.label, tab.id)
	}
	body.WriteString(`]}}`)
	return body.String()
}

func panesJSON(panes ...paneSpec) string {
	var body strings.Builder
	body.WriteString(`{"id":"cli:pane:list","result":{"type":"pane_list","panes":[`)
	for i, pane := range panes {
		if i > 0 {
			body.WriteByte(',')
		}
		fmt.Fprintf(&body, `{"agent_status":%q,"pane_id":%q,"tab_id":%q,"workspace_id":"w1","cwd":%q,"foreground_cwd":%q}`,
			pane.status, pane.id, pane.tab, pane.cwd, pane.fg)
	}
	body.WriteString(`]}}`)
	return body.String()
}

func gateMock(t *testing.T, tabs, gitOut, gitFail string, gitErr error) *executor.MockExecutor {
	t.Helper()
	return gateMockPanes(t, tabs, emptyPanes, gitOut, gitFail, gitErr)
}

func gateMockPanes(t *testing.T, tabs, panes, gitOut, gitFail string, gitErr error) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(panes, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	switch {
	case gitErr != nil:
		mock.ExpectCommand("git").WillError(gitErr).Once().Build()
	case gitFail != "":
		mock.ExpectCommand("git").WillFail(gitFail, 128).Once().Build()
	default:
		mock.ExpectCommand("git").WillSucceed(gitOut, 0).Once().Build()
	}
	useExec(t, mock)
	return mock
}

func assertGateCalls(t *testing.T, mock *executor.MockExecutor, cwd string) {
	t.Helper()
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	calls := mock.Executions()
	if len(calls) != 4 {
		t.Fatalf("executions = %d, want 4", len(calls))
	}
	if calls[0].Command != "herdr" || !slices.Equal(calls[0].Args, []string{"tab", "list"}) {
		t.Fatalf("first call = %s %#v", calls[0].Command, calls[0].Args)
	}
	if calls[1].Command != "herdr" || !slices.Equal(calls[1].Args, []string{"pane", "list"}) {
		t.Fatalf("pane call = %s %#v", calls[1].Command, calls[1].Args)
	}
	if calls[2].Command != "gh" {
		t.Fatalf("gh call = %s %#v", calls[2].Command, calls[2].Args)
	}
	wantArgs := []string{"-C", cwd, "status", "--porcelain"}
	if calls[3].Command != "git" || !slices.Equal(calls[3].Args, wantArgs) {
		t.Fatalf("git call = %s %#v, want %#v", calls[3].Command, calls[3].Args, wantArgs)
	}
	if calls[3].WorkingDir != "" {
		t.Fatalf("WorkingDir = %q, want empty", calls[3].WorkingDir)
	}
}

func TestGateOnBase(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[gate]
base_branch = "main"
require = "on_base"
`, gateSrc{name: "acme"})
	cwd := cwds["jira:acme"]
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateBranchMock(t, cwd, "main\n", "")
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateOnBaseRefuses(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[gate]
base_branch = "main"
require = "on_base"
`, gateSrc{name: "acme"})
	cwd := cwds["jira:acme"]
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateBranchMock(t, cwd, "feature\n", "")
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := cwd + ": expected branch main, found feature\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateCleanAgainstBaseRefuses(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[gate]
base_branch = "main"
require = "clean_against_base"
`, gateSrc{name: "acme"})
	cwd := cwds["jira:acme"]
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateBranchMock(t, cwd, "feature\n", "2\n")
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := cwd + ": expected no commits beyond origin/main, found 2 on feature\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

const oneOpenPR = `{
  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": [{
    "number": 7,
    "title": "review me",
    "url": "https://github.com/acme/widgets/pull/7",
    "isDraft": false,
    "merged": false,
    "mergedAt": null,
    "createdAt": "2026-01-01T00:00:00Z",
    "closedAt": null,
    "headRefName": "feature",
    "repository": {"nameWithOwner": "acme/widgets"},
    "closingIssuesReferences": {"nodes": []}
  }]}}
}`

func TestGateOpenPRCounts(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[defaults]
max_in_flight = 1
`, gateSrc{name: "acme"})
	cwd := cwds["jira:acme"]
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(oneOpenPR, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "in flight: 1 (claims 0, PRs 1, overlap 0) of max 1\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	assertGateCalls(t, mock, cwd)
}

func TestGateOpenPRFailure(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("gh").WillError(errors.New("gh down")).Once().Build()
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitError || stdout != "" || !strings.Contains(stderr, "gh open prs") || !strings.Contains(stderr, "gh down") {
		t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
	if len(mock.Executions()) != 3 {
		t.Fatalf("executions = %d, want 3", len(mock.Executions()))
	}
}

func TestGateUnknownRequire(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, `
[gate]
base_branch = "main"
require = "maybe"
`, gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitError {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, `gate.require: expected on_base or clean_against_base, found "maybe"`) {
		t.Fatalf("stdout = %q stderr = %q", stdout, stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestGateJSON(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, `
[defaults]
max_in_flight = 1
`, gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	at := time.Now().UTC().Add(-2 * time.Hour)
	seedDB(t, dbPath, func(st *store.Store) {
		if err := st.InsertClaim(context.Background(), store.Claim{
			Source: "jira:acme", ID: "PRJ-9", ClaimedAt: at,
			Machine: "laptop", State: store.Started, UpdatedAt: at,
		}); err != nil {
			t.Fatal(err)
		}
	})
	mock := gateMock(t, tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"}), "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--json", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	var got struct {
		OK       bool     `json:"ok"`
		Reasons  []string `json:"reasons"`
		InFlight int      `json:"inFlight"`
		Claims   int      `json:"claims"`
		OpenPRs  int      `json:"openPRs"`
		Overlap  int      `json:"overlap"`
	}
	if err := json.Unmarshal([]byte(stdout), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	wantReason := "in flight: 1 (claims 1, PRs 0, overlap 0) of max 1"
	if got.OK || len(got.Reasons) != 1 || got.Reasons[0] != wantReason || got.InFlight != 1 || got.Claims != 1 || got.OpenPRs != 0 || got.Overlap != 0 {
		t.Fatalf("json = %+v", got)
	}
	assertGateCalls(t, mock, cwds["jira:acme"])
}

func TestGateCustomTitlePattern(t *testing.T) {
	open := `{
	  "data": {"search": {"pageInfo": {"hasNextPage": false, "endCursor": null}, "nodes": [{
	    "number": 11,
	    "title": "ID:PRJ-1",
	    "url": "https://github.com/acme/widgets/pull/11",
	    "isDraft": false,
	    "merged": false,
	    "mergedAt": null,
	    "createdAt": "2026-01-01T00:00:00Z",
	    "closedAt": null,
	    "headRefName": "wip",
	    "repository": {"nameWithOwner": "acme/widgets"},
	    "closingIssuesReferences": {"nodes": []}
	  }]}}
	}`
	at := time.Now().UTC().Add(-2 * time.Hour)
	seed := func(t *testing.T, dbPath string) {
		t.Helper()
		seedDB(t, dbPath, func(st *store.Store) {
			if err := st.InsertClaim(context.Background(), store.Claim{
				Source: "jira:acme", ID: "PRJ-1", ClaimedAt: at,
				Machine: "laptop", State: store.Started, UpdatedAt: at,
			}); err != nil {
				t.Fatal(err)
			}
		})
	}
	run := func(t *testing.T, pattern string) (string, string, int) {
		t.Helper()
		cfgPath, _ := writeGateConfig(t, `
[defaults]
max_in_flight = 2
`, gateSrc{name: "acme", pattern: pattern})
		dbPath := filepath.Join(t.TempDir(), "factory.db")
		seed(t, dbPath)
		mock := executor.NewMockExecutor()
		mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
		mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabsJSON(tabSpec{id: "w1:t1", label: "idle", status: "idle"}), 0).Once().Build()
		mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
		mock.ExpectCommand("gh").WillSucceed(open, 0).Once().Build()
		mock.ExpectCommand("git").WillSucceed("", 0).Once().Build()
		useExec(t, mock)
		return runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	}
	stdout, stderr, code := run(t, "ID:{id}")
	if code != ExitOK || stderr != "" || stdout != "OK: 1 in flight (max 2)\n" {
		t.Fatalf("custom pattern exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	stdout, stderr, code = run(t, "")
	want := "in flight: 2 (claims 1, PRs 1, overlap 0) of max 2\n"
	if code != ExitOK || stderr != "" || stdout != want {
		t.Fatalf("default pattern exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
}

func gateBranchMock(t *testing.T, cwd, branch, ahead string) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	tabs := tabsJSON(tabSpec{id: "w1:t1", label: "PRJ-1", status: "idle"})
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(tabs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", cwd, "rev-parse", "--abbrev-ref", "HEAD").WillSucceed(branch, 0).Once().Build()
	if ahead != "" {
		mock.ExpectCommandWithArgs("git", "-C", cwd, "rev-list", "--count", "origin/main..HEAD").
			WillSucceed(ahead, 0).Once().Build()
	}
	useExec(t, mock)
	return mock
}
