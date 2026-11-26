package cli

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const (
	macCheckout = "/path/on/mac/checkout"
	macDown     = `[{"id":"m1","label":"mac1","status":"error","error":"ssh: connect to host mac1 port 22: Operation timed out"}]`
)

// laneSetup is a config with a local linux lane and a remote mac lane.
type laneSetup struct {
	cfgPath  string
	linux    string // Local linux lane checkout.
	defaults string // Extra TOML before the lanes.
	order    string // [lanes] default, as a TOML array body.
	linuxMax int
}

func writeLaneConfig(t *testing.T, setup laneSetup) laneSetup {
	t.Helper()
	setup.linux = t.TempDir()
	if setup.order == "" {
		setup.order = `"linux", "mac"`
	}
	if setup.linuxMax == 0 {
		setup.linuxMax = 2
	}
	lanes := fmt.Sprintf(`%s
[[lane]]
name = "linux"
checkout = %q
max_workers = %d
labels = ["needs-linux"]

[[lane]]
name = "mac"
machine = "mac1"
checkout = %q
max_workers = 2
labels = ["needs-mac"]

[lanes]
default = [%s]
`, setup.defaults, setup.linux, setup.linuxMax, macCheckout, setup.order)
	setup.cfgPath, _ = writeStartConfig(t, lanes, "")
	return setup
}

func laneMock(t *testing.T) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	return mock
}

// expectSnapshot is the local workspace, tab, and pull request reads start does first.
func expectSnapshot(mock *executor.MockExecutor) {
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
}

func expectLinuxLane(mock *executor.MockExecutor, checkout string) {
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommandWithArgs("git", "-C", checkout, "status", "--porcelain").WillSucceed("", 0).Once().Build()
}

func expectMacUp(mock *executor.MockExecutor) {
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
}

func expectMacDown(mock *executor.MockExecutor) {
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "tab", "list").WillTimeout(15 * time.Second).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "machine", "status", "--json", "mac1").WillSucceed(macDown, 0).Once().Build()
}

func seedLaneClaims(t *testing.T, dbPath string, lanes ...string) {
	t.Helper()
	now := time.Now().UTC().Add(-time.Hour)
	seedDB(t, dbPath, func(st *store.Store) {
		for i, lane := range lanes {
			if err := st.InsertClaim(context.Background(), store.Claim{
				Source: "jira:acme", ID: fmt.Sprintf("PRJ-%d", 90+i), ClaimedAt: now,
				Machine: "laptop", State: store.Started, UpdatedAt: now, Lane: lane,
			}); err != nil {
				t.Fatal(err)
			}
		}
	})
}

func TestGateLanesMacUnreachable(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := laneMock(t)
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	expectLinuxLane(mock, setup.linux)
	expectMacDown(mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := "OK: 0 in flight (max 3)\n" +
		"linux: OK workers 0/2 (running), active 0\n" +
		"mac: unreachable (skipped): host unreachable: ssh: connect to host mac1 port 22: Operation timed out\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateLanesMaxWorkersCountsOnlyThatLane(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{defaults: "[defaults]\nmax_in_flight = 10\n"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	// Two claims on mac, and one recorded before lanes existed, which counts for no lane.
	seedLaneClaims(t, dbPath, "mac", "mac", "")
	mock := laneMock(t)
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	expectLinuxLane(mock, setup.linux)
	expectMacUp(mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := "OK: 3 in flight (max 10)\n" +
		"linux: OK workers 0/2 (running), active 0\n" +
		"mac: blocked workers 2/2 (running), active 2: 2 running workers on lane mac (max_workers 2)\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateLanesRejectsSource(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	mock := laneMock(t)
	_, stderr, code := runCmd(t, "gate", "--source", "jira:acme", "--config", setup.cfgPath, "--db", filepath.Join(t.TempDir(), "factory.db"))
	if code != ExitUsage || !strings.Contains(stderr, "expected no --source with [[lane]] config") {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func lanePlan(cwd, lane string) string {
	return startPlan(cwd) + "lane     " + lane + "\n"
}

func TestStartLaneMacApply(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedLabeledStart(t, dbPath, []string{"needs-mac"})
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := laneMock(t)
	expectSnapshot(mock)
	expectMacUp(mock)
	mac := func(args ...string) *executor.MockExpectationBuilder {
		return mock.ExpectCommandWithArgs("herdr", append([]string{"--machine", "mac1"}, args...)...)
	}
	mac("workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mac("tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mac("workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mac("tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", macCheckout, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").WillSucceed(startTab, 0).Once().Build()
	mac("agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").WillSucceed(startAgent, 0).Once().Build()
	mac("agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
	mac("agent", "prompt", "w1:p1", startPrompt, "--wait", "--until", "working", "--timeout", "30000").WillSucceed(startPrompted, 0).Once().Build()
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if want := lanePlan(macCheckout, "mac (herdr --machine mac1)"); !strings.HasPrefix(stdout, want+"factory start PRJ-1: started (tab w1:t1, claim ") {
		t.Fatalf("stdout =\n%s\nwant prefix =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	for i, call := range mock.Executions() {
		if call.Command == "git" {
			t.Fatalf("call %d ran git %v on a remote lane", i, call.Args)
		}
		if i >= 3 && call.Command == "herdr" && !slices.Equal(call.Args[:2], []string{"--machine", "mac1"}) {
			t.Fatalf("call %d = herdr %v, want --machine mac1", i, call.Args)
		}
	}
	assertSourceCalls(t, src, []string{"Get", "ClaimWithOptions", "Get", "PostClaimComment"})
	claim := latestTestClaim(t, dbPath)
	if claim.Lane != "mac" || claim.State != store.Started || claim.StashCount != nil || claim.TabID != "w1:t1" {
		t.Fatalf("claim = %+v", claim)
	}
}

func latestTestClaim(t *testing.T, dbPath string) store.Claim {
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
	return got
}

func TestStartLaneUnlabeledSkipsUnreachableMac(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{order: `"mac", "linux"`})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := laneMock(t)
	expectSnapshot(mock)
	expectMacDown(mock)
	expectLinuxLane(mock, setup.linux)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if want := lanePlan(setup.linux, "linux (local herdr)"); stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartLaneFullFirstLaneFallsThrough(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{linuxMax: 1})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	seedLaneClaims(t, dbPath, "linux")
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := laneMock(t)
	expectSnapshot(mock)
	expectLinuxLane(mock, setup.linux)
	expectMacUp(mock)
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if want := lanePlan(macCheckout, "mac (herdr --machine mac1)"); stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartLaneNoneAvailableNamesEachLane(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{linuxMax: 1})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	seedLaneClaims(t, dbPath, "linux")
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := laneMock(t)
	expectSnapshot(mock)
	expectLinuxLane(mock, setup.linux)
	expectMacDown(mock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitStartRefused {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	wantErr := "start PRJ-1: gate: lane linux: blocked workers 1/1 (running), active 1: 1 running workers on lane linux (max_workers 1); " +
		"lane mac: unreachable (skipped): host unreachable: ssh: connect to host mac1 port 22: Operation timed out"
	if !strings.Contains(stderr, wantErr) {
		t.Fatalf("stderr = %q, want %q", stderr, wantErr)
	}
	if want := "blocked  yes\n" + lanePlan(setup.linux, "linux (local herdr)") + "factory start PRJ-1: refused, nothing changed: " + wantErr + "\n"; stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartLaneMachineIncompatible(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedLabeledStart(t, dbPath, []string{"needs-linux"})
	mock := laneMock(t)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--machine", "mac", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitError || stdout != "" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if !strings.Contains(stderr, "start PRJ-1: --machine mac: lane mac is not compatible with the item labels; expected one of linux") {
		t.Fatalf("stderr = %q", stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestStartLaneMachinePicksLane(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := laneMock(t)
	expectSnapshot(mock)
	expectMacUp(mock)
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--machine", "mac1", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if want := lanePlan(macCheckout, "mac (herdr --machine mac1)"); stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartLaneConflictingLabels(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedLabeledStart(t, dbPath, []string{"needs-linux", "needs-mac"})
	mock := laneMock(t)
	_, stderr, code := runCmd(t, "start", "PRJ-1", "--config", setup.cfgPath, "--db", dbPath)
	want := "start PRJ-1: lane config: expected item labels to match at most one lane, found linux (needs-linux); mac (needs-mac)"
	if code != ExitError || !strings.Contains(stderr, want) {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestNextLaneConflictingLabels(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedLabeledStart(t, dbPath, []string{"needs-linux", "needs-mac"})
	useHerdrGH(t, rankWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--config", setup.cfgPath, "--db", dbPath)
	want := "next PRJ-1: lane config: expected item labels to match at most one lane"
	if code != ExitError || stdout != "" || !strings.Contains(stderr, want) {
		t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
}

func TestNextExplainLanes(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedLabeledStart(t, dbPath, []string{"needs-mac"})
	useHerdrGH(t, rankWorkspaces, emptyTabs, emptyPRs)
	stdout, stderr, code := runCmd(t, "next", "--explain", "PRJ-1", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.Contains(stdout, "\nlanes  mac\n") {
		t.Fatalf("exit = %d, stdout = %q, stderr = %q", code, stdout, stderr)
	}
}

func TestStartMachineWithoutLanes(t *testing.T) {
	cfgPath, _ := writeStartConfig(t, "", "")
	mock := laneMock(t)
	_, stderr, code := runCmd(t, "start", "PRJ-1", "--machine", "mac", "--config", cfgPath, "--db", filepath.Join(t.TempDir(), "factory.db"))
	if code != ExitUsage || !strings.Contains(stderr, "start --machine mac: expected [[lane]] config, found none") {
		t.Fatalf("exit = %d, stderr = %q", code, stderr)
	}
	if len(mock.Executions()) != 0 {
		t.Fatalf("executions = %d, want 0", len(mock.Executions()))
	}
}

func TestAdoptLaneRemoteTab(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := laneMock(t)
	expectSnapshot(mock)
	remoteTabs := tabsJSON(tabSpec{id: "w1:t7", label: "PRJ-1", status: "working"})
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "--machine", "mac1", "tab", "list").WillSucceed(remoteTabs, 0).Once().Build()
	stdout, stderr, code := runCmd(t, "adopt", "PRJ-1", "--machine", "mac", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stdout != "" || stderr != "" {
		t.Fatalf("exit = %d stdout = %q stderr = %q", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	claim := latestTestClaim(t, dbPath)
	if claim.Lane != "mac" || claim.TabID != "w1:t7" || claim.State != store.Claimed {
		t.Fatalf("claim = %+v", claim)
	}
}

func TestAdoptLaneNeedsMachineWhenRoutedToMany(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := laneMock(t)
	expectSnapshot(mock)
	_, stderr, code := runCmd(t, "adopt", "PRJ-1", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitError || !strings.Contains(stderr, "adopt PRJ-1: expected --machine naming one of linux, mac, found none") {
		t.Fatalf("exit = %d stderr = %q", code, stderr)
	}
}

func TestTickLanesDryRun(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := laneMock(t)
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Times(2).Build()
	mock.ExpectCommandWithArgs("git", "-C", setup.linux, "status", "--porcelain").WillSucceed("", 0).Once().Build()
	expectMacDown(mock)
	stdout, stderr, code := runCmd(t, "tick", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %q, stdout = %s", code, stderr, stdout)
	}
	for _, want := range []string{
		"dry-run PRJ-1 in factory/PRJ-1 (lane linux)\n",
		"lane mac: skipped: host unreachable: ssh: connect to host mac1 port 22: Operation timed out\n",
	} {
		if !strings.Contains(stdout, want) {
			t.Fatalf("stdout = %q, want %q", stdout, want)
		}
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}
