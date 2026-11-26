package cli

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

const (
	leaseGuardedJSON = `{"guarded": true, "checkout": "local:/repo"}`
	leaseFreeJSON    = `{"checkout": "local:/repo", "state": "free", "queue": [], "takeover": "log"}`
	leaseHeldJSON    = `{"checkout": "local:/repo", "state": "held",
 "holder": {"handle": "herdr:local/default/w1:p2", "name": "pm/foo", "since": "2026-11-11T13:00:00Z", "deadline": "2026-11-11T14:20:00Z", "why": "PRJ-9 other work"},
 "queue": [{"handle": "herdr:local/default/w1:p3", "name": "", "urgent": false, "for": "40m", "since": "2026-11-11T13:10:00Z"}]}`
	leaseHeldByPRJ1 = `{"checkout": "local:/repo", "state": "held",
 "holder": {"handle": "herdr:local/default/w1:p2", "name": "", "since": "2026-11-11T13:00:00Z", "deadline": "2026-11-11T14:20:00Z", "why": "PRJ-1 Fix it"},
 "queue": []}`
)

func heldUntil() string {
	t, err := time.Parse(time.RFC3339, "2026-11-11T14:20:00Z")
	if err != nil {
		panic(err)
	}
	return t.Local().Format("15:04")
}

// expectLease adds the guarded check and the status read for dir.
func expectLease(mock *executor.MockExecutor, dir, status string) {
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--guarded", "--json", "--path", dir).WillSucceed(leaseGuardedJSON, 0).Once().Build()
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--path", dir, "--json").WillSucceed(status, 0).Once().Build()
}

func expectLeaseMissing(mock *executor.MockExecutor) {
	mock.ExpectCommand("agentbus").WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Once().Build()
}

func TestGateLeaseFree(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, "[lease]\ngate = \"auto\"\n", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, emptyTabs, "", "", nil)
	expectLease(mock, cwds["jira:acme"], leaseFreeJSON)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateLeaseHeldByAnotherAgent(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, "[lease]\ngate = \"auto\"\n", gateSrc{name: "acme"})
	cwd := cwds["jira:acme"]
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, emptyTabs, "", "", nil)
	expectLease(mock, cwd, leaseHeldJSON)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := cwd + ": lease held by pm/foo until " + heldUntil() + " (1 queued)\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateLeaseDefaultIsAutoMissingAgentbus(t *testing.T) {
	// No [lease] table: the default gate is auto. leaseOff only skips a
	// config that already names [lease], so write one with the default.
	cfgPath, cwds := writeGateConfig(t, "[lease]\n", gateSrc{name: "acme"})
	cwd := cwds["jira:acme"]
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, emptyTabs, "", "", nil)
	expectLeaseMissing(mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := "OK: 0 in flight (max 3)\n" +
		"note: " + cwd + ": lease not read (agentbus not installed); used the herdr and git checks only\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestGateLeaseRequiredMissingAgentbus(t *testing.T) {
	cfgPath, cwds := writeGateConfig(t, "[lease]\ngate = \"required\"\n", gateSrc{name: "acme"})
	cwd := cwds["jira:acme"]
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, emptyTabs, "", "", nil)
	expectLeaseMissing(mock)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := cwd + ": lease not read (agentbus not installed); lease gate is required\n"
	if stdout != want {
		t.Fatalf("stdout = %q, want %q", stdout, want)
	}
}

func TestGateLeaseOffNeverCallsAgentbus(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "[lease]\ngate = \"off\"\n", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := gateMock(t, emptyTabs, "", "", nil)
	stdout, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || stdout != "OK: 0 in flight (max 3)\n" {
		t.Fatalf("exit = %d, stdout = %q, stderr = %s", code, stdout, stderr)
	}
	for _, call := range mock.Executions() {
		if call.Command == "agentbus" {
			t.Fatalf("agentbus was called: %#v", call.Args)
		}
	}
}

func TestGateLeaseUnknownMode(t *testing.T) {
	cfgPath, _ := writeGateConfig(t, "[lease]\ngate = \"maybe\"\n", gateSrc{name: "acme"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	useExec(t, mock)
	_, stderr, code := runCmd(t, "gate", "--config", cfgPath, "--db", dbPath)
	if code == ExitOK || !strings.Contains(stderr, `lease.gate: expected auto, off, or required, found "maybe"`) {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
}

func TestStartLeaseHeldByThisItemProceeds(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[lease]\ngate = \"required\"\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	expectLease(mock, cwd, leaseHeldByPRJ1)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if stdout != startPlan(cwd) {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, startPlan(cwd))
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartLeaseHeldByAnotherRefuses(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[lease]\ngate = \"auto\"\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	expectLease(mock, cwd, leaseHeldJSON)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitStartRefused {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	wantOut := "blocked  yes\n" + startPlan(cwd) + "factory start PRJ-1: refused, nothing changed: start PRJ-1: gate: " + cwd + ": lease held by pm/foo until " + heldUntil() + " (1 queued)\n"
	if stdout != wantOut || !strings.Contains(stderr, "gate: "+cwd+": lease held by pm/foo until "+heldUntil()+" (1 queued)") {
		t.Fatalf("stdout =\n%s\nstderr = %q", stdout, stderr)
	}
	assertNoClaimCall(t, src)
}

func TestStartLeaseNotePrinted(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[lease]\ngate = \"auto\"\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	expectLeaseMissing(mock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := startPlan(cwd) + "note     " + cwd + ": lease not read (agentbus not installed); used the herdr and git checks only\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
}

func TestGateLanesLease(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{defaults: "[lease]\ngate = \"auto\"\n"})
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	mock := laneMock(t)
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Once().Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	expectLinuxLane(mock, setup.linux)
	expectLease(mock, setup.linux, leaseHeldJSON)
	expectMacUp(mock)
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--all", "--json").
		WillSucceed(`[{"checkout": "mac1:`+macCheckout+`", "state": "free", "queue": []}]`, 0).Once().Build()
	stdout, stderr, code := runCmd(t, "gate", "--config", setup.cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := "OK: 0 in flight (max 3)\n" +
		"linux: blocked workers 0/2 (running), active 0: " + setup.linux + ": lease held by pm/foo until " + heldUntil() + " (1 queued)\n" +
		"mac: OK workers 0/2 (running), active 0 (checkout and lease not checked on remote machine mac1)\n"
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}
