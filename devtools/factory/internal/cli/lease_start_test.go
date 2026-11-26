package cli

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

func ruleFor(checkout, claimFor string) string {
	return "Before changing the shared checkout (" + checkout + "), run:\n" +
		"  agentbus lease claim --for " + claimFor + " --why 'PRJ-1 Fix it'\n" +
		"If it says queued, end your turn and wait for the \"[lease] granted\" message.\n" +
		"Run `agentbus lease release` when the tree is clean and pushed. Reading needs no claim."
}

func rulePlan(mode, rule string) string {
	out := "lease    " + mode + ", rule at the top of the prompt:\n"
	for _, line := range strings.Split(rule, "\n") {
		out += "         " + line + "\n"
	}
	return out
}

func leasePlan(cwd, mode, rule string) string {
	first, _, _ := strings.Cut(rule, "\n")
	return "claim    PRJ-1 → In Progress (assign to me)\n" +
		"comment  Claimed by claude at <timestamp>.\n" +
		"tab      workspace \"factory\" / tab \"PRJ-1\" (cwd " + cwd + ")\n" +
		"agent    claude\n" +
		"args     (default)\n" +
		"prompt   " + first + "…\n" +
		rulePlan(mode, rule)
}

// leaseApplyMock is startMockFull with apply set and the prompt given.
func leaseApplyMock(t *testing.T, cwd, prompt string) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "list").WillSucceed(emptyTabs, 0).Times(2).Build()
	mock.ExpectCommand("gh").WillSucceed(emptyPRs, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "pane", "list").WillSucceed(emptyPanes, 0).Once().Build()
	mock.ExpectCommand("git").WillSucceed("", 0).Times(2).Build()
	mock.ExpectCommandWithArgs("herdr", "workspace", "list").WillSucceed(rankWorkspaces, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "tab", "create", "--workspace", "w1", "--label", "PRJ-1", "--cwd", cwd, "--env", "FACTORY_ITEM_SOURCE=jira:acme", "--env", "FACTORY_OUTPUT_MODE=pr", "--no-focus").
		WillSucceed(startTab, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "start", "prj-1", "--kind", "claude", "--pane", "w1:p1", "--timeout", "60000").
		WillSucceed(startAgent, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "wait", "w1:p1", "--until", "idle", "--timeout", "60000").WillSucceed(agentIdle, 0).Once().Build()
	mock.ExpectCommandWithArgs("herdr", "agent", "prompt", "w1:p1", prompt, "--wait", "--until", "working", "--timeout", "30000").
		WillSucceed(startPrompted, 0).Once().Build()
	useExec(t, mock)
	return mock
}

func TestStartNoLeaseSectionNeverCallsAgentbus(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := leaseApplyMock(t, cwd, startPrompt)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" || !strings.HasPrefix(stdout, startPlan(cwd)+"factory start PRJ-1: started (tab w1:t1, claim ") {
		t.Fatalf("exit = %d, stdout =\n%s\nstderr = %s", code, stdout, stderr)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	for _, call := range mock.Executions() {
		if call.Command == "agentbus" {
			t.Fatalf("agentbus was called: %#v", call.Args)
		}
	}
}

func TestStartLeasePromptDryRun(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[lease]\ngate = \"off\"\nmode = \"prompt\"\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	mock := startMock(t, rankWorkspaces, emptyTabs, false, cwd)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if want := leasePlan(cwd, "prompt", ruleFor(cwd, "45m")); stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartLeasePromptCheckApply(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[lease]\ngate = \"off\"\nmode = \"prompt+check\"\nclaim_for = \"30m\"\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	rule := ruleFor(cwd, "30m")
	mock := leaseApplyMock(t, cwd, rule+"\n\n"+startPrompt)
	expectLease(mock, cwd, leaseHeldJSON)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	want := leasePlan(cwd, "prompt+check", rule) + "lease: held by pm/foo until " + heldUntil() + " (1 queued)\n"
	if !strings.HasPrefix(stdout, want+"factory start PRJ-1: started (tab w1:t1, claim ") {
		t.Fatalf("stdout =\n%s\nwant prefix =\n%s", stdout, want)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	logs, logErr, logCode := runCmd(t, "logs", "--config", cfgPath, "--db", dbPath)
	if logCode != ExitOK || logErr != "" || !strings.Contains(logs, "started PRJ-1") {
		t.Fatalf("logs exit = %d stdout = %q stderr = %q", logCode, logs, logErr)
	}
}

func TestStartLeasePromptCheckMissingAgentbus(t *testing.T) {
	cfgPath, cwd := writeStartConfig(t, "[lease]\ngate = \"off\"\nmode = \"prompt+check\"\n", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	useSources(t, map[string]*fake.Source{"acme": startFake()})
	rule := ruleFor(cwd, "45m")
	mock := leaseApplyMock(t, cwd, rule+"\n\n"+startPrompt)
	expectLeaseMissing(mock)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--apply", "--config", cfgPath, "--db", dbPath)
	if code != ExitOK || stderr != "" {
		t.Fatalf("exit = %d, stderr = %s", code, stderr)
	}
	if !strings.Contains(stdout, "\nlease: unknown (agentbus not installed)\n") || !strings.Contains(stdout, "factory start PRJ-1: started (tab w1:t1, claim ") {
		t.Fatalf("stdout =\n%s", stdout)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestStartLeasePromptLaneCheckout(t *testing.T) {
	setup := writeLaneConfig(t, laneSetup{defaults: "[lease]\ngate = \"off\"\nmode = \"prompt\"\n"})
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
	rule := ruleFor(macCheckout, "45m")
	plan := leasePlan(macCheckout, "prompt", rule)
	want := strings.TrimSuffix(plan, rulePlan("prompt", rule)) + "lane     mac (herdr --machine mac1)\n" + rulePlan("prompt", rule)
	if stdout != want {
		t.Fatalf("stdout =\n%s\nwant =\n%s", stdout, want)
	}
}

func TestLaneSourceLeaseOverride(t *testing.T) {
	src := config.Source{Cwd: "/src", Lease: config.LeasePrompt{Mode: "prompt", ClaimFor: "45m"}}
	lane := config.Lane{Checkout: "/lane", Lease: config.LeasePrompt{ClaimFor: "20m"}}
	got := config.LaneSource(src, lane)
	if got.Cwd != "/lane" || got.Lease != (config.LeasePrompt{Mode: "prompt", ClaimFor: "20m"}) {
		t.Fatalf("lane source = %+v", got)
	}
}
