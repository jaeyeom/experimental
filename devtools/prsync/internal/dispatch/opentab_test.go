package dispatch

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

// newTabAgent is the agent the fake herdr's default `tab create` root
// pane runs once it has started.
const newTabAgent = `{"pane_id":"w9:p1","tab_id":"w9:t1","agent":"claude","agent_status":"idle"}`

// openFake is a fake herdr whose machine m has no PR tab, and whose new
// tab's agent is idle.
func openFake(t *testing.T, m string) *herdrtest.Fake {
	t.Helper()
	f := herdrtest.Install(t)
	for _, name := range []string{"local", m} {
		f.Write(name+".agents", `{"result":{"agents":[]}}`)
	}
	f.Write(m+".agents", `{"result":{"agents":[`+newTabAgent+`]}}`)
	f.Write(m+".prompt", `{"result":{"agent":`+newTabAgent+`}}`)
	return f
}

// openCfg turns on open_missing_tabs for a live local run.
func openCfg(t *testing.T) (config.Config, FileStore) {
	t.Helper()
	cfg, store := liveCfg(t)
	cfg.DispatchTimeout = 5 * time.Second
	cfg.OpenMissingTabs = true
	cfg.OpenTabsCwd = map[string]string{config.LocalMachine: "/src/widgets"}
	cfg.OpenTabsAgent = []string{"claude", "--model", "opus"}
	return cfg, store
}

func untabbedPR(n int) scan.PR {
	pr := fixtureEligiblePR()
	pr.Tab = nil
	if n != pr.Number {
		id := fmt.Sprintf("PROJ-%d", n)
		pr.Number = n
		pr.Identifier = &id
		pr.BlockingComments[0].CommentID = fmt.Sprintf("PRRC_%d", n)
	}
	return pr
}

func openRequest(prs ...scan.PR) (Request, *herdr.Client) {
	local := herdr.NewClient(executor.NewBasicExecutor(), "herdr")
	return Request{
		Doc:    scan.Document{PRs: prs},
		Remote: func(m string) Herdr { return local.OnMachine(m, 2*time.Second) },
		Opener: func(m string) TabOpener {
			if m == "" {
				return local
			}
			return local.OnMachine(m, 2*time.Second)
		},
	}, local
}

// argvIndex is the index of the first argv line containing want, or -1.
func argvIndex(argv []string, want string) int {
	for i, line := range argv {
		if strings.Contains(line, want) {
			return i
		}
	}
	return -1
}

func TestRunLiveOpensATabForAnUntabbedPR(t *testing.T) {
	f := openFake(t, "local")
	f.Write("local.workspaces", `{"result":{"workspaces":[{"workspace_id":"w5","label":"prs"}]}}`)
	cfg, store := openCfg(t)
	cfg.OpenTabsWorkspace = map[string]string{config.LocalMachine: "prs"}
	req, local := openRequest(untabbedPR(123))

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 {
		t.Fatalf("results = %+v, want one", got.Results)
	}
	item := got.Results[0]
	if item.Action != ActionOpenedTabAndDispatched || item.TabID != "w9:t1" || item.PaneID != "w9:p1" || item.RenderedPrompt == "" {
		t.Fatalf("result = %+v, want opened_tab_and_dispatched to w9:p1 in w9:t1", item)
	}
	argv := f.Argv()
	steps := []string{
		"tab create --workspace w5 --cwd /src/widgets --label PROJ-123 --no-focus",
		"agent start pr-proj-123 --kind claude --pane w9:p1 --timeout 90000 -- --model opus",
		"agent wait w9:p1 --until idle --timeout ",
		"agent prompt w9:p1 ",
	}
	last := -1
	for _, step := range steps {
		i := argvIndex(argv, step)
		if i <= last {
			t.Fatalf("argv = %s\nwant %q after the step before it", strings.Join(argv, "\n"), step)
		}
		last = i
	}
	st, err := store.Load()
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := st["acme/widgets#123"]; !ok {
		t.Fatalf("state = %v, want the dispatch recorded", st)
	}
}

func TestRunLiveWithoutOpenTabsSkipsNoTab(t *testing.T) {
	f := openFake(t, "local")
	cfg, store := openCfg(t)
	cfg.OpenMissingTabs = false
	req, local := openRequest(untabbedPR(123))

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedNoTab {
		t.Fatalf("results = %+v, want skipped_no_tab", got.Results)
	}
	if argv := strings.Join(f.Argv(), "\n"); strings.Contains(argv, "tab create") {
		t.Fatalf("argv = %s, want no tab created", argv)
	}
}

func TestRunLiveOpensTheTabOnTheRunsMachine(t *testing.T) {
	f := openFake(t, "mac")
	cfg, store := openCfg(t)
	cfg.OpenTabsCwd = nil
	cfg.Machines = []string{"mac"}
	cfg.MachineTimeout = 2 * time.Second
	cfg.TabMachine = "mac"
	cfg.Untabbed = "include"
	cfg.MachineCheckouts = map[string]string{"mac": macCheckout}
	cfg.GateLease = "off"
	ctx := WithProbes(context.Background(), Probes{Remote: fakeHerdrRemotes})
	req, local := openRequest(untabbedPR(123))

	got, err := Run(ctx, local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionOpenedTabAndDispatched || got.Results[0].Machine != "mac" {
		t.Fatalf("results = %+v, want opened_tab_and_dispatched on mac", got.Results)
	}
	argv := f.Argv()
	if i := argvIndex(argv, "tab create --cwd "+macCheckout+" --label PROJ-123"); i < 0 || !strings.HasPrefix(argv[i], "--machine mac ") {
		t.Fatalf("argv = %s, want the tab created on mac in its checkout", strings.Join(argv, "\n"))
	}
	for _, line := range argv {
		for _, verb := range []string{"tab create", "tab list", "agent start", "agent wait", "agent prompt"} {
			if strings.Contains(line, verb) && !strings.HasPrefix(line, "--machine mac ") {
				t.Fatalf("argv line %q runs %s without --machine mac", line, verb)
			}
		}
	}
}

func TestRunLiveReadyTimeoutClosesTheTab(t *testing.T) {
	f := openFake(t, "local")
	f.Write("local.prompt", `{"error":{"code":"timeout"}}`)
	cfg, store := openCfg(t)
	req, local := openRequest(untabbedPR(123))

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionOpenTabFailed || !strings.Contains(got.Results[0].Detail, "idle") {
		t.Fatalf("results = %+v, want open_tab_failed naming the ready wait", got.Results)
	}
	argv := f.Argv()
	if argvIndex(argv, "tab close w9:t1") < 0 {
		t.Fatalf("argv = %s, want the new tab closed", strings.Join(argv, "\n"))
	}
	if argvIndex(argv, "agent prompt") >= 0 {
		t.Fatalf("argv = %s, want no prompt", strings.Join(argv, "\n"))
	}
}

func TestRunLiveAgentStartFailureClosesTheTab(t *testing.T) {
	f := openFake(t, "local")
	f.Write("local.agent-start-error", `{"error":{"code":"unsupported_agent_kind"}}`)
	cfg, store := openCfg(t)
	req, local := openRequest(untabbedPR(123))

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionOpenTabFailed || !strings.Contains(got.Results[0].Detail, "unsupported_agent_kind") {
		t.Fatalf("results = %+v, want open_tab_failed", got.Results)
	}
	if argv := f.Argv(); argvIndex(argv, "tab close w9:t1") < 0 {
		t.Fatalf("argv = %s, want the new tab closed", strings.Join(argv, "\n"))
	}
}

func TestRunLiveOpensAtMostTheCap(t *testing.T) {
	f := openFake(t, "local")
	cfg, store := openCfg(t)
	req, local := openRequest(untabbedPR(1), untabbedPR(2), untabbedPR(3))

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	var actions []string
	for _, item := range got.Results {
		actions = append(actions, item.Action)
	}
	want := []string{ActionOpenedTabAndDispatched, ActionOpenedTabAndDispatched, ActionSkippedOpenTabCap}
	if strings.Join(actions, ",") != strings.Join(want, ",") {
		t.Fatalf("actions = %v, want %v", actions, want)
	}
	n := 0
	for _, line := range f.Argv() {
		if strings.Contains(line, "tab create") {
			n++
		}
	}
	if n != 2 {
		t.Fatalf("tab create calls = %d, want 2", n)
	}
}

func TestRunLiveNeverReplacesAnExistingTab(t *testing.T) {
	f := openFake(t, "local")
	f.Write("local.tabs", `{"result":{"tabs":[{"tab_id":"w2:tC","workspace_id":"w2","label":"PROJ-123"}]}}`)
	cfg, store := openCfg(t)
	req, local := openRequest(untabbedPR(123))

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedNoTab || !strings.Contains(got.Results[0].Detail, "w2:tC") {
		t.Fatalf("results = %+v, want skipped_no_tab naming the existing tab", got.Results)
	}
	if argv := strings.Join(f.Argv(), "\n"); strings.Contains(argv, "tab create") {
		t.Fatalf("argv = %s, want no tab created", argv)
	}
}

func TestRunLiveNeverOpensOutOfScope(t *testing.T) {
	f := openFake(t, "local")
	cfg, store := openCfg(t)
	pr := untabbedPR(123)
	pr.OutOfScope = "no tab"
	req, local := openRequest(pr)

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedOutOfScope {
		t.Fatalf("results = %+v, want skipped_out_of_scope", got.Results)
	}
	if argv := strings.Join(f.Argv(), "\n"); strings.Contains(argv, "tab create") {
		t.Fatalf("argv = %s, want no tab created", argv)
	}
}

func TestRunDryRunReportsWouldOpenTab(t *testing.T) {
	f := openFake(t, "local")
	cfg, store := openCfg(t)
	cfg.DryRun = true
	req, local := openRequest(untabbedPR(1), untabbedPR(2), untabbedPR(3))

	got, err := Run(context.Background(), local, store, cfg, req, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 3 {
		t.Fatalf("results = %+v, want three", got.Results)
	}
	first := got.Results[0]
	if first.Action != ActionWouldOpenTab || !strings.Contains(first.Detail, "PROJ-1") || !strings.Contains(first.Detail, "/src/widgets") || first.RenderedPrompt == "" {
		t.Fatalf("result = %+v, want would_open_tab with label and cwd", first)
	}
	if got.Results[1].Action != ActionWouldOpenTab || got.Results[2].Action != ActionSkippedOpenTabCap {
		t.Fatalf("results = %+v, want the cap to apply", got.Results)
	}
	for _, line := range f.Argv() {
		for _, write := range []string{"tab create", "agent start", "agent wait", "agent prompt", "tab close"} {
			if strings.Contains(line, write) {
				t.Fatalf("dry run ran %q", line)
			}
		}
	}
}

func TestRunDryRunUntabbedWithoutIdentifierIsNoTab(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.OpenMissingTabs = true
	cfg.OpenTabsCwd = map[string]string{config.LocalMachine: "/src/widgets"}
	pr := untabbedPR(123)
	pr.Identifier = nil
	got, err := Run(context.Background(), idleScript(), nil, cfg, Request{Doc: scan.Document{PRs: []scan.PR{pr}}}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionSkippedNoTab || !strings.Contains(got.Results[0].Detail, "identifier") {
		t.Fatalf("results = %+v, want skipped_no_tab without an identifier", got.Results)
	}
}

func TestAgentName(t *testing.T) {
	t.Parallel()

	for label, want := range map[string]string{
		"PROJ-123":              "pr-proj-123",
		"#42":                   "pr-42",
		"pr PROJ_9 ok":          "pr-pr-proj_9-ok",
		strings.Repeat("A", 40): "pr-" + strings.Repeat("a", 29),
	} {
		if got := agentName(label); got != want {
			t.Errorf("agentName(%q) = %q, want %q", label, got, want)
		}
	}
}
