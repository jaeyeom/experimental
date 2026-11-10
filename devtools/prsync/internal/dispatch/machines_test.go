package dispatch

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
)

// remotePR is the eligible fixture PR with its tab w3:t2 on machine m2.
func remotePR() scan.PR {
	pr := fixtureEligiblePR()
	pane := "w3:p2"
	pr.Tab.TabID = "w3:t2"
	pr.Tab.PaneID = &pane
	pr.Tab.Machine = "m2"
	return pr
}

func TestRunLiveDispatchesOnTheTabsMachine(t *testing.T) {
	f := herdrtest.Install(t)
	f.Write("local.agents", `{"result":{"agents":[]}}`)
	f.Write("m2.agents", `{"result":{"agents":[{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"idle"}]}}`)
	f.Write("m2.prompt", `{"result":{"agent":{"pane_id":"w3:p2","tab_id":"w3:t2","agent":"codex","agent_status":"idle"}}}`)
	local := herdr.NewClient(executor.NewBasicExecutor(), "herdr")
	cfg, store := liveCfg(t)
	cfg.DispatchTimeout = 5 * time.Second

	got, err := Run(context.Background(), local, store, cfg, Request{
		Doc:    scan.Document{PRs: []scan.PR{remotePR()}},
		Remote: func(m string) Herdr { return local.OnMachine(m, 2*time.Second) },
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionDispatched || got.Results[0].Machine != "m2" || got.Results[0].PaneID != "w3:p2" {
		t.Fatalf("results = %+v, want dispatched to w3:p2 on m2", got.Results)
	}
	argv := strings.Join(f.Argv(), "\n")
	if !strings.Contains(argv, "--machine m2 agent prompt w3:p2 ") {
		t.Fatalf("argv = %s, want a prompt on m2", argv)
	}
	if strings.Contains(argv, "\nagent prompt") || strings.HasPrefix(argv, "agent prompt") {
		t.Fatalf("argv = %s, prompt must not run locally", argv)
	}
}

func TestRunDryRunNamesTheTabsMachine(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	got, err := Run(context.Background(), h, nil, config.Defaults(), Request{
		Doc: scan.Document{PRs: []scan.PR{remotePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != ActionWouldDispatch || got.Results[0].Machine != "m2" {
		t.Fatalf("results = %+v, want would_dispatch on m2", got.Results)
	}
}

func TestRunLiveRemoteTabWithoutClientFails(t *testing.T) {
	t.Parallel()

	cfg, store := liveCfg(t)
	got, err := Run(context.Background(), idleScript(), store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{remotePR()}},
	}, fixtureNow)
	if err == nil || len(got.Results) != 1 || got.Results[0].Action != ActionFailed {
		t.Fatalf("results = %+v, err = %v, want failed", got.Results, err)
	}
}

func TestMatchedTabsKeepsMachinesApart(t *testing.T) {
	t.Parallel()

	local := fixtureEligiblePR()
	remote := fixtureEligiblePR()
	remote.Number = 124
	remote.Tab.Machine = "m2"
	got := MatchedTabs(scan.Document{PRs: []scan.PR{local, remote}})
	if len(got) != 2 {
		t.Fatalf("MatchedTabs = %v, want two keys", got)
	}
	for _, key := range []string{"w2:tC", "w2:tC@m2"} {
		if _, ok := got[key]; !ok {
			t.Fatalf("MatchedTabs = %v, missing %q", got, key)
		}
	}
}

func TestEvaluateTabUnknownIsSkippedNotNoTab(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	pr.Tab = nil
	pr.TabUnknown = scan.TabUnknownMachineUnreachable
	got := Evaluate(Candidate{Repo: pr.Repo, Number: pr.Number, PR: &pr}, config.Defaults(), State{}, false, false, false, fixtureNow)
	if got.Action != ActionSkippedTabUnknown || got.Detail != "unknown tab (machine unreachable)" {
		t.Fatalf("Evaluate = %+v, want skipped_tab_unknown", got)
	}
}
