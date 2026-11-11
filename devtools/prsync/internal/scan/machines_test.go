package scan

import (
	"context"
	"fmt"
	"slices"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	executor "github.com/jaeyeom/go-cmdexec"
)

// machineFake is a fake herdr on PATH whose local server and machine m2
// start with no tabs and no agents.
func machineFake(t *testing.T) *herdrtest.Fake {
	t.Helper()
	f := herdrtest.Install(t)
	for _, m := range []string{"local", "m2"} {
		f.Write(m+".tabs", `{"result":{"tabs":[]}}`)
		f.Write(m+".agents", `{"result":{"agents":[]}}`)
	}
	return f
}

// writeTab gives machine m one tab with one idle agent in pane.
func writeTab(f *herdrtest.Fake, m, tab, pane, label string) {
	f.Write(m+".tabs", fmt.Sprintf(`{"result":{"tabs":[{"tab_id":%q,"workspace_id":"w2","label":%q,"agent_status":"idle","pane_count":1}]}}`, tab, label))
	f.Write(m+".agents", fmt.Sprintf(`{"result":{"agents":[{"pane_id":%q,"tab_id":%q,"agent":"codex","agent_status":"idle"}]}}`, pane, tab))
}

// runMachineScan scans PRs #123 (PROJ-123) and #200 (PROJ-200) against the
// fake herdr, searching m2 as well as the local server. Each tweak edits
// the config first.
func runMachineScan(t *testing.T, tweaks ...func(*config.Config)) Document {
	t.Helper()
	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Machines = []string{"m2"}
	for _, tweak := range tweaks {
		tweak(&cfg)
	}
	second := fixturePRs()[0]
	second.Number = 200
	second.Title = "[PROJ-200] Fix the gadget"
	g := &scriptGH{
		list:    map[string]listResult{"acme/widgets": {prs: []gh.PRListItem{fixturePRs()[0], second}}},
		threads: map[int][]gh.Thread{123: fixtureThreads(), 200: fixtureThreads()},
	}
	local := herdr.NewClient(executor.NewBasicExecutor(), "herdr")
	deps := Deps{
		GH:     g,
		Herdr:  local,
		Remote: func(m string) Herdr { return local.OnMachine(m, 2*time.Second) },
	}
	doc, err := Run(context.Background(), deps, cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(doc.PRs) != 2 {
		t.Fatalf("prs = %+v, want two", doc.PRs)
	}
	return doc
}

func TestRunMatchesTabOnAnotherMachine(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "m2", "w3:t2", "w3:p2", "PROJ-123")

	doc := runMachineScan(t)
	tab := doc.PRs[0].Tab
	if tab == nil || tab.TabID != "w3:t2" || tab.Machine != "m2" || tab.PaneID == nil || *tab.PaneID != "w3:p2" {
		t.Fatalf("tab = %+v, want w3:t2 on m2 with pane w3:p2", tab)
	}
	if doc.PRs[0].TabUnknown != "" {
		t.Fatalf("tab_unknown = %q, want empty", doc.PRs[0].TabUnknown)
	}
	if doc.Summary.TabHere != 1 {
		t.Fatalf("summary = %+v, want tab_here 1", doc.Summary)
	}
}

func TestRunSameTabIDOnTwoMachines(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "local", "w2:tC", "w2:pC", "PROJ-123")
	writeTab(f, "m2", "w2:tC", "w2:pC", "PROJ-200")

	doc := runMachineScan(t)
	a, b := doc.PRs[0].Tab, doc.PRs[1].Tab
	if a == nil || a.Machine != "" || b == nil || b.Machine != "m2" {
		t.Fatalf("tabs = %+v, %+v, want local and m2", a, b)
	}
	if len(doc.Warnings) != 0 {
		t.Fatalf("warnings = %q, want none", doc.Warnings)
	}
}

func TestRunSamePRMatchedOnTwoMachinesIsAmbiguous(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "local", "w2:tC", "w2:pC", "PROJ-123")
	writeTab(f, "m2", "w3:t2", "w3:p2", "PROJ-123")

	doc := runMachineScan(t)
	if doc.PRs[0].Tab != nil {
		t.Fatalf("tab = %+v, want nil", doc.PRs[0].Tab)
	}
	want := "PROJ-123: ambiguous herdr tab label 'PROJ-123' matches w2:tC, w3:t2@m2"
	if !slices.Contains(doc.Warnings, want) {
		t.Fatalf("warnings = %q, want %q", doc.Warnings, want)
	}
}

func TestRunMachineUnreachable(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "local", "w2:tC", "w2:pC", "PROJ-200")
	f.Write("m2.down", "")

	doc := runMachineScan(t)
	want := []string{"machine m2 unreachable (host unreachable); its tabs were not searched"}
	if !slices.Equal(doc.Warnings, want) {
		t.Fatalf("warnings = %q, want %q", doc.Warnings, want)
	}
	unknown := doc.PRs[0]
	if unknown.Tab != nil || unknown.TabUnknown != "machine unreachable" {
		t.Fatalf("PR #123 tab = %+v, tab_unknown = %q, want unknown (machine unreachable)", unknown.Tab, unknown.TabUnknown)
	}
	if doc.PRs[1].Tab == nil || doc.PRs[1].TabUnknown != "" {
		t.Fatalf("PR #200 tab = %+v, tab_unknown = %q, want the local tab", doc.PRs[1].Tab, doc.PRs[1].TabUnknown)
	}
}

func TestRunWithoutMachinesDoesNotCallRemote(t *testing.T) {
	t.Parallel()
	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Repos = []string{"acme/widgets"}
	deps := Deps{GH: fixtureGH{}, Herdr: fixtureHerdr{}, Remote: func(string) Herdr {
		t.Error("Remote called with no machines configured")
		return fixtureHerdr{}
	}}
	if _, err := Run(context.Background(), deps, cfg, nil, fixtureNow); err != nil {
		t.Fatal(err)
	}
}
