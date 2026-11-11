package scan

import (
	"context"
	"slices"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

func scopeTo(machine, untabbed string) func(*config.Config) {
	return func(cfg *config.Config) {
		cfg.TabMachine = machine
		cfg.Untabbed = untabbed
	}
}

// scopes returns the out_of_scope of PRs #123 and #200.
func scopes(doc Document) (string, string) {
	return doc.PRs[0].OutOfScope, doc.PRs[1].OutOfScope
}

func TestRunScopedToMachineMarksOtherMachinesOutOfScope(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "local", "w2:tC", "w2:pC", "PROJ-200")
	writeTab(f, "m2", "w3:t2", "w3:p2", "PROJ-123")

	tests := []struct {
		machine        string
		want123, want2 string
	}{
		{machine: "m2", want2: "tab on local"},
		{machine: "local", want123: "tab on m2"},
		{machine: ""},
	}
	for _, tc := range tests {
		doc := runMachineScan(t, scopeTo(tc.machine, ""))
		got123, got200 := scopes(doc)
		if got123 != tc.want123 || got200 != tc.want2 {
			t.Errorf("--tab-machine %q out_of_scope = %q, %q; want %q, %q", tc.machine, got123, got200, tc.want123, tc.want2)
		}
		if doc.PRs[0].Tab == nil || doc.PRs[1].Tab == nil {
			t.Errorf("--tab-machine %q tabs = %+v, %+v; matching must stay global", tc.machine, doc.PRs[0].Tab, doc.PRs[1].Tab)
		}
	}
}

func TestRunScopedUntabbedPRBelongsToLocalByDefault(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "m2", "w3:t2", "w3:p2", "PROJ-123")
	noTab := "PROJ-200: identifier matched but no herdr tab labeled 'PROJ-200'"

	tests := []struct {
		machine, untabbed string
		want              string
	}{
		{machine: "local"},
		{machine: "local", untabbed: "exclude", want: "no tab"},
		{machine: "m2", want: "no tab"},
		{machine: "m2", untabbed: "include"},
	}
	for _, tc := range tests {
		doc := runMachineScan(t, scopeTo(tc.machine, tc.untabbed))
		if got := doc.PRs[1].OutOfScope; got != tc.want {
			t.Errorf("--tab-machine %s --untabbed %q: #200 out_of_scope = %q, want %q", tc.machine, tc.untabbed, got, tc.want)
		}
		if warned := slices.Contains(doc.Warnings, noTab); warned != (tc.want == "") {
			t.Errorf("--tab-machine %s --untabbed %q: warnings = %q; the no-tab warning belongs only to the run that owns #200", tc.machine, tc.untabbed, doc.Warnings)
		}
	}
}

func TestRunScopedDuplicateIsReportedInBothRuns(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "local", "w2:tC", "w2:pC", "PROJ-123")
	writeTab(f, "m2", "w3:t2", "w3:p2", "PROJ-123")
	want := "PROJ-123: ambiguous herdr tab label 'PROJ-123' matches w2:tC, w3:t2@m2"

	for _, machine := range []string{"local", "m2"} {
		doc := runMachineScan(t, scopeTo(machine, ""))
		pr := doc.PRs[0]
		if pr.Tab != nil || pr.OutOfScope != "" {
			t.Errorf("--tab-machine %s: #123 tab = %+v, out_of_scope = %q; want no tab, in scope", machine, pr.Tab, pr.OutOfScope)
		}
		if !slices.Contains(doc.Warnings, want) {
			t.Errorf("--tab-machine %s: warnings = %q, want %q", machine, doc.Warnings, want)
		}
	}
}

func TestRunScopedUnknownTabBelongsToTheUnreachableMachine(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "local", "w2:tC", "w2:pC", "PROJ-200")
	f.Write("m2.down", "")

	doc := runMachineScan(t, scopeTo("m2", ""))
	if got := doc.PRs[0]; got.TabUnknown != TabUnknownMachineUnreachable || got.OutOfScope != "" {
		t.Fatalf("m2 run: #123 tab_unknown = %q, out_of_scope = %q; want unknown, in scope", got.TabUnknown, got.OutOfScope)
	}
	if got := doc.PRs[1].OutOfScope; got != "tab on local" {
		t.Fatalf("m2 run: #200 out_of_scope = %q, want tab on local", got)
	}

	doc = runMachineScan(t, scopeTo("local", ""))
	if got := doc.PRs[0].OutOfScope; got != "tab may be on m2 (unreachable)" {
		t.Fatalf("local run: #123 out_of_scope = %q, want tab may be on m2 (unreachable)", got)
	}
	if got := doc.PRs[1].OutOfScope; got != "" {
		t.Fatalf("local run: #200 out_of_scope = %q, want in scope", got)
	}
}

func TestRunUnscopedOmitsOutOfScope(t *testing.T) {
	f := machineFake(t)
	writeTab(f, "m2", "w3:t2", "w3:p2", "PROJ-123")

	doc := runMachineScan(t)
	for _, pr := range doc.PRs {
		if pr.OutOfScope != "" {
			t.Fatalf("#%d out_of_scope = %q, want empty without tab_machine", pr.Number, pr.OutOfScope)
		}
	}
}

func TestRunSkipTeamCover(t *testing.T) {
	t.Parallel()

	g := teamCoverGH(map[string]memberCall{"acme/foo": {members: []gh.TeamMember{{Login: "carol", Type: "User"}}}})
	cfg := teamCoverCfg()
	cfg.SkipJobs = []string{config.JobTeamCover}
	doc, err := Run(context.Background(), depsWith(g, fixtureHerdr{}), cfg, []string{"acme/widgets"}, fixtureNow)
	if err != nil {
		t.Fatal(err)
	}
	if g.memberCalls != 0 || g.loadCalls != 0 {
		t.Fatalf("memberCalls = %d, loadCalls = %d, want no team-cover calls", g.memberCalls, g.loadCalls)
	}
	if doc.PRs[0].TeamReviewSuggestions != nil || doc.Summary.UncoveredTeams != nil {
		t.Fatalf("suggestions = %+v, uncovered = %v, want none", doc.PRs[0].TeamReviewSuggestions, doc.Summary.UncoveredTeams)
	}
}
