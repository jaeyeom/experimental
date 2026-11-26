package doctor

import (
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

var doctorNow = time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data

func TestFixtureEveryCheck(t *testing.T) {
	t.Parallel()

	got := mustCheck(t, fixtureInput())
	want := []Finding{
		cycleWant("PRJ-C1", "PRJ-C2", "PRJ-C1"),
		finding(CheckUnknownBlocker, SeverityWarn, "PRJ-UB is blocked by PRJ-NOPE, which is not in the mirror", "widen jql, or unlink", "PRJ-UB", "PRJ-NOPE"),
		finding(CheckOrphan, SeverityWarn, "PRJ-ORPH has no parent", "set its parent epic", "PRJ-ORPH"),
		finding(CheckDroppedClaim, SeverityWarn, "PRJ-DROP is In Progress with no tab and no open PR", "factory release PRJ-DROP or start it", "PRJ-DROP"),
		finding(CheckStaleTab, SeverityWarn, "PRJ-STAB is Done and tab PRJ-STAB is idle", "close the tab (prsync tabs)", "PRJ-STAB", "t-stab"),
		finding(CheckStalled, SeverityWarn, "PRJ-STALL is started and older than 2h0m0s with no open PR", "look at the tab", "PRJ-STALL"),
		finding(CheckAlmostReady, SeverityInfo, "PRJ-ALMOST is missing agent-ready", "add agent-ready", "PRJ-ALMOST"),
	}
	equalFindings(t, got, want)
	for _, found := range got {
		if strings.Contains(found.Suggestion, "factory unlink") && found.Check != CheckCycle {
			t.Fatalf("unlink suggestion on %s: %q", found.Check, found.Suggestion)
		}
		if found.Check == CheckAlmostReady {
			if found.Severity != SeverityInfo {
				t.Fatalf("almost-ready severity = %q", found.Severity)
			}
			continue
		}
		if found.Severity != SeverityWarn {
			t.Fatalf("%s severity = %q, want warn", found.Check, found.Severity)
		}
	}
}

func TestCleanGraph(t *testing.T) {
	t.Parallel()

	t.Run("jira", func(t *testing.T) {
		t.Parallel()
		items := map[string]source.Item{
			"EPIC-1":    jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
			"PRJ-OK":    jiraItem("PRJ-OK", "Task", "To Do", "EPIC-1", false, []string{"agent-ready"}),
			"PRJ-CH1":   jiraItem("PRJ-CH1", "Task", "To Do", "EPIC-1", false, []string{"agent-ready"}),
			"PRJ-CH2":   jiraItem("PRJ-CH2", "Task", "To Do", "EPIC-1", false, []string{"agent-ready"}, "PRJ-CH1"),
			"PRJ-DONE":  jiraItem("PRJ-DONE", "Task", "Done", "", true, []string{"agent-ready"}),
			"PRJ-AFTER": jiraItem("PRJ-AFTER", "Task", "To Do", "EPIC-1", false, []string{"agent-ready"}, "PRJ-DONE"),
		}
		in := baseInput(items)
		in.Claims = []store.Claim{claim("PRJ-OK", store.Started, doctorNow.Add(-time.Minute), doctorNow.Add(-time.Minute))}
		in.Snapshot.Claims = map[string]store.ClaimState{"PRJ-OK": store.Started}
		assertClean(t, in)
	})
	t.Run("github", func(t *testing.T) {
		t.Parallel()
		id := "acme/widgets#1"
		in := githubInput(map[string]source.Item{
			id: {ID: id, Type: "Issue", State: "OPEN", Labels: []string{"agent-ready"}},
		})
		assertClean(t, in)
	})
}

func TestDoneClaimAsksForRelease(t *testing.T) {
	t.Parallel()

	stale := doctorNow.Add(-3 * time.Hour)
	items := map[string]source.Item{
		"EPIC":  jiraItem("EPIC", "Epic", "To Do", "", false, nil),
		"PRJ-1": jiraItem("PRJ-1", "Task", "Done", "EPIC", true, []string{"agent-ready"}),
	}
	in := baseInput(items)
	in.Claims = []store.Claim{claim("PRJ-1", store.Started, stale, stale)}
	got := mustCheck(t, in)
	want := []Finding{
		finding(CheckStalled, SeverityWarn, "PRJ-1 done: release claim", "factory settle", "PRJ-1"),
		finding(CheckClosableParent, SeverityInfo, "EPIC has all children done (PRJ-1)", "factory close EPIC --apply", "EPIC"),
	}
	equalFindings(t, got, want)
	if strings.Contains(FormatText(got), "started and older") {
		t.Fatalf("done claim was reported as stalled:\n%s", FormatText(got))
	}
}

func TestOrphanIgnore(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"EPIC-1":   jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
		"PRJ-KEEP": jiraItem("PRJ-KEEP", "Task", "To Do", "", false, []string{"agent-ready"}),
		"PRJ-ID":   jiraItem("PRJ-ID", "Task", "To Do", "", false, []string{"agent-ready"}),
		"PRJ-LAB":  jiraItem("PRJ-LAB", "Task", "To Do", "", false, []string{"agent-ready", "Wont-Fix"}),
		"PRJ-DROP": jiraItem("PRJ-DROP", "Task", "In Progress", "", false, []string{"agent-ready", "wont-fix"}),
	}
	in := baseInput(items)
	in.Config.OrphanIgnore = []string{"PRJ-ID"}
	in.Config.OrphanIgnoreLabels = []string{"wont-fix"}
	got := mustCheck(t, in)
	want := []Finding{
		finding(CheckOrphan, SeverityWarn, "PRJ-KEEP has no parent", "set its parent epic", "PRJ-KEEP"),
		finding(CheckDroppedClaim, SeverityWarn, "PRJ-DROP is In Progress with no tab and no open PR", "factory release PRJ-DROP or start it", "PRJ-DROP"),
	}
	equalFindings(t, got, want)
}

func TestDoneBlockerIsNotAFinding(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"EPIC":   jiraItem("EPIC", "Epic", "To Do", "", false, nil),
		"PRJ-SD": jiraItem("PRJ-SD", "Task", "Done", "EPIC", true, []string{"agent-ready"}),
		"PRJ-SB": jiraItem("PRJ-SB", "Task", "To Do", "EPIC", false, []string{"agent-ready"}, "PRJ-SD", "PRJ-SD"),
		"PRJ-UB": jiraItem("PRJ-UB", "Task", "To Do", "EPIC", false, []string{"agent-ready"}, "PRJ-NOPE", "PRJ-NOPE"),
		"PRJ-DD": jiraItem("PRJ-DD", "Task", "Done", "EPIC", true, nil, "PRJ-NOPE"),
	}
	got := mustCheck(t, baseInput(items))
	want := []Finding{
		finding(CheckUnknownBlocker, SeverityWarn, "PRJ-DD is blocked by PRJ-NOPE, which is not in the mirror", "widen jql, or unlink", "PRJ-DD", "PRJ-NOPE"),
		finding(CheckUnknownBlocker, SeverityWarn, "PRJ-UB is blocked by PRJ-NOPE, which is not in the mirror", "widen jql, or unlink", "PRJ-UB", "PRJ-NOPE"),
	}
	equalFindings(t, got, want)
	text := FormatText(got)
	if strings.Contains(text, "factory unlink") || strings.Contains(text, "PRJ-SD") || strings.Contains(text, "stale-blocker") {
		t.Fatalf("done blocker was reported:\n%s", text)
	}
}

func TestStaleTabStatus(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"PRJ-IDLE":  jiraItem("PRJ-IDLE", "Task", "Done", "", true, nil),
		"PRJ-ADONE": jiraItem("PRJ-ADONE", "Task", "Done", "", true, nil),
		"PRJ-WORK":  jiraItem("PRJ-WORK", "Task", "Done", "", true, nil),
		"PRJ-BLOCK": jiraItem("PRJ-BLOCK", "Task", "Done", "", true, nil),
		"PRJ-UNK":   jiraItem("PRJ-UNK", "Task", "Done", "", true, nil),
	}
	in := baseInput(items)
	in.Snapshot.Tabs = []herdr.Tab{
		{ID: "t-idle", WorkspaceID: "somewhere-else", Label: "PRJ-IDLE", AgentStatus: herdr.AgentStatusIdle},
		{ID: "t-adone", WorkspaceID: "w1", Label: "PRJ-ADONE", AgentStatus: herdr.AgentStatusDone},
		{ID: "t-work", WorkspaceID: "w1", Label: "PRJ-WORK", AgentStatus: herdr.AgentStatusWorking},
		{ID: "t-block", WorkspaceID: "w1", Label: "PRJ-BLOCK", AgentStatus: herdr.AgentStatusBlocked},
		{ID: "t-unk", WorkspaceID: "w1", Label: "PRJ-UNK", AgentStatus: herdr.AgentStatusUnknown},
	}
	got := mustCheck(t, in)
	want := []Finding{
		finding(CheckStaleTab, SeverityWarn, "PRJ-ADONE is Done and tab PRJ-ADONE is done", "close the tab (prsync tabs)", "PRJ-ADONE", "t-adone"),
		finding(CheckStaleTab, SeverityWarn, "PRJ-IDLE is Done and tab PRJ-IDLE is idle", "close the tab (prsync tabs)", "PRJ-IDLE", "t-idle"),
	}
	equalFindings(t, got, want)
}

func TestGitHubOrphanAndTabs(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"acme/widgets#1": ghItem("acme/widgets#1", false, ""),
		"acme/widgets#2": ghItem("acme/widgets#2", true, "CLOSED"),
		"acme/widgets#3": ghItem("acme/widgets#3", true, "CLOSED"),
		"acme/widgets#4": ghItem("acme/widgets#4", true, "CLOSED"),
		"acme/widgets#5": ghItem("acme/widgets#5", true, "CLOSED"),
	}
	items["acme/widgets#1"] = source.Item{
		ID: "acme/widgets#1", Type: "Issue", Labels: []string{"agent-ready"},
	}
	in := githubInput(items)
	in.Snapshot.Workspaces = []herdr.Workspace{
		{ID: "w-widgets", Label: "widgets"},
		{ID: "w-other", Label: "other"},
	}
	in.Snapshot.Tabs = []herdr.Tab{
		{ID: "t2", WorkspaceID: "w-widgets", Label: "#2", AgentStatus: herdr.AgentStatusIdle},
		{ID: "t3", WorkspaceID: "w-other", Label: "#3", AgentStatus: herdr.AgentStatusIdle},
		{ID: "t4", WorkspaceID: "w-widgets", Label: "#4", AgentStatus: herdr.AgentStatusWorking},
		{ID: "t5", WorkspaceID: "w-widgets", Label: "#5", AgentStatus: herdr.AgentStatusDone},
	}
	got := mustCheck(t, in)
	want := []Finding{
		finding(CheckStaleTab, SeverityWarn, "acme/widgets#2 is Done and tab #2 is idle", "close the tab (prsync tabs)", "acme/widgets#2", "t2"),
		finding(CheckStaleTab, SeverityWarn, "acme/widgets#5 is Done and tab #5 is done", "close the tab (prsync tabs)", "acme/widgets#5", "t5"),
	}
	equalFindings(t, got, want)
}

func TestCheckRejects(t *testing.T) {
	t.Parallel()

	ok := jiraItem("PRJ-1", "Task", "To Do", "EPIC", false, []string{"agent-ready"})
	items := map[string]source.Item{"PRJ-1": ok}
	tests := []struct {
		name    string
		in      Input
		wantErr string
	}{
		{name: "kind", in: Input{Kind: "linear", SourceKey: "linear:x", Now: doctorNow}, wantErr: `found "linear"`},
		{name: "source key", in: Input{Kind: "jira", Now: doctorNow}, wantErr: "found empty"},
		{name: "clock", in: Input{Kind: "jira", SourceKey: "jira:acme"}, wantErr: "found zero"},
		{
			name: "item id",
			in: inputWith(items, func(in *Input) {
				it := in.Snapshot.Items["PRJ-1"]
				it.ID = "OTHER"
				in.Snapshot.Items["PRJ-1"] = it
			}),
			wantErr: `found "OTHER"`,
		},
		{
			name: "empty blocker",
			in: inputWith(items, func(in *Input) {
				it := in.Snapshot.Items["PRJ-1"]
				it.BlockedBy = []source.BlockRef{{ID: ""}}
				in.Snapshot.Items["PRJ-1"] = it
			}),
			wantErr: "found empty",
		},
		{
			name: "github id",
			in: inputWith(map[string]source.Item{
				"nope": {ID: "nope", Type: "Issue"},
			}, func(in *Input) {
				in.Kind = "github"
				in.SourceKey = "github:widgets"
				in.Config.Workspace = "widgets"
			}),
			wantErr: "github id",
		},
		{
			name: "github repo",
			in: inputWith(map[string]source.Item{
				"acme/widgets#1": {ID: "acme/widgets#1", Type: "Issue"},
			}, func(in *Input) {
				in.Kind = "github"
				in.SourceKey = "github:widgets"
				in.Config.Workspace = ""
				in.Config.Repo = "not a repo"
			}),
			wantErr: "owner/repo",
		},
		{
			name: "claim id",
			in: inputWith(items, func(in *Input) {
				in.Claims = []store.Claim{{Source: "jira:acme", State: store.Started}}
			}),
			wantErr: "non-empty id",
		},
		{
			name: "claim state",
			in: inputWith(items, func(in *Input) {
				in.Claims = []store.Claim{{Source: "jira:acme", ID: "PRJ-1", State: "nope", UpdatedAt: doctorNow}}
			}),
			wantErr: `found "nope"`,
		},
		{
			name: "updated_at",
			in: inputWith(items, func(in *Input) {
				in.Claims = []store.Claim{{Source: "jira:acme", ID: "PRJ-1", State: store.Started}}
			}),
			wantErr: "updated_at",
		},
		{
			name: "missing item",
			in: inputWith(items, func(in *Input) {
				in.Claims = []store.Claim{{
					Source: "jira:acme", ID: "PRJ-MISSING", State: store.Started, UpdatedAt: doctorNow,
				}}
			}),
			wantErr: "found none",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			_, err := Check(tc.in)
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("Check error = %v, want substring %q", err, tc.wantErr)
			}
		})
	}
}

func TestOtherSourceClaimIgnored(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"PRJ-1": jiraItem("PRJ-1", "Task", "To Do", "EPIC", false, []string{"agent-ready"}),
		"EPIC":  jiraItem("EPIC", "Epic", "To Do", "", false, nil),
	}
	in := baseInput(items)
	in.Claims = []store.Claim{{
		Source: "jira:other", ID: "", State: "nope",
	}}
	assertClean(t, in)
}

func TestFormatTextGroups(t *testing.T) {
	t.Parallel()

	in := []Finding{
		finding(CheckAlmostReady, SeverityInfo, "B is missing agent-ready", "add agent-ready", "B"),
		finding(CheckUnknownBlocker, SeverityWarn, "O is blocked by D, which is not in the mirror", "widen jql, or unlink", "O", "D"),
		finding(CheckUnknownBlocker, SeverityWarn, "C is blocked by A, which is not in the mirror", "widen jql, or unlink", "C", "A"),
	}
	const want = "" +
		"unknown-blocker (2)\n" +
		"  warn  C is blocked by A, which is not in the mirror\n" +
		"        widen jql, or unlink\n" +
		"  warn  O is blocked by D, which is not in the mirror\n" +
		"        widen jql, or unlink\n" +
		"almost-ready (1)\n" +
		"  info  B is missing agent-ready\n" +
		"        add agent-ready\n"
	if got := FormatText(in); got != want {
		t.Fatalf("text:\n%s\nwant:\n%s", got, want)
	}
	if FormatText(nil) != "" {
		t.Fatal("empty text was not empty")
	}
}

func TestFormatJSON(t *testing.T) {
	t.Parallel()

	got, err := FormatJSON(nil)
	if err != nil {
		t.Fatal(err)
	}
	if got != "[]\n" {
		t.Fatalf("empty json = %q", got)
	}
	got, err = FormatJSON([]Finding{
		finding(CheckAlmostReady, SeverityInfo, "B is missing agent-ready", "add agent-ready", "B"),
		finding(CheckOrphan, SeverityWarn, "A has no parent", "set its parent epic", "A"),
	})
	if err != nil {
		t.Fatal(err)
	}
	const want = `[
  {
    "check": "orphan",
    "severity": "warn",
    "items": [
      "A"
    ],
    "message": "A has no parent",
    "suggestion": "set its parent epic"
  },
  {
    "check": "almost-ready",
    "severity": "info",
    "items": [
      "B"
    ],
    "message": "B is missing agent-ready",
    "suggestion": "add agent-ready"
  }
]
`
	if got != want {
		t.Fatalf("json:\n%s\nwant:\n%s", got, want)
	}
}

func fixtureInput() Input {
	ready := []string{"agent-ready"}
	items := map[string]source.Item{
		"EPIC-1":     jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
		"EPIC-2":     jiraItem("EPIC-2", "epic", "To Do", "", false, nil),
		"PRJ-C1":     jiraItem("PRJ-C1", "Task", "To Do", "EPIC-1", false, ready, "PRJ-C2"),
		"PRJ-C2":     jiraItem("PRJ-C2", "Task", "To Do", "EPIC-1", false, ready, "PRJ-C1"),
		"PRJ-SD":     jiraItem("PRJ-SD", "Task", "Done", "EPIC-1", true, ready),
		"PRJ-SB":     jiraItem("PRJ-SB", "Task", "To Do", "EPIC-1", false, ready, "PRJ-SD"),
		"PRJ-UB":     jiraItem("PRJ-UB", "Task", "To Do", "EPIC-1", false, ready, "PRJ-NOPE"),
		"PRJ-ORPH":   jiraItem("PRJ-ORPH", "Task", "To Do", "", false, ready),
		"PRJ-DROP":   jiraItem("PRJ-DROP", "Task", "In Progress", "EPIC-1", false, ready),
		"PRJ-STAB":   jiraItem("PRJ-STAB", "Task", "Done", "EPIC-1", true, ready),
		"PRJ-STALL":  jiraItem("PRJ-STALL", "Task", "To Do", "EPIC-1", false, ready),
		"PRJ-ALMOST": jiraItem("PRJ-ALMOST", "Task", "To Do", "EPIC-1", false, nil),
		"PRJ-OK":     jiraItem("PRJ-OK", "Task", "To Do", "EPIC-1", false, ready),
		"PRJ-HELD":   jiraItem("PRJ-HELD", "Task", "In Progress", "EPIC-1", false, ready),
		"PRJ-TABBED": jiraItem("PRJ-TABBED", "Task", "In Progress", "EPIC-1", false, ready),
		"PRJ-EXCL":   jiraItem("PRJ-EXCL", "Task", "To Do", "EPIC-1", false, []string{"needs-human"}),
		"PRJ-STATUS": jiraItem("PRJ-STATUS", "Task", "Backlog", "EPIC-1", false, nil),
		"PRJ-YOUNG":  jiraItem("PRJ-YOUNG", "Task", "To Do", "EPIC-1", false, ready),
		"PRJ-EXACT":  jiraItem("PRJ-EXACT", "Task", "To Do", "EPIC-1", false, ready),
		"PRJ-FRESH":  jiraItem("PRJ-FRESH", "Task", "To Do", "EPIC-1", false, ready),
		"PRJ-WITHPR": jiraItem("PRJ-WITHPR", "Task", "To Do", "EPIC-1", false, ready),
		"PRJ-DD1":    jiraItem("PRJ-DD1", "Task", "Done", "EPIC-1", true, ready, "PRJ-DD2"),
		"PRJ-DD2":    jiraItem("PRJ-DD2", "Task", "Done", "EPIC-1", true, ready),
		"PRJ-CH1":    jiraItem("PRJ-CH1", "Task", "To Do", "EPIC-1", false, ready),
		"PRJ-CH2":    jiraItem("PRJ-CH2", "Task", "To Do", "EPIC-1", false, ready, "PRJ-CH1"),
		"PRJ-WORK":   jiraItem("PRJ-WORK", "Task", "Done", "EPIC-1", true, ready),
		"PRJ-BARE":   jiraItem("PRJ-BARE", "Task", "Done", "", true, ready),
		"PRJ-LBL":    jiraItem("PRJ-LBL", "Task", "To Do", "EPIC-1", false, nil),
		"PRJ-OLD":    jiraItem("PRJ-OLD", "Task", "To Do", "EPIC-1", false, ready),
	}
	in := baseInput(items)
	old := doctorNow.Add(-5 * time.Hour)
	recent := doctorNow.Add(-time.Minute)
	exact := doctorNow.Add(-2 * time.Hour)
	stale := doctorNow.Add(-3 * time.Hour)
	in.Claims = []store.Claim{
		claim("PRJ-STALL", store.Started, stale, stale),
		claim("PRJ-YOUNG", store.Started, recent, recent),
		claim("PRJ-EXACT", store.Started, exact, exact),
		claim("PRJ-FRESH", store.Started, old, recent),
		claim("PRJ-WITHPR", store.Started, old, old),
		claim("PRJ-LBL", store.Started, recent, recent),
		claim("PRJ-OLD", store.Claimed, old, old),
		{Source: "jira:other", ID: "OTHER", State: store.Started, ClaimedAt: old, UpdatedAt: old},
	}
	in.Snapshot.Claims = map[string]store.ClaimState{
		"PRJ-STALL":  store.Started,
		"PRJ-YOUNG":  store.Started,
		"PRJ-EXACT":  store.Started,
		"PRJ-FRESH":  store.Started,
		"PRJ-WITHPR": store.Started,
		"PRJ-LBL":    store.Started,
		"PRJ-OLD":    store.Claimed,
	}
	in.Snapshot.Tabs = []herdr.Tab{
		{ID: "t-stab", WorkspaceID: "w-other", Label: "PRJ-STAB", AgentStatus: herdr.AgentStatusIdle},
		{ID: "t-stall", WorkspaceID: "w1", Label: "PRJ-STALL", AgentStatus: herdr.AgentStatusWorking},
		{ID: "t-tabbed", WorkspaceID: "w1", Label: "PRJ-TABBED", AgentStatus: herdr.AgentStatusWorking},
		{ID: "t-work", WorkspaceID: "w1", Label: "PRJ-WORK", AgentStatus: herdr.AgentStatusWorking},
	}
	in.Snapshot.OpenPRs = []gh.PR{
		{Title: "[PRJ-HELD] held", URL: "https://example/pr/1"},
		{Title: "[PRJ-WITHPR] review", URL: "https://example/pr/2"},
	}
	return in
}

func baseInput(items map[string]source.Item) Input {
	return Input{
		Kind:      "jira",
		SourceKey: "jira:acme",
		Config: config.Source{
			Kind:              "jira",
			Workspace:         "factory",
			RequireLabels:     []string{"agent-ready"},
			ExcludeLabels:     []string{"needs-human", "blocked-external"},
			SkipTypes:         []string{"Epic"},
			ReadyStatuses:     []string{"To Do"},
			ClaimStatus:       "In Progress",
			AssigneeOK:        "me",
			ParentBlocksChild: config.ParentBlocksIgnore,
			PRTitlePattern:    `\[{id}\]`,
			Trust:             config.Trust{Environment: config.TrustEnvTrusted},
		},
		Snapshot:   ready.Snapshot{Items: items, Me: "alice"},
		Now:        doctorNow,
		StallAfter: 2 * time.Hour,
	}
}

func githubInput(items map[string]source.Item) Input {
	in := baseInput(items)
	in.Kind = "github"
	in.SourceKey = "github:widgets"
	in.Config.Kind = "github"
	in.Config.Repo = "acme/widgets"
	in.Config.Workspace = "widgets"
	in.Config.ClaimStatus = ""
	in.Config.ReadyStatuses = nil
	in.Config.AssigneeOK = "any"
	return in
}

func inputWith(items map[string]source.Item, edit func(*Input)) Input {
	cloned := make(map[string]source.Item, len(items))
	for id, it := range items {
		cloned[id] = it
	}
	in := baseInput(cloned)
	if edit != nil {
		edit(&in)
	}
	return in
}

func jiraItem(id, typ, state, parent string, done bool, labels []string, blockers ...string) source.Item {
	it := source.Item{
		ID: id, Type: typ, State: state, Parent: parent, Done: done, Labels: labels,
	}
	for _, blocker := range blockers {
		it.BlockedBy = append(it.BlockedBy, source.BlockRef{ID: blocker})
	}
	return it
}

func ghItem(id string, done bool, state string) source.Item {
	return source.Item{ID: id, Type: "Issue", State: state, Done: done}
}

func claim(id string, state store.ClaimState, claimedAt, updatedAt time.Time) store.Claim {
	return store.Claim{
		Source: "jira:acme", ID: id, State: state, ClaimedAt: claimedAt, UpdatedAt: updatedAt,
	}
}

func cycleWant(path ...string) Finding {
	items := path[:len(path)-1]
	return finding(
		CheckCycle,
		SeverityWarn,
		strings.Join(path, cycleArrow),
		"factory unlink "+path[0]+" "+path[1],
		items...,
	)
}

func mustCheck(t *testing.T, in Input) []Finding {
	t.Helper()
	got, err := Check(in)
	if err != nil {
		t.Fatalf("Check: %v", err)
	}
	if got == nil {
		t.Fatal("findings are nil")
	}
	return got
}

func assertClean(t *testing.T, in Input) {
	t.Helper()
	got := mustCheck(t, in)
	if len(got) != 0 {
		t.Fatalf("findings = %#v, want none", got)
	}
	if FormatText(got) != "" {
		t.Fatalf("text = %q", FormatText(got))
	}
	raw, err := FormatJSON(got)
	if err != nil {
		t.Fatal(err)
	}
	if raw != "[]\n" {
		t.Fatalf("json = %q", raw)
	}
}

func equalFindings(t *testing.T, got, want []Finding) {
	t.Helper()
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("findings:\n%#v\nwant:\n%#v", got, want)
	}
}

func TestDuplicateClaims(t *testing.T) {
	in := baseInput(map[string]source.Item{
		"PRJ-1": {ID: "PRJ-1", State: "In Progress", Parent: "PRJ-0"},
		"PRJ-2": {ID: "PRJ-2", State: "In Progress", Parent: "PRJ-0"},
		"PRJ-0": {ID: "PRJ-0", Type: "Epic", State: "In Progress"},
	})
	in.Snapshot.Tabs = []herdr.Tab{
		{ID: "t1", WorkspaceID: "w1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusWorking},
		{ID: "t2", WorkspaceID: "w1", Label: "PRJ-2", AgentStatus: herdr.AgentStatusWorking},
	}
	in.Claims = []store.Claim{
		{Source: "jira:acme", ID: "PRJ-1", ClaimedAt: in.Now.Add(-2 * time.Minute), UpdatedAt: in.Now.Add(-2 * time.Minute), State: store.Claimed},
		{Source: "jira:acme", ID: "PRJ-1", ClaimedAt: in.Now.Add(-time.Minute), UpdatedAt: in.Now.Add(-time.Minute), State: store.Started},
		// One released and one active on PRJ-2 should not trigger duplicate-claim.
		{Source: "jira:acme", ID: "PRJ-2", ClaimedAt: in.Now.Add(-2 * time.Minute), UpdatedAt: in.Now.Add(-2 * time.Minute), State: store.Released},
		{Source: "jira:acme", ID: "PRJ-2", ClaimedAt: in.Now.Add(-time.Minute), UpdatedAt: in.Now.Add(-time.Minute), State: store.Started},
	}
	got, err := Check(in)
	if err != nil {
		t.Fatal(err)
	}
	want := []Finding{{
		Check:      CheckDuplicateClaim,
		Severity:   SeverityWarn,
		Items:      []string{"PRJ-1"},
		Message:    "PRJ-1 has 2 active claim rows",
		Suggestion: "factory release PRJ-1 or close the extra claim",
	}}
	equalFindings(t, got, want)
}

func TestClosableParents(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"EPIC-MIRROR": jiraItem("EPIC-MIRROR", "Epic", "To Do", "", false, nil),
		"PRJ-1":       jiraItem("PRJ-1", "Task", "Done", "EPIC-MIRROR", true, nil),
		"PRJ-2":       jiraItem("PRJ-2", "Task", "Done", "EPIC-MIRROR", true, nil),
		"EPIC-HIST":   jiraItem("EPIC-HIST", "Epic", "To Do", "", false, nil),
		"EPIC-REOPEN": jiraItem("EPIC-REOPEN", "Epic", "To Do", "", false, nil),
		"PRJ-REOPEN":  jiraItem("PRJ-REOPEN", "Task", "To Do", "EPIC-REOPEN", false, []string{"agent-ready"}),
		"EPIC-MULTI":  jiraItem("EPIC-MULTI", "Epic", "To Do", "", false, nil),
		"STORY-1":     jiraItem("STORY-1", "Story", "To Do", "EPIC-MULTI", false, []string{"agent-ready"}),
		"SUB-1":       jiraItem("SUB-1", "Sub-task", "Done", "STORY-1", true, nil),
		"EPIC-EMPTY":  jiraItem("EPIC-EMPTY", "Epic", "To Do", "", false, nil),
		"EPIC-OTHER":  jiraItem("EPIC-OTHER", "Epic", "To Do", "", false, nil),
	}
	in := baseInput(items)
	in.Finished = []store.Finished{
		{Source: "jira:acme", ID: "PRJ-HIST-2", Parent: "EPIC-HIST", FinishedAt: doctorNow},
		{Source: "jira:acme", ID: "PRJ-HIST-1", Parent: "EPIC-HIST", FinishedAt: doctorNow},
		// Stale finished row for reopened child must not make EPIC-REOPEN closable.
		{Source: "jira:acme", ID: "PRJ-REOPEN", Parent: "EPIC-REOPEN", FinishedAt: doctorNow},
		// Finished row for another source must be ignored.
		{Source: "jira:other", ID: "OTHER-1", Parent: "EPIC-OTHER", FinishedAt: doctorNow},
	}
	got := checkOnly(t, in, CheckClosableParent)
	want := []Finding{
		finding(CheckClosableParent, SeverityInfo, "EPIC-HIST has all children done (PRJ-HIST-1, PRJ-HIST-2)", "factory close EPIC-HIST --apply", "EPIC-HIST"),
		finding(CheckClosableParent, SeverityInfo, "EPIC-MIRROR has all children done (PRJ-1, PRJ-2)", "factory close EPIC-MIRROR --apply", "EPIC-MIRROR"),
		finding(CheckClosableParent, SeverityInfo, "STORY-1 has all children done (SUB-1)", "factory close STORY-1 --apply", "STORY-1"),
	}
	equalFindings(t, got, want)
}

func TestSubagentClaimNotDroppedOrStalled(t *testing.T) {
	t.Parallel()

	stale := doctorNow.Add(-4 * time.Hour)
	items := map[string]source.Item{
		"EPIC-1":  jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
		"PRJ-SUB": jiraItem("PRJ-SUB", "Task", "In Progress", "EPIC-1", false, []string{"agent-ready"}),
	}
	in := baseInput(items)
	subClaim := claim("PRJ-SUB", store.Started, stale, stale)
	subClaim.Spawner = config.SpawnerSubagent
	in.Claims = []store.Claim{subClaim}
	assertClean(t, in)

	// Without spawner="subagent", the same claim is both dropped-claim and stalled.
	herdrClaim := claim("PRJ-SUB", store.Started, stale, stale)
	herdrClaim.Spawner = config.SpawnerHerdr
	in.Claims = []store.Claim{herdrClaim}
	got := mustCheck(t, in)
	want := []Finding{
		finding(CheckDroppedClaim, SeverityWarn, "PRJ-SUB is In Progress with no tab and no open PR", "factory release PRJ-SUB or start it", "PRJ-SUB"),
		finding(CheckStalled, SeverityWarn, "PRJ-SUB is started and older than 2h0m0s with no open PR", "look at the tab", "PRJ-SUB"),
	}
	equalFindings(t, got, want)
}

func TestMergeModeClaims(t *testing.T) {
	t.Parallel()

	stale := doctorNow.Add(-4 * time.Hour)
	recent := doctorNow.Add(-10 * time.Minute)
	items := map[string]source.Item{
		"EPIC-1":  jiraItem("EPIC-1", "Epic", "To Do", "", false, nil),
		"PRJ-MRG": jiraItem("PRJ-MRG", "Task", "In Progress", "EPIC-1", false, []string{"agent-ready"}),
	}

	t.Run("landed commit suppresses dropped-claim", func(t *testing.T) {
		t.Parallel()
		in := baseInput(items)
		in.Config.Output = config.Output{Mode: config.OutputModeMerge}
		cl := claim("PRJ-MRG", store.Started, recent, recent)
		cl.OutputMode = config.OutputModeMerge
		in.Claims = []store.Claim{cl}
		in.LandedCommits = map[string]bool{"PRJ-MRG": true}
		assertClean(t, in)
	})

	t.Run("stalled claim with landed commit suggests settle", func(t *testing.T) {
		t.Parallel()
		in := baseInput(items)
		in.Config.Output = config.Output{Mode: config.OutputModeMerge}
		cl := claim("PRJ-MRG", store.Started, stale, stale)
		cl.OutputMode = config.OutputModeMerge
		in.Claims = []store.Claim{cl}
		in.LandedCommits = map[string]bool{"PRJ-MRG": true}
		got := mustCheck(t, in)
		want := []Finding{
			finding(CheckStalled, SeverityWarn, "PRJ-MRG landed: settle claim", "factory settle PRJ-MRG", "PRJ-MRG"),
		}
		equalFindings(t, got, want)
	})

	t.Run("unlanded merge claim reports no landed commit", func(t *testing.T) {
		t.Parallel()
		in := baseInput(items)
		in.Config.Output = config.Output{Mode: config.OutputModeMerge}
		cl := claim("PRJ-MRG", store.Started, stale, stale)
		cl.OutputMode = config.OutputModeMerge
		in.Claims = []store.Claim{cl}
		got := mustCheck(t, in)
		want := []Finding{
			finding(CheckDroppedClaim, SeverityWarn, "PRJ-MRG is In Progress with no tab and no landed commit", "factory release PRJ-MRG or start it", "PRJ-MRG"),
			finding(CheckStalled, SeverityWarn, "PRJ-MRG is started and older than 2h0m0s with no landed commit", "look at the tab", "PRJ-MRG"),
		}
		equalFindings(t, got, want)
	})
}
