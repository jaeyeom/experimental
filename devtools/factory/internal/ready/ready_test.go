package ready

import (
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

var ruleOrder = []Rule{
	RuleDone,
	RuleType,
	RuleLabels,
	RuleAssignee,
	RuleStatus,
	RuleTrust,
	RuleBlockers,
	RuleParent,
	RuleChildren,
	RuleClaimed,
	RuleTab,
	RulePR,
	RuleNotBefore,
}

func TestRuleNames(t *testing.T) {
	t.Parallel()

	want := []string{"done", "type", "labels", "assignee", "status", "trust", "blockers", "parent", "children", "claimed", "tab", "pr", "not_before"}
	if len(ruleOrder) != len(want) {
		t.Fatalf("rules = %d, want %d", len(ruleOrder), len(want))
	}
	for i, name := range want {
		if string(ruleOrder[i]) != name {
			t.Fatalf("rule %d = %q, want %q", i, ruleOrder[i], name)
		}
	}
}

func TestEvaluateReady(t *testing.T) {
	t.Parallel()

	it, snap, cfg := jiraFixture()
	got := eval(t, it, snap, cfg, "jira")
	if !got.Ready {
		t.Fatalf("Ready = false, checks = %+v", got.Checks)
	}
	for _, c := range got.Checks {
		if !c.Pass || c.Detail != "" {
			t.Fatalf("rule %s = %+v, want pass with empty detail", c.Rule, c)
		}
	}
}

func TestEvaluateReadyGitHub(t *testing.T) {
	t.Parallel()

	it, snap, cfg := githubFixture()
	got := eval(t, it, snap, cfg, "github")
	if !got.Ready {
		t.Fatalf("Ready = false, checks = %+v", got.Checks)
	}
}

func TestEvaluateRunsEveryRule(t *testing.T) {
	t.Parallel()

	it, snap, cfg := jiraFixture()
	it.Done = true
	it.Type = "Epic"
	it.Labels = []string{"needs-human"}
	it.Assignee = "bob"
	it.State = "In Progress"
	it.Author = "mallory"
	it.AuthorAssociation = "NONE"
	cfg.Trust = config.Trust{Environment: config.TrustEnvUntrusted}
	it.BlockedBy = []source.BlockRef{{ID: "PRJ-9"}}
	later := time.Date(2026, 12, 1, 0, 0, 0, 0, time.UTC)
	it.NotBefore = &later
	snap.Now = time.Date(2026, 11, 15, 12, 0, 0, 0, time.Local)
	snap.Items["PRJ-100"] = source.Item{ID: "PRJ-100", Done: true}
	snap.Items["PRJ-2"] = source.Item{ID: "PRJ-2", Parent: it.ID}
	snap.Claims[it.ID] = store.Claimed
	snap.Tabs = []herdr.Tab{{ID: "t1", WorkspaceID: "w1", Label: it.ID}}
	snap.OpenPRs = []gh.PR{{Title: "fix [PRJ-1] yes", URL: "https://example/pr/1"}}

	got := eval(t, it, snap, cfg, "jira")
	if got.Ready {
		t.Fatal("Ready = true, want false")
	}
	want := []Check{
		{Rule: RuleDone, Detail: "done"},
		{Rule: RuleType, Detail: `type "Epic" in skip_types`},
		{Rule: RuleLabels, Detail: "missing agent-ready; excluded needs-human"},
		{Rule: RuleAssignee, Detail: `assignee "bob", want empty or "alice"`},
		{Rule: RuleStatus, Detail: `status "In Progress" not in ready_statuses`},
		{Rule: RuleTrust, Detail: `untrusted author "mallory" requires status in [Todo] (got "In Progress"); untrusted author "mallory" requires locked issue`},
		{Rule: RuleBlockers, Detail: "blocker PRJ-9 not in mirror"},
		{Rule: RuleParent, Detail: "parent PRJ-100 is done"},
		{Rule: RuleChildren, Detail: "has open child PRJ-2"},
		{Rule: RuleClaimed, Detail: "claim claimed"},
		{Rule: RuleTab, Detail: "tab PRJ-1"},
		{Rule: RulePR, Detail: "https://example/pr/1"},
		{Rule: RuleNotBefore, Detail: "deferred until 2026-12-01"},
	}
	if len(got.Checks) != len(want) {
		t.Fatalf("checks = %d, want %d", len(got.Checks), len(want))
	}
	for i, c := range got.Checks {
		if c.Pass {
			t.Fatalf("rule %s passed, want a failure", c.Rule)
		}
		if c.Rule != want[i].Rule || c.Detail != want[i].Detail {
			t.Fatalf("check %d = %+v, want %+v", i, c, want[i])
		}
	}
}

func TestEvaluateUnknownKindPanics(t *testing.T) {
	t.Parallel()

	defer func() {
		msg := recovered(t, recover())
		for _, part := range []string{"expected", "jira", "github", "found", "nope"} {
			if !strings.Contains(msg, part) {
				t.Fatalf("panic = %q, want it to contain %q", msg, part)
			}
		}
	}()
	it, snap, cfg := jiraFixture()
	Evaluate(it, snap, cfg, "nope")
}

func TestEvaluateBadGitHubIDPanics(t *testing.T) {
	t.Parallel()

	defer func() {
		msg := recovered(t, recover())
		for _, part := range []string{"expected", "found", "PRJ-1"} {
			if !strings.Contains(msg, part) {
				t.Fatalf("panic = %q, want it to contain %q", msg, part)
			}
		}
	}()
	it, snap, cfg := githubFixture()
	it.ID = "PRJ-1"
	Evaluate(it, snap, cfg, "github")
}

func TestRuleDone(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		done       bool
		wantPass   bool
		wantDetail string
	}{
		{name: "open", wantPass: true},
		{name: "done", done: true, wantDetail: "done"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			it.Done = tc.done
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleDone, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleType(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		itemType   string
		skip       []string
		wantPass   bool
		wantDetail string
	}{
		{name: "task", itemType: "Task", skip: []string{"Epic"}, wantPass: true},
		{name: "epic", itemType: "Epic", skip: []string{"Epic"}, wantDetail: `type "Epic" in skip_types`},
		{name: "lowercase epic", itemType: "epic", skip: []string{"Epic"}, wantDetail: `type "epic" in skip_types`},
		{name: "upper skip list", itemType: "Bug", skip: []string{"bug"}, wantDetail: `type "Bug" in skip_types`},
		{name: "empty skip list", itemType: "Epic", skip: []string{}, wantPass: true},
		{name: "empty type", itemType: "", skip: []string{"Epic"}, wantPass: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			it.Type = tc.itemType
			cfg.SkipTypes = tc.skip
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleType, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleLabels(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		labels     []string
		require    []string
		exclude    []string
		wantPass   bool
		wantDetail string
	}{
		{
			name:     "required and not excluded",
			labels:   []string{"agent-ready", "extra"},
			require:  []string{"agent-ready"},
			exclude:  []string{"needs-human", "blocked-external"},
			wantPass: true,
		},
		{
			name:       "missing required",
			labels:     nil,
			require:    []string{"agent-ready"},
			exclude:    []string{"needs-human"},
			wantDetail: "missing agent-ready",
		},
		{
			name:       "missing two",
			labels:     []string{"other"},
			require:    []string{"agent-ready", "queued"},
			wantDetail: "missing agent-ready, queued",
		},
		{
			name:       "excluded",
			labels:     []string{"agent-ready", "needs-human"},
			require:    []string{"agent-ready"},
			exclude:    []string{"needs-human", "blocked-external"},
			wantDetail: "excluded needs-human",
		},
		{
			name:       "two excluded",
			labels:     []string{"agent-ready", "needs-human", "blocked-external"},
			require:    []string{"agent-ready"},
			exclude:    []string{"needs-human", "blocked-external"},
			wantDetail: "excluded needs-human, blocked-external",
		},
		{
			name:       "missing and excluded",
			labels:     []string{"needs-human"},
			require:    []string{"agent-ready"},
			exclude:    []string{"needs-human"},
			wantDetail: "missing agent-ready; excluded needs-human",
		},
		{
			name:     "no label rules",
			labels:   []string{"anything"},
			require:  nil,
			exclude:  nil,
			wantPass: true,
		},
		{
			name:       "case sensitive",
			labels:     []string{"agent-ready"},
			require:    []string{"Agent-Ready"},
			wantDetail: "missing Agent-Ready",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			it.Labels = tc.labels
			cfg.RequireLabels = tc.require
			cfg.ExcludeLabels = tc.exclude
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleLabels, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleAssignee(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		mode       string
		assignee   string
		me         string
		wantPass   bool
		wantDetail string
	}{
		{name: "me and empty", mode: "me", me: "alice", wantPass: true},
		{name: "me and self", mode: "me", assignee: "alice", me: "alice", wantPass: true},
		{name: "me and someone else", mode: "me", assignee: "bob", me: "alice", wantDetail: `assignee "bob", want empty or "alice"`},
		{name: "me is case sensitive", mode: "me", assignee: "Alice", me: "alice", wantDetail: `assignee "Alice", want empty or "alice"`},
		{name: "unassigned empty", mode: "unassigned", wantPass: true},
		{name: "unassigned filled", mode: "unassigned", assignee: "alice", me: "alice", wantDetail: `assignee "alice", want empty`},
		{name: "any", mode: "any", assignee: "bob", me: "alice", wantPass: true},
		{name: "unexpected mode", mode: "sometimes", assignee: "bob", wantDetail: `assignee_ok: expected me, unassigned, or any, found "sometimes"`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			it.Assignee = tc.assignee
			snap.Me = tc.me
			cfg.AssigneeOK = tc.mode
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleAssignee, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleStatus(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		state      string
		ready      []string
		wantPass   bool
		wantDetail string
	}{
		{name: "in list", state: "To Do", ready: []string{"To Do", "Backlog"}, wantPass: true},
		{name: "second status", state: "Backlog", ready: []string{"To Do", "Backlog"}, wantPass: true},
		{name: "not in list", state: "In Progress", ready: []string{"To Do", "Backlog"}, wantDetail: `status "In Progress" not in ready_statuses`},
		{name: "case sensitive", state: "to do", ready: []string{"To Do"}, wantDetail: `status "to do" not in ready_statuses`},
		{name: "empty state", state: "", ready: []string{"To Do"}, wantDetail: `status "" not in ready_statuses`},
		{name: "empty list", state: "Anything", ready: []string{}, wantPass: true},
		{name: "nil list", state: "Anything", ready: nil, wantPass: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			it.State = tc.state
			cfg.ReadyStatuses = tc.ready
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleStatus, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleBlockers(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		refs       []source.BlockRef
		items      map[string]source.Item
		wantPass   bool
		wantDetail string
	}{
		{name: "none", wantPass: true},
		{
			name:     "done blocker",
			refs:     []source.BlockRef{{ID: "PRJ-2"}},
			items:    map[string]source.Item{"PRJ-2": {ID: "PRJ-2", State: "In Progress", Done: true}},
			wantPass: true,
		},
		{
			name:       "not done",
			refs:       []source.BlockRef{{ID: "PRJ-2"}},
			items:      map[string]source.Item{"PRJ-2": {ID: "PRJ-2", State: "Done", Done: false}},
			wantDetail: "blocker PRJ-2 is not done",
		},
		{
			name:       "unknown blocker",
			refs:       []source.BlockRef{{ID: "PRJ-9"}},
			wantDetail: "blocker PRJ-9 not in mirror",
		},
		{
			name: "done and unknown",
			refs: []source.BlockRef{{ID: "PRJ-2"}, {ID: "PRJ-9"}},
			items: map[string]source.Item{
				"PRJ-2": {ID: "PRJ-2", Done: true},
			},
			wantDetail: "blocker PRJ-9 not in mirror",
		},
		{
			name:       "unknown and not done",
			refs:       []source.BlockRef{{ID: "PRJ-9"}, {ID: "PRJ-2"}},
			items:      map[string]source.Item{"PRJ-2": {ID: "PRJ-2", Done: false}},
			wantDetail: "blocker PRJ-9 not in mirror; blocker PRJ-2 is not done",
		},
		{
			name:       "two unknown",
			refs:       []source.BlockRef{{ID: "PRJ-9"}, {ID: "PRJ-8"}},
			wantDetail: "blocker PRJ-9 not in mirror; blocker PRJ-8 not in mirror",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			it.BlockedBy = tc.refs
			for id, item := range tc.items {
				snap.Items[id] = item
			}
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleBlockers, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleBlockersOutsideMirror(t *testing.T) {
	t.Parallel()

	external := map[string]store.ExternalBlocker{
		"X-DONE":  {ID: "X-DONE", Status: store.ExternalFound, State: "Done", Done: true},
		"X-OPEN":  {ID: "X-OPEN", Status: store.ExternalFound, State: "In Review", Assignee: "bob"},
		"X-NOBOD": {ID: "X-NOBOD", Status: store.ExternalFound, State: "To Do"},
		"X-GONE":  {ID: "X-GONE", Status: store.ExternalNotFound},
		"X-SKIP":  {ID: "X-SKIP", Status: store.ExternalUnchecked, Reason: "over external_blocker_lookups (1)"},
		"X-FAIL":  {ID: "X-FAIL", Status: store.ExternalUnchecked, Reason: "lookup failed: 500", Done: true},
	}
	tests := []struct {
		name       string
		blocker    string
		lookups    int
		wantPass   bool
		wantDetail string
	}{
		{name: "done", blocker: "X-DONE", lookups: 100, wantPass: true},
		{name: "open", blocker: "X-OPEN", lookups: 100, wantDetail: "blocker X-OPEN is In Review (outside mirror, assigned to bob)"},
		{name: "unassigned", blocker: "X-NOBOD", lookups: 100, wantDetail: "blocker X-NOBOD is To Do (outside mirror, unassigned)"},
		{name: "not found or no access", blocker: "X-GONE", lookups: 100, wantDetail: "blocker X-GONE not found or no access"},
		{name: "over the bound", blocker: "X-SKIP", lookups: 1, wantDetail: "blocker X-SKIP not in mirror (not checked: over external_blocker_lookups (1))"},
		{name: "failed fetch never passes", blocker: "X-FAIL", lookups: 100, wantDetail: "blocker X-FAIL not in mirror (not checked: lookup failed: 500)"},
		{name: "never fetched", blocker: "X-NEW", lookups: 100, wantDetail: "blocker X-NEW not in mirror (not fetched yet)"},
		{name: "lookups off", blocker: "X-NEW", lookups: 0, wantDetail: "blocker X-NEW not in mirror"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			cfg.Kind = "jira"
			cfg.ExternalBlockerLookups = tc.lookups
			it.BlockedBy = []source.BlockRef{{ID: tc.blocker}}
			snap.External = external
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleBlockers, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleParent(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		parent     string
		allowSet   bool
		allow      []string
		inSnap     bool
		parentDone bool
		wantPass   bool
		wantDetail string
	}{
		{name: "open parent", parent: "PRJ-100", inSnap: true, wantPass: true},
		{name: "parent not mirrored", parent: "PRJ-404", wantPass: true, wantDetail: "parent not mirrored"},
		{name: "done parent", parent: "PRJ-100", inSnap: true, parentDone: true, wantDetail: "parent PRJ-100 is done"},
		{name: "allowlist hit", parent: "PRJ-100", allowSet: true, allow: []string{"PRJ-100"}, inSnap: true, wantPass: true},
		{name: "allowlist miss", parent: "PRJ-100", allowSet: true, allow: []string{"PRJ-200"}, inSnap: true, wantDetail: `parent "PRJ-100" not in allowlist`},
		{name: "allowlist is case sensitive", parent: "PRJ-100", allowSet: true, allow: []string{"prj-100"}, inSnap: true, wantDetail: `parent "PRJ-100" not in allowlist`},
		{name: "allowlist hit and missing", parent: "PRJ-404", allowSet: true, allow: []string{"PRJ-404"}, wantPass: true, wantDetail: "parent not mirrored"},
		{name: "allowlist miss and missing", parent: "PRJ-404", allowSet: true, allow: []string{"PRJ-100"}, wantDetail: `parent "PRJ-404" not in allowlist; parent not mirrored`},
		{name: "allowlist hit and done parent", parent: "PRJ-100", allowSet: true, allow: []string{"PRJ-100"}, inSnap: true, parentDone: true, wantDetail: "parent PRJ-100 is done"},
		{name: "allowlist miss and done parent", parent: "PRJ-100", allowSet: true, allow: []string{"PRJ-200"}, inSnap: true, parentDone: true, wantDetail: `parent "PRJ-100" not in allowlist; parent PRJ-100 is done`},
		{name: "no parent", parent: "", wantPass: true},
		{name: "no parent outside allowlist", parent: "", allowSet: true, allow: []string{"PRJ-100"}, wantDetail: `parent "" not in allowlist`},
		{name: "empty allowlist is set", parent: "PRJ-100", allowSet: true, allow: []string{}, inSnap: true, wantDetail: `parent "PRJ-100" not in allowlist`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			it.Parent = tc.parent
			if tc.allowSet {
				cfg.ParentAllowlist = tc.allow
			}
			delete(snap.Items, "PRJ-100")
			if tc.parent != "" && tc.inSnap {
				snap.Items[tc.parent] = source.Item{ID: tc.parent, Done: tc.parentDone}
			}
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleParent, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleClaimed(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name       string
		id         string
		state      store.ClaimState
		present    bool
		nilMap     bool
		wantPass   bool
		wantDetail string
	}{
		{name: "absent", wantPass: true},
		{name: "nil map", nilMap: true, wantPass: true},
		{name: "claimed", present: true, state: store.Claimed, wantDetail: "claim claimed"},
		{name: "started", present: true, state: store.Started, wantDetail: "claim started"},
		{name: "handed off", present: true, state: store.HandedOff, wantDetail: "claim handed_off"},
		{name: "done row still blocks", present: true, state: store.Done, wantDetail: "claim done"},
		{name: "other item", id: "PRJ-2", present: true, state: store.Claimed, wantPass: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it, snap, cfg := jiraFixture()
			if tc.nilMap {
				snap.Claims = nil
			}
			if tc.present {
				id := tc.id
				if id == "" {
					id = it.ID
				}
				snap.Claims[id] = tc.state
			}
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleClaimed, tc.wantPass, tc.wantDetail)
		})
	}
}

func TestRuleTab(t *testing.T) {
	t.Parallel()

	t.Run("jira no tab", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleTab, true, "")
	})

	t.Run("jira tab in any workspace", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.Tabs = []herdr.Tab{{ID: "t1", WorkspaceID: "somewhere-else", Label: "PRJ-1"}}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleTab, false, "tab PRJ-1")
	})

	t.Run("jira other label", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.Tabs = []herdr.Tab{{ID: "t1", WorkspaceID: "w1", Label: "PRJ-2"}}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleTab, true, "")
	})

	t.Run("github tab in other workspace does not block", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		snap.Tabs = []herdr.Tab{{ID: "t1", WorkspaceID: "w-other", Label: "#57"}}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTab, true, "")
	})

	t.Run("github tab in resolved workspace", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		snap.Tabs = []herdr.Tab{{ID: "t1", WorkspaceID: "w-exp", Label: "#57"}}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTab, false, "tab #57 in workspace experimental")
	})

	t.Run("github workspace override", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Workspace = "other"
		snap.Tabs = []herdr.Tab{
			{ID: "t1", WorkspaceID: "w-exp", Label: "#57"},
			{ID: "t2", WorkspaceID: "w-other", Label: "#57"},
		}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTab, false, "tab #57 in workspace other")
	})

	t.Run("github duplicate workspace label", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		snap.Workspaces = append(snap.Workspaces, herdr.Workspace{ID: "w-exp-2", Label: "experimental"})
		snap.Tabs = []herdr.Tab{{ID: "t1", WorkspaceID: "w-exp-2", Label: "#57"}}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTab, false, "tab #57 in workspace experimental")
	})
}

func TestRulePR(t *testing.T) {
	t.Parallel()

	t.Run("none", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RulePR, true, "")
	})

	t.Run("jira title match", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.OpenPRs = []gh.PR{{Title: "fix [PRJ-1] now", URL: "https://example/pr/1"}}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RulePR, false, "https://example/pr/1")
	})

	t.Run("jira different id", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.OpenPRs = []gh.PR{{Title: "fix [PRJ-10] now", URL: "https://example/pr/10"}}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RulePR, true, "")
	})

	t.Run("two urls", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.OpenPRs = []gh.PR{
			{Title: "fix [PRJ-1] a", URL: "https://example/a"},
			{Title: "fix [PRJ-1] b", URL: "https://example/b"},
		}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RulePR, false, "https://example/a, https://example/b")
	})

	t.Run("github closing issue", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		snap.OpenPRs = []gh.PR{{
			Title:         "unrelated",
			URL:           "https://example/closes",
			ClosingIssues: []string{"jaeyeom/experimental#57"},
		}}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RulePR, false, "https://example/closes")
	})

	t.Run("github title is not a match", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		snap.OpenPRs = []gh.PR{{
			Title: "[jaeyeom/experimental#57] only in the title",
			URL:   "https://example/title",
		}}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RulePR, true, "")
	})
}

func jiraFixture() (source.Item, Snapshot, config.Source) {
	it := source.Item{
		ID:       "PRJ-1",
		Type:     "Task",
		State:    "To Do",
		Parent:   "PRJ-100",
		Labels:   []string{"agent-ready"},
		Assignee: "",
	}
	snap := Snapshot{
		Items: map[string]source.Item{
			"PRJ-100": {ID: "PRJ-100"},
		},
		Claims: map[string]store.ClaimState{},
		Me:     "alice",
		Workspaces: []herdr.Workspace{
			{ID: "w1", Label: "factory"},
		},
	}
	cfg := config.Source{
		Kind:           "jira",
		Workspace:      "factory",
		RequireLabels:  []string{"agent-ready"},
		ExcludeLabels:  []string{"needs-human", "blocked-external"},
		SkipTypes:      []string{"Epic"},
		AssigneeOK:     "me",
		ReadyStatuses:  []string{"To Do", "Backlog"},
		PRTitlePattern: `\[{id}\]`,
	}
	return it, snap, cfg
}

func githubFixture() (source.Item, Snapshot, config.Source) {
	it := source.Item{
		ID:     "jaeyeom/experimental#57",
		Type:   "issue",
		State:  "OPEN",
		Labels: []string{"agent-ready"},
	}
	snap := Snapshot{
		Claims: map[string]store.ClaimState{},
		Me:     "jaeyeom",
		Workspaces: []herdr.Workspace{
			{ID: "w-exp", Label: "experimental"},
			{ID: "w-other", Label: "other"},
		},
	}
	cfg := config.Source{
		Kind:           "github",
		Repo:           "jaeyeom/experimental",
		RequireLabels:  []string{"agent-ready"},
		ExcludeLabels:  []string{"needs-human"},
		SkipTypes:      []string{"Epic"},
		AssigneeOK:     "me",
		PRTitlePattern: `\[{id}\]`,
	}
	return it, snap, cfg
}

func eval(t *testing.T, it source.Item, snap Snapshot, cfg config.Source, kind string) Result {
	t.Helper()
	got := Evaluate(it, snap, cfg, kind)
	if got.ItemID != it.ID {
		t.Fatalf("ItemID = %q, want %q", got.ItemID, it.ID)
	}
	if len(got.Checks) != len(ruleOrder) {
		t.Fatalf("checks = %d, want %d (%+v)", len(got.Checks), len(ruleOrder), got.Checks)
	}
	ready := true
	for i, rule := range ruleOrder {
		if got.Checks[i].Rule != rule {
			t.Fatalf("check %d rule = %q, want %q", i, got.Checks[i].Rule, rule)
		}
		if !got.Checks[i].Pass {
			ready = false
			if got.Checks[i].Detail == "" {
				t.Fatalf("rule %s failed with empty detail", rule)
			}
		}
	}
	if got.Ready != ready {
		t.Fatalf("Ready = %v, checks say %v", got.Ready, ready)
	}
	return got
}

func assertRule(t *testing.T, got Result, rule Rule, wantPass bool, wantDetail string) {
	t.Helper()
	for _, c := range got.Checks {
		if c.Rule != rule {
			continue
		}
		if c.Pass != wantPass || c.Detail != wantDetail {
			t.Fatalf("rule %s = {Pass:%v Detail:%q}, want {Pass:%v Detail:%q}", rule, c.Pass, c.Detail, wantPass, wantDetail)
		}
		return
	}
	t.Fatalf("missing rule %s", rule)
}

func recovered(t *testing.T, rec any) string {
	t.Helper()
	if rec == nil {
		t.Fatal("recovered = nil, want a panic")
	}
	switch v := rec.(type) {
	case string:
		return v
	case error:
		return v.Error()
	default:
		t.Fatalf("panic type %T = %v, want a string or error", rec, rec)
		return ""
	}
}

func TestNotBefore(t *testing.T) {
	day := func(y int, m time.Month, d int) *time.Time {
		v := time.Date(y, m, d, 0, 0, 0, 0, time.UTC)
		return &v
	}
	now := time.Date(2026, 11, 15, 23, 30, 0, 0, time.Local)
	tests := []struct {
		name      string
		notBefore *time.Time
		now       time.Time
		pass      bool
		detail    string
	}{
		{name: "none", pass: true},
		{name: "tomorrow", notBefore: day(2026, 11, 16), now: now, detail: "deferred until 2026-11-16"},
		{name: "today", notBefore: day(2026, 11, 15), now: now, pass: true, detail: "not before 2026-11-15 (passed)"},
		{name: "past", notBefore: day(2026, 1, 2), now: now, pass: true, detail: "not before 2026-01-02 (passed)"},
		{name: "clock moved past the date", notBefore: day(2026, 11, 16), now: now.Add(time.Hour), pass: true, detail: "not before 2026-11-16 (passed)"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			it, snap, cfg := jiraFixture()
			it.NotBefore = tc.notBefore
			snap.Now = tc.now
			got := eval(t, it, snap, cfg, "jira")
			assertRule(t, got, RuleNotBefore, tc.pass, tc.detail)
			if got.Ready != tc.pass {
				t.Fatalf("Ready = %v, want %v", got.Ready, tc.pass)
			}
		})
	}
}

func TestRuleChildren(t *testing.T) {
	t.Parallel()

	t.Run("jira task with open sub-task is not ready", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.Items["PRJ-2"] = source.Item{ID: "PRJ-2", Type: "Sub-task", Parent: it.ID, Done: false}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleChildren, false, "has open child PRJ-2")
		if got.Ready {
			t.Fatal("Ready = true, want false")
		}
		open, blocked, notEligible := Classify(got)
		if open != 1 || blocked != 0 || notEligible != 1 {
			t.Fatalf("Classify = (%d, %d, %d), want (1, 0, 1)", open, blocked, notEligible)
		}
	})

	t.Run("jira task with multiple open sub-tasks sorts deterministically", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.Items["PRJ-9"] = source.Item{ID: "PRJ-9", Type: "Sub-task", Parent: it.ID, Done: false}
		snap.Items["PRJ-2"] = source.Item{ID: "PRJ-2", Type: "Sub-task", Parent: it.ID, Done: false}
		snap.Items["PRJ-5"] = source.Item{ID: "PRJ-5", Type: "Sub-task", Parent: it.ID, Done: true}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleChildren, false, "has open child PRJ-2; has open child PRJ-9")
		if got.Ready {
			t.Fatal("Ready = true, want false")
		}
	})

	t.Run("jira task becomes ready once all sub-tasks are done", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		snap.Items["PRJ-2"] = source.Item{ID: "PRJ-2", Type: "Sub-task", Parent: it.ID, Done: true}
		snap.Items["PRJ-3"] = source.Item{ID: "PRJ-3", Type: "Sub-task", Parent: it.ID, Done: true}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleChildren, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})

	t.Run("github issue with open sub-issue is not ready", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		it.Type = "Issue"
		snap.Items = map[string]source.Item{
			it.ID:                     it,
			"jaeyeom/experimental#58": {ID: "jaeyeom/experimental#58", Type: "Issue", Parent: it.ID, Done: false},
		}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleChildren, false, "has open child jaeyeom/experimental#58")
		if got.Ready {
			t.Fatal("Ready = true, want false")
		}
	})

	t.Run("github issue becomes ready once all sub-issues are done", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		it.Type = "Issue"
		snap.Items = map[string]source.Item{
			it.ID:                     it,
			"jaeyeom/experimental#58": {ID: "jaeyeom/experimental#58", Type: "Issue", Parent: it.ID, Done: true},
		}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleChildren, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})

	t.Run("unrelated items and self-parent do not fail children", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		it.Parent = it.ID
		snap.Items[it.ID] = it
		snap.Items["PRJ-2"] = source.Item{ID: "PRJ-2", Parent: "PRJ-100", Done: false}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleChildren, true, "")
	})
}

func TestInProgressRule(t *testing.T) {
	t.Parallel()

	t.Run("github claim_label blocks by default", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.ClaimLabel = "in-progress"
		it.Labels = []string{"agent-ready", "in-progress"}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleLabels, false, "already in progress (in-progress)")
		if got.Ready {
			t.Fatal("Ready = true, want false")
		}
	})

	t.Run("github claim_label allowed when in_progress_rule is allow", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Claim = config.Claim{
			MarkInProgress: true,
			InProgressRule: config.InProgressAllow,
			Label:          "in-progress",
		}
		it.Labels = []string{"agent-ready", "in-progress"}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleLabels, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})

	t.Run("github claim_label ignored when mark_in_progress is false", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Claim = config.Claim{
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
			Label:          "in-progress",
		}
		it.Labels = []string{"agent-ready", "in-progress"}
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleLabels, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})

	t.Run("jira claim_status blocks when ready_statuses is empty", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		cfg.ReadyStatuses = nil
		cfg.ClaimStatus = "In Progress"
		it.State = "In Progress"
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleStatus, false, `status "In Progress" is already in progress`)
		if got.Ready {
			t.Fatal("Ready = true, want false")
		}
	})

	t.Run("jira claim_status allowed when in_progress_rule is allow", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		cfg.ReadyStatuses = nil
		cfg.Claim = config.Claim{
			MarkInProgress: true,
			InProgressRule: config.InProgressAllow,
			Status:         "In Progress",
		}
		it.State = "In Progress"
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleStatus, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})

	t.Run("jira claim_label blocks by default", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := jiraFixture()
		cfg.Claim = config.Claim{
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Status:         "In Progress",
			Label:          "agent-active",
		}
		it.Labels = []string{"agent-ready", "agent-active"}
		got := eval(t, it, snap, cfg, "jira")
		assertRule(t, got, RuleLabels, false, "already in progress (agent-active)")
	})
}

func TestRuleTrust(t *testing.T) {
	t.Parallel()

	t.Run("trusted environment ignores external author and lock", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Trust = config.Trust{Environment: config.TrustEnvTrusted}
		it.State = "Backlog"
		it.Author = "alice"
		it.AuthorAssociation = "NONE"
		it.Locked = false
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})

	t.Run("untrusted allows OWNER and me in Backlog and OPEN without lock", func(t *testing.T) {
		t.Parallel()
		for _, tc := range []struct {
			name   string
			author string
			assoc  string
			state  string
		}{
			{name: "OWNER in Backlog", author: "repo-owner", assoc: "OWNER", state: "Backlog"},
			{name: "OWNER in OPEN", author: "repo-owner", assoc: "OWNER", state: "OPEN"},
			{name: "me in Backlog", author: "jaeyeom", assoc: "NONE", state: "Backlog"},
			{name: "me in OPEN", author: "jaeyeom", assoc: "CONTRIBUTOR", state: "OPEN"},
			{name: "trusted_authors in Backlog", author: "trusted-bot", assoc: "NONE", state: "Backlog"},
		} {
			t.Run(tc.name, func(t *testing.T) {
				t.Parallel()
				it, snap, cfg := githubFixture()
				cfg.Trust = config.Trust{
					Environment:    config.TrustEnvUntrusted,
					TrustedAuthors: []string{"trusted-bot"},
				}
				it.State = tc.state
				it.Author = tc.author
				it.AuthorAssociation = tc.assoc
				it.Locked = false
				got := eval(t, it, snap, cfg, "github")
				assertRule(t, got, RuleTrust, true, "")
				if !got.Ready {
					t.Fatalf("Ready = false, checks = %+v", got.Checks)
				}
			})
		}
	})

	t.Run("untrusted blocks external author in Backlog and OPEN", func(t *testing.T) {
		t.Parallel()
		for _, state := range []string{"Backlog", "OPEN"} {
			t.Run(state, func(t *testing.T) {
				t.Parallel()
				it, snap, cfg := githubFixture()
				cfg.Trust = config.Trust{Environment: config.TrustEnvUntrusted}
				it.State = state
				it.Author = "stranger"
				it.AuthorAssociation = "NONE"
				it.Locked = true
				got := eval(t, it, snap, cfg, "github")
				assertRule(t, got, RuleTrust, false, `untrusted author "stranger" requires status in [Todo] (got "`+state+`")`)
				if got.Ready {
					t.Fatal("Ready = true, want false")
				}
				open, blocked, notEligible := Classify(got)
				if open != 1 || blocked != 0 || notEligible != 1 {
					t.Fatalf("Classify = (%d, %d, %d), want (1, 0, 1)", open, blocked, notEligible)
				}
			})
		}
	})

	t.Run("untrusted MEMBER and COLLABORATOR are not trusted by default", func(t *testing.T) {
		t.Parallel()
		for _, assoc := range []string{"MEMBER", "COLLABORATOR"} {
			t.Run(assoc, func(t *testing.T) {
				t.Parallel()
				it, snap, cfg := githubFixture()
				cfg.Trust = config.Trust{Environment: config.TrustEnvUntrusted}
				it.State = "Backlog"
				it.Author = "someone"
				it.AuthorAssociation = assoc
				it.Locked = true
				got := eval(t, it, snap, cfg, "github")
				assertRule(t, got, RuleTrust, false, `untrusted author "someone" requires status in [Todo] (got "Backlog")`)
			})
		}
	})

	t.Run("untrusted Todo unlocked blocked when locked guard enabled, allowed when locked", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Trust = config.Trust{Environment: config.TrustEnvUntrusted}
		it.State = "Todo"
		it.Author = "stranger"
		it.AuthorAssociation = "NONE"
		it.Locked = false
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, false, `untrusted author "stranger" requires locked issue`)
		if got.Ready {
			t.Fatal("Ready = true, want false")
		}

		it.Locked = true
		got = eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})

	t.Run("empty guards disables tamper guard while keeping status gating", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Trust = config.Trust{
			Environment: config.TrustEnvUntrusted,
			Guards:      []string{},
		}
		it.State = "Todo"
		it.Author = "stranger"
		it.AuthorAssociation = "NONE"
		it.Locked = false
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}

		it.State = "Backlog"
		got = eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, false, `untrusted author "stranger" requires status in [Todo] (got "Backlog")`)
	})

	t.Run("empty untrusted_ready_statuses blocks all external issues", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Trust = config.Trust{
			Environment:            config.TrustEnvUntrusted,
			UntrustedReadyStatuses: []string{},
		}
		it.State = "Todo"
		it.Author = "stranger"
		it.Locked = true
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, false, `untrusted author "stranger" is blocked (untrusted_ready_statuses is empty)`)
	})

	t.Run("deleted author with empty string is treated as untrusted", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Trust = config.Trust{Environment: config.TrustEnvUntrusted}
		snap.Me = ""
		it.State = "Todo"
		it.Author = ""
		it.AuthorAssociation = ""
		it.Locked = false
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, false, `untrusted author "" requires locked issue`)
	})

	t.Run("approved_hash guard blocks when unapproved or edited and passes when matching", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Trust = config.Trust{
			Environment: config.TrustEnvUntrusted,
			Guards:      []string{config.TrustGuardApprovedHash},
		}
		it.State = "Todo"
		it.Author = "stranger"
		it.Locked = false
		it.ContentHash = source.ContentHash(it.Title, "original body")

		// Unapproved.
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, false, `untrusted author "stranger" requires approved_hash (not approved)`)
		if got.Ready {
			t.Fatal("Ready = true, want false when unapproved")
		}

		// Matching approval hash passes even when unlocked.
		snap.Approvals = map[string]store.Approval{
			it.ID: {
				Source:      it.Source,
				ItemID:      it.ID,
				ContentHash: it.ContentHash,
				ApprovedAt:  time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC),
			},
		}
		got = eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}

		// Edited title or body invalidates approval.
		it.ContentHash = source.ContentHash(it.Title, "tampered body")
		got = eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, false, `untrusted author "stranger" requires approved_hash (edited since approval)`)
		if got.Ready {
			t.Fatal("Ready = true, want false after edit")
		}
	})

	t.Run("locked and approved_hash OR semantics", func(t *testing.T) {
		t.Parallel()
		it, snap, cfg := githubFixture()
		cfg.Trust = config.Trust{
			Environment: config.TrustEnvUntrusted,
			Guards:      []string{config.TrustGuardLocked, config.TrustGuardApprovedHash},
		}
		it.State = "Todo"
		it.Author = "stranger"
		it.ContentHash = source.ContentHash(it.Title, "body")

		// Neither locked nor approved -> blocked.
		it.Locked = false
		got := eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, false, `untrusted author "stranger" requires one of guards [locked, approved_hash] (not approved)`)

		// Locked alone satisfies OR even without approval.
		it.Locked = true
		got = eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, true, "")

		// Approved hash alone satisfies OR even when unlocked.
		it.Locked = false
		snap.Approvals = map[string]store.Approval{
			it.ID: {
				Source:      it.Source,
				ItemID:      it.ID,
				ContentHash: it.ContentHash,
				ApprovedAt:  time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC),
			},
		}
		got = eval(t, it, snap, cfg, "github")
		assertRule(t, got, RuleTrust, true, "")
		if !got.Ready {
			t.Fatalf("Ready = false, checks = %+v", got.Checks)
		}
	})
}
