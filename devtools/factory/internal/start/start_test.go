package start

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"log/slog"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

const wantPlan = "" +
	"claim    PRJ-123 → In Progress (assign to me)\n" +
	"tab      workspace \"factory\" / tab \"PRJ-123\" (cwd ~/src/monorepo)\n" +
	"agent    claude\n" +
	"args     (default)\n" +
	"prompt   PRJ-123 Ship it https://example.test/PRJ-123 acme main PRJ-9…\n"

var (
	fixedNow = time.Date(2026, 11, 1, 12, 0, 0, 0, time.UTC) //nolint:pkgvar // time.Date with time.UTC does not load zone data
	okGate   = gate.Result{OK: true}
	applyOpt = Options{Apply: true}
	forceOpt = Options{Apply: true, Force: true}

	happyCalls = []string{"Tabs", "Get", "Claim", "Get", "FindWorkspace", "CreateTab", "StartAgent", "WaitAgent", "Prompt"}
)

func TestAgentArgs(t *testing.T) {
	t.Parallel()
	rules := []config.AgentRule{
		{Labels: []string{"mechanical"}, AgentArgs: []string{"--model", "sonnet"}},
		{Labels: []string{"report-only"}, AgentArgs: []string{"--model", "haiku"}},
		{Name: "big", Labels: []string{"heavy"}, AgentArgs: []string{"--model=opus"}},
	}
	tests := []struct {
		name     string
		labels   []string
		defaults []string
		opts     Options
		wantLine string
		wantArgs []string
		// wantRule and wantModel are what the claim records.
		wantRule  string
		wantModel string
	}{
		{
			name:      "rule",
			labels:    []string{"mechanical"},
			defaults:  []string{"--model", "opus"},
			opts:      applyOpt,
			wantLine:  "args     --model sonnet (rule 1 labels mechanical)\n",
			wantArgs:  []string{"--model", "sonnet"},
			wantRule:  "mechanical",
			wantModel: "sonnet",
		},
		{
			name:      "named rule",
			labels:    []string{"heavy"},
			opts:      applyOpt,
			wantLine:  "args     --model=opus (rule 3 labels heavy)\n",
			wantArgs:  []string{"--model=opus"},
			wantRule:  "big",
			wantModel: "opus",
		},
		{
			name:      "default",
			defaults:  []string{"--model", "opus"},
			opts:      applyOpt,
			wantLine:  "args     --model opus (default)\n",
			wantArgs:  []string{"--model", "opus"},
			wantRule:  "default",
			wantModel: "opus",
		},
		{
			name:      "flag",
			labels:    []string{"mechanical"},
			defaults:  []string{"--model", "opus"},
			opts:      Options{Apply: true, AgentArgs: []string{"--model", "haiku"}, AgentArgsSet: true},
			wantLine:  "args     --model haiku (flag)\n",
			wantArgs:  []string{"--model", "haiku"},
			wantRule:  "flag",
			wantModel: "haiku",
		},
		{
			name:     "flag clears",
			labels:   []string{"report-only"},
			opts:     Options{Apply: true, AgentArgsSet: true},
			wantLine: "args     (flag)\n",
			wantRule: "flag",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			h := newHarness(t)
			h.starter.Defaults.AgentArgs = tc.defaults
			h.starter.Rules = rules
			it := h.snap.Items[h.id]
			it.Labels = tc.labels
			h.snap.Items[h.id] = it
			if err := h.start(h.snap, okGate, tc.opts); err != nil {
				t.Fatal(err)
			}
			if !strings.Contains(h.out.String(), tc.wantLine) {
				t.Fatalf("plan =\n%s\nwant line %q", h.out.String(), tc.wantLine)
			}
			if !slices.Equal(h.herdr.agentArgs, tc.wantArgs) {
				t.Fatalf("agent args = %#v, want %#v", h.herdr.agentArgs, tc.wantArgs)
			}
			claim, err := h.st.LatestClaim(context.Background(), "jira:acme", h.id)
			if err != nil {
				t.Fatal(err)
			}
			if claim.AgentRule != tc.wantRule || claim.Model != tc.wantModel {
				t.Fatalf("claim rule %q model %q, want %q and %q", claim.AgentRule, claim.Model, tc.wantRule, tc.wantModel)
			}
		})
	}
}

func TestDryRun(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	if h.out.String() != wantPlan {
		t.Fatalf("plan =\n%s\nwant =\n%s", h.out.String(), wantPlan)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestCallerInPrompt(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Caller = "coord"
	h.starter.Cfg.StartPrompt = "report to {caller} about {id}\nsecond"
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if h.herdr.promptText != "report to coord about PRJ-123\nsecond" {
		t.Fatalf("prompt = %q", h.herdr.promptText)
	}
	if !strings.Contains(h.out.String(), "prompt   report to coord about PRJ-123…\n") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
	if strings.Contains(h.out.String(), "{caller}") {
		t.Fatalf("plan still has the placeholder:\n%s", h.out.String())
	}
}

func TestCallerEmptyWarns(t *testing.T) {
	h := newHarness(t)
	h.starter.Cfg.StartPrompt = "report to {caller} about {id}"
	var logs bytes.Buffer
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&logs, nil)))
	t.Cleanup(func() { slog.SetDefault(prev) })
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(h.out.String(), "prompt   report to  about PRJ-123…\n") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
	if strings.Contains(h.out.String(), "{caller}") {
		t.Fatalf("plan still has the placeholder:\n%s", h.out.String())
	}
	if !strings.Contains(logs.String(), "level=WARN") || !strings.Contains(logs.String(), "{caller}") {
		t.Fatalf("log = %q", logs.String())
	}
}

func TestCallerUnsetWithoutPlaceholderIsQuiet(t *testing.T) {
	h := newHarness(t)
	var logs bytes.Buffer
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&logs, nil)))
	t.Cleanup(func() { slog.SetDefault(prev) })
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	if logs.Len() != 0 {
		t.Fatalf("log = %q", logs.String())
	}
}

func TestDryRunNilClients(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Src = nil
	h.starter.Store = nil
	h.starter.Herdr = nil
	h.starter.Now = nil
	h.starter.Hostname = ""
	if err := h.start(h.snap, okGate, Options{}); err != nil {
		t.Fatal(err)
	}
	if h.out.String() != wantPlan {
		t.Fatalf("plan =\n%s\nwant =\n%s", h.out.String(), wantPlan)
	}
}

func TestHappyPath(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	if got, want := h.out.String(), wantPlan+"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n"; got != want {
		t.Fatalf("plan =\n%s\nwant =\n%s", got, want)
	}
	assertCalls(t, h.log, happyCalls)
	assertNoRelease(t, h.src)
	if h.herdr.agentName != "prj-123" || h.herdr.agentKind != "claude" || h.herdr.paneID != "w1:p1" {
		t.Fatalf("agent = %s %s pane %s", h.herdr.agentName, h.herdr.agentKind, h.herdr.paneID)
	}
	if h.herdr.tabLabel != "PRJ-123" {
		t.Fatalf("tab label = %q", h.herdr.tabLabel)
	}
	if h.herdr.startTimeout != 60*time.Second || h.herdr.promptTimeout != 30*time.Second {
		t.Fatalf("timeouts = %s %s", h.herdr.startTimeout, h.herdr.promptTimeout)
	}
	if !slices.Equal(h.herdr.promptUntil, []string{herdr.AgentStatusWorking}) {
		t.Fatalf("until = %#v", h.herdr.promptUntil)
	}
	wantPrompt := "PRJ-123 Ship it https://example.test/PRJ-123 acme main PRJ-9\nsecond line"
	if h.herdr.promptText != wantPrompt {
		t.Fatalf("prompt = %q", h.herdr.promptText)
	}
	got := assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	if !got.ClaimedAt.Equal(fixedNow) || got.Machine != "laptop" || got.Detail != "" {
		t.Fatalf("claim = %+v", got)
	}
	if got.StashCount == nil || *got.StashCount != 0 {
		t.Fatalf("stash count = %v", got.StashCount)
	}
	assertRun(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
}

func TestApplyRecordsStashCount(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Stash = func(context.Context, string) (int, error) { return 3, nil }
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	got := assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	if got.StashCount == nil || *got.StashCount != 3 {
		t.Fatalf("stash count = %v", got.StashCount)
	}
}

func TestApplyRequiresStashCount(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Stash = nil
	err := h.start(h.snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "expected a stash count function, found nil") {
		t.Fatalf("error = %v", err)
	}
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestCreateWorkspace(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.herdr.found = false
	h.herdr.createWS = herdr.Workspace{ID: "w9", Label: "factory", AgentStatus: herdr.AgentStatusIdle}
	h.herdr.createPane = herdr.Pane{ID: "w9:p1", TabID: "w9:t1", WorkspaceID: "w9"}
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, []string{"Tabs", "Get", "Claim", "Get", "FindWorkspace", "CreateWorkspace", "StartAgent", "WaitAgent", "Prompt"})
	if h.herdr.paneID != "w9:p1" || h.herdr.cwd != "~/src/monorepo" {
		t.Fatalf("pane %s cwd %s", h.herdr.paneID, h.herdr.cwd)
	}
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w9:t1", "w9")
}

func TestLostRace(t *testing.T) {
	t.Parallel()
	t.Run("assignee", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.src.inner.Account = "other"
		err := h.start(h.snap, okGate, applyOpt)
		assertLost(t, h, err)
	})
	t.Run("status", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.ClaimStatus = "Doing"
		err := h.start(h.snap, okGate, applyOpt)
		assertLost(t, h, err)
	})
}

func TestGitHubHappyPath(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.useGitHub(true)
	if err := h.start(h.snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, []string{"Tabs", "Workspaces", "Get", "Claim", "Get", "FindWorkspace", "CreateTab", "StartAgent", "WaitAgent", "Prompt"})
	if !strings.Contains(h.out.String(), "acme/widgets#12 → in-progress (assign to me)") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
	if !strings.Contains(h.out.String(), "prompt   Fix acme/widgets#12…") {
		t.Fatalf("plan =\n%s", h.out.String())
	}
	if strings.Contains(h.out.String(), "jira:acme") || strings.Contains(h.herdr.promptText, "github:experimental") {
		t.Fatalf("prompt used the store key: %q\n%s", h.herdr.promptText, h.out.String())
	}
	if h.herdr.agentName != "widgets-12" || h.herdr.tabLabel != "#12" {
		t.Fatalf("agent %q tab %q", h.herdr.agentName, h.herdr.tabLabel)
	}
	assertStarted(t, h.st, "github:experimental", "acme/widgets#12", "tg", "wg")
}

func TestAgentName(t *testing.T) {
	t.Parallel()
	herdrName := regexp.MustCompile(`^[a-z][a-z0-9_-]{0,31}$`)
	derived := []struct {
		name  string
		label string
		kind  string
		repo  string
		want  string
	}{
		{name: "jira key", label: "AP-1110", kind: "jira", want: "ap-1110"},
		{name: "github issue", label: "#12", kind: "github", repo: "acme/foo", want: "foo-12"},
	}
	for _, tc := range derived {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := agentName(tc.label, tc.kind, tc.repo)
			if err != nil {
				t.Fatal(err)
			}
			if got != tc.want {
				t.Fatalf("agentName(%q) = %q, want %q", tc.label, got, tc.want)
			}
			if !herdrName.MatchString(got) {
				t.Fatalf("agentName(%q) = %q, herdr would reject it", tc.label, got)
			}
		})
	}

	t.Run("start jira", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		const id = "AP-1110"
		it := jiraItem()
		it.ID = id
		h.id = id
		h.snap.Items = map[string]source.Item{id: it}
		h.src.inner.Items = map[string]source.Item{id: it}
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if h.herdr.agentName != "ap-1110" || !herdrName.MatchString(h.herdr.agentName) {
			t.Fatalf("agent = %q", h.herdr.agentName)
		}
		if h.herdr.tabLabel != "AP-1110" {
			t.Fatalf("tab label = %q", h.herdr.tabLabel)
		}
	})

	t.Run("start github", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(true)
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if h.herdr.agentName != "widgets-12" || !herdrName.MatchString(h.herdr.agentName) {
			t.Fatalf("agent = %q", h.herdr.agentName)
		}
		if h.herdr.tabLabel != "#12" {
			t.Fatalf("tab label = %q", h.herdr.tabLabel)
		}
	})

	t.Run("empty", func(t *testing.T) {
		t.Parallel()
		err := checkAgentName("AP-1110", "")
		if err == nil || !strings.Contains(err.Error(), "found empty") {
			t.Fatalf("error = %v", err)
		}
	})

	t.Run("invalid before claim", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(true)
		h.starter.Cfg.Repo = "acme/Foo.Bar"
		err := h.start(h.snap, okGate, applyOpt)
		if err == nil || !strings.Contains(err.Error(), `found "Foo.Bar-12"`) {
			t.Fatalf("error = %v", err)
		}
		if got, want := h.out.String(), "factory start acme/widgets#12: refused, nothing changed: start acme/widgets#12: expected an agent name of 1-32 characters starting with a lowercase letter and containing only [a-z0-9_-], found \"Foo.Bar-12\"\n"; got != want {
			t.Fatalf("output = %q, want %q", got, want)
		}
		assertCalls(t, h.log, nil)
		assertNoClaim(t, h.st, "github:experimental", "acme/widgets#12")
	})
}

func TestGitHubLostRace(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.useGitHub(false)
	err := h.start(h.snap, okGate, applyOpt)
	assertLost(t, h, err)
}

func TestClaimAssignFalse(t *testing.T) {
	t.Parallel()
	t.Run("plan omits assign", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.ClaimAssign = false
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if strings.Contains(h.out.String(), "(assign to me)") {
			t.Fatalf("plan =\n%s", h.out.String())
		}
		if !strings.Contains(h.out.String(), "claim    PRJ-123 → In Progress\n") {
			t.Fatalf("plan =\n%s", h.out.String())
		}
		assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	})
	t.Run("confirms in-progress without requiring assignee", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.ClaimAssign = false
		h.src.inner.Account = "other"
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatalf("error = %v, want nil when assign=false and status matches", err)
		}
		assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
	})
	t.Run("still detects lost race on missing in-progress status", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.ClaimAssign = false
		h.starter.Cfg.ClaimStatus = "Doing"
		err := h.start(h.snap, okGate, applyOpt)
		assertLost(t, h, err)
	})
}

func TestStartAgentFailure(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	cause := errors.New("agent down")
	h.herdr.startErr = cause
	err := h.start(h.snap, okGate, applyOpt)
	// The agent never ran: the tab is closed and the claim released, so a
	// plain re-run works.
	if !errors.Is(err, cause) || !strings.Contains(err.Error(), "closed tab w1:t1 and released PRJ-123; re-run: factory start PRJ-123 --apply") {
		t.Fatalf("error = %v", err)
	}
	assertCalls(t, h.log, []string{"Tabs", "Get", "Claim", "Get", "FindWorkspace", "CreateTab", "StartAgent", "CloseTab", "Release"})
	got, gerr := h.st.LatestClaim(context.Background(), "jira:acme", "PRJ-123")
	if gerr != nil || got.State != store.Released || !strings.Contains(got.Detail, "step 5: agent down") {
		t.Fatalf("claim = %+v, err = %v", got, gerr)
	}
	assertNoRun(t, h.st)
}

func TestEmptyPane(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.herdr.pane = herdr.Pane{}
	err := h.start(h.snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "expected workspace, pane, and tab ids") {
		t.Fatalf("error = %v", err)
	}
	assertFailedStep(t, h, err, nil, 4)
	if slices.Contains(*h.log, "StartAgent") {
		t.Fatalf("calls = %v", *h.log)
	}
}

func TestAmbiguousWorkspace(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.herdr.findErr = errors.Join(errors.New("workspace \"factory\" matched w1, w2"), herdr.ErrAmbiguous)
	err := h.start(h.snap, okGate, applyOpt)
	if !errors.Is(err, herdr.ErrAmbiguous) {
		t.Fatalf("error = %v", err)
	}
	assertFailedStep(t, h, err, herdr.ErrAmbiguous, 3)
	assertCalls(t, h.log, []string{"Tabs", "Get", "Claim", "Get", "FindWorkspace"})
}

func TestExistingTab(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	snap := h.snap
	snap.Tabs = []herdr.Tab{{
		ID: "t1", WorkspaceID: "w1", Label: "PRJ-123", AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	err := h.start(snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "tab") {
		t.Fatalf("error = %v", err)
	}
	wantOut := "factory start PRJ-123: refused, nothing changed: start PRJ-123: not ready: tab: tab PRJ-123\n"
	if h.out.String() != wantOut {
		t.Fatalf("output = %q, want %q", h.out.String(), wantOut)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
	assertNoRelease(t, h.src)
}

func TestForceExistingTab(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.herdr.tabs = []herdr.Tab{{
		ID: "t1", WorkspaceID: "w1", Label: "PRJ-123", AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	err := h.start(h.snap, okGate, forceOpt)
	if err == nil || !strings.Contains(err.Error(), `tab "PRJ-123" already exists`) {
		t.Fatalf("error = %v", err)
	}
	wantOut := forceWarning + wantPlan + "factory start PRJ-123: refused, nothing changed: tab \"PRJ-123\" already exists\n"
	if h.out.String() != wantOut {
		t.Fatalf("output =\n%s\nwant =\n%s", h.out.String(), wantOut)
	}
	assertCalls(t, h.log, []string{"Tabs"})
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestForceDryRunExistingTab(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	snap := h.snap
	snap.Tabs = []herdr.Tab{{
		ID: "t1", WorkspaceID: "w1", Label: "PRJ-123", AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	if err := h.start(snap, okGate, Options{Force: true}); err != nil {
		t.Fatal(err)
	}
	if h.out.String() != forceWarningNamed("tab PRJ-123")+wantPlan {
		t.Fatalf("output =\n%s", h.out.String())
	}
	assertCalls(t, h.log, nil)
}

func TestGateRefused(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	res := gate.Result{OK: false, Reasons: []string{"agent PRJ-9 (w1:t9) is working"}}
	err := h.start(h.snap, res, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "agent PRJ-9 (w1:t9) is working") {
		t.Fatalf("error = %v", err)
	}
	wantOut := "factory start PRJ-123: refused, nothing changed: start PRJ-123: gate: agent PRJ-9 (w1:t9) is working\n"
	if h.out.String() != wantOut {
		t.Fatalf("output = %q, want %q", h.out.String(), wantOut)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestDryRunGateRefusedPrintsBlockedPlan(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	res := gate.Result{OK: false, Reasons: []string{"agent PRJ-9 (w1:t9) is working"}}
	err := h.start(h.snap, res, Options{})
	if err == nil || !strings.Contains(err.Error(), "agent PRJ-9 (w1:t9) is working") {
		t.Fatalf("error = %v", err)
	}
	wantOut := "blocked  yes\n" + wantPlan + "factory start PRJ-123: refused, nothing changed: start PRJ-123: gate: agent PRJ-9 (w1:t9) is working\n"
	if h.out.String() != wantOut {
		t.Fatalf("output =\n%s\nwant =\n%s", h.out.String(), wantOut)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestDryRunNotReadyDoesNotPrintPlan(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	it := jiraItem()
	it.Done = true
	snap := h.snap
	snap.Items = map[string]source.Item{it.ID: it}
	err := h.start(snap, okGate, Options{})
	if err == nil || !strings.Contains(err.Error(), "not ready") {
		t.Fatalf("error = %v", err)
	}
	wantOut := "factory start PRJ-123: refused, nothing changed: start PRJ-123: not ready: done: done\n"
	if h.out.String() != wantOut {
		t.Fatalf("output = %q, want %q", h.out.String(), wantOut)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestDryRunBadGateShapeDoesNotPrintPlan(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	err := h.start(h.snap, gate.Result{OK: true, Reasons: []string{"nope"}}, Options{})
	if err == nil || !strings.Contains(err.Error(), "expected OK or reasons") {
		t.Fatalf("error = %v", err)
	}
	wantOut := "factory start PRJ-123: refused, nothing changed: start PRJ-123: gate: expected OK or reasons, found ok=true reasons=1\n"
	if h.out.String() != wantOut {
		t.Fatalf("output = %q, want %q", h.out.String(), wantOut)
	}
	assertCalls(t, h.log, nil)
}

func TestGateShape(t *testing.T) {
	t.Parallel()
	for _, res := range []gate.Result{{OK: true, Reasons: []string{"nope"}}, {OK: false}} {
		h := newHarness(t)
		err := h.start(h.snap, res, applyOpt)
		if err == nil || !strings.Contains(err.Error(), "expected OK or reasons") {
			t.Fatalf("gate %+v error = %v", res, err)
		}
		assertCalls(t, h.log, nil)
	}
}

func TestForceProceeds(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	res := gate.Result{OK: false, Reasons: []string{"agent PRJ-9 (w1:t9) is working"}}
	if err := h.start(h.snap, res, forceOpt); err != nil {
		t.Fatal(err)
	}
	if h.out.String() != forceWarning+wantPlan+"factory start PRJ-123: started (tab w1:t1, claim 2026-11-01T12:00:00Z)\n" {
		t.Fatalf("output =\n%s", h.out.String())
	}
	assertCalls(t, h.log, happyCalls)
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
}

func TestLiveNotReady(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	live := jiraItem()
	live.Done = true
	h.src.inner.Items[live.ID] = live
	err := h.start(h.snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "done") {
		t.Fatalf("error = %v", err)
	}
	if h.out.String() != wantPlan+"factory start PRJ-123: refused, nothing changed: start PRJ-123: not ready: done: done\n" {
		t.Fatalf("output =\n%s", h.out.String())
	}
	assertCalls(t, h.log, []string{"Tabs", "Get"})
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
	assertNoRelease(t, h.src)
}

func TestForceRefusesStaleDone(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	stale := jiraItem()
	stale.Done = true
	snap := h.snap
	snap.Items = map[string]source.Item{stale.ID: stale}
	for _, opts := range []Options{forceOpt, {Apply: true, Force: true, ForceTakeOwnership: true}} {
		err := h.start(snap, okGate, opts)
		if err == nil || !strings.Contains(err.Error(), "done") {
			t.Fatalf("opts=%+v error = %v, want done refusal", opts, err)
		}
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestStartClaimStatusRespectsInProgressRule(t *testing.T) {
	t.Parallel()
	t.Run("blocks by default even with --force", func(t *testing.T) {
		t.Parallel()
		for _, force := range []bool{false, true} {
			h := newHarness(t)
			h.starter.Cfg.ReadyStatuses = []string{"To Do", "Backlog"}
			it := jiraItem()
			it.State = "In Progress"
			h.snap.Items[it.ID] = it
			h.src.inner.Items[it.ID] = it
			opts := applyOpt
			if force {
				opts = forceOpt
			}
			err := h.start(h.snap, okGate, opts)
			if err == nil {
				t.Fatalf("force=%t: expected refusal for already in-progress item", force)
			}
			assertCalls(t, h.log, nil)
		}
	})
	t.Run("allowed when in_progress_rule is allow", func(t *testing.T) {
		t.Parallel()
		for _, force := range []bool{false, true} {
			h := newHarness(t)
			h.starter.Cfg.ReadyStatuses = []string{"To Do", "Backlog"}
			h.starter.Cfg.Claim = config.Claim{
				RequireLeaf:       true,
				RequireUnblocked:  true,
				SourceContainment: true,
				Assign:            true,
				OwnershipRule:     config.OwnershipUnassignedOrMe,
				MarkInProgress:    true,
				InProgressRule:    config.InProgressAllow,
				Status:            "In Progress",
			}
			it := jiraItem()
			it.State = "In Progress"
			h.snap.Items[it.ID] = it
			h.src.inner.Items[it.ID] = it
			opts := applyOpt
			if force {
				opts = forceOpt
			}
			if err := h.start(h.snap, okGate, opts); err != nil {
				t.Fatalf("force=%t: %v", force, err)
			}
			assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
		}
	})
}

func TestStartClaimStatusKeepsOtherRules(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.ReadyStatuses = []string{"To Do"}
	it := jiraItem()
	it.State = "In Progress"
	it.Assignee = "ada"
	it.Done = true
	h.snap.Items[it.ID] = it
	err := h.start(h.snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "done") {
		t.Fatalf("error = %v", err)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestStartRejectsOtherStatus(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.ReadyStatuses = []string{"To Do"}
	it := jiraItem()
	it.State = "Review"
	h.snap.Items[it.ID] = it
	h.src.inner.Items[it.ID] = it
	err := h.start(h.snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), `status "Review" not in ready_statuses`) {
		t.Fatalf("error = %v", err)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestLiveStatusNotReady(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.ReadyStatuses = []string{"To Do"}
	live := jiraItem()
	live.State = "Review"
	h.src.inner.Items[live.ID] = live
	err := h.start(h.snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), `status "Review" not in ready_statuses`) {
		t.Fatalf("error = %v", err)
	}
	assertCalls(t, h.log, []string{"Tabs", "Get"})
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
}

func TestForceSkipsLiveStatus(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.starter.Cfg.ReadyStatuses = []string{"To Do"}
	it := jiraItem()
	it.State = "Review"
	h.snap.Items[it.ID] = it
	h.src.inner.Items[it.ID] = it
	if err := h.start(h.snap, okGate, forceOpt); err != nil {
		t.Fatal(err)
	}
	assertCalls(t, h.log, happyCalls)
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
}

func TestForceRefusesLiveDone(t *testing.T) {
	t.Parallel()
	for _, opts := range []Options{forceOpt, {Apply: true, Force: true, ForceTakeOwnership: true}} {
		h := newHarness(t)
		live := jiraItem()
		live.Done = true
		h.src.inner.Items[live.ID] = live
		err := h.start(h.snap, okGate, opts)
		if err == nil || !strings.Contains(err.Error(), "done") {
			t.Fatalf("opts=%+v error = %v, want done refusal", opts, err)
		}
		assertCalls(t, h.log, []string{"Tabs", "Get"})
		assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
	}
}

func TestForceSkipsDoneParent(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	snap := withDoneParent(h)
	err := h.start(snap, okGate, applyOpt)
	if err == nil || !strings.Contains(err.Error(), "parent PRJ-9 is done") {
		t.Fatalf("error = %v", err)
	}
	wantOut := "factory start PRJ-123: refused, nothing changed: start PRJ-123: not ready: parent: parent PRJ-9 is done\n"
	if h.out.String() != wantOut {
		t.Fatalf("output = %q, want %q", h.out.String(), wantOut)
	}
	assertCalls(t, h.log, nil)
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")

	h = newHarness(t)
	snap = withDoneParent(h)
	if err := h.start(snap, okGate, forceOpt); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(h.out.String(), forceWarningNamed("parent PRJ-9 is done")) {
		t.Fatalf("output =\n%s", h.out.String())
	}
	assertCalls(t, h.log, happyCalls)
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
}

func TestForceRespectsRequireUnblocked(t *testing.T) {
	t.Parallel()
	// With default require_unblocked = true, both normal and --force refuse.
	for _, opts := range []Options{applyOpt, forceOpt} {
		h := newHarness(t)
		snap := withOpenBlocker(h)
		err := h.start(snap, okGate, opts)
		if err == nil || !strings.Contains(err.Error(), "blocker PRJ-8 is not done") {
			t.Fatalf("opts=%+v error = %v", opts, err)
		}
		assertCalls(t, h.log, nil)
		assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
	}

	// With require_unblocked = false, start proceeds.
	h := newHarness(t)
	h.starter.Cfg.Claim = config.Claim{
		RequireLeaf:       true,
		RequireUnblocked:  false,
		SourceContainment: true,
		Assign:            true,
		OwnershipRule:     config.OwnershipUnassignedOrMe,
		MarkInProgress:    true,
		InProgressRule:    config.InProgressBlock,
		Status:            "In Progress",
	}
	snap := withOpenBlocker(h)
	if err := h.start(snap, okGate, applyOpt); err != nil {
		t.Fatal(err)
	}
	assertStarted(t, h.st, "jira:acme", "PRJ-123", "w1:t1", "w1")
}

func TestForceRefusesOtherAssignee(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	live := jiraItem()
	live.Assignee = "other"
	h.src.inner.Items[live.ID] = live
	err := h.start(h.snap, okGate, forceOpt)
	if err == nil || !strings.Contains(err.Error(), `expected assignee empty or "ada", found "other"`) {
		t.Fatalf("error = %v", err)
	}
	assertCalls(t, h.log, []string{"Tabs", "Get"})
	assertNoClaim(t, h.st, "jira:acme", "PRJ-123")
	assertNoRelease(t, h.src)
}

func TestForceDryRunAndApplyNameTheSameRules(t *testing.T) {
	t.Parallel()
	dry := newHarness(t)
	snap := withDoneParent(dry)
	if err := dry.start(snap, okGate, Options{Force: true}); err != nil {
		t.Fatal(err)
	}
	apply := newHarness(t)
	snap = withDoneParent(apply)
	if err := apply.start(snap, okGate, forceOpt); err != nil {
		t.Fatal(err)
	}
	dryLine, _, _ := strings.Cut(dry.out.String(), "\n")
	applyLine, _, _ := strings.Cut(apply.out.String(), "\n")
	want := strings.TrimSuffix(forceWarningNamed("parent PRJ-9 is done"), "\n")
	if dryLine != want || applyLine != want {
		t.Fatalf("dry = %q\napply = %q", dryLine, applyLine)
	}
	assertCalls(t, dry.log, nil)
	assertCalls(t, apply.log, happyCalls)
}

func TestRejectsBadInput(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	var nilCtx context.Context
	if err := h.starter.Start(nilCtx, "PRJ-123", h.snap, okGate, Options{}); err == nil || !strings.Contains(err.Error(), "context") {
		t.Fatalf("nil ctx = %v", err)
	}
	if err := h.starter.Start(context.Background(), "", h.snap, okGate, Options{}); err == nil || !strings.Contains(err.Error(), "empty") {
		t.Fatalf("empty id = %v", err)
	}
	snap := h.snap
	snap.Me = ""
	if err := h.start(snap, okGate, Options{}); err == nil || !strings.Contains(err.Error(), "me") {
		t.Fatalf("empty me = %v", err)
	}
	h.starter.Kind = "github"
	h.starter.Cfg.Kind = "github"
	h.starter.Cfg.Repo = "acme/widgets"
	h.starter.Cfg.ClaimLabel = "in-progress"
	bad := h.snap
	if err := h.start(bad, okGate, Options{}); err == nil || !strings.Contains(err.Error(), "owner/repo#") {
		t.Fatalf("bad github id = %v", err)
	}
	assertCalls(t, h.log, nil)
}

type harness struct {
	st      *store.Store
	src     *loggedSource
	herdr   *fakeHerdr
	log     *[]string
	out     *bytes.Buffer
	starter *Starter
	snap    ready.Snapshot
	id      string
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	log := []string{}
	it := jiraItem()
	inner := &fake.Source{
		SourceName: "jira:acme",
		Account:    "ada",
		Items:      map[string]source.Item{it.ID: it},
	}
	src := &loggedSource{inner: inner, log: &log}
	api := &fakeHerdr{
		log:   &log,
		found: true,
		ws:    herdr.Workspace{ID: "w1", Label: "factory", AgentStatus: herdr.AgentStatusIdle},
		tab:   herdr.Tab{ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123", AgentStatus: herdr.AgentStatusIdle, PaneCount: 1},
		pane:  herdr.Pane{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1"},
	}
	out := &bytes.Buffer{}
	st := openTestStore(t)
	s := &Starter{
		Src:      src,
		Cfg:      jiraCfg(),
		Kind:     "jira",
		Defaults: config.Defaults{AgentKind: "claude"},
		Store:    st,
		Herdr:    api,
		Hostname: "laptop",
		Now:      func() time.Time { return fixedNow },
		Out:      out,
		Stash:    func(context.Context, string) (int, error) { return 0, nil },
		Sleep:    func(context.Context, time.Duration) error { return nil },
	}
	return &harness{
		st: st, src: src, herdr: api, log: &log, out: out, starter: s,
		snap: ready.Snapshot{Items: map[string]source.Item{it.ID: it}, Me: "ada"},
		id:   it.ID,
	}
}

func (h *harness) start(snap ready.Snapshot, res gate.Result, opts Options) error {
	return h.starter.Start(context.Background(), h.id, snap, res, opts)
}

func (h *harness) useGitHub(withLabel bool) {
	const id = "acme/widgets#12"
	it := source.Item{
		ID: id, Title: "Ship it", URL: "https://github.com/acme/widgets/issues/12",
		State: "open",
	}
	h.id = id
	h.snap = ready.Snapshot{Items: map[string]source.Item{id: it}, Me: "ada"}
	h.src.inner.SourceName = "github:experimental"
	h.src.inner.Items = map[string]source.Item{id: it}
	if withLabel {
		h.src.inner.ClaimLabel = "in-progress"
	} else {
		h.src.inner.ClaimLabel = ""
	}
	h.starter.Kind = "github"
	h.starter.Cfg = config.Source{
		Name: "experimental", Kind: "github", Cwd: "/repo", Base: "main",
		Repo: "acme/widgets", ClaimLabel: "in-progress", StartPrompt: "Fix {id}",
		AssigneeOK: "me", PRTitlePattern: `\[{id}\]`,
	}
	h.herdr.ws = herdr.Workspace{ID: "wg", Label: "widgets", AgentStatus: herdr.AgentStatusIdle}
	h.herdr.tab = herdr.Tab{ID: "tg", WorkspaceID: "wg", Label: "#12", AgentStatus: herdr.AgentStatusIdle, PaneCount: 1}
	h.herdr.pane = herdr.Pane{ID: "pg", TabID: "tg", WorkspaceID: "wg"}
	h.herdr.found = true
}

func jiraItem() source.Item {
	return source.Item{
		ID: "PRJ-123", Title: "Ship it", URL: "https://example.test/PRJ-123",
		State: "To Do", Parent: "PRJ-9",
	}
}

func jiraCfg() config.Source {
	return config.Source{
		Name: "acme", Kind: "jira", Cwd: "~/src/monorepo", Base: "main",
		StartPrompt: "{id} {title} {url} {source} {base} {parent}\nsecond line",
		ClaimStatus: "In Progress", ClaimAssign: true, AssigneeOK: "me",
		PRTitlePattern: `\[{id}\]`,
	}
}

func openTestStore(t *testing.T) *store.Store {
	t.Helper()
	st, err := store.Open(context.Background(), filepath.Join(t.TempDir(), "factory.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("close store: %v", err)
		}
	})
	return st
}

func assertCalls(t *testing.T, log *[]string, want []string) {
	t.Helper()
	if !slices.Equal(*log, want) {
		t.Fatalf("calls = %v, want %v", *log, want)
	}
}

func assertNoRelease(t *testing.T, src *loggedSource) {
	t.Helper()
	for _, call := range src.inner.Calls {
		if call.Method == "Release" {
			t.Fatalf("Release was called: %#v", src.inner.Calls)
		}
	}
}

func assertNoClaim(t *testing.T, st *store.Store, sourceName, id string) {
	t.Helper()
	_, err := st.LatestClaim(context.Background(), sourceName, id)
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("LatestClaim() error = %v, want ErrNotFound", err)
	}
}

func assertLost(t *testing.T, h *harness, err error) {
	t.Helper()
	if !errors.Is(err, ErrLostRace) {
		t.Fatalf("error = %v, want ErrLostRace", err)
	}
	if strings.Contains(err.Error(), "item is claimed") {
		t.Fatalf("lost race told the user to release: %v", err)
	}
	wantCalls := []string{"Tabs", "Get", "Claim", "Get"}
	if h.starter.Kind == "github" {
		wantCalls = []string{"Tabs", "Workspaces", "Get", "Claim", "Get"}
	}
	assertCalls(t, h.log, wantCalls)
	assertNoRelease(t, h.src)
	assertNoClaim(t, h.st, h.src.inner.SourceName, h.id)
}

func assertStarted(t *testing.T, st *store.Store, sourceName, id, tabID, wsID string) store.Claim {
	t.Helper()
	got, err := st.LatestClaim(context.Background(), sourceName, id)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != store.Started || got.TabID != tabID || got.WorkspaceID != wsID {
		t.Fatalf("claim = %+v", got)
	}
	return got
}

func assertFailedStep(t *testing.T, h *harness, err, cause error, step int) {
	t.Helper()
	if err == nil {
		t.Fatal("error = nil")
	}
	if !strings.Contains(err.Error(), "item is claimed") || !strings.Contains(err.Error(), "factory release "+h.id) {
		t.Fatalf("error = %v", err)
	}
	if cause != nil && !errors.Is(err, cause) {
		t.Fatalf("error = %v, want %v", err, cause)
	}
	got, gerr := h.st.LatestClaim(context.Background(), h.src.inner.SourceName, h.id)
	if gerr != nil {
		t.Fatal(gerr)
	}
	if got.State != store.Failed || !strings.Contains(got.Detail, "step "+strconv.Itoa(step)) {
		t.Fatalf("claim = %+v", got)
	}
	assertNoRelease(t, h.src)
}

func assertRun(t *testing.T, st *store.Store, sourceName, id, tabID, wsID string) {
	t.Helper()
	runs, err := st.Runs(context.Background(), 1)
	if err != nil {
		t.Fatal(err)
	}
	if len(runs) != 1 || runs[0].Command != "start" || runs[0].Outcome != "ok" {
		t.Fatalf("runs = %+v", runs)
	}
	if strings.ContainsAny(runs[0].DetailJSON, "\r\n") {
		t.Fatalf("detail = %q", runs[0].DetailJSON)
	}
	var detail struct {
		Summary     string `json:"summary"`
		Source      string `json:"source"`
		ID          string `json:"id"`
		TabID       string `json:"tab_id"`       //nolint:tagliatelle // documented factory JSON output field
		WorkspaceID string `json:"workspace_id"` //nolint:tagliatelle // documented factory JSON output field
	}
	if err := json.Unmarshal([]byte(runs[0].DetailJSON), &detail); err != nil {
		t.Fatal(err)
	}
	if detail.Summary != "started "+id || detail.Source != sourceName || detail.ID != id ||
		detail.TabID != tabID || detail.WorkspaceID != wsID {
		t.Fatalf("detail = %+v", detail)
	}
}

func assertNoRun(t *testing.T, st *store.Store) {
	t.Helper()
	runs, err := st.Runs(context.Background(), 1)
	if err != nil {
		t.Fatal(err)
	}
	if len(runs) != 0 {
		t.Fatalf("runs = %+v", runs)
	}
}

const forceWarning = "warning: --force skips the ready checks and the start gate\n"

func forceWarningNamed(rules string) string {
	return "warning: --force skips the ready checks (" + rules + ") and the start gate\n"
}

func withDoneParent(h *harness) ready.Snapshot {
	it := jiraItem()
	parent := source.Item{ID: "PRJ-9", Done: true}
	snap := h.snap
	snap.Items = map[string]source.Item{it.ID: it, parent.ID: parent}
	h.src.inner.Items[it.ID] = it
	return snap
}

func withOpenBlocker(h *harness) ready.Snapshot {
	it := jiraItem()
	it.BlockedBy = []source.BlockRef{{ID: "PRJ-8"}}
	blocker := source.Item{ID: "PRJ-8", State: "To Do"}
	snap := h.snap
	snap.Items = map[string]source.Item{it.ID: it, blocker.ID: blocker}
	h.src.inner.Items[it.ID] = it
	return snap
}

type loggedSource struct {
	inner *fake.Source
	log   *[]string
}

func (s *loggedSource) mark(name string) { *s.log = append(*s.log, name) }

func (s *loggedSource) Name() string { return s.inner.Name() }

func (s *loggedSource) Me(ctx context.Context) (string, error) {
	s.mark("Me")
	return s.inner.Me(ctx) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Sync(ctx context.Context, since time.Time) ([]source.Item, error) {
	s.mark("Sync")
	return s.inner.Sync(ctx, since) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Get(ctx context.Context, id string) (source.Item, error) {
	s.mark("Get")
	return s.inner.Get(ctx, id) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Claim(ctx context.Context, id string) error {
	s.mark("Claim")
	return s.inner.Claim(ctx, id) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) ClaimWithOptions(ctx context.Context, id string, opts source.ClaimOptions) error {
	s.mark("ClaimWithOptions")
	return s.inner.ClaimWithOptions(ctx, id, opts) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) PostClaimComment(ctx context.Context, id, body string) error {
	s.mark("PostClaimComment")
	return s.inner.PostClaimComment(ctx, id, body) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) LatestClaimComment(ctx context.Context, id string) (*source.ClaimComment, error) {
	s.mark("LatestClaimComment")
	return s.inner.LatestClaimComment(ctx, id) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Release(ctx context.Context, id string) error {
	s.mark("Release")
	return s.inner.Release(ctx, id) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Finish(ctx context.Context, id string) error {
	s.mark("Finish")
	return s.inner.Finish(ctx, id) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Close(ctx context.Context, id, comment string) error {
	s.mark("Close")
	return s.inner.Close(ctx, id, comment) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Link(ctx context.Context, blocker, blocked string) error {
	s.mark("Link")
	return s.inner.Link(ctx, blocker, blocked) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Unlink(ctx context.Context, blocker, blocked string) error {
	s.mark("Unlink")
	return s.inner.Unlink(ctx, blocker, blocked) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) Create(ctx context.Context, req source.CreateRequest) (string, error) {
	s.mark("Create")
	return s.inner.Create(ctx, req) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) IDs(ctx context.Context) ([]string, error) {
	s.mark("IDs")
	return s.inner.IDs(ctx) //nolint:wrapcheck // propagate context cancellation
}

func (s *loggedSource) SetPriority(ctx context.Context, id string, rank int) error {
	s.mark("SetPriority")
	return s.inner.SetPriority(ctx, id, rank) //nolint:wrapcheck // propagate context cancellation
}

type fakeHerdr struct {
	log           *[]string
	found         bool
	findErr       error
	ws            herdr.Workspace
	createWS      herdr.Workspace
	createPane    herdr.Pane
	createErr     error
	tab           herdr.Tab
	pane          herdr.Pane
	startErr      error
	agentName     string
	agentKind     string
	agentArgs     []string
	paneID        string
	startTimeout  time.Duration
	promptText    string
	promptUntil   []string
	promptTimeout time.Duration
	cwd           string
	tabLabel      string
	env           []string
	startErrs     []error           // Returned by StartAgent in order before startErr.
	promptErrs    []error           // Returned by Prompt in order, then nil.
	waits         []waitResult      // Returned by WaitAgent in order, then its first until state.
	screen        string            // ReadAgent output.
	keys          []string          // SendKeys calls.
	closedTabs    []string          // CloseTab calls.
	prompts       int               // Prompt calls.
	workspaces    []herdr.Workspace // Workspaces returns these.
	workspacesErr error
	tabs          []herdr.Tab // Tabs returns these.
	tabsErr       error
}

type waitResult struct {
	status string
	err    error
}

func (f *fakeHerdr) WaitAgent(ctx context.Context, _ string, until []string, _ time.Duration) (string, error) {
	if ctx == nil {
		return "", errors.New("herdr: expected a context, found nil")
	}
	f.mark("WaitAgent")
	if len(f.waits) > 0 {
		next := f.waits[0]
		f.waits = f.waits[1:]
		return next.status, next.err
	}
	return until[0], nil
}

func (f *fakeHerdr) ReadAgent(context.Context, string, int) (string, error) {
	f.mark("ReadAgent")
	return f.screen, nil
}

func (f *fakeHerdr) SendKeys(_ context.Context, _ string, keys ...string) error {
	f.mark("SendKeys")
	f.keys = append(f.keys, keys...)
	return nil
}

func (f *fakeHerdr) CloseTab(_ context.Context, tabID string) (string, error) {
	f.mark("CloseTab")
	f.closedTabs = append(f.closedTabs, tabID)
	return "ok", nil
}

func (f *fakeHerdr) mark(name string) { *f.log = append(*f.log, name) }

func (f *fakeHerdr) Workspaces(ctx context.Context) ([]herdr.Workspace, error) {
	if ctx == nil {
		return nil, errors.New("herdr: expected a context, found nil")
	}
	f.mark("Workspaces")
	if f.workspacesErr != nil {
		return nil, f.workspacesErr
	}
	return append([]herdr.Workspace(nil), f.workspaces...), nil
}

func (f *fakeHerdr) Tabs(ctx context.Context) ([]herdr.Tab, error) {
	if ctx == nil {
		return nil, errors.New("herdr: expected a context, found nil")
	}
	f.mark("Tabs")
	if f.tabsErr != nil {
		return nil, f.tabsErr
	}
	return append([]herdr.Tab(nil), f.tabs...), nil
}

func (f *fakeHerdr) FindWorkspace(ctx context.Context, label string) (herdr.Workspace, bool, error) {
	if ctx == nil {
		return herdr.Workspace{}, false, errors.New("herdr: expected a context, found nil")
	}
	f.mark("FindWorkspace")
	if f.findErr != nil {
		return herdr.Workspace{}, false, f.findErr
	}
	if f.found && f.ws.Label != label {
		return herdr.Workspace{}, false, errors.New("workspace: expected label " + label + ", found " + f.ws.Label)
	}
	return f.ws, f.found, nil
}

func (f *fakeHerdr) CreateWorkspace(ctx context.Context, label, cwd string, env []string) (herdr.Workspace, herdr.Pane, error) {
	if ctx == nil {
		return herdr.Workspace{}, herdr.Pane{}, errors.New("herdr: expected a context, found nil")
	}
	f.mark("CreateWorkspace")
	f.cwd = cwd
	f.env = append([]string(nil), env...)
	if f.createErr != nil {
		return herdr.Workspace{}, herdr.Pane{}, f.createErr
	}
	ws := f.createWS
	if ws.Label == "" {
		ws.Label = label
	}
	return ws, f.createPane, nil
}

func (f *fakeHerdr) CreateTab(ctx context.Context, workspaceID, label, cwd string, env []string) (herdr.Tab, herdr.Pane, error) {
	if ctx == nil {
		return herdr.Tab{}, herdr.Pane{}, errors.New("herdr: expected a context, found nil")
	}
	f.mark("CreateTab")
	f.cwd = cwd
	f.env = append([]string(nil), env...)
	f.tabLabel = label
	if workspaceID == "" || label == "" {
		return herdr.Tab{}, herdr.Pane{}, errors.New("tab: expected a workspace id and a label, found empty")
	}
	return f.tab, f.pane, nil
}

func (f *fakeHerdr) StartAgent(ctx context.Context, name, kind, paneID string, timeout time.Duration, agentArgs []string) error {
	if ctx == nil {
		return errors.New("herdr: expected a context, found nil")
	}
	f.mark("StartAgent")
	if len(f.startErrs) > 0 {
		err := f.startErrs[0]
		f.startErrs = f.startErrs[1:]
		return err
	}
	f.agentName = name
	f.agentKind = kind
	f.agentArgs = append([]string(nil), agentArgs...)
	f.paneID = paneID
	f.startTimeout = timeout
	return f.startErr
}

func (f *fakeHerdr) Prompt(ctx context.Context, target, text string, until []string, timeout time.Duration) error {
	if ctx == nil {
		return errors.New("herdr: expected a context, found nil")
	}
	f.mark("Prompt")
	f.prompts++
	if len(f.promptErrs) > 0 {
		err := f.promptErrs[0]
		f.promptErrs = f.promptErrs[1:]
		if err != nil {
			return err
		}
	}
	f.promptText = text
	f.promptUntil = append([]string(nil), until...)
	f.promptTimeout = timeout
	if target != f.paneID && f.paneID != "" {
		return errors.New("prompt: expected pane " + f.paneID + ", found " + target)
	}
	return nil
}
