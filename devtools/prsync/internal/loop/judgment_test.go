package loop

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"text/template"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/policy"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func strPtr(s string) *string { return &s }

func writeTestInboxFile(t *testing.T, dir, name string, v any) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	data, err := json.Marshal(v)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, name), data, 0o600); err != nil {
		t.Fatal(err)
	}
}

func hasReason(items []JudgmentItem, repo string, pr int, reason string) (JudgmentItem, bool) {
	for _, it := range items {
		if it.Repo == repo && it.PR == pr && it.Reason == reason {
			return it, true
		}
	}
	return JudgmentItem{}, false
}

func TestEvaluateNeedsJudgment_CIFailingUnclassified(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)

	t.Run("appears when failing with no inbox and no ledger", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "ci.config")
		got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
			Config: cfg,
			Now:    now,
			Scan: scan.Document{PRs: []scan.PR{{
				Repo:    "acme/widgets",
				Number:  10,
				State:   "open",
				HeadSHA: "head10",
				CIState: "failing",
				Tab:     &scan.Tab{TabID: "t1", AgentStatus: "idle"},
			}}},
		})
		if len(got) != 1 || got[0].Reason != ReasonCIFailingUnclassified || got[0].HeadSHA != "head10" {
			t.Fatalf("got = %+v, want single ci_failing_unclassified", got)
		}
	})

	t.Run("suppressed by pending inbox file for same head", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "ci-pending.config")
		cfg.CIFixInbox = filepath.Join(t.TempDir(), "inbox")
		writeTestInboxFile(t, cfg.CIFixInbox, "10.json", dispatch.InboxRequest{
			Repo: "acme/widgets", PR: 10, HeadSHA: "head10", Hint: "flake in TestFoo",
		})
		got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
			Config: cfg,
			Now:    now,
			Scan: scan.Document{PRs: []scan.PR{{
				Repo:    "acme/widgets",
				Number:  10,
				State:   "open",
				HeadSHA: "head10",
				CIState: "failing",
				Tab:     &scan.Tab{TabID: "t1", AgentStatus: "idle"},
			}}},
		})
		if len(got) != 0 {
			t.Fatalf("got = %+v, want empty when pending inbox matches head", got)
		}
	})

	t.Run("suppressed by handled done inbox file for same head", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "ci-done.config")
		cfg.CIFixInbox = filepath.Join(t.TempDir(), "inbox")
		writeTestInboxFile(t, filepath.Join(cfg.CIFixInbox, "done"), "10.json", dispatch.InboxRecord{
			Repo: "acme/widgets", PR: 10, HeadSHA: "head10", Hint: "fix", Outcome: dispatch.ActionDispatchedBlocked,
		})
		got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
			Config: cfg,
			Now:    now,
			Scan: scan.Document{PRs: []scan.PR{{
				Repo:    "acme/widgets",
				Number:  10,
				State:   "open",
				HeadSHA: "head10",
				CIState: "failing",
				Tab:     &scan.Tab{TabID: "t1", AgentStatus: "idle"},
			}}},
		})
		if len(got) != 0 {
			t.Fatalf("got = %+v, want empty when done inbox matches head", got)
		}
	})

	t.Run("suppressed by ci-fix ledger entry for same head", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "ci-ledger.config")
		lgr := &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")}
		if err := lgr.Append(ledger.Entry{
			PR: "acme/widgets#10", HeadSHA: "head10", Kind: dispatch.KindCIFix, Outcome: ledger.OutcomeDispatched, Time: now,
		}); err != nil {
			t.Fatal(err)
		}
		got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
			Config: cfg,
			Now:    now,
			Ledger: lgr,
			Scan: scan.Document{PRs: []scan.PR{{
				Repo:    "acme/widgets",
				Number:  10,
				State:   "open",
				HeadSHA: "head10",
				CIState: "failing",
				Tab:     &scan.Tab{TabID: "t1", AgentStatus: "idle"},
			}}},
		})
		if len(got) != 0 {
			t.Fatalf("got = %+v, want empty when ledger has ci-fix for head", got)
		}
	})

	t.Run("appears when inbox and ledger only cover older head or non-ci-fix kind", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "ci-old.config")
		cfg.CIFixInbox = filepath.Join(t.TempDir(), "inbox")
		writeTestInboxFile(t, cfg.CIFixInbox, "old.json", dispatch.InboxRequest{
			Repo: "acme/widgets", PR: 10, HeadSHA: "oldhead", Hint: "old",
		})
		writeTestInboxFile(t, filepath.Join(cfg.CIFixInbox, "stale"), "stale.json", dispatch.InboxRequest{
			Repo: "acme/widgets", PR: 10, HeadSHA: "head10", Hint: "stale",
		})
		lgr := &ledger.Store{Path: filepath.Join(t.TempDir(), "ledger.jsonl")}
		if err := lgr.Append(ledger.Entry{
			PR: "acme/widgets#10", HeadSHA: "head10", Kind: dispatch.KindRebase, Outcome: ledger.OutcomeDispatched, Time: now,
		}); err != nil {
			t.Fatal(err)
		}
		got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
			Config: cfg,
			Now:    now,
			Ledger: lgr,
			Scan: scan.Document{PRs: []scan.PR{{
				Repo:    "acme/widgets",
				Number:  10,
				State:   "open",
				HeadSHA: "head10",
				CIState: "failing",
				Tab:     &scan.Tab{TabID: "t1", AgentStatus: "idle"},
			}}},
		})
		if _, ok := hasReason(got, "acme/widgets", 10, ReasonCIFailingUnclassified); !ok {
			t.Fatalf("got = %+v, want ci_failing_unclassified for new head", got)
		}
	})

	t.Run("absent when CI is green", func(t *testing.T) {
		t.Parallel()
		cfg := testCfg(t, "ci-green.config")
		got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
			Config: cfg,
			Now:    now,
			Scan: scan.Document{PRs: []scan.PR{{
				Repo:    "acme/widgets",
				Number:  10,
				State:   "open",
				HeadSHA: "head10",
				CIState: "green",
				Tab:     &scan.Tab{TabID: "t1", AgentStatus: "idle"},
			}}},
		})
		if len(got) != 0 {
			t.Fatalf("got = %+v, want empty", got)
		}
	})
}

func TestEvaluateNeedsJudgment_NoTabAndAmbiguousTab(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)
	cfg := testCfg(t, "tabs.config")

	got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
		Config: cfg,
		Now:    now,
		Scan: scan.Document{
			Warnings: []string{
				"PROJ-2: ambiguous herdr tab label 'PROJ-2' matches w2:t1, w3:t2",
			},
			PRs: []scan.PR{
				{Repo: "acme/widgets", Number: 1, State: "open", HeadSHA: "h1", Identifier: strPtr("PROJ-1"), Tab: nil},
				{Repo: "acme/widgets", Number: 2, State: "open", HeadSHA: "h2", Identifier: strPtr("PROJ-2"), Tab: nil, TabAmbiguous: true},
				{Repo: "acme/widgets", Number: 3, State: "open", HeadSHA: "h3", Identifier: strPtr("PROJ-3"), Tab: nil, TabUnknown: scan.TabUnknownMachineUnreachable},
				{Repo: "acme/widgets", Number: 4, State: "open", HeadSHA: "h4", Identifier: strPtr("PROJ-4"), Tab: nil},
				{Repo: "acme/widgets", Number: 5, State: "open", HeadSHA: "h5", Identifier: strPtr("PROJ-5"), Tab: &scan.Tab{TabID: "w2:t5", AgentStatus: "idle"}},
				{Repo: "acme/widgets", Number: 6, State: "closed", HeadSHA: "h6", Identifier: strPtr("PROJ-6"), Tab: nil},
				{Repo: "acme/widgets", Number: 7, State: "open", HeadSHA: "h7", Identifier: strPtr("PROJ-7"), Tab: nil, OutOfScope: "tab on linux2"},
			},
		},
		Dispatch: dispatch.Document{
			Results: []dispatch.Item{
				{Repo: "acme/widgets", Number: 4, Action: dispatch.ActionOpenedTabAndDispatched, TabID: "w2:t4"},
			},
		},
	})

	if _, ok := hasReason(got, "acme/widgets", 1, ReasonNoTab); !ok {
		t.Fatalf("PR #1 missing no_tab: %+v", got)
	}
	amb, ok := hasReason(got, "acme/widgets", 2, ReasonAmbiguousTab)
	if !ok {
		t.Fatalf("PR #2 missing ambiguous_tab: %+v", got)
	}
	if !strings.Contains(amb.Detail, "w2:t1, w3:t2") {
		t.Fatalf("ambiguous_tab detail = %q, want tab ids", amb.Detail)
	}
	if _, ok := hasReason(got, "acme/widgets", 2, ReasonNoTab); ok {
		t.Fatalf("PR #2 must not emit no_tab when ambiguous_tab: %+v", got)
	}
	if len(got) != 2 {
		t.Fatalf("got = %+v, want exactly 2 entries (#1 no_tab, #2 ambiguous_tab)", got)
	}
}

func TestEvaluateNeedsJudgment_TabBlockedWithMinutes(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 12, 30, 0, 0, time.UTC)
	cfg := testCfg(t, "blocked.config")

	got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
		Config: cfg,
		Now:    now,
		BlockedSince: map[string]time.Time{
			"w2:t1": now.Add(-17 * time.Minute),
		},
		Scan: scan.Document{PRs: []scan.PR{
			{Repo: "acme/widgets", Number: 1, State: "open", HeadSHA: "h1", Tab: &scan.Tab{TabID: "w2:t1", AgentStatus: "blocked"}},
			{Repo: "acme/widgets", Number: 2, State: "open", HeadSHA: "h2", Tab: &scan.Tab{TabID: "w2:t2", AgentStatus: "blocked"}},
			{Repo: "acme/widgets", Number: 3, State: "open", HeadSHA: "h3", Tab: &scan.Tab{TabID: "w2:t3", AgentStatus: "idle"}},
			{Repo: "acme/widgets", Number: 4, State: "open", HeadSHA: "h4", Tab: &scan.Tab{TabID: "w2:t4", AgentStatus: "idle"}},
		}},
		Dispatch: dispatch.Document{Results: []dispatch.Item{
			{Repo: "acme/widgets", Number: 3, Action: dispatch.ActionDispatchedBlocked},
		}},
	})

	it1, ok := hasReason(got, "acme/widgets", 1, ReasonTabBlocked)
	if !ok || it1.Minutes == nil || *it1.Minutes != 17 {
		t.Fatalf("PR #1 tab_blocked = %+v (ok=%v), want minutes=17", it1, ok)
	}
	it2, ok := hasReason(got, "acme/widgets", 2, ReasonTabBlocked)
	if !ok || it2.Minutes == nil || *it2.Minutes != 0 {
		t.Fatalf("PR #2 tab_blocked = %+v (ok=%v), want minutes=0", it2, ok)
	}
	it3, ok := hasReason(got, "acme/widgets", 3, ReasonTabBlocked)
	if !ok || it3.Minutes == nil || *it3.Minutes != 0 {
		t.Fatalf("PR #3 tab_blocked (from dispatched_blocked) = %+v (ok=%v)", it3, ok)
	}
	if _, ok := hasReason(got, "acme/widgets", 4, ReasonTabBlocked); ok {
		t.Fatalf("PR #4 idle tab must not be tab_blocked: %+v", got)
	}
}

func TestEvaluateNeedsJudgment_LeaseHolderBlocked(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)
	cfg := testCfg(t, "lease.config")

	got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
		Config: cfg,
		Now:    now,
		Scan: scan.Document{PRs: []scan.PR{
			{Repo: "acme/widgets", Number: 1, State: "open", HeadSHA: "h1", Tab: &scan.Tab{TabID: "w2:t1", AgentStatus: "idle"}},
			{Repo: "acme/widgets", Number: 2, State: "open", HeadSHA: "h2", Tab: &scan.Tab{TabID: "w2:t2", AgentStatus: "idle"}},
			{Repo: "acme/widgets", Number: 3, State: "open", HeadSHA: "h3", Tab: &scan.Tab{TabID: "w2:t3", AgentStatus: "idle"}},
			{Repo: "acme/widgets", Number: 4, State: "open", HeadSHA: "h4", Tab: &scan.Tab{TabID: "w2:t4", AgentStatus: "idle"}},
			{Repo: "acme/widgets", Number: 5, State: "open", HeadSHA: "h5", Tab: &scan.Tab{TabID: "w2:t5", AgentStatus: "idle"}},
		}},
		Dispatch: dispatch.Document{Results: []dispatch.Item{
			{
				Repo:       "acme/widgets",
				Number:     1,
				Action:     dispatch.ActionGateTimeout,
				GateSignal: dispatch.SignalLease,
				Detail:     "waiting on checkout /repo — lease held by pm/foo until 14:20; not dispatching acme/widgets#1 until it clears",
			},
			{
				Repo:       "acme/widgets",
				Number:     2,
				Action:     dispatch.ActionGateTimeout,
				GateSignal: dispatch.SignalWorking,
				Detail:     "waiting on w9:t9 — working; not dispatching acme/widgets#2 until it clears",
			},
			{
				Repo:       "acme/widgets",
				Number:     3,
				Action:     dispatch.ActionGateTimeout,
				GateSignal: dispatch.SignalLease,
				Detail:     "waiting on checkout /repo — lease cannot read the lease: hub down; not dispatching acme/widgets#3 until it clears",
			},
			{
				Repo:       "acme/widgets",
				Number:     4,
				Action:     dispatch.ActionWouldDispatch,
				GateSignal: dispatch.SignalLease,
				Detail:     "gate currently busy: checkout /repo (lease paused by user)",
			},
		}},
		CIFixInbox: []dispatch.Item{
			{
				Repo:       "acme/widgets",
				Number:     5,
				Action:     dispatch.ActionGateTimeout,
				GateSignal: dispatch.SignalLease,
				Detail:     "waiting on checkout /repo — lease held by other-agent; not dispatching acme/widgets#5 until it clears",
			},
		},
	})

	if it, ok := hasReason(got, "acme/widgets", 1, ReasonLeaseHolderBlocked); !ok || it.HeadSHA != "h1" {
		t.Fatalf("PR #1 lease_holder_blocked = %+v (ok=%v)", it, ok)
	}
	if _, ok := hasReason(got, "acme/widgets", 2, ReasonLeaseHolderBlocked); ok {
		t.Fatalf("PR #2 working-tab gate timeout must not be lease_holder_blocked: %+v", got)
	}
	if _, ok := hasReason(got, "acme/widgets", 3, ReasonLeaseHolderBlocked); ok {
		t.Fatalf("PR #3 unreadable lease must not be lease_holder_blocked: %+v", got)
	}
	if _, ok := hasReason(got, "acme/widgets", 4, ReasonLeaseHolderBlocked); !ok {
		t.Fatalf("PR #4 dry-run lease pause missing lease_holder_blocked: %+v", got)
	}
	if _, ok := hasReason(got, "acme/widgets", 5, ReasonLeaseHolderBlocked); !ok {
		t.Fatalf("PR #5 ci_fix_inbox lease timeout missing lease_holder_blocked: %+v", got)
	}
}

func TestEvaluateNeedsJudgment_MergeReady(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)
	cfg := testCfg(t, "merge.config")

	tab := &scan.Tab{TabID: "w2:t1", AgentStatus: "idle"}
	got := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
		Config: cfg,
		Now:    now,
		Scan: scan.Document{PRs: []scan.PR{
			{Repo: "acme/widgets", Number: 1, State: "open", HeadSHA: "h1", Mergeable: "MERGEABLE", MergeStateStatus: "CLEAN", ReviewDecision: "APPROVED", CIState: "green", Tab: tab},
			{Repo: "acme/widgets", Number: 2, State: "open", HeadSHA: "h2", IsDraft: true, Mergeable: "MERGEABLE", ReviewDecision: "APPROVED", CIState: "green", Tab: tab},
			{Repo: "acme/widgets", Number: 3, State: "open", HeadSHA: "h3", Mergeable: "CONFLICTING", ReviewDecision: "APPROVED", CIState: "green", Tab: tab},
			{Repo: "acme/widgets", Number: 4, State: "open", HeadSHA: "h4", Mergeable: "MERGEABLE", ReviewDecision: "CHANGES_REQUESTED", CIState: "green", Tab: tab},
			{Repo: "acme/widgets", Number: 5, State: "open", HeadSHA: "h5", Mergeable: "MERGEABLE", ReviewDecision: "APPROVED", CIState: "pending", Tab: tab},
			{Repo: "acme/widgets", Number: 6, State: "open", HeadSHA: "h6", Unaddressed: true, Mergeable: "MERGEABLE", ReviewDecision: "APPROVED", CIState: "green", Tab: tab},
			{Repo: "acme/widgets", Number: 7, State: "open", HeadSHA: "h7", ReviewRequests: []string{"User:bob"}, Mergeable: "MERGEABLE", ReviewDecision: "APPROVED", CIState: "green", Tab: tab},
		}},
	})

	if len(got) != 1 || got[0].PR != 1 || got[0].Reason != ReasonMergeReady {
		t.Fatalf("got = %+v, want only PR #1 merge_ready", got)
	}
}

func TestEvaluateNeedsJudgment_RereviewDueWhenR5Off(t *testing.T) {
	t.Parallel()

	now := time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)

	scanDoc := scan.Document{PRs: []scan.PR{
		{Repo: "acme/widgets", Number: 1, State: "open", HeadSHA: "h1", Tab: &scan.Tab{TabID: "w2:t1", AgentStatus: "idle"}},
		{Repo: "acme/widgets", Number: 2, State: "open", HeadSHA: "h2", Tab: &scan.Tab{TabID: "w2:t2", AgentStatus: "idle"}},
	}}
	preview := []policy.Decision{
		{PR: "acme/widgets#1", Repo: "acme/widgets", Number: 1, HeadSHA: "h1", Kind: policy.KindBotRereview, Decision: policy.DecisionDryRun},
		{PR: "acme/widgets#2", Repo: "acme/widgets", Number: 2, HeadSHA: "h2", Kind: policy.KindBotRereview, Decision: policy.DecisionSkip, Reason: "unresolved_bot_threads"},
	}

	// When R5 is off, eligible PR #1 appears and skipped PR #2 does not.
	cfgOff := testCfg(t, "r5-off.config")
	cfgOff.Policy.BotRereview = config.PolicyOff
	gotOff := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
		Config:          cfgOff,
		Now:             now,
		Scan:            scanDoc,
		RereviewPreview: preview,
	})
	if len(gotOff) != 1 || gotOff[0].PR != 1 || gotOff[0].Reason != ReasonRereviewDue {
		t.Fatalf("R5 off got = %+v, want only PR #1 rereview_due", gotOff)
	}

	// When R5 is dry-run or auto, rereview_due must NOT appear.
	for _, mode := range []string{config.PolicyDryRun, config.PolicyAuto} {
		cfgOn := testCfg(t, "r5-"+mode+".config")
		cfgOn.Policy.BotRereview = mode
		gotOn := EvaluateNeedsJudgment(context.Background(), JudgmentInput{
			Config:          cfgOn,
			Now:             now,
			Scan:            scanDoc,
			BotRereview:     preview,
			RereviewPreview: preview,
		})
		if len(gotOn) != 0 {
			t.Fatalf("R5 %s got = %+v, want empty", mode, gotOn)
		}
	}
}

func TestRoundResultJSONAndFormatTemplate(t *testing.T) {
	t.Parallel()

	cfg := testCfg(t, "json.config")
	clk := &fakeClock{now: time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)}
	slp := &fakeSleeper{clock: clk}

	var stdout bytes.Buffer
	mins := 12
	runner := RoundFunc(func(_ context.Context, _ config.Config, round int) RoundResult {
		if round == 1 {
			return RoundResult{Scanned: 0, Dispatch: dispatch.Document{Results: []dispatch.Item{}}}
		}
		return RoundResult{
			Scanned:  2,
			Dispatch: dispatch.Document{Results: []dispatch.Item{}},
			NeedsJudgment: []JudgmentItem{
				{Repo: "acme/widgets", PR: 2, HeadSHA: "h2", Reason: ReasonTabBlocked, Minutes: &mins},
				{Repo: "acme/widgets", PR: 1, HeadSHA: "h1", Reason: ReasonMergeReady},
			},
		}
	})

	if err := Run(context.Background(), cfg, runner, Options{
		MaxRounds:         2,
		HeartbeatInterval: time.Hour,
		Clock:             clk,
		Sleeper:           slp,
		Stdout:            &stdout,
	}); err != nil {
		t.Fatalf("Run() error = %v", err)
	}

	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	if len(lines) != 2 {
		t.Fatalf("lines = %d, want 2:\n%s", len(lines), stdout.String())
	}

	// Round 1 must serialize needs_judgment: [] and needs_judgment_count: 0.
	if !strings.Contains(lines[0], `"needs_judgment":[]`) || !strings.Contains(lines[0], `"needs_judgment_count":0`) {
		t.Fatalf("round 1 JSON missing empty needs_judgment fields: %s", lines[0])
	}

	// Round 2 must serialize sorted needs_judgment and needs_judgment_count: 2.
	var r2 RoundResult
	if err := json.Unmarshal([]byte(lines[1]), &r2); err != nil {
		t.Fatalf("unmarshal round 2: %v", err)
	}
	if r2.NeedsJudgmentCount != 2 || len(r2.NeedsJudgment) != 2 {
		t.Fatalf("round 2 = %+v, want count=2", r2)
	}
	if r2.NeedsJudgment[0].PR != 1 || r2.NeedsJudgment[1].PR != 2 {
		t.Fatalf("round 2 needs_judgment not sorted: %+v", r2.NeedsJudgment)
	}

	tmpl, err := template.New("round").Parse("round={{.Round}} count={{.NeedsJudgmentCount}}{{range .NeedsJudgment}} [{{.Repo}}#{{.PR}}:{{.Reason}}{{if .Minutes}}({{.Minutes}}m){{end}}]{{end}}")
	if err != nil {
		t.Fatal(err)
	}
	var rendered bytes.Buffer
	if err := tmpl.Execute(&rendered, r2); err != nil {
		t.Fatal(err)
	}
	want := "round=2 count=2 [acme/widgets#1:merge_ready] [acme/widgets#2:tab_blocked(12m)]"
	if rendered.String() != want {
		t.Fatalf("template output = %q, want %q", rendered.String(), want)
	}
}

func TestBlockedTrackerPersistsAndClears(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	path := BlockedStatePath(dir, filepath.Join(dir, "test.config"))
	t0 := time.Date(2026, 11, 27, 12, 0, 0, 0, time.UTC)

	tr, err := LoadBlockedTracker(path)
	if err != nil {
		t.Fatalf("LoadBlockedTracker() error = %v", err)
	}
	doc1 := scan.Document{
		Author: "alice",
		PRs: []scan.PR{
			{Repo: "acme/widgets", Number: 1, State: "open", Tab: &scan.Tab{TabID: "w2:t1", AgentStatus: "blocked"}},
		},
	}
	tr.Update(doc1, RoundResult{}, t0)
	if err := tr.Save(); err != nil {
		t.Fatal(err)
	}

	// Reload in a fresh tracker 12 minutes later.
	tr2, err := LoadBlockedTracker(path)
	if err != nil {
		t.Fatal(err)
	}
	tr2.Update(doc1, RoundResult{}, t0.Add(12*time.Minute))
	if mins := tr2.Minutes(doc1.PRs[0], t0.Add(12*time.Minute)); mins != 12 {
		t.Fatalf("Minutes() = %d, want 12", mins)
	}

	// Unblocking clears the recorded start time.
	doc2 := scan.Document{
		Author: "alice",
		PRs: []scan.PR{
			{Repo: "acme/widgets", Number: 1, State: "open", Tab: &scan.Tab{TabID: "w2:t1", AgentStatus: "idle"}},
		},
	}
	tr2.Update(doc2, RoundResult{}, t0.Add(15*time.Minute))
	if len(tr2.Since) != 0 {
		t.Fatalf("Since after unblock = %+v, want empty", tr2.Since)
	}
}
