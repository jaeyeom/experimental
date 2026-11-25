package policy

import (
	"context"
	"regexp"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/backend"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/lease"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

type fakeGuard struct {
	allowed bool
	detail  string
	err     error
	calls   []GuardContext
}

func (g *fakeGuard) Check(_ context.Context, _ string, gctx GuardContext) (bool, string, error) {
	g.calls = append(g.calls, gctx)
	return g.allowed, g.detail, g.err
}

func strPtr(s string) *string { return &s }

func basePR() scan.PR {
	return scan.PR{
		Repo:             "acme/widgets",
		Number:           10,
		Title:            "[PROJ-10] Test PR",
		URL:              "https://github.com/acme/widgets/pull/10",
		State:            "open",
		Identifier:       strPtr("PROJ-10"),
		Base:             "main",
		Head:             "feat/test",
		HeadSHA:          "abc1234567890",
		MergeStateStatus: "BEHIND",
		Mergeable:        "MERGEABLE",
	}
}

func TestClassifyConflict(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name      string
		mergeable string
		state     string
		want      ConflictClass
	}{
		{name: "up to date", mergeable: "MERGEABLE", state: "CLEAN", want: ClassUpToDate},
		{name: "behind unknown mergeable", mergeable: "UNKNOWN", state: "BEHIND", want: ClassBehind},
		{name: "clean rebase", mergeable: "MERGEABLE", state: "BEHIND", want: ClassCleanRebase},
		{name: "conflicting mergeable", mergeable: "CONFLICTING", state: "BEHIND", want: ClassConflicting},
		{name: "dirty state", mergeable: "UNKNOWN", state: "DIRTY", want: ClassConflicting},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := ClassifyConflict(scan.PR{Mergeable: tc.mergeable, MergeStateStatus: tc.state})
			if got != tc.want {
				t.Fatalf("ClassifyConflict() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestPolicyModes(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name         string
		mode         string
		wantCount    int
		wantDecision DecisionAction
		wantSent     int
	}{
		{name: "off emits nothing", mode: config.PolicyOff, wantCount: 0, wantSent: 0},
		{name: "dry-run does not send", mode: config.PolicyDryRun, wantCount: 1, wantDecision: DecisionDryRun, wantSent: 0},
		{name: "auto sends prompt", mode: config.PolicyAuto, wantCount: 1, wantDecision: DecisionDispatch, wantSent: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := config.Defaults()
			cfg.Policy.Rebase = tc.mode
			fb := &backend.FakeBackend{
				Sessions: []backend.Session{{ID: "p1", Label: "PROJ-10", Status: "idle"}},
			}
			fl := &lease.FakeProvider{}
			eng := &Engine{
				Backend: fb,
				Lease:   fl,
				Ledger:  &ledger.Store{},
				Now:     func() time.Time { return time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC) },
			}
			doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{basePR()}}, RunOptions{})
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(doc.Decisions) != tc.wantCount {
				t.Fatalf("decisions = %+v, want count %d", doc.Decisions, tc.wantCount)
			}
			if tc.wantCount > 0 && doc.Decisions[0].Decision != tc.wantDecision {
				t.Fatalf("decision = %q, want %q", doc.Decisions[0].Decision, tc.wantDecision)
			}
			if len(fb.Sent) != tc.wantSent {
				t.Fatalf("sent = %d, want %d", len(fb.Sent), tc.wantSent)
			}
			// Dry-run must never claim a lease.
			if tc.mode == config.PolicyDryRun && len(fl.Claims) != 0 {
				t.Fatalf("dry-run claimed lease: %v", fl.Claims)
			}
		})
	}
}

func TestPolicyGuards(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC)
	tests := []struct {
		name       string
		setup      func(*config.Config, *scan.PR, *backend.FakeBackend, *ledger.Store, *fakeGuard)
		wantAction DecisionAction
		wantReason string
	}{
		{
			name: "skip draft",
			setup: func(_ *config.Config, pr *scan.PR, _ *backend.FakeBackend, _ *ledger.Store, _ *fakeGuard) {
				pr.IsDraft = true
			},
			wantAction: DecisionSkip,
			wantReason: "draft",
		},
		{
			name: "skip held label",
			setup: func(cfg *config.Config, pr *scan.PR, _ *backend.FakeBackend, _ *ledger.Store, _ *fakeGuard) {
				cfg.Policy.HoldLabels = []string{"do-not-merge", "hold"}
				pr.Labels = []string{"HOLD"}
			},
			wantAction: DecisionSkip,
			wantReason: "held_label:HOLD",
		},
		{
			name: "skip stacked PR",
			setup: func(_ *config.Config, pr *scan.PR, _ *backend.FakeBackend, _ *ledger.Store, _ *fakeGuard) {
				pr.Base = "feat/parent"
			},
			wantAction: DecisionSkip,
			wantReason: "stacked_pr",
		},
		{
			name: "skip other machine tab",
			setup: func(_ *config.Config, _ *scan.PR, fb *backend.FakeBackend, _ *ledger.Store, _ *fakeGuard) {
				fb.Sessions[0].Machine = "linux2"
			},
			wantAction: DecisionSkip,
			wantReason: "other_machine:linux2",
		},
		{
			name: "skip busy tab",
			setup: func(_ *config.Config, _ *scan.PR, fb *backend.FakeBackend, _ *ledger.Store, _ *fakeGuard) {
				fb.Sessions[0].Status = "working"
			},
			wantAction: DecisionSkip,
			wantReason: "tab_not_idle:working",
		},
		{
			name: "skip open dialog",
			setup: func(_ *config.Config, _ *scan.PR, fb *backend.FakeBackend, _ *ledger.Store, _ *fakeGuard) {
				fb.Sessions[0].HasDialog = true
			},
			wantAction: DecisionSkip,
			wantReason: "open_dialog",
		},
		{
			name: "skip daily cap reached",
			setup: func(cfg *config.Config, _ *scan.PR, _ *backend.FakeBackend, st *ledger.Store, _ *fakeGuard) {
				cfg.Policy.DailyCap = 1
				_ = st.Append(ledger.Entry{PR: "acme/widgets#99", HeadSHA: "zzz", Kind: KindRebase, Time: now, Outcome: ledger.OutcomeDispatched})
			},
			wantAction: DecisionSkip,
			wantReason: "daily_cap",
		},
		{
			name: "skip guard command non-zero",
			setup: func(cfg *config.Config, _ *scan.PR, _ *backend.FakeBackend, _ *ledger.Store, g *fakeGuard) {
				cfg.Policy.GuardCommand = "check-guard"
				g.allowed = false
				g.detail = "freeze active"
			},
			wantAction: DecisionSkip,
			wantReason: "guard_command_failed",
		},
		{
			name: "allow guard command exit 0",
			setup: func(cfg *config.Config, _ *scan.PR, _ *backend.FakeBackend, _ *ledger.Store, g *fakeGuard) {
				cfg.Policy.GuardCommand = "check-guard"
				g.allowed = true
			},
			wantAction: DecisionDispatch,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := config.Defaults()
			cfg.Policy.Rebase = config.PolicyAuto
			pr := basePR()
			fb := &backend.FakeBackend{
				Sessions: []backend.Session{{ID: "p1", Label: "PROJ-10", Status: "idle"}},
			}
			st := &ledger.Store{}
			g := &fakeGuard{allowed: true}
			tc.setup(&cfg, &pr, fb, st, g)

			eng := &Engine{
				Backend: fb,
				Ledger:  st,
				Guard:   g,
				Now:     func() time.Time { return now },
			}
			doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(doc.Decisions) != 1 {
				t.Fatalf("decisions = %+v, want 1", doc.Decisions)
			}
			got := doc.Decisions[0]
			if got.Decision != tc.wantAction || got.Reason != tc.wantReason {
				t.Fatalf("got (%q, %q), want (%q, %q)", got.Decision, got.Reason, tc.wantAction, tc.wantReason)
			}
		})
	}
}

func TestMaxPerRunGuard(t *testing.T) {
	t.Parallel()
	cfg := config.Defaults()
	cfg.Policy.Rebase = config.PolicyAuto
	cfg.Policy.MaxPerRun = 1

	pr1 := basePR()
	pr2 := basePR()
	pr2.Number = 11
	pr2.Identifier = strPtr("PROJ-11")
	pr2.HeadSHA = "def456"

	fb := &backend.FakeBackend{
		Sessions: []backend.Session{
			{ID: "p1", Label: "PROJ-10", Status: "idle"},
			{ID: "p2", Label: "PROJ-11", Status: "idle"},
		},
	}
	eng := &Engine{Backend: fb, Ledger: &ledger.Store{}}
	doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr1, pr2}}, RunOptions{})
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if len(doc.Decisions) != 2 {
		t.Fatalf("decisions = %+v", doc.Decisions)
	}
	if doc.Decisions[0].Decision != DecisionDispatch {
		t.Fatalf("first decision = %+v, want dispatch", doc.Decisions[0])
	}
	if doc.Decisions[1].Decision != DecisionSkip || doc.Decisions[1].Reason != "max_per_run" {
		t.Fatalf("second decision = %+v, want skip max_per_run", doc.Decisions[1])
	}
}

func TestConflictingClassPolicy(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name                string
		dispatchConflicting bool
		wantDecision        DecisionAction
		wantReason          string
	}{
		{name: "reported by default", dispatchConflicting: false, wantDecision: DecisionReport, wantReason: "conflicting"},
		{name: "dispatched when enabled", dispatchConflicting: true, wantDecision: DecisionDispatch},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := config.Defaults()
			cfg.Policy.Rebase = config.PolicyAuto
			cfg.Policy.DispatchConflicting = tc.dispatchConflicting
			pr := basePR()
			pr.Mergeable = "CONFLICTING"
			pr.MergeStateStatus = "DIRTY"

			fb := &backend.FakeBackend{
				Sessions: []backend.Session{{ID: "p1", Label: "PROJ-10", Status: "idle"}},
			}
			eng := &Engine{Backend: fb, Ledger: &ledger.Store{}}
			doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(doc.Decisions) != 1 || doc.Decisions[0].Decision != tc.wantDecision || doc.Decisions[0].Reason != tc.wantReason {
				t.Fatalf("decisions = %+v", doc.Decisions)
			}
		})
	}
}

func TestNitReplyPolicy(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name      string
		comments  []scan.Comment
		wantCount int
		wantSent  int
	}{
		{
			name: "bot nit thread with no author reply dispatches",
			comments: []scan.Comment{
				{ThreadID: "t1", CommentID: "c1", Author: "coderabbitai", Body: "nit: rename variable", Path: "a.go", URL: "https://example/1"},
			},
			wantCount: 1,
			wantSent:  1,
		},
		{
			name: "human reviewer nit thread is never auto",
			comments: []scan.Comment{
				{ThreadID: "t1", CommentID: "c1", Author: "alice", Body: "nit: rename variable", Path: "a.go", URL: "https://example/1"},
			},
			wantCount: 0,
			wantSent:  0,
		},
		{
			name: "bot non-nit thread is not dispatched",
			comments: []scan.Comment{
				{ThreadID: "t1", CommentID: "c1", Author: "coderabbitai", Body: "critical security bug", Path: "a.go", URL: "https://example/1"},
			},
			wantCount: 0,
			wantSent:  0,
		},
		{
			name: "bot nit thread with author reply is skipped",
			comments: []scan.Comment{
				{ThreadID: "t1", CommentID: "c1", Author: "coderabbitai", Body: "nit: rename variable", HasAuthorReply: true},
			},
			wantCount: 0,
			wantSent:  0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := config.Defaults()
			cfg.Policy.NitReply = config.PolicyAuto
			cfg.Policy.NitReplyBots = []string{"coderabbitai"}
			cfg.Policy.NitReplyPatterns = []*regexp.Regexp{regexp.MustCompile(`(?i)\b(nit|non-blocking)\b`)}

			pr := basePR()
			pr.MergeStateStatus = "CLEAN"
			pr.BlockingComments = tc.comments

			fb := &backend.FakeBackend{
				Sessions: []backend.Session{{ID: "p1", Label: "PROJ-10", Status: "idle"}},
			}
			fl := &lease.FakeProvider{}
			eng := &Engine{Backend: fb, Lease: fl, Ledger: &ledger.Store{}}
			doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(doc.Decisions) != tc.wantCount || len(fb.Sent) != tc.wantSent {
				t.Fatalf("decisions=%d sent=%d, want %d/%d", len(doc.Decisions), len(fb.Sent), tc.wantCount, tc.wantSent)
			}
			// Reply-only nit-reply must not claim a working-copy lease.
			if len(fl.Claims) != 0 {
				t.Fatalf("nit-reply should not claim working-copy lease, got %v", fl.Claims)
			}
			if tc.wantSent == 1 && !strings.Contains(fb.Sent[0].Prompt, "reply-only") {
				t.Fatalf("expected reply-only prompt, got %q", fb.Sent[0].Prompt)
			}
		})
	}
}

func TestFailureMarkerPickup(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name         string
		marker       *scan.FailureMarker
		wantDecision DecisionAction
		wantReason   string
		wantSent     int
	}{
		{
			name:         "our-code unchanged head dispatches queue-fix",
			marker:       &scan.FailureMarker{Kind: scan.FailureKindOurCode, HeadSHA: "abc1234567890", QueueRunID: "run-1", CommentURL: "https://example/c1"},
			wantDecision: DecisionDispatch,
			wantSent:     1,
		},
		{
			name:         "our-code changed head skips",
			marker:       &scan.FailureMarker{Kind: scan.FailureKindOurCode, HeadSHA: "oldsha", QueueRunID: "run-1"},
			wantDecision: DecisionSkip,
			wantReason:   "head_changed",
			wantSent:     0,
		},
		{
			name:         "infra-flake no dispatch",
			marker:       &scan.FailureMarker{Kind: scan.FailureKindInfraFlake, HeadSHA: "abc1234567890", QueueRunID: "run-2"},
			wantDecision: DecisionSkip,
			wantReason:   "infra_flake",
			wantSent:     0,
		},
		{
			name:         "unknown report only",
			marker:       &scan.FailureMarker{Kind: scan.FailureKindUnknown, HeadSHA: "abc1234567890", QueueRunID: "run-3"},
			wantDecision: DecisionReport,
			wantReason:   "unknown_failure_marker",
			wantSent:     0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := config.Defaults()
			cfg.Policy.QueueFix = config.PolicyAuto
			pr := basePR()
			pr.MergeStateStatus = "CLEAN"
			pr.FailureMarker = tc.marker

			fb := &backend.FakeBackend{
				Sessions: []backend.Session{{ID: "p1", Label: "PROJ-10", Status: "idle"}},
			}
			st := &ledger.Store{}
			eng := &Engine{Backend: fb, Ledger: st}
			doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(doc.Decisions) != 1 {
				t.Fatalf("decisions = %+v", doc.Decisions)
			}
			got := doc.Decisions[0]
			if got.Decision != tc.wantDecision || got.Reason != tc.wantReason || len(fb.Sent) != tc.wantSent {
				t.Fatalf("got (%q, %q, sent=%d), want (%q, %q, sent=%d)", got.Decision, got.Reason, len(fb.Sent), tc.wantDecision, tc.wantReason, tc.wantSent)
			}
			// Second run with same head must be deduped unless forced.
			if tc.wantSent == 1 {
				doc2, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
				if err != nil {
					t.Fatalf("second Run() error = %v", err)
				}
				if doc2.Decisions[0].Decision != DecisionSkip || doc2.Decisions[0].Reason != "already_dispatched" {
					t.Fatalf("second run = %+v, want already_dispatched", doc2.Decisions[0])
				}
				// Force overrides ledger dedupe.
				doc3, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{Force: true})
				if err != nil {
					t.Fatalf("forced Run() error = %v", err)
				}
				if doc3.Decisions[0].Decision != DecisionDispatch {
					t.Fatalf("forced run = %+v, want dispatch", doc3.Decisions[0])
				}
			}
		})
	}
}

func TestLeaseHookAroundDispatch(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name         string
		claimResult  lease.ClaimResult
		sendStatus   string
		wantDecision DecisionAction
		wantReason   string
		wantSent     int
		wantReleased int
	}{
		{
			name:         "acquired held during send then released on done",
			claimResult:  lease.ClaimResult{Status: lease.ClaimAcquired},
			sendStatus:   backend.SendDone,
			wantDecision: DecisionDispatch,
			wantSent:     1,
			wantReleased: 1,
		},
		{
			name:         "released on timeout too",
			claimResult:  lease.ClaimResult{Status: lease.ClaimAcquired},
			sendStatus:   backend.SendTimeout,
			wantDecision: DecisionDispatch,
			wantSent:     1,
			wantReleased: 1,
		},
		{
			name:         "queued skips and reports without sending",
			claimResult:  lease.ClaimResult{Status: lease.ClaimQueued, Reason: "another job waiting"},
			wantDecision: DecisionSkip,
			wantReason:   "lease_queued",
			wantSent:     0,
			wantReleased: 0,
		},
		{
			name:         "refused skips and reports without sending",
			claimResult:  lease.ClaimResult{Status: lease.ClaimRefused, Holder: "pm"},
			wantDecision: DecisionSkip,
			wantReason:   "lease_refused",
			wantSent:     0,
			wantReleased: 0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			cfg := config.Defaults()
			cfg.Policy.Rebase = config.PolicyAuto
			cfg.Policy.LeaseName = "shared-checkout"

			fl := &lease.FakeProvider{Result: tc.claimResult}
			heldDuringSend := false
			fb := &backend.FakeBackend{
				Sessions: []backend.Session{{ID: "p1", Label: "PROJ-10", Status: "idle"}},
				SendResults: map[string]backend.SendResult{
					"p1": {Status: tc.sendStatus},
				},
				OnSend: func(string, string) {
					heldDuringSend = fl.Held
				},
			}
			eng := &Engine{Backend: fb, Lease: fl, Ledger: &ledger.Store{}}
			doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{basePR()}}, RunOptions{})
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			got := doc.Decisions[0]
			if got.Decision != tc.wantDecision || got.Reason != tc.wantReason {
				t.Fatalf("decision = %+v, want (%q, %q)", got, tc.wantDecision, tc.wantReason)
			}
			if len(fb.Sent) != tc.wantSent {
				t.Fatalf("sent = %d, want %d", len(fb.Sent), tc.wantSent)
			}
			if tc.wantSent == 1 && !heldDuringSend {
				t.Fatal("lease was not held while SendPrompt ran")
			}
			if len(fl.Releases) != tc.wantReleased {
				t.Fatalf("releases = %d, want %d", len(fl.Releases), tc.wantReleased)
			}
			if fl.Held {
				t.Fatal("lease still held after Run returned")
			}
		})
	}
}

func TestFormatLines(t *testing.T) {
	t.Parallel()
	doc := Document{
		Decisions: []Decision{
			{PR: "acme/widgets#10", HeadSHA: "abc1234567", Kind: KindRebase, Decision: DecisionDispatch, ConflictClass: ClassCleanRebase, SessionID: "p1", Outcome: ledger.OutcomeDispatched},
			{PR: "acme/widgets#11", HeadSHA: "def5678901", Kind: KindQueueFix, Decision: DecisionSkip, Reason: "infra_flake"},
		},
	}
	out := FormatLines(doc)
	lines := strings.Split(strings.TrimSpace(out), "\n")
	if len(lines) != 2 {
		t.Fatalf("FormatLines() = %q, want 2 lines", out)
	}
	if !strings.Contains(lines[0], "DISPATCH") || !strings.Contains(lines[0], "acme/widgets#10") || !strings.Contains(lines[0], "class=clean-rebase") {
		t.Fatalf("line 0 = %q", lines[0])
	}
	if !strings.Contains(lines[1], "SKIP") || !strings.Contains(lines[1], "reason=infra_flake") {
		t.Fatalf("line 1 = %q", lines[1])
	}
}
