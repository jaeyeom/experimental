package dispatch

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func TestEligibleSkipReasons(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	tests := []struct {
		name   string
		c      Candidate
		cfg    config.Config
		st     State
		rebase bool
		ciFix  bool
		force  bool
		want   string
	}{
		{
			name: "not found",
			c:    Candidate{Repo: "acme/missing", Number: 9},
			cfg:  cfg,
			want: ActionSkippedNotFound,
		},
		{
			name: "addressed",
			c:    Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.BlockingComments = nil })},
			cfg:  cfg,
			want: ActionSkippedAddressed,
		},
		{
			name:   "rebase skips addressed gate",
			c:      Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.BlockingComments = nil })},
			cfg:    cfg,
			rebase: true,
			want:   "",
		},
		{
			name:   "rebase still skips no tab",
			c:      Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.Tab = nil })},
			cfg:    cfg,
			rebase: true,
			want:   ActionSkippedNoTab,
		},
		{
			name:   "rebase still skips draft",
			c:      Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.IsDraft = true })},
			cfg:    cfg,
			rebase: true,
			want:   ActionSkippedDraft,
		},
		{
			name:  "ci-fix skips addressed gate",
			c:     Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.BlockingComments = nil })},
			cfg:   cfg,
			ciFix: true,
			want:  "",
		},
		{
			name:  "ci-fix still skips no tab",
			c:     Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.Tab = nil })},
			cfg:   cfg,
			ciFix: true,
			want:  ActionSkippedNoTab,
		},
		{
			name:  "ci-fix still skips draft",
			c:     Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.IsDraft = true })},
			cfg:   cfg,
			ciFix: true,
			want:  ActionSkippedDraft,
		},
		{
			name:  "ci-fix not deduped by comment ids",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123" })},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedCommentIDs: []string{"PRRC_widget"}}},
			ciFix: true,
			want:  "",
		},
		{
			name:  "ci-fix not deduped by rebase head SHA",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123" })},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			ciFix: true,
			want:  "",
		},
		{
			name:  "ci-fix deduped by ci-fix SHA",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123" })},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedCIFixSHA: "abc123"}},
			ciFix: true,
			want:  ActionSkippedDeduped,
		},
		{
			name:  "ci-fix retries when still failing",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123"; p.CIState = "failing" })},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedCIFixSHA: "abc123"}},
			ciFix: true,
			want:  "",
		},
		{
			name:  "ci-fix still deduped when green",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123"; p.CIState = "green" })},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedCIFixSHA: "abc123"}},
			ciFix: true,
			want:  ActionSkippedDeduped,
		},
		{
			name:  "ci-fix completed with new sha still deduped when green",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "fff000"; p.CIState = "green" })},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedCIFixSHA: "abc123"}},
			ciFix: true,
			want:  ActionSkippedDeduped,
		},
		{
			name:  "ci-fix force redispatches same head",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123"; p.CIState = "green" })},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedCIFixSHA: "abc123"}},
			ciFix: true,
			force: true,
			want:  "",
		},
		{
			name: "comment mode not deduped by ci-fix SHA",
			c:    Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.HeadSHA = "abc123" })},
			cfg:  cfg,
			st:   State{"acme/widgets#123": {DispatchedCIFixSHA: "abc123"}},
			want: "",
		},
		{
			name:   "rebase not deduped by comment ids",
			c:      Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123" })},
			cfg:    cfg,
			st:     State{"acme/widgets#123": {DispatchedCommentIDs: []string{"PRRC_widget"}}},
			rebase: true,
			want:   "",
		},
		{
			name:   "rebase deduped by head SHA",
			c:      Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123" })},
			cfg:    cfg,
			st:     State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			rebase: true,
			want:   ActionSkippedDeduped,
		},
		{
			name:   "rebase retries when still behind",
			c:      Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123"; p.MergeStateStatus = "BEHIND" })},
			cfg:    cfg,
			st:     State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			rebase: true,
			want:   "",
		},
		{
			name:   "rebase retries when still dirty",
			c:      Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123"; p.MergeStateStatus = "DIRTY" })},
			cfg:    cfg,
			st:     State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			rebase: true,
			want:   "",
		},
		{
			name:   "rebase still deduped when clean",
			c:      Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123"; p.MergeStateStatus = "CLEAN" })},
			cfg:    cfg,
			st:     State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			rebase: true,
			want:   ActionSkippedDeduped,
		},
		{
			name:   "rebase completed with new sha still deduped when clean",
			c:      Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "fff000"; p.MergeStateStatus = "CLEAN" })},
			cfg:    cfg,
			st:     State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			rebase: true,
			want:   ActionSkippedDeduped,
		},
		{
			name:   "rebase force redispatches same head",
			c:      Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Unaddressed = false; p.HeadSHA = "abc123"; p.MergeStateStatus = "CLEAN" })},
			cfg:    cfg,
			st:     State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			rebase: true,
			force:  true,
			want:   "",
		},
		{
			name:  "comment force redispatches same comments",
			c:     Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(nil)},
			cfg:   cfg,
			st:    State{"acme/widgets#123": {DispatchedCommentIDs: []string{"PRRC_widget"}}},
			force: true,
			want:  "",
		},
		{
			name: "comment mode not deduped by head SHA",
			c:    Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.HeadSHA = "abc123" })},
			cfg:  cfg,
			st:   State{"acme/widgets#123": {DispatchedHeadSHA: "abc123"}},
			want: "",
		},
		{
			name: "draft",
			c:    Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.IsDraft = true })},
			cfg:  cfg,
			want: ActionSkippedDraft,
		},
		{
			name: "no tab",
			c:    Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Tab = nil })},
			cfg:  cfg,
			want: ActionSkippedNoTab,
		},
		{
			name: "no agent",
			c:    Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Tab.PaneID = nil })},
			cfg:  cfg,
			want: ActionSkippedNoAgent,
		},
		{
			name: "busy working",
			c:    Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Tab.AgentStatus = "working" })},
			cfg:  cfg,
			want: ActionSkippedBusy,
		},
		{
			name: "busy blocked",
			c:    Candidate{Repo: "acme/widgets", Number: 1, PR: prWith(func(p *scan.PR) { p.Tab.AgentStatus = "blocked" })},
			cfg:  cfg,
			want: ActionSkippedBusy,
		},
		{
			name: "deduped",
			c:    Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(nil)},
			cfg:  cfg,
			st:   State{"acme/widgets#123": {DispatchedCommentIDs: []string{"PRRC_widget"}}},
			want: ActionSkippedDeduped,
		},
		{
			name: "eligible idle",
			c:    Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(nil)},
			cfg:  cfg,
			want: "",
		},
		{
			name: "eligible done",
			c:    Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.Tab.AgentStatus = "done" })},
			cfg:  cfg,
			want: "",
		},
		{
			name: "include drafts",
			c:    Candidate{Repo: "acme/widgets", Number: 123, PR: prWith(func(p *scan.PR) { p.IsDraft = true })},
			cfg:  func() config.Config { c := config.Defaults(); c.IncludeDrafts = true; return c }(),
			want: "",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := Evaluate(tc.c, tc.cfg, tc.st, tc.rebase, tc.ciFix, tc.force)
			if got.Action != tc.want {
				t.Fatalf("action = %q, want %q", got.Action, tc.want)
			}
			if got.Repo != tc.c.Repo || got.Number != tc.c.Number {
				t.Fatalf("result = %+v, want repo/number from candidate", got)
			}
		})
	}
}

func TestParsePR(t *testing.T) {
	t.Parallel()

	repo, n, err := ParsePR("acme/widgets#123")
	if err != nil {
		t.Fatalf("ParsePR() error = %v", err)
	}
	if repo != "acme/widgets" || n != 123 {
		t.Fatalf("ParsePR() = %s #%d", repo, n)
	}
}

func TestParsePRInvalid(t *testing.T) {
	t.Parallel()

	for _, in := range []string{"widgets#123", "acme/widgets", "acme/widgets#", "acme/widgets#x", "#123", "acme/#1"} {
		if _, _, err := ParsePR(in); err == nil {
			t.Fatalf("ParsePR(%q) error = nil, want error", in)
		}
	}
}

func TestCandidatesFromDocument(t *testing.T) {
	t.Parallel()

	doc := scan.Document{PRs: []scan.PR{
		{Repo: "acme/widgets", Number: 200},
		{Repo: "acme/gizmos", Number: 50},
		{Repo: "acme/widgets", Number: 123},
	}}
	got, err := Candidates(doc, nil)
	if err != nil {
		t.Fatalf("Candidates() error = %v", err)
	}
	if len(got) != 3 {
		t.Fatalf("len = %d, want 3", len(got))
	}
	if got[0].Repo != "acme/gizmos" || got[0].Number != 50 {
		t.Fatalf("first = %s#%d, want acme/gizmos#50", got[0].Repo, got[0].Number)
	}
	if got[1].Repo != "acme/widgets" || got[1].Number != 123 {
		t.Fatalf("second = %s#%d, want acme/widgets#123", got[1].Repo, got[1].Number)
	}
	if got[2].Repo != "acme/widgets" || got[2].Number != 200 {
		t.Fatalf("third = %s#%d, want acme/widgets#200", got[2].Repo, got[2].Number)
	}
}

func TestCandidatesFromFlagsIncludesMissing(t *testing.T) {
	t.Parallel()

	doc := scan.Document{PRs: []scan.PR{{Repo: "acme/widgets", Number: 123}}}
	got, err := Candidates(doc, []string{"acme/missing#9", "acme/widgets#123"})
	if err != nil {
		t.Fatalf("Candidates() error = %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("len = %d, want 2 (never drop --pr)", len(got))
	}
	if got[0].Repo != "acme/missing" || got[0].PR != nil {
		t.Fatalf("first = %+v, want missing not found", got[0])
	}
	if got[1].PR == nil || got[1].Number != 123 {
		t.Fatalf("second = %+v, want widgets#123", got[1])
	}
}

func TestCandidatesBadFlag(t *testing.T) {
	t.Parallel()

	_, err := Candidates(scan.Document{}, []string{"nope"})
	if err == nil {
		t.Fatal("Candidates(bad --pr) error = nil, want error")
	}
}

func fixtureEligiblePR() scan.PR {
	line := 42
	id := "PROJ-123"
	pane := "w2:pC"
	return scan.PR{
		Repo:        "acme/widgets",
		Number:      123,
		Title:       "[PROJ-123] Fix the widget",
		URL:         "https://github.com/acme/widgets/pull/123",
		Identifier:  &id,
		Head:        "fix-widget",
		Base:        "main",
		HeadSHA:     "abc123def456",
		Unaddressed: true,
		BlockingComments: []scan.Comment{{
			ThreadID:  "PRRT_widget",
			CommentID: "PRRC_widget",
			Author:    "reviewer-login",
			Path:      "src/widget.go",
			Line:      &line,
			URL:       "https://github.com/acme/widgets/pull/123#discussion_r1",
			Body:      "This should handle the nil case.",
		}},
		Tab: &scan.Tab{
			TabID:       "w2:tC",
			PaneID:      &pane,
			WorkspaceID: "w2",
			Label:       "PROJ-123",
			AgentStatus: "idle",
		},
	}
}

func prWith(edit func(*scan.PR)) *scan.PR {
	p := fixtureEligiblePR()
	if edit != nil {
		edit(&p)
	}
	return &p
}
