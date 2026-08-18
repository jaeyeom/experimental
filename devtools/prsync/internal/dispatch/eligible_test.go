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
		name string
		c    Candidate
		cfg  config.Config
		st   State
		want string
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
			got := Evaluate(tc.c, tc.cfg, tc.st)
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
