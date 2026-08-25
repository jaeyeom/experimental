package comment

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

var fixtureNow = time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC)

func TestRunDryRunWouldDispatch(t *testing.T) {
	t.Parallel()

	g := &fakeGH{}
	got, err := Run(context.Background(), g, config.Defaults(), Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, false)}},
		Body: "/ci",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if !got.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if got.GeneratedAt != "2026-01-01T09:00:00Z" {
		t.Fatalf("generated_at = %q", got.GeneratedAt)
	}
	if len(got.Results) != 1 {
		t.Fatalf("len(results) = %d, want 1", len(got.Results))
	}
	item := got.Results[0]
	if item.Repo != "acme/widgets" || item.Number != 123 {
		t.Fatalf("item = %+v", item)
	}
	if item.Action != dispatch.ActionWouldDispatch {
		t.Fatalf("action = %q, want would_dispatch", item.Action)
	}
	if item.RenderedPrompt != "/ci" {
		t.Fatalf("rendered_prompt = %q, want /ci", item.RenderedPrompt)
	}
	if item.PaneID != "" {
		t.Fatalf("pane_id = %q, want empty (not tab-bound)", item.PaneID)
	}
	if len(g.calls) != 0 {
		t.Fatalf("CommentPR called on dry-run: %+v", g.calls)
	}
}

func TestRunDryRunCommentsOffMachinePR(t *testing.T) {
	t.Parallel()

	got, err := Run(context.Background(), &fakeGH{}, config.Defaults(), Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, true)}},
		Body: "please /ci",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("results = %+v, want would_dispatch for no-tab PR", got.Results)
	}
}

func TestRunSkippedNotFound(t *testing.T) {
	t.Parallel()

	got, err := Run(context.Background(), &fakeGH{}, config.Defaults(), Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, false)}},
		PRs:  []string{"acme/widgets#123", "acme/missing#9"},
		Body: "/ci",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2 (never drop)", len(got.Results))
	}
	byRepo := map[string]string{}
	for _, item := range got.Results {
		byRepo[item.Repo] = item.Action
	}
	if byRepo["acme/missing"] != dispatch.ActionSkippedNotFound {
		t.Fatalf("actions = %v", byRepo)
	}
	if byRepo["acme/widgets"] != dispatch.ActionWouldDispatch {
		t.Fatalf("actions = %v", byRepo)
	}
}

func TestRunLivePostsComment(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	g := &fakeGH{}
	got, err := Run(context.Background(), g, cfg, Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/widgets", 123, true), pr("acme/gizmos", 50, false)}},
		Body: "/ci",
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if got.DryRun {
		t.Fatal("dry_run = true, want false")
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	if got.Results[0].Repo != "acme/gizmos" || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("first = %+v, want dispatched acme/gizmos#50 (sorted)", got.Results[0])
	}
	if got.Results[1].Repo != "acme/widgets" || got.Results[1].Action != dispatch.ActionDispatched {
		t.Fatalf("second = %+v, want dispatched acme/widgets#123", got.Results[1])
	}
	if got.Results[0].RenderedPrompt != "/ci" || got.Results[1].RenderedPrompt != "/ci" {
		t.Fatalf("missing comment body on results: %+v", got.Results)
	}
	want := []commentCall{
		{repo: "acme/gizmos", number: 50, body: "/ci"},
		{repo: "acme/widgets", number: 123, body: "/ci"},
	}
	if len(g.calls) != len(want) {
		t.Fatalf("calls = %+v, want %+v", g.calls, want)
	}
	for i, call := range g.calls {
		if call != want[i] {
			t.Fatalf("call[%d] = %+v, want %+v", i, call, want[i])
		}
	}
}

func TestRunLiveFailureStopsBatch(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	cfg.DryRun = false
	g := &fakeGH{err: errors.New("boom")}
	got, err := Run(context.Background(), g, cfg, Request{
		Doc:  scan.Document{PRs: []scan.PR{pr("acme/gizmos", 50, false), pr("acme/widgets", 123, false)}},
		Body: "/ci",
	}, fixtureNow)
	if !errors.Is(err, dispatch.ErrFailed) {
		t.Fatalf("error = %v, want ErrFailed", err)
	}
	if len(got.Results) != 1 {
		t.Fatalf("len(results) = %d, want 1 (stop on failure)", len(got.Results))
	}
	if got.Results[0].Action != dispatch.ActionFailed || got.Results[0].Detail != "boom" {
		t.Fatalf("result = %+v, want failed boom", got.Results[0])
	}
	if len(g.calls) != 1 {
		t.Fatalf("calls = %+v, want 1", g.calls)
	}
}

func TestRunInvalidPRFlag(t *testing.T) {
	t.Parallel()

	_, err := Run(context.Background(), &fakeGH{}, config.Defaults(), Request{
		PRs:  []string{"not-a-pr"},
		Body: "/ci",
	}, fixtureNow)
	if err == nil {
		t.Fatal("error = nil, want invalid --pr")
	}
	if !strings.Contains(err.Error(), "invalid --pr") {
		t.Fatalf("error = %v, want invalid --pr", err)
	}
}

type commentCall struct {
	repo   string
	number int
	body   string
}

type fakeGH struct {
	calls []commentCall
	err   error
}

func (f *fakeGH) CommentPR(_ context.Context, repo string, number int, body string) error {
	f.calls = append(f.calls, commentCall{repo: repo, number: number, body: body})
	return f.err
}

func pr(repo string, number int, noTab bool) scan.PR {
	p := scan.PR{Repo: repo, Number: number}
	if !noTab {
		pane := "w2:pC"
		p.Tab = &scan.Tab{TabID: "w2:tC", PaneID: &pane, AgentStatus: "idle"}
	}
	return p
}
