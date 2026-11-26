package tick

import (
	"context"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// TestTickRecordsPRTimeline starts an item, then observes its open draft
// pull request, then the merge. A later observation does not clear merged_at.
func TestTickRecordsPRTimeline(t *testing.T) {
	ctx := context.Background()
	it := readyItem("PRJ-123", "Ship it")
	it.Labels = []string{"heavy"}
	env := newTickEnv(t, []source.Item{it}, true, false)
	env.eng.Config.AgentRules = []config.AgentRule{{
		Name:      "big-model",
		Labels:    []string{"heavy"},
		AgentArgs: []string{"--model", "opus"},
	}}
	if err := env.eng.Tick(ctx); err != nil {
		t.Fatal(err)
	}
	claim := claimRow(t, env.st, "PRJ-123")
	if claim.AgentRule != "big-model" || claim.Model != "opus" {
		t.Fatalf("claim rule %q model %q, want big-model and opus", claim.AgentRule, claim.Model)
	}

	opened := env.now.Add(30 * time.Minute)
	seen := env.now.Add(time.Hour)
	env.eng.Now = func() time.Time { return seen }
	draft := gh.PR{
		Repo: "acme/widgets", Number: 7, Title: "[PRJ-123] ship", URL: "https://example.test/pull/7",
		Draft: true, CreatedAt: opened,
	}
	env.pulls.open = []gh.PR{draft}
	if err := env.eng.Tick(ctx); err != nil {
		t.Fatal(err)
	}
	pr := mustPR(t, env.st, 7)
	if !pr.OpenedAt.Equal(opened) || !pr.Draft || !pr.MergedAt.IsZero() || !pr.LastSeen.Equal(seen) {
		t.Fatalf("pr after open = %+v", pr)
	}
	if pr.Source != "jira:acme" || pr.ItemID != "PRJ-123" {
		t.Fatalf("pr item = %s/%s", pr.Source, pr.ItemID)
	}

	merged := env.now.Add(2 * time.Hour)
	later := env.now.Add(3 * time.Hour)
	env.eng.Now = func() time.Time { return later }
	done := draft
	done.Draft, done.Merged, done.MergedAt, done.ClosedAt = false, true, merged, merged
	env.pulls.open = nil
	env.pulls.merged = []gh.PR{done}
	if err := env.eng.Tick(ctx); err != nil {
		t.Fatal(err)
	}
	pr = mustPR(t, env.st, 7)
	if !pr.OpenedAt.Equal(opened) || !pr.MergedAt.Equal(merged) || pr.Draft {
		t.Fatalf("pr after merge = %+v", pr)
	}

	stale := store.PR{
		Repo: "acme/widgets", Number: 7, Source: "jira:acme", ItemID: "PRJ-123",
		OpenedAt: opened, Draft: true, LastSeen: later.Add(time.Hour),
	}
	if err := env.st.UpsertPRs(ctx, []store.PR{stale}); err != nil {
		t.Fatal(err)
	}
	if pr = mustPR(t, env.st, 7); !pr.MergedAt.Equal(merged) || !pr.ClosedAt.Equal(merged) {
		t.Fatalf("pr after a later observation = %+v", pr)
	}

	finished, err := env.st.FinishedItems(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(finished) != 1 || finished[0].ID != "PRJ-123" || !finished[0].ClaimedAt.Equal(claim.ClaimedAt) {
		t.Fatalf("finished = %+v", finished)
	}
}

func TestTickSyncRecordsBacklogSnapshot(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "One"), readyItem("PRJ-2", "Two")}, false, false)
	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	snaps, err := env.st.BacklogSnapshots(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(snaps) != 1 || snaps[0].Source != "jira:acme" || snaps[0].Ready != 2 || snaps[0].ByStatus["To Do"] != 2 {
		t.Fatalf("snapshots = %+v", snaps)
	}
}

func mustPR(t *testing.T, st *store.Store, number int) store.PR {
	t.Helper()
	pr, err := st.PR(context.Background(), "acme/widgets", number)
	if err != nil {
		t.Fatal(err)
	}
	return pr
}
