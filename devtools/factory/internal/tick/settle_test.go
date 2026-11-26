package tick

import (
	"bytes"
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/verify"
)

func TestSettleClosesIdleTabAndDoesNotStart(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-123] ship")
	seedClaim(t, env.st, "PRJ-123", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if got := env.src.finishIDs(); len(got) != 1 || got[0] != "PRJ-123" {
		t.Fatalf("finishes = %v", got)
	}
	if got := claimRow(t, env.st, "PRJ-123"); got.State != store.Done {
		t.Fatalf("claim = %+v", got)
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t1" {
		t.Fatalf("closed = %v", got)
	}
	if env.out.String() != "tab_closed\nclosed tab PRJ-123 (w1:t1)\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if len(env.herd.createdTabs()) != 0 || len(env.herd.agentArgs()) != 0 {
		t.Fatalf("started tabs %v args %v", env.herd.createdTabs(), env.herd.agentArgs())
	}
	if env.pulls.allCalls != 0 || env.pulls.mergedCalls != 1 {
		t.Fatalf("merged calls = %d, all calls = %d", env.pulls.mergedCalls, env.pulls.allCalls)
	}
	assertNoTickRun(t, env.st)
}

func TestSettleLeavesWorkingTabOpen(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-123] ship")
	seedClaim(t, env.st, "PRJ-123", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if got := env.herd.closedTabs(); len(got) != 0 {
		t.Fatalf("closed = %v", got)
	}
	if env.out.String() != "left tab PRJ-123 (w1:t1) open: agent is working\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := claimRow(t, env.st, "PRJ-123"); got.State != store.Done {
		t.Fatalf("claim = %+v", got)
	}
}

func TestTickClosesIdleTabBeforeIdleLine(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-123",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-123] ship")
	seedClaim(t, env.st, "PRJ-123", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Tick(context.Background()); err != nil {
		t.Fatal(err)
	}
	if env.out.String() != "tab_closed\nclosed tab PRJ-123 (w1:t1)\nno ready items\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t1" {
		t.Fatalf("closed = %v", got)
	}
}

func TestSettlePrintsUnblockedDependent(t *testing.T) {
	next := readyItem("PRJ-2", "Next")
	next.BlockedBy = []source.BlockRef{{ID: "PRJ-1"}}
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Blocker"), next}, true, true)
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if env.out.String() != "unblocked PRJ-2\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if got := env.src.claimIDs(); len(got) != 0 {
		t.Fatalf("claims = %v", got)
	}
}

func TestSettleSkipsDependentStillBlocked(t *testing.T) {
	next := readyItem("PRJ-2", "Next")
	next.BlockedBy = []source.BlockRef{{ID: "PRJ-1"}, {ID: "PRJ-3"}}
	env := newTickEnv(t, []source.Item{
		readyItem("PRJ-1", "Blocker"),
		next,
		readyItem("PRJ-3", "Other"),
	}, true, true)
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if env.out.String() != "" {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleUnblocksAfterEveryBlockerMerges(t *testing.T) {
	next := readyItem("PRJ-2", "Next")
	next.BlockedBy = []source.BlockRef{{ID: "PRJ-1"}, {ID: "PRJ-3"}}
	env := newTickEnv(t, []source.Item{
		readyItem("PRJ-1", "One"),
		next,
		readyItem("PRJ-3", "Three"),
	}, true, true)
	env.pulls.merged = []gh.PR{
		pullRequest("[PRJ-1] ship", true),
		{
			Repo: "acme/widgets", Number: 3, Title: "[PRJ-3] ship", URL: "https://example.test/pull/3", Merged: true,
			CreatedAt: env.now.Add(-2 * time.Hour), MergedAt: env.now.Add(-time.Hour),
		},
	}
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")
	seedClaim(t, env.st, "PRJ-3", store.Started, env.now.Add(-time.Minute), "", "")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if strings.Count(env.out.String(), "unblocked PRJ-2\n") != 1 {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if claimRow(t, env.st, "PRJ-1").State != store.Done || claimRow(t, env.st, "PRJ-3").State != store.Done {
		t.Fatal("claims were not both done")
	}
}

func TestSettleSkipsFinishWhenDisabledOrDone(t *testing.T) {
	t.Run("disabled", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, false)
		env.pulls.merged = ghPR("[PRJ-1] ship")
		seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")
		if err := env.eng.Settle(context.Background(), nil); err != nil {
			t.Fatal(err)
		}
		if got := env.src.finishIDs(); len(got) != 0 {
			t.Fatalf("finishes = %v", got)
		}
		if env.out.String() != "" {
			t.Fatalf("stdout = %q", env.out.String())
		}
	})
	t.Run("already done", func(t *testing.T) {
		item := readyItem("PRJ-1", "Ship")
		item.Done = true
		item.State = "Done"
		env := newTickEnv(t, []source.Item{item}, true, true)
		env.pulls.merged = ghPR("[PRJ-1] ship")
		seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")
		if err := env.eng.Settle(context.Background(), nil); err != nil {
			t.Fatal(err)
		}
		if got := env.src.finishIDs(); len(got) != 0 {
			t.Fatalf("finishes = %v", got)
		}
	})
}

func TestSettleNamedIDSearchesEveryMergedPullRequest(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-9", "Old")}, true, true)
	old := time.Date(2020, 1, 15, 0, 0, 0, 0, time.UTC)
	prs := ghPR("[PRJ-9] ship")
	prs[0].MergedAt = old
	env.pulls.byTitle = prs
	seedClaim(t, env.st, "PRJ-9", store.HandedOff, env.now, "", "")

	if err := env.eng.Settle(context.Background(), []string{"PRJ-9"}); err != nil {
		t.Fatal(err)
	}
	if !old.Before(env.now.Add(-defaultTickLookback)) {
		t.Fatalf("merged at %s is inside the %s window ending %s", old, defaultTickLookback, env.now)
	}
	if env.pulls.mergedCalls != 0 || env.pulls.allCalls != 0 || env.pulls.titleCalls != 1 {
		t.Fatalf("window = %d, all = %d, title = %d", env.pulls.mergedCalls, env.pulls.allCalls, env.pulls.titleCalls)
	}
	if len(env.pulls.titleIDs) != 1 || env.pulls.titleIDs[0] != "PRJ-9" {
		t.Fatalf("title ids = %v", env.pulls.titleIDs)
	}
	if got := env.src.finishIDs(); len(got) != 1 || got[0] != "PRJ-9" {
		t.Fatalf("finishes = %v", got)
	}
	if claimRow(t, env.st, "PRJ-9").State != store.Done {
		t.Fatal("claim was not done")
	}
}

func TestSettleNamedIDWithoutPullRequestChangesNothing(t *testing.T) {
	env := newTickEnv(t, []source.Item{
		readyItem("PRJ-1", "One"),
		readyItem("PRJ-9", "Missing"),
	}, true, true)
	env.pulls.byTitle = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")
	seedClaim(t, env.st, "PRJ-9", store.Started, env.now, "", "")

	err := env.eng.Settle(context.Background(), []string{"PRJ-1", "PRJ-9"})
	if err == nil || !strings.Contains(err.Error(), "settle PRJ-9: expected a merged pull request, found none") {
		t.Fatalf("error = %v", err)
	}
	if claimRow(t, env.st, "PRJ-1").State != store.Started || claimRow(t, env.st, "PRJ-9").State != store.Started {
		t.Fatal("a claim changed")
	}
	if got := env.src.finishIDs(); len(got) != 0 {
		t.Fatalf("finishes = %v", got)
	}
}

func TestSettleUnknownID(t *testing.T) {
	env := newTickEnv(t, nil, true, true)
	env.pulls.allMerged = ghPR("[PRJ-9] ship")
	err := env.eng.Settle(context.Background(), []string{"PRJ-9"})
	if err == nil || !strings.Contains(err.Error(), "settle PRJ-9: expected one item, found none") {
		t.Fatalf("error = %v", err)
	}
}

func TestSettleRepeatedID(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "One")}, true, true)
	err := env.eng.Settle(context.Background(), []string{"PRJ-1", "PRJ-1"})
	if err == nil || !strings.Contains(err.Error(), "expected distinct ids, found PRJ-1 repeated") {
		t.Fatalf("error = %v", err)
	}
	if env.pulls.allCalls != 0 {
		t.Fatalf("all calls = %d", env.pulls.allCalls)
	}
}

func TestSettleFinishErrorStillClosesClaim(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-123", "Ship it")}, true, true)
	env.pulls.merged = ghPR("[PRJ-123] ship")
	env.src.finishErr = errors.New("finish broke")
	seedClaim(t, env.st, "PRJ-123", store.Started, env.now, "", "")
	var logs bytes.Buffer
	restore := captureLog(t, &logs)
	t.Cleanup(restore)

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if claimRow(t, env.st, "PRJ-123").State != store.Done {
		t.Fatal("claim was not done")
	}
	if !bytes.Contains(logs.Bytes(), []byte("finish on merge failed")) || !bytes.Contains(logs.Bytes(), []byte("finish broke")) {
		t.Fatalf("log = %q", logs.String())
	}
	if env.out.String() != "" {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleMissingRecordedTabDoesNotCloseAnother(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t9", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t-gone", "w1")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if got := env.herd.closedTabs(); len(got) != 0 {
		t.Fatalf("closed = %v", got)
	}
}

func TestSettleClosesLabelTabWhenClaimHasNone(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t9", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusDone, PaneCount: 1,
	}}
	env.pulls.byTitle = ghPR("[PRJ-1] ship")

	if err := env.eng.Settle(context.Background(), []string{"PRJ-1"}); err != nil {
		t.Fatal(err)
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t9" {
		t.Fatalf("closed = %v", got)
	}
	assertNoClaim(t, env.st, "PRJ-1")
	if env.out.String() != "tab_closed\nclosed tab PRJ-1 (w1:t9)\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleTabAlreadyGone(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.herd.closeErr = herdr.ErrTabNotFound
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if env.out.String() != "" {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleRequiresCloserWhenTabShouldClose(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.eng.Closer = nil
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")

	err := env.eng.Settle(context.Background(), nil)
	if err == nil || !strings.Contains(err.Error(), "expected a tab closer, found nil") {
		t.Fatalf("error = %v", err)
	}
}

func TestSettleReleasesDoneClaimWithoutMergedPullRequest(t *testing.T) {
	item := readyItem("PRJ-1", "Already done")
	item.Done = true
	item.State = "Done"
	open := readyItem("PRJ-2", "Still open")
	env := newTickEnv(t, []source.Item{item, open}, true, true)
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")
	seedClaim(t, env.st, "PRJ-2", store.Started, env.now, "", "")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Released {
		t.Fatalf("PRJ-1 claim = %+v", got)
	}
	if got := claimRow(t, env.st, "PRJ-2"); got.State != store.Started {
		t.Fatalf("PRJ-2 claim = %+v", got)
	}
	if got := env.src.finishIDs(); len(got) != 0 {
		t.Fatalf("finishes = %v", got)
	}
	if got := env.herd.closedTabs(); len(got) != 0 {
		t.Fatalf("closed = %v", got)
	}
	if env.out.String() != "released PRJ-1\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
	got, err := env.st.Item(context.Background(), "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatal(err)
	}
	if !got.Done || got.State != "Done" {
		t.Fatalf("item = %+v, want it to stay done", got)
	}
}

func TestSettleAcceptsDoneItemWhosePullRequestPredatesTheClaim(t *testing.T) {
	item := readyItem("PRJ-1", "Already done")
	item.Done = true
	item.State = "Done"
	env := newTickEnv(t, []source.Item{item}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	claimedAt := env.now.Add(-time.Hour)
	mergedAt := time.Date(2020, 1, 15, 0, 0, 0, 0, time.UTC)
	if !mergedAt.Before(claimedAt) {
		t.Fatalf("merged at %s does not predate the claim at %s", mergedAt, claimedAt)
	}
	prs := ghPR("[PRJ-1] ship")
	prs[0].MergedAt = mergedAt
	env.pulls.byTitle = prs
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Settle(context.Background(), []string{"PRJ-1"}); err != nil {
		t.Fatal(err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done || !got.ClaimedAt.Equal(claimedAt) {
		t.Fatalf("claim = %+v", got)
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t1" {
		t.Fatalf("closed = %v", got)
	}
	if got := env.src.finishIDs(); len(got) != 0 {
		t.Fatalf("finishes = %v", got)
	}
	assertTrackerDone(t, env.src, "PRJ-1")
	got, err := env.st.Item(context.Background(), "jira:acme", "PRJ-1")
	if err != nil {
		t.Fatal(err)
	}
	if !got.Done || got.State != "Done" {
		t.Fatalf("mirror item = %+v, want it to stay done", got)
	}
}

func TestSettleDoneItemWithMergedPullRequestStaysDone(t *testing.T) {
	item := readyItem("PRJ-1", "Merged and done")
	item.Done = true
	item.State = "Done"
	env := newTickEnv(t, []source.Item{item}, true, true)
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.HandedOff, env.now, "", "")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
		t.Fatalf("claim = %+v", got)
	}
	if strings.Contains(env.out.String(), "released") {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleOKCloseFinishesItem(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.herd.closeType = "ok"
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if got := env.src.finishIDs(); len(got) != 1 || got[0] != "PRJ-1" {
		t.Fatalf("finishes = %v", got)
	}
	assertTrackerDone(t, env.src, "PRJ-1")
	if env.out.String() != "ok\nclosed tab PRJ-1 (w1:t1)\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleUnexpectedCloseStillFinishesItem(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	env.herd.closeErr = errors.New("herdr: boom")
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")

	err := env.eng.Settle(context.Background(), nil)
	if err == nil || !strings.Contains(err.Error(), "herdr: boom") {
		t.Fatalf("Settle() error = %v", err)
	}
	if got := env.src.finishIDs(); len(got) != 1 || got[0] != "PRJ-1" {
		t.Fatalf("finishes = %v", got)
	}
	assertTrackerDone(t, env.src, "PRJ-1")
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
		t.Fatalf("claim = %+v", got)
	}
}

func assertTrackerDone(t *testing.T, src *memSource, id string) {
	t.Helper()
	src.mu.Lock()
	it, ok := src.items[id]
	src.mu.Unlock()
	if !ok || !it.Done || it.State != "Done" {
		t.Fatalf("tracker item = %+v, ok = %v", it, ok)
	}
}

func ghPR(title string) []gh.PR {
	return []gh.PR{pullRequest(title, true)}
}

func TestSettleFinishRemovesRowThatLeftQuery(t *testing.T) {
	next := readyItem("PRJ-2", "Next")
	next.BlockedBy = []source.BlockRef{{ID: "PRJ-1"}}
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Blocker"), next}, true, true)
	// The query is statusCategory != Done, so Finish moves PRJ-1 out of it.
	env.src.query = func(it source.Item) bool { return !it.Done }
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	if env.out.String() != "unblocked PRJ-2\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
	if _, err := env.st.Item(context.Background(), "jira:acme", "PRJ-1"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("PRJ-1 error = %v, want ErrNotFound", err)
	}
	got, err := env.st.Item(context.Background(), "jira:acme", "PRJ-2")
	if err != nil || len(got.BlockedBy) != 0 {
		t.Fatalf("PRJ-2 = %+v, %v, want no blockers left", got, err)
	}
}

func TestSettleFinishUpdatesRowThatStillMatches(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	got, err := env.st.Item(context.Background(), "jira:acme", "PRJ-1")
	if err != nil || !got.Done || got.State != "Done" {
		t.Fatalf("PRJ-1 = %+v, %v, want the done row", got, err)
	}
}

func TestSettleFinishMembershipFailureLogsAndUpdatesRow(t *testing.T) {
	var logBuf bytes.Buffer
	defer captureLog(t, &logBuf)()
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "", "")
	// The first sync is full and does not ask for IDs; the refresh does.
	env.src.query = func(it source.Item) bool { return !it.Done }
	env.src.idsErr = errors.New("search down")

	if err := env.eng.Settle(context.Background(), nil); err != nil {
		t.Fatal(err)
	}
	got, err := env.st.Item(context.Background(), "jira:acme", "PRJ-1")
	if err != nil || !got.Done {
		t.Fatalf("PRJ-1 = %+v, %v, want the done row kept", got, err)
	}
	if !strings.Contains(logBuf.String(), "refresh after finish failed") || !strings.Contains(logBuf.String(), "search down") {
		t.Fatalf("log = %q", logBuf.String())
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
		t.Fatalf("claim = %+v", got)
	}
}

func TestTickAndSettleIgnoreUnconfiguredSourceClaims(t *testing.T) {
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	ctx := context.Background()

	// Seed an active claim from a source not present in env.eng.Config.Sources.
	if err := env.st.InsertClaim(ctx, store.Claim{
		Source: "github:other", ID: "acme/other#9", State: store.Started,
		ClaimedAt: env.now.Add(-time.Hour), UpdatedAt: env.now.Add(-time.Hour), Machine: "test-host",
	}); err != nil {
		t.Fatal(err)
	}

	// Tick with an open PR for the configured claim should hand it off and ignore github:other.
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now.Add(-time.Hour), "", "")
	env.pulls.open = ghPR("[PRJ-1] ship")
	if err := env.eng.Tick(ctx); err != nil {
		t.Fatalf("Tick() error = %v", err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.HandedOff {
		t.Fatalf("PRJ-1 claim = %+v, want handed_off", got)
	}

	// Settle with a merged PR for the configured claim should finish it and ignore github:other.
	env.pulls.open = nil
	env.pulls.merged = ghPR("[PRJ-1] ship")
	if err := env.eng.Settle(ctx, nil); err != nil {
		t.Fatalf("Settle() error = %v", err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
		t.Fatalf("PRJ-1 claim = %+v, want done", got)
	}
}

func TestTickAndSettleMergeModeClaim(t *testing.T) {
	ctx := context.Background()
	next := readyItem("PRJ-2", "Next")
	next.BlockedBy = []source.BlockRef{{ID: "PRJ-1"}}
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship"), next}, true, true)
	env.eng.Config.Sources[0].Cwd = "/repo"
	env.eng.Config.Sources[0].Output = config.Output{Mode: config.OutputModeMerge}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	if err := env.st.InsertClaim(ctx, store.Claim{
		Source:      "jira:acme",
		ID:          "PRJ-1",
		ClaimedAt:   env.now.Add(-time.Hour),
		UpdatedAt:   env.now.Add(-time.Hour),
		Machine:     "test-host",
		TabID:       "w1:t1",
		WorkspaceID: "w1",
		State:       store.Started,
		OutputMode:  config.OutputModeMerge,
	}); err != nil {
		t.Fatal(err)
	}

	var checkedDir, checkedBase, checkedID string
	landed := false
	env.eng.GitMerge = func(_ context.Context, dir, base, id, _ string) (verify.LandedCommit, string, error) {
		checkedDir, checkedBase, checkedID = dir, base, id
		if !landed {
			return verify.LandedCommit{}, "merge: expected a commit referencing " + id + " on origin/" + base + ", found none", nil
		}
		return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
	}

	// Before commit lands, Tick does not settle or hand off the merge claim.
	if err := env.eng.Tick(ctx); err != nil {
		t.Fatalf("Tick() error = %v", err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Started {
		t.Fatalf("before land: claim state = %s, want started", got.State)
	}
	if checkedBase != "main" || checkedID != "PRJ-1" || checkedDir == "" {
		t.Fatalf("checked = (%q, %q, %q)", checkedDir, checkedBase, checkedID)
	}

	// settle PRJ-1 before commit lands errors clearly.
	if err := env.eng.Settle(ctx, []string{"PRJ-1"}); err == nil || !strings.Contains(err.Error(), "expected a commit referencing PRJ-1 on origin/main, found none") {
		t.Fatalf("Settle(PRJ-1) before land err = %v", err)
	}

	// Once commit lands on origin/main, Settle finishes Jira, closes tab, records finished, and prints unblocked.
	landed = true
	env.out.Reset()
	if err := env.eng.Settle(ctx, nil); err != nil {
		t.Fatalf("Settle() error = %v", err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
		t.Fatalf("after land: claim = %+v", got)
	}
	if got := env.src.finishIDs(); len(got) != 1 || got[0] != "PRJ-1" {
		t.Fatalf("finishes = %v", got)
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t1" {
		t.Fatalf("closed = %v", got)
	}
	finished, err := env.st.FinishedItems(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(finished) != 1 || finished[0].ID != "PRJ-1" {
		t.Fatalf("finished = %+v", finished)
	}
	if !strings.Contains(env.out.String(), "unblocked PRJ-2\n") {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleIDMergeModeRemoteLane(t *testing.T) {
	ctx := context.Background()
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.eng.Config.Lanes = []config.Lane{{
		Name:     "mac",
		Machine:  "mac-box",
		Checkout: "/remote/repo",
		Exec:     []string{"ssh", "mac-box"},
		Output:   config.Output{Mode: config.OutputModeMerge},
	}}
	remoteHerd := &fakeHerd{}
	env.eng.Machine = func(machine string) (LaneMachine, error) {
		if machine != "mac-box" {
			t.Fatalf("machine = %q", machine)
		}
		return LaneMachine{Sessions: remoteHerd, Closer: remoteHerd, Agent: remoteHerd}, nil
	}
	if err := env.st.InsertClaim(ctx, store.Claim{
		Source:     "jira:acme",
		ID:         "PRJ-1",
		ClaimedAt:  env.now.Add(-time.Hour),
		UpdatedAt:  env.now.Add(-time.Hour),
		Machine:    "mac-box",
		Lane:       "mac",
		State:      store.Started,
		OutputMode: config.OutputModeMerge,
	}); err != nil {
		t.Fatal(err)
	}

	var gotLane string
	var gotDir string
	env.eng.RemoteGitMerge = func(lane config.Lane) (MergeChecker, error) {
		gotLane = lane.Name
		return func(_ context.Context, dir, _, _, _ string) (verify.LandedCommit, string, error) {
			gotDir = dir
			return verify.LandedCommit{SHA: "def5678", Subject: "[PRJ-1] Remote ship"}, "", nil
		}, nil
	}

	if err := env.eng.Settle(ctx, []string{"PRJ-1"}); err != nil {
		t.Fatalf("Settle(PRJ-1) error = %v", err)
	}
	if gotLane != "mac" || gotDir != "/remote/repo" {
		t.Fatalf("lane = %q, dir = %q", gotLane, gotDir)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
		t.Fatalf("claim = %+v", got)
	}
}

func TestSettleOneClosesTabLast(t *testing.T) {
	ctx := context.Background()
	next := readyItem("PRJ-2", "Next")
	next.BlockedBy = []source.BlockRef{{ID: "PRJ-1"}}
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship"), next}, true, true)
	env.eng.Config.Lanes = []config.Lane{{
		Name:     "linux",
		Checkout: env.eng.Config.Sources[0].Cwd,
		Output:   config.Output{Mode: config.OutputModeMerge},
	}}
	env.eng.Config.LaneOrder = config.LaneOrder{Default: []string{"linux"}}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusIdle, PaneCount: 1,
	}}
	wtDir := "/repo/.worktrees/PRJ-1"
	if err := env.st.InsertClaim(ctx, store.Claim{
		Source:      "jira:acme",
		ID:          "PRJ-1",
		ClaimedAt:   env.now.Add(-time.Hour),
		UpdatedAt:   env.now.Add(-time.Hour),
		Machine:     "test-host",
		Lane:        "linux",
		TabID:       "w1:t1",
		WorkspaceID: "w1",
		Worktree:    wtDir,
		State:       store.Started,
		OutputMode:  config.OutputModeMerge,
	}); err != nil {
		t.Fatal(err)
	}
	env.eng.GitMerge = func(_ context.Context, _, _, _, _ string) (verify.LandedCommit, string, error) {
		return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
	}
	env.eng.Git = func(_ context.Context, _ string) (string, error) { return "", nil }
	env.eng.WorktreeRemove = func(_ context.Context, _, dir string) error {
		if dir != wtDir {
			t.Fatalf("worktree remove dir = %q, want %q", dir, wtDir)
		}
		if got := env.herd.closedTabs(); len(got) != 0 {
			t.Fatalf("CloseTab called before WorktreeRemove: %v", got)
		}
		return nil
	}
	env.eng.Closer = tabCloserFunc(func(closeCtx context.Context, tabID string) (string, error) {
		if got := env.src.finishIDs(); len(got) != 1 || got[0] != "PRJ-1" {
			t.Fatalf("Finish not called before CloseTab: %v", got)
		}
		gotClaim, err := env.st.LatestClaim(closeCtx, "jira:acme", "PRJ-1")
		if err != nil || gotClaim.State != store.Done {
			t.Fatalf("claim not done before CloseTab: %+v, %v", gotClaim, err)
		}
		if it, err := env.st.Item(closeCtx, "jira:acme", "PRJ-1"); err != nil || !it.Done {
			t.Fatalf("mirror not refreshed before CloseTab: %+v, %v", it, err)
		}
		runs, err := env.st.Runs(closeCtx, 10)
		if err != nil {
			t.Fatal(err)
		}
		if len(runs) == 0 || runs[0].Command != settleCommand {
			t.Fatalf("settle run not recorded before CloseTab: %+v", runs)
		}
		if !strings.Contains(env.out.String(), "unblocked PRJ-2\n") {
			t.Fatalf("unblocked not printed before CloseTab: %q", env.out.String())
		}
		return env.herd.CloseTab(closeCtx, tabID)
	})

	if err := env.eng.Settle(ctx, []string{"PRJ-1"}); err != nil {
		t.Fatalf("Settle() error = %v", err)
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t1" {
		t.Fatalf("closed = %v", got)
	}
}

type tabCloserFunc func(context.Context, string) (string, error)

func (f tabCloserFunc) CloseTab(ctx context.Context, tabID string) (string, error) {
	return f(ctx, tabID)
}

func TestSettleMergeModeSelfSettleClosesWorkingTab(t *testing.T) {
	ctx := context.Background()
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.eng.Config.CallerPaneID = "w1:p1"
	env.eng.Config.Sources[0].Output = config.Output{Mode: config.OutputModeMerge}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	env.herd.panes = []herdr.Pane{{
		ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking,
	}}
	if err := env.st.InsertClaim(ctx, store.Claim{
		Source:      "jira:acme",
		ID:          "PRJ-1",
		ClaimedAt:   env.now.Add(-time.Hour),
		UpdatedAt:   env.now.Add(-time.Hour),
		Machine:     "test-host",
		TabID:       "w1:t1",
		WorkspaceID: "w1",
		State:       store.Started,
		OutputMode:  config.OutputModeMerge,
	}); err != nil {
		t.Fatal(err)
	}
	env.eng.GitMerge = func(_ context.Context, _, _, _, _ string) (verify.LandedCommit, string, error) {
		return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
	}
	waitCalled := false
	env.eng.WaitAgent = func(_ context.Context, _ string, _ []string, _ time.Duration) (string, error) {
		waitCalled = true
		return herdr.AgentStatusIdle, nil
	}

	if err := env.eng.Settle(ctx, []string{"PRJ-1"}); err != nil {
		t.Fatalf("Settle() error = %v", err)
	}
	if waitCalled {
		t.Fatal("self-settle called WaitAgent, want direct CloseTab")
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t1" {
		t.Fatalf("closed = %v", got)
	}
	if !strings.Contains(env.out.String(), "closed tab PRJ-1 (w1:t1)\n") {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleMergeModeExternalWaitsAndClosesWorkingTab(t *testing.T) {
	ctx := context.Background()
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.eng.Config.CallerPaneID = "w1:p9"
	env.eng.Config.Sources[0].Output = config.Output{Mode: config.OutputModeMerge}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	env.herd.panes = []herdr.Pane{
		{ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking},
		{ID: "w1:p9", TabID: "w1:t9", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking},
	}
	if err := env.st.InsertClaim(ctx, store.Claim{
		Source:      "jira:acme",
		ID:          "PRJ-1",
		ClaimedAt:   env.now.Add(-time.Hour),
		UpdatedAt:   env.now.Add(-time.Hour),
		Machine:     "test-host",
		TabID:       "w1:t1",
		WorkspaceID: "w1",
		State:       store.Started,
		OutputMode:  config.OutputModeMerge,
	}); err != nil {
		t.Fatal(err)
	}
	env.eng.GitMerge = func(_ context.Context, _, _, _, _ string) (verify.LandedCommit, string, error) {
		return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
	}
	var gotTarget string
	var gotUntil []string
	var gotTimeout time.Duration
	env.eng.WaitAgent = func(_ context.Context, target string, until []string, timeout time.Duration) (string, error) {
		gotTarget, gotUntil, gotTimeout = target, until, timeout
		return herdr.AgentStatusDone, nil
	}

	if err := env.eng.Tick(ctx); err != nil {
		t.Fatalf("Tick() error = %v", err)
	}
	if gotTarget != "w1:t1" || len(gotUntil) != 2 || gotUntil[0] != herdr.AgentStatusIdle || gotUntil[1] != herdr.AgentStatusDone || gotTimeout != mergeSettleWaitTimeout {
		t.Fatalf("WaitAgent = (%q, %v, %s)", gotTarget, gotUntil, gotTimeout)
	}
	if got := env.herd.closedTabs(); len(got) != 1 || got[0] != "w1:t1" {
		t.Fatalf("closed = %v", got)
	}
}

func TestSettleMergeModeWaitTimeoutLeavesTabOpenWithoutFailing(t *testing.T) {
	ctx := context.Background()
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.eng.Config.Sources[0].Output = config.Output{Mode: config.OutputModeMerge}
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	if err := env.st.InsertClaim(ctx, store.Claim{
		Source:      "jira:acme",
		ID:          "PRJ-1",
		ClaimedAt:   env.now.Add(-time.Hour),
		UpdatedAt:   env.now.Add(-time.Hour),
		Machine:     "test-host",
		TabID:       "w1:t1",
		WorkspaceID: "w1",
		State:       store.Started,
		OutputMode:  config.OutputModeMerge,
	}); err != nil {
		t.Fatal(err)
	}
	env.eng.GitMerge = func(_ context.Context, _, _, _, _ string) (verify.LandedCommit, string, error) {
		return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
	}
	env.eng.WaitAgent = func(_ context.Context, _ string, _ []string, _ time.Duration) (string, error) {
		return "", errors.New("wait timed out")
	}

	if err := env.eng.Settle(ctx, []string{"PRJ-1"}); err != nil {
		t.Fatalf("Settle() error = %v", err)
	}
	if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
		t.Fatalf("claim = %+v", got)
	}
	if got := env.herd.closedTabs(); len(got) != 0 {
		t.Fatalf("closed = %v", got)
	}
	if !strings.Contains(env.out.String(), "left tab PRJ-1 (w1:t1) open: agent is working (wait timed out)\n") {
		t.Fatalf("stdout = %q", env.out.String())
	}
}

func TestSettleMergeModeAlreadyClosedAndSubagentCleanNoOps(t *testing.T) {
	t.Run("missing tab", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
		env.eng.Config.Sources[0].Output = config.Output{Mode: config.OutputModeMerge}
		if err := env.st.InsertClaim(context.Background(), store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: env.now.Add(-time.Hour), UpdatedAt: env.now.Add(-time.Hour),
			Machine: "test-host", TabID: "w1:t1", WorkspaceID: "w1", State: store.Started, OutputMode: config.OutputModeMerge,
		}); err != nil {
			t.Fatal(err)
		}
		env.eng.GitMerge = func(_ context.Context, _, _, _, _ string) (verify.LandedCommit, string, error) {
			return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
		}
		if err := env.eng.Settle(context.Background(), []string{"PRJ-1"}); err != nil {
			t.Fatalf("Settle() error = %v", err)
		}
		if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
			t.Fatalf("claim = %+v", got)
		}
		if got := env.herd.closedTabs(); len(got) != 0 {
			t.Fatalf("closed = %v", got)
		}
	})

	t.Run("ErrTabNotFound", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
		env.eng.Config.Sources[0].Output = config.Output{Mode: config.OutputModeMerge}
		env.herd.tabs = []herdr.Tab{{ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1", AgentStatus: herdr.AgentStatusIdle}}
		env.herd.closeErr = herdr.ErrTabNotFound
		if err := env.st.InsertClaim(context.Background(), store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: env.now.Add(-time.Hour), UpdatedAt: env.now.Add(-time.Hour),
			Machine: "test-host", TabID: "w1:t1", WorkspaceID: "w1", State: store.Started, OutputMode: config.OutputModeMerge,
		}); err != nil {
			t.Fatal(err)
		}
		env.eng.GitMerge = func(_ context.Context, _, _, _, _ string) (verify.LandedCommit, string, error) {
			return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
		}
		if err := env.eng.Settle(context.Background(), []string{"PRJ-1"}); err != nil {
			t.Fatalf("Settle() error = %v", err)
		}
		if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
			t.Fatalf("claim = %+v", got)
		}
	})

	t.Run("subagent", func(t *testing.T) {
		env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
		env.eng.Config.Sources[0].Output = config.Output{Mode: config.OutputModeMerge}
		if err := env.st.InsertClaim(context.Background(), store.Claim{
			Source: "jira:acme", ID: "PRJ-1", ClaimedAt: env.now.Add(-time.Hour), UpdatedAt: env.now.Add(-time.Hour),
			Machine: "test-host", State: store.Started, OutputMode: config.OutputModeMerge, Spawner: config.SpawnerSubagent,
		}); err != nil {
			t.Fatal(err)
		}
		env.eng.GitMerge = func(_ context.Context, _, _, _, _ string) (verify.LandedCommit, string, error) {
			return verify.LandedCommit{SHA: "abc1234", Subject: "[PRJ-1] Ship"}, "", nil
		}
		if err := env.eng.Settle(context.Background(), []string{"PRJ-1"}); err != nil {
			t.Fatalf("Settle() error = %v", err)
		}
		if got := claimRow(t, env.st, "PRJ-1"); got.State != store.Done {
			t.Fatalf("claim = %+v", got)
		}
		if got := env.herd.closedTabs(); len(got) != 0 {
			t.Fatalf("closed = %v", got)
		}
	})
}

func TestSettlePRModeLeavesWorkingTabOpenEvenWithCallerPane(t *testing.T) {
	ctx := context.Background()
	env := newTickEnv(t, []source.Item{readyItem("PRJ-1", "Ship")}, true, true)
	env.eng.Config.CallerPaneID = "w1:p1"
	env.herd.tabs = []herdr.Tab{{
		ID: "w1:t1", WorkspaceID: "w1", Label: "PRJ-1",
		AgentStatus: herdr.AgentStatusWorking, PaneCount: 1,
	}}
	env.herd.panes = []herdr.Pane{{
		ID: "w1:p1", TabID: "w1:t1", WorkspaceID: "w1", AgentStatus: herdr.AgentStatusWorking,
	}}
	env.pulls.merged = ghPR("[PRJ-1] ship")
	seedClaim(t, env.st, "PRJ-1", store.Started, env.now, "w1:t1", "w1")

	if err := env.eng.Settle(ctx, nil); err != nil {
		t.Fatal(err)
	}
	if got := env.herd.closedTabs(); len(got) != 0 {
		t.Fatalf("closed = %v", got)
	}
	if env.out.String() != "left tab PRJ-1 (w1:t1) open: agent is working\n" {
		t.Fatalf("stdout = %q", env.out.String())
	}
}
