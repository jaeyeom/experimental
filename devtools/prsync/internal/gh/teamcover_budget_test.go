package gh

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestPathHistoriesGrowsBatchAfterTwoSuccesses(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 100}
	client, probe := probedAliasClient(t, fake)
	paths := historyPaths(20)
	got, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", paths, time.Now(), 1, 5)
	if err != nil {
		t.Fatal(err)
	}
	if want := []int{1, 1, 2, 2, 4, 4, 5, 1}; !slices.Equal(fake.sizes, want) {
		t.Fatalf("request sizes = %v, want %v", fake.sizes, want)
	}
	if got.Batch != 5 {
		t.Fatalf("Batch = %d, want the configured maximum 5", got.Batch)
	}
	if len(got.Histories) != len(paths) {
		t.Fatalf("histories = %d, want %d", len(got.Histories), len(paths))
	}
}

func TestPathHistoriesStartsAtMostAtMaximum(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 100}
	client, probe := probedAliasClient(t, fake)
	if _, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", historyPaths(6), time.Now(), 20, 3); err != nil {
		t.Fatal(err)
	}
	if want := []int{3, 3}; !slices.Equal(fake.sizes, want) {
		t.Fatalf("request sizes = %v, want %v", fake.sizes, want)
	}
}

func TestPathHistoriesKeepsBatchOfTwoWhenOnlyOnePathFails(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 100, bad: "src/f00.go"}
	client, probe := probedAliasClient(t, fake)
	got, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", historyPaths(2), time.Now(), 2, 5)
	if err != nil {
		t.Fatal(err)
	}
	if got.Batch != 2 {
		t.Fatalf("Batch = %d, want 2: a path failing on its own is no reason to save 1", got.Batch)
	}
}

func TestPathHistoriesSavesOneWhenOnlySinglePathsWork(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 1}
	client, probe := probedAliasClient(t, fake)
	got, err := client.PathHistories(probe.ctx(), "acme/widgets", "main", historyPaths(2), time.Now(), 2, 5)
	if err != nil {
		t.Fatal(err)
	}
	if got.Batch != 1 {
		t.Fatalf("Batch = %d, want 1: two paths fail together and work alone", got.Batch)
	}
}

func TestPathHistoriesStopsAtCallBudget(t *testing.T) {
	t.Parallel()

	fake := &aliasLimitGH{limit: 100}
	client, probe := probedAliasClient(t, fake)
	budget := NewCallBudget(3)
	ctx := WithCallBudget(probe.ctx(), budget)
	paths := historyPaths(10)
	got, err := client.PathHistories(ctx, "acme/widgets", "main", paths, time.Now(), 2, 2)
	if err != nil {
		t.Fatal(err)
	}
	if len(fake.sizes) != 3 || budget.Used() != 3 {
		t.Fatalf("requests = %v used = %d, want 3", fake.sizes, budget.Used())
	}
	if !slices.Equal(got.Deferred, paths[6:]) {
		t.Fatalf("Deferred = %v, want %v", got.Deferred, paths[6:])
	}
	if len(got.Histories) != 6 {
		t.Fatalf("histories = %d, want 6", len(got.Histories))
	}
}

// reviewHistoryGH answers history requests with one commit per path in
// pull request 100+i, and reviews requests with one reviewer per PR.
type reviewHistoryGH struct{ calls int }

func (g *reviewHistoryGH) IsAvailable(string) bool { return true }

func (g *reviewHistoryGH) Execute(_ context.Context, cfg executor.ToolConfig) (*executor.ExecutionResult, error) {
	g.calls++
	query := fmt.Sprint(cfg.Args)
	if !strings.Contains(query, ":history(first:30") {
		return &executor.ExecutionResult{Output: `{"data":{"repository":{}}}`}, nil
	}
	return &executor.ExecutionResult{Output: `{"data":{"repository":{"object":{` +
		`"p0":{"nodes":[{"author":{"user":{"login":"alice"}},"associatedPullRequests":{"nodes":[{"number":100}]}}]}}}}}`}, nil
}

func TestPathHistoriesReservesACallForReviewers(t *testing.T) {
	t.Parallel()

	fake := &reviewHistoryGH{}
	client := NewClient(fake, testGHBin)
	budget := NewCallBudget(3)
	ctx := WithCallBudget(context.Background(), budget)
	paths := historyPaths(5)
	got, err := client.PathHistories(ctx, "acme/widgets", "main", paths, time.Now(), 1, 1)
	if err != nil {
		t.Fatal(err)
	}
	if fake.calls != 3 {
		t.Fatalf("calls = %d, want 2 history and 1 reviews", fake.calls)
	}
	if len(got.Histories) != 2 || !slices.Equal(got.Deferred, paths[2:]) {
		t.Fatalf("histories = %v deferred = %v, want two fetched and three deferred", got.Histories, got.Deferred)
	}
}

// secondaryLimitGH fails every call with GitHub's secondary rate limit.
type secondaryLimitGH struct{ calls int }

func (g *secondaryLimitGH) IsAvailable(string) bool { return true }

func (g *secondaryLimitGH) Execute(context.Context, executor.ToolConfig) (*executor.ExecutionResult, error) {
	g.calls++
	return &executor.ExecutionResult{ExitCode: 1, Stderr: "gh: You have exceeded a secondary rate limit. Please wait a few minutes before you try again.\n"}, nil
}

func TestCallBudgetStopsOnSecondaryRateLimit(t *testing.T) {
	t.Parallel()

	fake := &secondaryLimitGH{}
	client := NewClient(fake, testGHBin)
	client.sleep = func(context.Context, time.Duration) error { return nil }
	budget := NewCallBudget(10)
	ctx := WithCallBudget(context.Background(), budget)
	if _, err := client.DirectReviewCount(ctx, "acme", "bob"); !IsRateLimited(err) {
		t.Fatalf("err = %v, want a rate limit", err)
	}
	if fake.calls != 1 {
		t.Fatalf("calls = %d, want no retry under a budget", fake.calls)
	}
	if !budget.RateLimited() {
		t.Fatal("budget not marked rate limited")
	}
	if _, err := client.TeamMembers(ctx, "acme", "foo"); !IsRateLimited(err) {
		t.Fatalf("err = %v, want the stopped budget's rate limit", err)
	}
	if fake.calls != 1 {
		t.Fatalf("calls = %d, want no call after the rate limit", fake.calls)
	}
}

func TestCallBudgetRefusesCallsPastTheCap(t *testing.T) {
	t.Parallel()

	fake := &reviewHistoryGH{}
	client := NewClient(fake, testGHBin)
	budget := NewCallBudget(1)
	ctx := WithCallBudget(context.Background(), budget)
	if _, err := client.DirectReviewCount(ctx, "acme", "bob"); err != nil {
		t.Fatal(err)
	}
	_, err := client.DirectReviewCount(ctx, "acme", "carol")
	if !errors.Is(err, ErrCallCap) {
		t.Fatalf("err = %v, want ErrCallCap", err)
	}
	if IsRateLimited(err) {
		t.Fatal("the call cap must not read as a rate limit")
	}
	if fake.calls != 1 || budget.Used() != 1 {
		t.Fatalf("calls = %d used = %d, want 1", fake.calls, budget.Used())
	}
}

func TestNilCallBudgetIsUnlimited(t *testing.T) {
	t.Parallel()

	var budget *CallBudget
	if err := budget.Take(); err != nil {
		t.Fatalf("Take = %v, want nil", err)
	}
	if CallBudgetFrom(context.Background()) != nil {
		t.Fatal("budget without WithCallBudget")
	}
}
