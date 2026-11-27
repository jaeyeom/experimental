package policy

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

type postedComment struct {
	repo   string
	number int
	body   string
}

type fakeBotGH struct {
	snapshots map[string]gh.BotRereviewSnapshot
	snapCalls int
	snapErr   error
	commentID string
	postErr   error
	posted    []postedComment
}

func (f *fakeBotGH) BotRereviewSnapshot(_ context.Context, owner, repo string, number int) (gh.BotRereviewSnapshot, error) {
	f.snapCalls++
	if f.snapErr != nil {
		return gh.BotRereviewSnapshot{}, f.snapErr
	}
	key := fmt.Sprintf("%s/%s#%d", owner, repo, number)
	snap, ok := f.snapshots[key]
	if !ok {
		return gh.BotRereviewSnapshot{}, fmt.Errorf("missing snapshot for %s", key)
	}
	return snap, nil
}

func (f *fakeBotGH) CommentPRWithID(_ context.Context, repo string, number int, body string) (string, error) {
	if f.postErr != nil {
		return "", f.postErr
	}
	f.posted = append(f.posted, postedComment{repo: repo, number: number, body: body})
	return f.commentID, nil
}

func botRereviewCfg(mode string) config.Config {
	cfg := config.Defaults()
	cfg.Policy.BotRereview = mode
	cfg.Policy.BotRereviewLogin = "coderabbitai[bot]"
	cfg.Policy.BotRereviewBody = "@coderabbitai review"
	return cfg
}

func eligibleSnapshot(headSHA string, now time.Time) gh.BotRereviewSnapshot {
	return gh.BotRereviewSnapshot{
		HeadSHA:      headSHA,
		LastCommitAt: now.Add(-10 * time.Minute),
		Reviews: []gh.Review{
			{Author: "coderabbitai", State: "CHANGES_REQUESTED", SubmittedAt: now.Add(-30 * time.Minute)},
		},
		Threads: []gh.ReviewThreadSummary{
			{ID: "t1", IsResolved: true, StarterAuthor: "coderabbitai"},
			{ID: "t2", IsResolved: false, StarterAuthor: "human-reviewer"},
		},
		Comments: []gh.IssueComment{
			{Body: "@coderabbitai review", CreatedAt: now.Add(-20 * time.Minute)}, // older than last commit
			{Body: "unrelated comment", CreatedAt: now.Add(-2 * time.Minute)},
		},
	}
}

func TestBotRereviewEachConditionFalseAlonePreventsPost(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC)

	tests := []struct {
		name       string
		mutate     func(*gh.BotRereviewSnapshot)
		wantReason string
	}{
		{
			name: "bot has not reviewed (only human reviewed)",
			mutate: func(s *gh.BotRereviewSnapshot) {
				s.Reviews = []gh.Review{
					{Author: "alice", State: "COMMENTED", SubmittedAt: now.Add(-20 * time.Minute)},
				}
			},
			wantReason: "bot_has_not_reviewed",
		},
		{
			name: "bot has not reviewed (pending review only)",
			mutate: func(s *gh.BotRereviewSnapshot) {
				s.Reviews = []gh.Review{
					{Author: "coderabbitai", State: "PENDING"},
				}
			},
			wantReason: "bot_has_not_reviewed",
		},
		{
			name: "bot latest review is APPROVED",
			mutate: func(s *gh.BotRereviewSnapshot) {
				s.Reviews = []gh.Review{
					{Author: "coderabbitai", State: "CHANGES_REQUESTED", SubmittedAt: now.Add(-40 * time.Minute)},
					{Author: "coderabbitai", State: "APPROVED", SubmittedAt: now.Add(-15 * time.Minute)},
				}
			},
			wantReason: "latest_review_approved",
		},
		{
			name: "unresolved review thread started by bot",
			mutate: func(s *gh.BotRereviewSnapshot) {
				s.Threads = []gh.ReviewThreadSummary{
					{ID: "t1", IsResolved: true, StarterAuthor: "coderabbitai"},
					{ID: "t2", IsResolved: false, StarterAuthor: "coderabbitai[bot]"},
				}
			},
			wantReason: "unresolved_bot_threads",
		},
		{
			name: "comment with same trimmed body newer than last commit",
			mutate: func(s *gh.BotRereviewSnapshot) {
				s.Comments = []gh.IssueComment{
					{Body: "  @coderabbitai review \n", CreatedAt: now.Add(-5 * time.Minute)},
				}
			},
			wantReason: "comment_newer_than_last_commit",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			pr := basePR()
			pr.MergeStateStatus = "CLEAN"
			snap := eligibleSnapshot(pr.HeadSHA, now)
			tc.mutate(&snap)

			fgh := &fakeBotGH{
				snapshots: map[string]gh.BotRereviewSnapshot{"acme/widgets#10": snap},
				commentID: "c1",
			}
			st := &ledger.Store{}
			eng := &Engine{GH: fgh, Ledger: st, Now: func() time.Time { return now }}
			doc, err := eng.Run(context.Background(), botRereviewCfg(config.PolicyAuto), scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
			if err != nil {
				t.Fatalf("Run() error = %v", err)
			}
			if len(doc.Decisions) != 1 {
				t.Fatalf("decisions = %+v, want 1", doc.Decisions)
			}
			got := doc.Decisions[0]
			if got.Decision != DecisionSkip || got.Reason != tc.wantReason {
				t.Fatalf("decision = %+v, want skip/%s", got, tc.wantReason)
			}
			if len(fgh.posted) != 0 {
				t.Fatalf("posted = %+v, want 0 posts", fgh.posted)
			}
			entries, err := st.Load()
			if err != nil || len(entries) != 0 {
				t.Fatalf("ledger entries = %+v, err=%v, want empty", entries, err)
			}
		})
	}
}

func TestBotRereviewPostsOncePerHeadAndDedupe(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC)
	pr := basePR()
	pr.MergeStateStatus = "CLEAN"
	pr.Tab = nil // no herdr tab required

	fgh := &fakeBotGH{
		snapshots: map[string]gh.BotRereviewSnapshot{
			"acme/widgets#10": eligibleSnapshot(pr.HeadSHA, now),
		},
		commentID: "777",
	}
	st := &ledger.Store{}
	eng := &Engine{GH: fgh, Ledger: st, Now: func() time.Time { return now }}
	cfg := botRereviewCfg(config.PolicyAuto)

	// 1. All 4 conditions hold -> posts once and records ledger.
	doc1, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
	if err != nil {
		t.Fatalf("first Run() error = %v", err)
	}
	if len(doc1.Decisions) != 1 || doc1.Decisions[0].Decision != DecisionDispatch || doc1.Decisions[0].Kind != KindBotRereview {
		t.Fatalf("first decisions = %+v", doc1.Decisions)
	}
	if len(fgh.posted) != 1 || fgh.posted[0].body != "@coderabbitai review" {
		t.Fatalf("posted = %+v", fgh.posted)
	}
	entries, err := st.Load()
	if err != nil || len(entries) != 1 || entries[0].Kind != KindBotRereview || entries[0].HeadSHA != pr.HeadSHA || entries[0].CommentID != "777" {
		t.Fatalf("ledger = %+v, err=%v", entries, err)
	}

	// 2. Second run on same head -> deduped by ledger without calling GitHub again.
	prevCalls := fgh.snapCalls
	doc2, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
	if err != nil {
		t.Fatalf("second Run() error = %v", err)
	}
	if len(doc2.Decisions) != 1 || doc2.Decisions[0].Decision != DecisionSkip || doc2.Decisions[0].Reason != "already_dispatched" {
		t.Fatalf("second decisions = %+v", doc2.Decisions)
	}
	if fgh.snapCalls != prevCalls {
		t.Fatalf("snapCalls increased from %d to %d despite ledger dedupe", prevCalls, fgh.snapCalls)
	}
	if len(fgh.posted) != 1 {
		t.Fatalf("posted = %+v, want still 1", fgh.posted)
	}

	// 3. New head SHA -> posts again once!
	pr.HeadSHA = "def9876543210"
	fgh.snapshots["acme/widgets#10"] = eligibleSnapshot(pr.HeadSHA, now.Add(time.Hour))
	doc3, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
	if err != nil {
		t.Fatalf("third Run() error = %v", err)
	}
	if len(doc3.Decisions) != 1 || doc3.Decisions[0].Decision != DecisionDispatch {
		t.Fatalf("third decisions = %+v", doc3.Decisions)
	}
	if len(fgh.posted) != 2 {
		t.Fatalf("posted = %+v, want 2", fgh.posted)
	}
}

func TestBotRereviewModesAndLoginMatching(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC)

	t.Run("off mode makes no GitHub calls", func(t *testing.T) {
		t.Parallel()
		pr := basePR()
		fgh := &fakeBotGH{}
		eng := &Engine{GH: fgh, Ledger: &ledger.Store{}}
		doc, err := eng.Run(context.Background(), botRereviewCfg(config.PolicyOff), scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(doc.Decisions) != 0 || fgh.snapCalls != 0 || len(fgh.posted) != 0 {
			t.Fatalf("off mode did work: dec=%+v calls=%d posted=%+v", doc.Decisions, fgh.snapCalls, fgh.posted)
		}
	})

	t.Run("dry-run previews without posting or writing ledger", func(t *testing.T) {
		t.Parallel()
		pr := basePR()
		fgh := &fakeBotGH{snapshots: map[string]gh.BotRereviewSnapshot{"acme/widgets#10": eligibleSnapshot(pr.HeadSHA, now)}}
		st := &ledger.Store{}
		eng := &Engine{GH: fgh, Ledger: st, Now: func() time.Time { return now }}
		doc, err := eng.Run(context.Background(), botRereviewCfg(config.PolicyDryRun), scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(doc.Decisions) != 1 || doc.Decisions[0].Decision != DecisionDryRun || doc.Decisions[0].Prompt != "@coderabbitai review" {
			t.Fatalf("decisions = %+v", doc.Decisions)
		}
		if len(fgh.posted) != 0 {
			t.Fatalf("dry-run posted = %+v", fgh.posted)
		}
		if entries, _ := st.Load(); len(entries) != 0 {
			t.Fatalf("dry-run wrote ledger: %+v", entries)
		}
	})

	t.Run("RunOptions.DryRun overrides auto mode", func(t *testing.T) {
		t.Parallel()
		pr := basePR()
		fgh := &fakeBotGH{snapshots: map[string]gh.BotRereviewSnapshot{"acme/widgets#10": eligibleSnapshot(pr.HeadSHA, now)}}
		st := &ledger.Store{}
		eng := &Engine{GH: fgh, Ledger: st, Now: func() time.Time { return now }}
		doc, err := eng.Run(context.Background(), botRereviewCfg(config.PolicyAuto), scan.Document{PRs: []scan.PR{pr}}, RunOptions{DryRun: true})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(doc.Decisions) != 1 || doc.Decisions[0].Decision != DecisionDryRun {
			t.Fatalf("decisions = %+v", doc.Decisions)
		}
		if len(fgh.posted) != 0 {
			t.Fatalf("posted = %+v", fgh.posted)
		}
	})

	t.Run("latest non-approved after earlier approved posts, and case-insensitive [bot] matching", func(t *testing.T) {
		t.Parallel()
		pr := basePR()
		snap := gh.BotRereviewSnapshot{
			HeadSHA:      pr.HeadSHA,
			LastCommitAt: now.Add(-5 * time.Minute),
			Reviews: []gh.Review{
				{Author: "CodeRabbitAI[bot]", State: "APPROVED", SubmittedAt: now.Add(-30 * time.Minute)},
				{Author: "coderabbitai", State: "COMMENTED", SubmittedAt: now.Add(-20 * time.Minute)},
			},
			Threads: []gh.ReviewThreadSummary{
				{ID: "t1", IsResolved: true, StarterAuthor: "CodeRabbitAI"},
			},
		}
		cfg := botRereviewCfg(config.PolicyAuto)
		cfg.Policy.BotRereviewLogin = "coderabbitai"
		fgh := &fakeBotGH{snapshots: map[string]gh.BotRereviewSnapshot{"acme/widgets#10": snap}, commentID: "99"}
		eng := &Engine{GH: fgh, Ledger: &ledger.Store{}, Now: func() time.Time { return now }}
		doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(doc.Decisions) != 1 || doc.Decisions[0].Decision != DecisionDispatch {
			t.Fatalf("decisions = %+v", doc.Decisions)
		}
	})

	t.Run("does not consume MaxPerRun or DailyCap", func(t *testing.T) {
		t.Parallel()
		pr1 := basePR()
		pr2 := basePR()
		pr2.Number = 11
		pr2.HeadSHA = "head11"
		fgh := &fakeBotGH{
			snapshots: map[string]gh.BotRereviewSnapshot{
				"acme/widgets#10": eligibleSnapshot(pr1.HeadSHA, now),
				"acme/widgets#11": eligibleSnapshot(pr2.HeadSHA, now),
			},
			commentID: "100",
		}
		cfg := botRereviewCfg(config.PolicyAuto)
		cfg.Policy.MaxPerRun = 1
		cfg.Policy.DailyCap = 1
		eng := &Engine{GH: fgh, Ledger: &ledger.Store{}, Now: func() time.Time { return now }}
		doc, err := eng.Run(context.Background(), cfg, scan.Document{PRs: []scan.PR{pr1, pr2}}, RunOptions{})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(doc.Decisions) != 2 || doc.Decisions[0].Decision != DecisionDispatch || doc.Decisions[1].Decision != DecisionDispatch || len(fgh.posted) != 2 {
			t.Fatalf("decisions=%+v posted=%+v, want both dispatched", doc.Decisions, fgh.posted)
		}
	})

	t.Run("post failure records failed ledger entry and DecisionError", func(t *testing.T) {
		t.Parallel()
		pr := basePR()
		fgh := &fakeBotGH{
			snapshots: map[string]gh.BotRereviewSnapshot{"acme/widgets#10": eligibleSnapshot(pr.HeadSHA, now)},
			postErr:   errors.New("boom"),
		}
		st := &ledger.Store{}
		eng := &Engine{GH: fgh, Ledger: st, Now: func() time.Time { return now }}
		doc, err := eng.Run(context.Background(), botRereviewCfg(config.PolicyAuto), scan.Document{PRs: []scan.PR{pr}}, RunOptions{})
		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if len(doc.Decisions) != 1 || doc.Decisions[0].Decision != DecisionError || doc.Decisions[0].Reason != "comment_failed" {
			t.Fatalf("decisions = %+v", doc.Decisions)
		}
		deduped, err := st.HasDispatched("acme/widgets#10", pr.HeadSHA, KindBotRereview)
		if err != nil || deduped {
			t.Fatalf("failed post should not dedupe future runs: deduped=%v err=%v", deduped, err)
		}
	})
}
