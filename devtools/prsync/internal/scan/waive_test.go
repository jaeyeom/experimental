package scan

import (
	"context"
	"encoding/json"
	"os/exec"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

func TestMatchLatestWaiveComment(t *testing.T) {
	t.Parallel()

	t0 := time.Date(2026, 2, 1, 10, 0, 0, 0, time.UTC)
	comments := []gh.WaiveComment{
		{ID: "1", Author: "alice", Body: "LGTM\n/waive", CreatedAt: t0},
		{ID: "2", Author: "alice", Body: "/waive extra", CreatedAt: t0.Add(time.Minute)},
		{ID: "3", Author: "alice", Body: "/waive\nfirst valid", CreatedAt: t0.Add(2 * time.Minute)},
		{ID: "4", Author: "mallory", Body: "/waive\nuntrusted author", CreatedAt: t0.Add(3 * time.Minute)},
		{ID: "5", Author: "Bob", Body: "/waive\r\nsecond valid\r\n", CreatedAt: t0.Add(4 * time.Minute)},
		{ID: "6", Author: "alice", Body: "unrelated comment", CreatedAt: t0.Add(5 * time.Minute)},
	}

	t.Run("matches exact first line and authorized author and latest wins", func(t *testing.T) {
		t.Parallel()
		got, ok := MatchLatestWaiveComment(comments, "/waive", []string{"alice", "bob"})
		if !ok {
			t.Fatal("expected match, got false")
		}
		if got.ID != "5" {
			t.Fatalf("matched ID = %q, want 5", got.ID)
		}
		if got.Body != "/waive\r\nsecond valid\r\n" {
			t.Fatalf("body = %q, want exact byte-for-byte body", got.Body)
		}
	})

	t.Run("empty authors allows any author", func(t *testing.T) {
		t.Parallel()
		got, ok := MatchLatestWaiveComment(comments[:4], "/waive", nil)
		if !ok {
			t.Fatal("expected match, got false")
		}
		if got.ID != "4" {
			t.Fatalf("matched ID = %q, want 4", got.ID)
		}
	})

	t.Run("empty prefix turns detection off", func(t *testing.T) {
		t.Parallel()
		if _, ok := MatchLatestWaiveComment(comments, "", nil); ok {
			t.Fatal("expected false for empty prefix")
		}
	})

	t.Run("no matching first line", func(t *testing.T) {
		t.Parallel()
		if _, ok := MatchLatestWaiveComment(comments[:2], "/waive", nil); ok {
			t.Fatal("expected false when only non-first-line or non-exact lines exist")
		}
	})
}

func TestResolveWaivedHead(t *testing.T) {
	t.Parallel()

	t0 := time.Date(2026, 2, 1, 10, 0, 0, 0, time.UTC)

	t.Run("commit before waive then merge after", func(t *testing.T) {
		t.Parallel()
		commits := []gh.PRCommit{
			{OID: "c1", CommittedAt: t0, PushedAt: t0},
			{OID: "c2", CommittedAt: t0.Add(5 * time.Minute), PushedAt: t0.Add(5 * time.Minute)},
			{OID: "m1", CommittedAt: t0.Add(20 * time.Minute), PushedAt: t0.Add(20 * time.Minute), Parents: []string{"c2", "b1"}},
		}
		old, fp := ResolveWaivedHead("m1", t0.Add(10*time.Minute), commits, nil)
		if old != "c2" || fp {
			t.Fatalf("got (%q, %v), want (c2, false)", old, fp)
		}
	})

	t.Run("pushedDate wins over earlier committedDate", func(t *testing.T) {
		t.Parallel()
		commits := []gh.PRCommit{
			{OID: "c1", CommittedAt: t0, PushedAt: t0},
			// Committed locally before waive, pushed after waive.
			{OID: "c2", CommittedAt: t0.Add(2 * time.Minute), PushedAt: t0.Add(15 * time.Minute)},
		}
		old, fp := ResolveWaivedHead("c2", t0.Add(10*time.Minute), commits, nil)
		if old != "c1" || fp {
			t.Fatalf("got (%q, %v), want (c1, false)", old, fp)
		}
	})

	t.Run("force-push after waive yields beforeCommit", func(t *testing.T) {
		t.Parallel()
		commits := []gh.PRCommit{
			{OID: "rebased1", CommittedAt: t0, PushedAt: t0.Add(20 * time.Minute)},
		}
		fps := []gh.ForcePushEvent{
			{CreatedAt: t0.Add(20 * time.Minute), BeforeOID: "old1", AfterOID: "rebased1"},
		}
		old, fp := ResolveWaivedHead("rebased1", t0.Add(10*time.Minute), commits, fps)
		if old != "old1" || !fp {
			t.Fatalf("got (%q, %v), want (old1, true)", old, fp)
		}
	})

	t.Run("current head already waived is not stale", func(t *testing.T) {
		t.Parallel()
		commits := []gh.PRCommit{
			{OID: "c1", CommittedAt: t0, PushedAt: t0},
		}
		old, fp := ResolveWaivedHead("c1", t0.Add(5*time.Minute), commits, nil)
		if old != "c1" || fp {
			t.Fatalf("got (%q, %v), want (c1, false)", old, fp)
		}
	})
}

func TestFailingWaivedChecks(t *testing.T) {
	t.Parallel()

	checks := []gh.StatusCheck{
		{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
		{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
		{Name: "ci/build", Status: "COMPLETED", Conclusion: "TIMED_OUT"},
		{Context: "legacy/status", State: "FAILURE"},
	}

	got := FailingWaivedChecks(checks, "", []string{"ci/flake", "legacy/status", "ci/lint"})
	want := []string{"ci/flake", "legacy/status"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("FailingWaivedChecks() = %v, want %v", got, want)
	}

	allFailing := FailingWaivedChecks(checks, "", nil)
	if len(allFailing) != 3 {
		t.Fatalf("FailingWaivedChecks(empty filter) = %v, want 3 failing checks", allFailing)
	}

	if gotErr := FailingWaivedChecks(checks, "rollup failed", []string{"ci/flake"}); len(gotErr) != 0 {
		t.Fatalf("FailingWaivedChecks(with error) = %v, want empty", gotErr)
	}
}

func TestEvaluateRequiredChecks(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		checks   []gh.StatusCheck
		required []string
		want     RequiredCheckOutcome
	}{
		{
			name:     "empty required list",
			checks:   []gh.StatusCheck{{Name: "ci/lint", Status: "IN_PROGRESS"}},
			required: nil,
		},
		{
			name: "all green across check run and status context",
			checks: []gh.StatusCheck{
				{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Context: "ci/unit", State: "SUCCESS"},
			},
			required: []string{"ci/lint", "ci/unit"},
		},
		{
			name: "pending check run",
			checks: []gh.StatusCheck{
				{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
				{Name: "ci/unit", Status: "IN_PROGRESS"},
			},
			required: []string{"ci/lint", "ci/unit"},
			want:     RequiredCheckOutcome{Pending: []string{"ci/unit"}},
		},
		{
			name: "pending status context",
			checks: []gh.StatusCheck{
				{Context: "ci/unit", State: "PENDING"},
			},
			required: []string{"ci/unit"},
			want:     RequiredCheckOutcome{Pending: []string{"ci/unit"}},
		},
		{
			name: "failing check run and status context",
			checks: []gh.StatusCheck{
				{Name: "ci/lint", Status: "COMPLETED", Conclusion: "FAILURE"},
				{Context: "ci/unit", State: "ERROR"},
			},
			required: []string{"ci/lint", "ci/unit"},
			want:     RequiredCheckOutcome{Failing: []string{"ci/lint", "ci/unit"}},
		},
		{
			name: "completed non-success conclusion counts as failing",
			checks: []gh.StatusCheck{
				{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SKIPPED"},
			},
			required: []string{"ci/lint"},
			want:     RequiredCheckOutcome{Failing: []string{"ci/lint"}},
		},
		{
			name: "missing check",
			checks: []gh.StatusCheck{
				{Name: "ci/lint", Status: "COMPLETED", Conclusion: "SUCCESS"},
			},
			required: []string{"ci/lint", "ci/unit"},
			want:     RequiredCheckOutcome{Missing: []string{"ci/unit"}},
		},
		{
			name: "deduplicates repeated required names",
			checks: []gh.StatusCheck{
				{Name: "ci/lint", Status: "IN_PROGRESS"},
			},
			required: []string{"ci/lint", "ci/lint"},
			want:     RequiredCheckOutcome{Pending: []string{"ci/lint"}},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := EvaluateRequiredChecks(tc.checks, tc.required)
			if !slices.Equal(got.Failing, tc.want.Failing) ||
				!slices.Equal(got.Pending, tc.want.Pending) ||
				!slices.Equal(got.Missing, tc.want.Missing) {
				t.Fatalf("EvaluateRequiredChecks() = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestScanDetectsAndReportsStaleWaives(t *testing.T) {
	t.Parallel()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not found")
	}

	fx := newTempWaiveRepo(t)
	oldHead, newHead := fx.cleanBaseMerge()

	t0 := time.Date(2026, 2, 1, 10, 0, 0, 0, time.UTC)
	pr := fixturePRs()[0]
	pr.HeadRefOid = newHead
	pr.StatusCheckRollup = []gh.StatusCheck{
		{Name: "ci/flake", Status: "COMPLETED", Conclusion: "FAILURE"},
		{Name: "ci/other", Status: "COMPLETED", Conclusion: "FAILURE"},
	}
	draft := fixtureDraftPR()
	draft.HeadRefOid = newHead

	g := &scriptGH{
		list: map[string]listResult{
			"acme/widgets": {prs: []gh.PRListItem{pr, draft}},
		},
		threads: map[int][]gh.Thread{123: fixtureThreads(), 200: {}},
		waives: map[int]gh.WaiveTimeline{
			123: {
				Comments: []gh.WaiveComment{
					{ID: "901", Author: "maintainer", Body: "/waive\nknown flake\n", CreatedAt: t0.Add(5 * time.Minute), URL: "https://example/901"},
				},
				Commits: []gh.PRCommit{
					{OID: oldHead, CommittedAt: t0, PushedAt: t0},
					{OID: newHead, CommittedAt: t0.Add(10 * time.Minute), PushedAt: t0.Add(10 * time.Minute), Parents: []string{oldHead, fx.baseTip}},
				},
			},
			// PR 200 will be marked out of scope by TabMachine=m2, so its waive must not be queried.
			200: {
				Comments: []gh.WaiveComment{
					{ID: "902", Author: "maintainer", Body: "/waive", CreatedAt: t0.Add(5 * time.Minute)},
				},
			},
		},
	}

	cfg := config.Defaults()
	cfg.Author = "alice"
	cfg.Repos = []string{"acme/widgets"}
	cfg.Machines = []string{"m2"}
	cfg.TabMachine = "local"
	cfg.Untabbed = "exclude"
	cfg.WaiveCommentPrefix = "/waive"
	cfg.WaiveAuthors = []string{"maintainer"}
	cfg.WaiveChecks = []string{"ci/flake"}

	// Only PR 123 has a local tab (PR 200 has no tab -> out of scope under untabbed=exclude).
	doc, err := Run(context.Background(), Deps{
		GH:       g,
		Herdr:    fixtureHerdr{},
		RepoDirs: map[string]string{"acme/widgets": fx.dir},
	}, cfg, nil, fixtureNow)
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if g.waiveCalls[200] != 0 {
		t.Fatalf("out-of-scope PR 200 had %d waive timeline calls, want 0", g.waiveCalls[200])
	}
	if doc.PRs[0].StaleWaive == nil {
		t.Fatal("PR 123 StaleWaive = nil, want populated")
	}
	sw := doc.PRs[0].StaleWaive
	if sw.OldHead != oldHead || sw.NewHead != newHead || sw.Class != WaiveClassBaseOnly {
		t.Fatalf("StaleWaive = %+v", sw)
	}
	if strings.Join(sw.FailingChecks, ",") != "ci/flake" {
		t.Fatalf("FailingChecks = %v, want [ci/flake]", sw.FailingChecks)
	}
	if sw.CommentBody != "/waive\nknown flake\n" {
		t.Fatalf("CommentBody = %q", sw.CommentBody)
	}

	raw, err := json.Marshal(doc)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(raw), `"stale_waive"`) {
		t.Fatalf("JSON missing stale_waive: %s", raw)
	}

	text := FormatStaleWaiveLines(doc)
	for _, want := range []string{"acme/widgets#123", oldHead, newHead, "base-only", "ci/flake"} {
		if !strings.Contains(text, want) {
			t.Fatalf("FormatStaleWaiveLines() = %q, missing %q", text, want)
		}
	}
}

type fakeCompareGH struct {
	statuses map[string]struct{ status, mb string }
	diffs    map[string][]byte
}

func (f fakeCompareGH) CompareCommits(_ context.Context, _, _, base, head string) (string, string, error) {
	v := f.statuses[base+"..."+head]
	return v.status, v.mb, nil
}

func (f fakeCompareGH) CompareDiff(_ context.Context, _, _, base, head string) ([]byte, error) {
	return f.diffs[base+".."+head], nil
}

func TestClassifyWithGHFallback(t *testing.T) {
	t.Parallel()

	commits := []gh.PRCommit{
		{OID: "a1", Parents: []string{"b0"}},
		{OID: "m1", Parents: []string{"a1", "b1"}},
	}
	cmp := fakeCompareGH{
		statuses: map[string]struct{ status, mb string }{
			"b1...main": {status: "identical", mb: "b1"},
			"main...a1": {status: "diverged", mb: "b0"},
			"main...m1": {status: "ahead", mb: "b1"},
		},
		diffs: map[string][]byte{
			"b0..a1": []byte("diff --git a/f b/f\n+feat\n"),
			"b1..m1": []byte("diff --git a/f b/f\n+feat\n"),
		},
	}
	got, err := classifyWithGH(context.Background(), cmp, "acme", "widgets", "main", "a1", "m1", commits)
	if err != nil || got != WaiveClassBaseOnly {
		t.Fatalf("got (%q, %v), want %q", got, err, WaiveClassBaseOnly)
	}

	// Changing diff triggers author changes.
	cmp.diffs["b1..m1"] = []byte("diff --git a/f b/f\n+edited\n")
	got, err = classifyWithGH(context.Background(), cmp, "acme", "widgets", "main", "a1", "m1", commits)
	if err != nil || got != WaiveClassAuthorChanges {
		t.Fatalf("changed diff: got (%q, %v), want %q", got, err, WaiveClassAuthorChanges)
	}

	// Non-base merge parent triggers author changes.
	cmp.diffs["b1..m1"] = cmp.diffs["b0..a1"]
	cmp.statuses["b1...main"] = struct{ status, mb string }{status: "diverged", mb: "b0"}
	got, err = classifyWithGH(context.Background(), cmp, "acme", "widgets", "main", "a1", "m1", commits)
	if err != nil || got != WaiveClassAuthorChanges {
		t.Fatalf("non-base parent: got (%q, %v), want %q", got, err, WaiveClassAuthorChanges)
	}
}
