package forkkeeper

import (
	"context"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestComputeRangeFirstRun(t *testing.T) {
	fx := newTestFixture(t)
	u0 := fx.uref("refs/heads/main")
	f1 := fx.fcommit("mine.txt", "my patch")
	u1 := fx.ucommit("README", "upstream change")

	w := fx.fetchOnlyWorkdir()
	r, err := ComputeRange(context.Background(), filepath.Join(w, ".git"), "upstream", "main")
	if err != nil {
		t.Fatalf("ComputeRange: %v", err)
	}
	if !r.FirstRun {
		t.Errorf("FirstRun = false, want true")
	}
	if r.UpToDate {
		t.Errorf("UpToDate = true, want false")
	}
	if r.UpstreamOld != u0 {
		t.Errorf("UpstreamOld = %s, want %s", r.UpstreamOld, u0)
	}
	if r.UpstreamNew != u1 {
		t.Errorf("UpstreamNew = %s, want %s", r.UpstreamNew, u1)
	}
	if r.ReviewBase == nil || *r.ReviewBase != u0 {
		t.Errorf("ReviewBase = %v, want %s", r.ReviewBase, u0)
	}
	if !slices.Equal(r.UserCommits, []string{f1}) {
		t.Errorf("UserCommits = %v, want [%s]", r.UserCommits, f1)
	}
	if r.MovedTags == nil || len(r.MovedTags) != 0 {
		t.Errorf("MovedTags = %#v, want non-nil empty slice", r.MovedTags)
	}
	if r.DeletedTags == nil || len(r.DeletedTags) != 0 {
		t.Errorf("DeletedTags = %#v, want non-nil empty slice", r.DeletedTags)
	}
}

func TestComputeRangeUpToDateAndMerge(t *testing.T) {
	fx := newTestFixture(t)
	u0 := fx.uref("refs/heads/main")
	fx.recordHead(u0)
	fx.fcommit("mine.txt", "my patch")

	w := fx.fetchOnlyWorkdir()
	r, err := ComputeRange(context.Background(), filepath.Join(w, ".git"), "upstream", "main")
	if err != nil {
		t.Fatalf("ComputeRange: %v", err)
	}
	if r.FirstRun {
		t.Errorf("FirstRun = true, want false")
	}
	if !r.UpToDate {
		t.Errorf("UpToDate = false, want true")
	}

	// Merge upstream into fork so fork_has_merges triggers.
	u1 := fx.ucommit("README", "u1")
	fx.mustRun(fx.fseed, "git", "fetch", "-q", fx.upstreamGit, "main:refs/remotes/up/main")
	fx.mustRun(fx.fseed, "git", "merge", "-q", "--no-ff", "-m", "merge up", "refs/remotes/up/main")
	fx.mustRun(fx.fseed, "git", "push", "-q", "origin", "main")
	u2 := fx.ucommit("README", "u2")

	w2 := fx.fetchOnlyWorkdir()
	r2, err := ComputeRange(context.Background(), filepath.Join(w2, ".git"), "upstream", "main")
	if err != nil {
		t.Fatalf("ComputeRange after merge: %v", err)
	}
	if !r2.ForkHasMerges {
		t.Errorf("ForkHasMerges = false, want true")
	}
	if r2.UpstreamNew != u2 || r2.UpToDate {
		t.Errorf("unexpected state: new=%s (want %s) upToDate=%v", r2.UpstreamNew, u2, r2.UpToDate)
	}
	_ = u1
}

func TestComputeRangeRewrittenAndBehind(t *testing.T) {
	t.Run("rewritten", func(t *testing.T) {
		fx := newTestFixture(t)
		u0 := fx.uref("refs/heads/main")
		u1 := fx.ucommit("README", "dropped")
		fx.forkAtUpstream()
		fx.recordHead(u1)
		f1 := fx.fcommit("mine.txt", "my patch")
		fx.mustRun(fx.useed, "git", "reset", "-q", "--hard", "HEAD~1")
		u2 := fx.ucommit("README", "replacement")

		w := fx.fetchOnlyWorkdir()
		r, err := ComputeRange(context.Background(), filepath.Join(w, ".git"), "upstream", "main")
		if err != nil {
			t.Fatalf("ComputeRange: %v", err)
		}
		if !r.Rewritten {
			t.Errorf("Rewritten = false, want true")
		}
		if r.ReviewBase == nil || *r.ReviewBase != u0 {
			t.Errorf("ReviewBase = %v, want %s", r.ReviewBase, u0)
		}
		if r.UpstreamOld != u1 || r.UpstreamNew != u2 {
			t.Errorf("old=%s new=%s, want old=%s new=%s", r.UpstreamOld, r.UpstreamNew, u1, u2)
		}
		// Dropped upstream commit u1 must not be treated as a user commit.
		if !slices.Equal(r.UserCommits, []string{f1}) {
			t.Errorf("UserCommits = %v, want [%s]", r.UserCommits, f1)
		}
	})

	t.Run("fork-behind-recorded-head", func(t *testing.T) {
		fx := newTestFixture(t)
		u0 := fx.uref("refs/heads/main")
		u1 := fx.ucommit("README", "u1")
		fx.recordHead(u1)
		u2 := fx.ucommit("README", "u2")

		w := fx.fetchOnlyWorkdir()
		r, err := ComputeRange(context.Background(), filepath.Join(w, ".git"), "upstream", "main")
		if err != nil {
			t.Fatalf("ComputeRange: %v", err)
		}
		if !r.ForkBehindRecordedHead {
			t.Errorf("ForkBehindRecordedHead = false, want true")
		}
		if r.ReviewBase == nil || *r.ReviewBase != u0 {
			t.Errorf("ReviewBase = %v, want %s", r.ReviewBase, u0)
		}
		if r.UpstreamNew != u2 {
			t.Errorf("UpstreamNew = %s, want %s", r.UpstreamNew, u2)
		}
	})
}

func TestComputeRangeTagsAndErrors(t *testing.T) {
	fx := newTestFixture(t)
	u0 := fx.uref("refs/heads/main")
	fx.recordHead(u0)
	fx.mustRun("", "git", "--git-dir="+fx.originGit, "update-ref", "refs/fork-keeper/upstream-tags/v1", u0)
	fx.mustRun("", "git", "--git-dir="+fx.originGit, "update-ref", "refs/fork-keeper/upstream-tags/v0", u0)
	u1 := fx.ucommit("README", "docs")
	fx.mustRun("", "git", "--git-dir="+fx.upstreamGit, "update-ref", "refs/tags/v1", u1)

	w := fx.fetchOnlyWorkdir()
	gitDir := filepath.Join(w, ".git")
	r, err := ComputeRange(context.Background(), gitDir, "upstream", "main")
	if err != nil {
		t.Fatalf("ComputeRange: %v", err)
	}
	if len(r.MovedTags) != 1 || r.MovedTags[0] != (MovedTag{Name: "v1", Was: u0, Now: u1}) {
		t.Errorf("MovedTags = %+v", r.MovedTags)
	}
	if !slices.Equal(r.DeletedTags, []string{"v0"}) {
		t.Errorf("DeletedTags = %v, want [v0]", r.DeletedTags)
	}

	diff, err := BuildUpstreamDiff(context.Background(), gitDir, r)
	if err != nil {
		t.Fatalf("BuildUpstreamDiff: %v", err)
	}
	if !strings.Contains(diff, "+docs") {
		t.Errorf("diff missing +docs:\n%s", diff)
	}
	summary := BuildUpstreamSummary(context.Background(), gitDir, r)
	if !strings.Contains(summary, "upstream: README") || !strings.Contains(summary, "README") {
		t.Errorf("summary missing expected content:\n%s", summary)
	}

	if _, err := ComputeRange(context.Background(), gitDir, "upstream", "missing-branch"); err == nil {
		t.Fatalf("expected error for missing upstream branch")
	}
	if _, err := ComputeRange(context.Background(), gitDir, "-bad", "main"); err == nil {
		t.Fatalf("expected error for invalid upstream remote")
	}

	rNilBase := r
	rNilBase.ReviewBase = nil
	diffNil, err := BuildUpstreamDiff(context.Background(), gitDir, rNilBase)
	if err != nil || !strings.Contains(diffNil, "unrelated") {
		t.Errorf("BuildUpstreamDiff nil base = %q, err=%v", diffNil, err)
	}
}
