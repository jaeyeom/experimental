package worker

import (
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
)

const testCatalogJSON = `{
  "version": 1,
  "skills": {
    "review": {
      "description": "Review repository code.",
      "mode": "readonly",
      "skillPath": ".agent-pool/skills/review/SKILL.md",
      "argsSchema": {
        "type": "object",
        "properties": {},
        "additionalProperties": false
      },
      "outputSchema": {
        "type": "object",
        "properties": {},
        "additionalProperties": false
      }
    }
  }
}
`

type testRepoFixture struct {
	bareDir       string
	defaultBranch string
	commitA       string
	commitB       string
	featureCommit string
}

func setupTestRepo(t *testing.T, defaultBranch string) testRepoFixture {
	t.Helper()

	root := t.TempDir()
	bareDir := filepath.Join(root, "remote.git")
	workDir := filepath.Join(root, "work")

	runGitTest(t, "", "init", "--bare", bareDir)
	runGitTest(t, bareDir, "symbolic-ref", "HEAD", "refs/heads/"+defaultBranch)

	runGitTest(t, "", "init", workDir)
	runGitTest(t, workDir, "checkout", "-b", defaultBranch)
	runGitTest(t, workDir, "config", "user.name", "Test User")
	runGitTest(t, workDir, "config", "user.email", "test@example.com")
	runGitTest(t, workDir, "remote", "add", "origin", bareDir)

	// Commit A: valid catalog and skill on default branch.
	writeTestFile(t, filepath.Join(workDir, ".agent-pool", "catalog.json"), testCatalogJSON)
	writeTestFile(t, filepath.Join(workDir, ".agent-pool", "skills", "review", "SKILL.md"), "# Review skill\n")
	runGitTest(t, workDir, "add", ".")
	runGitTest(t, workDir, "commit", "-m", "commit A: add catalog")
	commitA := strings.TrimSpace(runGitTest(t, workDir, "rev-parse", "HEAD"))

	// Commit B: newer commit on default branch.
	writeTestFile(t, filepath.Join(workDir, "later.txt"), "commit B content\n")
	runGitTest(t, workDir, "add", "later.txt")
	runGitTest(t, workDir, "commit", "-m", "commit B: add later file")
	commitB := strings.TrimSpace(runGitTest(t, workDir, "rev-parse", "HEAD"))
	runGitTest(t, workDir, "push", "origin", defaultBranch)

	// Commit C: unmerged feature/PR branch commit.
	runGitTest(t, workDir, "checkout", "-b", "feature/pr-123", commitA)
	writeTestFile(t, filepath.Join(workDir, "untrusted.txt"), "unreviewed PR change\n")
	runGitTest(t, workDir, "add", "untrusted.txt")
	runGitTest(t, workDir, "commit", "-m", "commit C: unmerged PR head")
	featureCommit := strings.TrimSpace(runGitTest(t, workDir, "rev-parse", "HEAD"))
	runGitTest(t, workDir, "push", "origin", "feature/pr-123")

	return testRepoFixture{
		bareDir:       bareDir,
		defaultBranch: defaultBranch,
		commitA:       commitA,
		commitB:       commitB,
		featureCommit: featureCommit,
	}
}

func runGitTest(t *testing.T, dir string, args ...string) string {
	t.Helper()

	cmd := exec.Command("git", args...)
	if dir != "" {
		cmd.Dir = dir
	}
	cmd.Env = cleanGitTestEnv()
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v (dir=%q) failed: %v\noutput: %s", args, dir, err, string(out))
	}
	return string(out)
}

func cleanGitTestEnv() []string {
	var env []string
	for _, kv := range os.Environ() {
		if strings.HasPrefix(kv, "GIT_") {
			continue
		}
		env = append(env, kv)
	}
	return append(env,
		"GIT_CONFIG_NOSYSTEM=1",
		"GIT_CONFIG_GLOBAL=/dev/null",
		"GIT_TERMINAL_PROMPT=0",
	)
}

func writeTestFile(t *testing.T, path, content string) {
	t.Helper()

	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatalf("mkdir %q: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("write %q: %v", path, err)
	}
}

func testLogger() *slog.Logger {
	return slog.New(slog.NewTextHandler(io.Discard, nil))
}

func TestWorktreePrepareDetachedAtCatalogSha(t *testing.T) {
	fix := setupTestRepo(t, "trunk")
	stateRoot := t.TempDir()
	remotes := map[string]RepoRemote{
		"org/core-api": {
			Name:   "org/core-api",
			GitURL: fix.bareDir,
		},
	}

	mgr := NewWorktreeManager(stateRoot, remotes, testLogger())
	wt, err := mgr.Prepare(context.Background(), "job-101", "org/core-api", "", fix.commitA)
	if err != nil {
		t.Fatalf("Prepare() unexpected error: %v", err)
	}

	if wt.Repo != "org/core-api" {
		t.Errorf("wt.Repo = %q, want %q", wt.Repo, "org/core-api")
	}
	if wt.CatalogSha != fix.commitA {
		t.Errorf("wt.CatalogSha = %q, want %q", wt.CatalogSha, fix.commitA)
	}

	gotHead := strings.TrimSpace(runGitTest(t, wt.Path, "rev-parse", "HEAD"))
	if gotHead != fix.commitA {
		t.Errorf("worktree HEAD = %q, want %q", gotHead, fix.commitA)
	}

	// Verify HEAD is detached (symbolic-ref must fail).
	symCmd := exec.Command("git", "symbolic-ref", "--short", "HEAD")
	symCmd.Dir = wt.Path
	symCmd.Env = cleanGitTestEnv()
	if out, symErr := symCmd.CombinedOutput(); symErr == nil {
		t.Errorf("expected detached HEAD, got branch %q", strings.TrimSpace(string(out)))
	}

	// File added in commit B must not exist at commit A.
	if _, statErr := os.Stat(filepath.Join(wt.Path, "later.txt")); !errors.Is(statErr, os.ErrNotExist) {
		t.Errorf("expected later.txt not to exist at commit A, stat err = %v", statErr)
	}

	// Catalog must load cleanly from the detached worktree.
	cat, err := catalog.LoadFromDir(wt.Path)
	if err != nil {
		t.Fatalf("catalog.LoadFromDir(%q) failed: %v", wt.Path, err)
	}
	if _, ok := cat.Skills["review"]; !ok {
		t.Fatalf("loaded catalog missing expected skill %q", "review")
	}

	// Cleanup must remove directory completely and be idempotent.
	if err := wt.Cleanup(); err != nil {
		t.Fatalf("first wt.Cleanup() failed: %v", err)
	}
	if _, statErr := os.Stat(wt.Path); !errors.Is(statErr, os.ErrNotExist) {
		t.Errorf("expected worktree path %q to be removed, stat err = %v", wt.Path, statErr)
	}
	if err := wt.Cleanup(); err != nil {
		t.Fatalf("second wt.Cleanup() (idempotent) failed: %v", err)
	}
}

func TestWorktreeRejectsCommitNotOnDefaultBranch(t *testing.T) {
	fix := setupTestRepo(t, "trunk")
	stateRoot := t.TempDir()
	remotes := map[string]RepoRemote{
		"org/core-api": {
			Name:          "org/core-api",
			GitURL:        fix.bareDir,
			DefaultBranch: "trunk",
		},
	}

	mgr := NewWorktreeManager(stateRoot, remotes, testLogger())

	wt, err := mgr.Prepare(context.Background(), "job-untrusted", "org/core-api", "", fix.featureCommit)
	if err == nil {
		_ = wt.Cleanup()
		t.Fatal("Prepare() succeeded for unmerged feature commit, want ancestry error")
	}
	if !errors.Is(err, ErrNotDefaultBranchAncestor) {
		t.Errorf("Prepare() error = %v, want errors.Is(_, ErrNotDefaultBranchAncestor)", err)
	}

	worktreePath := filepath.Join(stateRoot, "worktrees", "job-untrusted")
	if _, statErr := os.Stat(worktreePath); !errors.Is(statErr, os.ErrNotExist) {
		t.Errorf("expected no worktree directory at %q, stat err = %v", worktreePath, statErr)
	}

	// Also reject an unknown 40-char hex SHA.
	unknownSHA := strings.Repeat("a", 40)
	wt2, err := mgr.Prepare(context.Background(), "job-unknown", "org/core-api", "trunk", unknownSHA)
	if err == nil {
		_ = wt2.Cleanup()
		t.Fatal("Prepare() succeeded for unknown SHA, want ancestry error")
	}
	if !errors.Is(err, ErrNotDefaultBranchAncestor) {
		t.Errorf("Prepare(unknownSHA) error = %v, want errors.Is(_, ErrNotDefaultBranchAncestor)", err)
	}
}

func TestWorktreeRejectsInvalidInputs(t *testing.T) {
	stateRoot := t.TempDir()
	mgr := NewWorktreeManager(stateRoot, nil, testLogger())
	validSHA := strings.Repeat("0", 40)

	tests := []struct {
		name          string
		jobID         string
		repo          string
		defaultBranch string
		catalogSha    string
	}{
		{
			name:       "repo traversal prefix",
			jobID:      "job-1",
			repo:       "../evil",
			catalogSha: validSHA,
		},
		{
			name:       "repo traversal suffix",
			jobID:      "job-1",
			repo:       "org/..",
			catalogSha: validSHA,
		},
		{
			name:       "repo dot segment",
			jobID:      "job-1",
			repo:       "./repo",
			catalogSha: validSHA,
		},
		{
			name:       "repo missing slash",
			jobID:      "job-1",
			repo:       "orgrepo",
			catalogSha: validSHA,
		},
		{
			name:       "repo extra slash",
			jobID:      "job-1",
			repo:       "a/b/c",
			catalogSha: validSHA,
		},
		{
			name:       "jobID traversal",
			jobID:      "../evil",
			repo:       "org/repo",
			catalogSha: validSHA,
		},
		{
			name:       "jobID dotdot",
			jobID:      "..",
			repo:       "org/repo",
			catalogSha: validSHA,
		},
		{
			name:       "jobID empty",
			jobID:      "",
			repo:       "org/repo",
			catalogSha: validSHA,
		},
		{
			name:       "catalogSha short",
			jobID:      "job-1",
			repo:       "org/repo",
			catalogSha: "deadbeef",
		},
		{
			name:       "catalogSha uppercase hex",
			jobID:      "job-1",
			repo:       "org/repo",
			catalogSha: strings.Repeat("A", 40),
		},
		{
			name:       "catalogSha non-hex",
			jobID:      "job-1",
			repo:       "org/repo",
			catalogSha: strings.Repeat("g", 40),
		},
		{
			name:          "defaultBranch leading dash",
			jobID:         "job-1",
			repo:          "org/repo",
			defaultBranch: "--upload-pack=evil",
			catalogSha:    validSHA,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			wt, err := mgr.Prepare(context.Background(), tc.jobID, tc.repo, tc.defaultBranch, tc.catalogSha)
			if err == nil {
				_ = wt.Cleanup()
				t.Fatalf("Prepare(%q, %q, %q, %q) succeeded, want error", tc.jobID, tc.repo, tc.defaultBranch, tc.catalogSha)
			}
		})
	}
}

func TestWorktreePruneOrphans(t *testing.T) {
	fix := setupTestRepo(t, "trunk")
	stateRoot := t.TempDir()
	remotes := map[string]RepoRemote{
		"org/core-api": {
			Name:   "org/core-api",
			GitURL: fix.bareDir,
		},
	}

	mgr := NewWorktreeManager(stateRoot, remotes, testLogger())

	// Pruning on a fresh stateRoot with no directories yet must succeed.
	if err := mgr.PruneOrphans(context.Background()); err != nil {
		t.Fatalf("PruneOrphans() on empty stateRoot failed: %v", err)
	}

	// Prepare a worktree and simulate a crash by not calling Cleanup.
	wt, err := mgr.Prepare(context.Background(), "job-crashed", "org/core-api", "trunk", fix.commitB)
	if err != nil {
		t.Fatalf("Prepare() failed: %v", err)
	}

	// Create an additional stale directory under worktrees/orphan-1.
	orphanDir := filepath.Join(stateRoot, "worktrees", "orphan-1")
	writeTestFile(t, filepath.Join(orphanDir, "leftover.txt"), "stale\n")

	if err := mgr.PruneOrphans(context.Background()); err != nil {
		t.Fatalf("PruneOrphans() failed: %v", err)
	}

	if _, statErr := os.Stat(orphanDir); !errors.Is(statErr, os.ErrNotExist) {
		t.Errorf("expected %q to be removed, stat err = %v", orphanDir, statErr)
	}
	if _, statErr := os.Stat(wt.Path); !errors.Is(statErr, os.ErrNotExist) {
		t.Errorf("expected crashed worktree %q to be removed, stat err = %v", wt.Path, statErr)
	}

	// Verify git worktree list on the bare mirror has no leftover worktree entries.
	mirrorPath := mgr.mirrorPath("org/core-api")
	out := runGitTest(t, "", "--git-dir="+mirrorPath, "worktree", "list", "--porcelain")
	if strings.Contains(out, "job-crashed") || strings.Contains(out, "orphan-1") {
		t.Errorf("expected clean git worktree list, got:\n%s", out)
	}
}

func TestWorktreeConcurrentPrepareAndCleanup(t *testing.T) {
	fix := setupTestRepo(t, "trunk")
	stateRoot := t.TempDir()
	remotes := map[string]RepoRemote{
		"org/core-api": {
			Name:          "org/core-api",
			GitURL:        fix.bareDir,
			DefaultBranch: "trunk",
		},
	}

	mgr := NewWorktreeManager(stateRoot, remotes, testLogger())

	const workers = 4
	var wg sync.WaitGroup
	errCh := make(chan error, workers)

	for i := range workers {
		wg.Add(1)
		go func(idx int) {
			defer wg.Done()
			jobID := fmt.Sprintf("job-concurrent-%d", idx)
			wt, err := mgr.Prepare(context.Background(), jobID, "org/core-api", "", fix.commitA)
			if err != nil {
				errCh <- fmt.Errorf("Prepare(%s): %w", jobID, err)
				return
			}
			if err := wt.Cleanup(); err != nil {
				errCh <- fmt.Errorf("Cleanup(%s): %w", jobID, err)
			}
		}(i)
	}

	wg.Wait()
	close(errCh)

	for err := range errCh {
		t.Errorf("concurrent worker error: %v", err)
	}
}
