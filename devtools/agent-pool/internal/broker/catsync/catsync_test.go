package catsync

import (
	"bytes"
	"context"
	"io"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/sqlite"
)

const validCatalogV1 = `{
  "version": 1,
  "skills": {
    "review-pr": {
      "description": "Review a pull request.",
      "mode": "readonly",
      "skillPath": ".claude/skills/review-pr/SKILL.md",
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

const validCatalogV2 = `{
  "version": 1,
  "skills": {
    "review-pr": {
      "description": "Review a pull request (v2).",
      "mode": "readonly",
      "skillPath": ".claude/skills/review-pr/SKILL.md",
      "argsSchema": {
        "type": "object",
        "properties": {
          "prNumber": { "type": "integer" }
        },
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

const invalidMutatingCatalog = `{
  "version": 1,
  "skills": {
    "review-pr": {
      "description": "Mutating skill must be rejected.",
      "mode": "mutating",
      "skillPath": ".claude/skills/review-pr/SKILL.md",
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

const missingSkillCatalog = `{
  "version": 1,
  "skills": {
    "missing-skill": {
      "description": "References a missing SKILL.md.",
      "mode": "readonly",
      "skillPath": ".claude/skills/does-not-exist/SKILL.md",
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

type testRemote struct {
	bareDir string
	workDir string
}

func newTestRemote(t *testing.T, headBranch string) testRemote {
	t.Helper()

	root := t.TempDir()
	bareDir := filepath.Join(root, "remote.git")
	workDir := filepath.Join(root, "work")

	runGitTest(t, "", "init", "--bare", bareDir)
	runGitTest(t, bareDir, "symbolic-ref", "HEAD", "refs/heads/"+headBranch)

	runGitTest(t, "", "init", workDir)
	runGitTest(t, workDir, "checkout", "-b", headBranch)
	runGitTest(t, workDir, "config", "user.name", "Test User")
	runGitTest(t, workDir, "config", "user.email", "test@example.com")
	runGitTest(t, workDir, "remote", "add", "origin", bareDir)

	return testRemote{bareDir: bareDir, workDir: workDir}
}

func (r testRemote) commitAndPush(t *testing.T, branch, message string, files map[string]string) string {
	t.Helper()

	for rel, content := range files {
		writeTestFile(t, filepath.Join(r.workDir, filepath.FromSlash(rel)), content)
	}
	runGitTest(t, r.workDir, "add", ".")
	runGitTest(t, r.workDir, "commit", "-m", message)
	sha := strings.TrimSpace(runGitTest(t, r.workDir, "rev-parse", "HEAD"))
	runGitTest(t, r.workDir, "push", "origin", branch)
	return sha
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

func openTestStore(t *testing.T) *sqlite.Store {
	t.Helper()

	store, err := sqlite.Open(filepath.Join(t.TempDir(), "broker.db"))
	if err != nil {
		t.Fatalf("sqlite.Open() failed: %v", err)
	}
	t.Cleanup(func() {
		if closeErr := store.Close(); closeErr != nil {
			t.Errorf("store.Close() failed: %v", closeErr)
		}
	})
	return store
}

func testLogger() *slog.Logger {
	return slog.New(slog.NewTextHandler(io.Discard, nil))
}

func TestSyncRepoAutoDetectsDefaultBranch(t *testing.T) {
	remote := newTestRemote(t, "trunk")
	commitA := remote.commitAndPush(t, "trunk", "add valid catalog", map[string]string{
		".agent-pool/catalog.json":          validCatalogV1,
		".claude/skills/review-pr/SKILL.md": "# Review PR\n",
	})

	store := openTestStore(t)
	rc := RepoConfig{
		Name:   "org/core-api",
		GitURL: remote.bareDir,
	}
	syncer := NewSyncer(store, t.TempDir(), []RepoConfig{rc}, testLogger())

	rec, err := syncer.SyncRepo(context.Background(), rc)
	if err != nil {
		t.Fatalf("SyncRepo() unexpected error: %v", err)
	}
	if rec.Repo != "org/core-api" {
		t.Errorf("rec.Repo = %q, want %q", rec.Repo, "org/core-api")
	}
	if rec.DefaultBranch != "trunk" {
		t.Errorf("rec.DefaultBranch = %q, want %q", rec.DefaultBranch, "trunk")
	}
	if rec.CatalogSha != commitA {
		t.Errorf("rec.CatalogSha = %q, want %q", rec.CatalogSha, commitA)
	}
	if !bytes.Equal(rec.CatalogJSON, []byte(validCatalogV1)) {
		t.Errorf("rec.CatalogJSON = %q, want %q", string(rec.CatalogJSON), validCatalogV1)
	}

	stored, err := store.GetCatalog(context.Background(), "org/core-api")
	if err != nil {
		t.Fatalf("store.GetCatalog() failed: %v", err)
	}
	if stored.DefaultBranch != "trunk" {
		t.Errorf("stored.DefaultBranch = %q, want %q", stored.DefaultBranch, "trunk")
	}
	if stored.CatalogSha != commitA {
		t.Errorf("stored.CatalogSha = %q, want %q", stored.CatalogSha, commitA)
	}
	if !bytes.Equal(stored.CatalogJSON, []byte(validCatalogV1)) {
		t.Errorf("stored.CatalogJSON = %q, want %q", string(stored.CatalogJSON), validCatalogV1)
	}
}

func TestSyncRepoExplicitDefaultBranch(t *testing.T) {
	remote := newTestRemote(t, "main")
	_ = remote.commitAndPush(t, "main", "main catalog", map[string]string{
		".agent-pool/catalog.json":          validCatalogV1,
		".claude/skills/review-pr/SKILL.md": "# Review PR\n",
	})

	runGitTest(t, remote.workDir, "checkout", "-b", "release")
	releaseSHA := remote.commitAndPush(t, "release", "release catalog", map[string]string{
		".agent-pool/catalog.json":          validCatalogV2,
		".claude/skills/review-pr/SKILL.md": "# Review PR v2\n",
	})

	store := openTestStore(t)
	rc := RepoConfig{
		Name:          "org/core-api",
		GitURL:        remote.bareDir,
		DefaultBranch: "release",
	}
	syncer := NewSyncer(store, t.TempDir(), []RepoConfig{rc}, testLogger())

	rec, err := syncer.SyncRepo(context.Background(), rc)
	if err != nil {
		t.Fatalf("SyncRepo() unexpected error: %v", err)
	}
	if rec.DefaultBranch != "release" {
		t.Errorf("rec.DefaultBranch = %q, want %q", rec.DefaultBranch, "release")
	}
	if rec.CatalogSha != releaseSHA {
		t.Errorf("rec.CatalogSha = %q, want %q", rec.CatalogSha, releaseSHA)
	}
	if !bytes.Equal(rec.CatalogJSON, []byte(validCatalogV2)) {
		t.Errorf("rec.CatalogJSON = %q, want %q", string(rec.CatalogJSON), validCatalogV2)
	}
}

func TestSyncRepoRejectsInvalidCatalogPreservesLastGood(t *testing.T) {
	tests := []struct {
		name       string
		badCatalog string
	}{
		{
			name:       "invalid mode mutating",
			badCatalog: invalidMutatingCatalog,
		},
		{
			name:       "missing SKILL.md",
			badCatalog: missingSkillCatalog,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			remote := newTestRemote(t, "trunk")
			commitA := remote.commitAndPush(t, "trunk", "commit A valid", map[string]string{
				".agent-pool/catalog.json":          validCatalogV1,
				".claude/skills/review-pr/SKILL.md": "# Review PR\n",
			})

			store := openTestStore(t)
			rc := RepoConfig{
				Name:   "org/core-api",
				GitURL: remote.bareDir,
			}
			syncer := NewSyncer(store, t.TempDir(), []RepoConfig{rc}, testLogger())

			if _, err := syncer.SyncRepo(context.Background(), rc); err != nil {
				t.Fatalf("initial SyncRepo() unexpected error: %v", err)
			}

			_ = remote.commitAndPush(t, "trunk", "commit B invalid", map[string]string{
				".agent-pool/catalog.json": tc.badCatalog,
			})

			if _, err := syncer.SyncRepo(context.Background(), rc); err == nil {
				t.Fatal("second SyncRepo() succeeded for invalid catalog, want error")
			}

			stored, err := store.GetCatalog(context.Background(), "org/core-api")
			if err != nil {
				t.Fatalf("store.GetCatalog() failed: %v", err)
			}
			if stored.CatalogSha != commitA {
				t.Errorf("stored.CatalogSha = %q, want last good %q", stored.CatalogSha, commitA)
			}
			if !bytes.Equal(stored.CatalogJSON, []byte(validCatalogV1)) {
				t.Errorf("stored.CatalogJSON was clobbered: got %q, want %q", string(stored.CatalogJSON), validCatalogV1)
			}
		})
	}
}

func TestSyncRepoUpdatesOnNewCommit(t *testing.T) {
	remote := newTestRemote(t, "trunk")
	commitA := remote.commitAndPush(t, "trunk", "commit A valid", map[string]string{
		".agent-pool/catalog.json":          validCatalogV1,
		".claude/skills/review-pr/SKILL.md": "# Review PR\n",
	})

	store := openTestStore(t)
	rc := RepoConfig{
		Name:   "org/core-api",
		GitURL: remote.bareDir,
	}
	syncer := NewSyncer(store, t.TempDir(), []RepoConfig{rc}, testLogger())

	recA, err := syncer.SyncRepo(context.Background(), rc)
	if err != nil {
		t.Fatalf("first SyncRepo() failed: %v", err)
	}
	if recA.CatalogSha != commitA {
		t.Fatalf("first CatalogSha = %q, want %q", recA.CatalogSha, commitA)
	}

	commitC := remote.commitAndPush(t, "trunk", "commit C updated valid", map[string]string{
		".agent-pool/catalog.json": validCatalogV2,
	})

	recC, err := syncer.SyncRepo(context.Background(), rc)
	if err != nil {
		t.Fatalf("second SyncRepo() failed: %v", err)
	}
	if recC.CatalogSha != commitC {
		t.Errorf("recC.CatalogSha = %q, want %q", recC.CatalogSha, commitC)
	}
	if !bytes.Equal(recC.CatalogJSON, []byte(validCatalogV2)) {
		t.Errorf("recC.CatalogJSON = %q, want %q", string(recC.CatalogJSON), validCatalogV2)
	}

	stored, err := store.GetCatalog(context.Background(), "org/core-api")
	if err != nil {
		t.Fatalf("store.GetCatalog() failed: %v", err)
	}
	if stored.CatalogSha != commitC {
		t.Errorf("stored.CatalogSha = %q, want %q", stored.CatalogSha, commitC)
	}
	if !bytes.Equal(stored.CatalogJSON, []byte(validCatalogV2)) {
		t.Errorf("stored.CatalogJSON = %q, want %q", string(stored.CatalogJSON), validCatalogV2)
	}
}

func TestSyncRepoNoSymbolicHeadReturnsHelpfulError(t *testing.T) {
	remote := newTestRemote(t, "trunk")
	commitA := remote.commitAndPush(t, "trunk", "commit A", map[string]string{
		".agent-pool/catalog.json":          validCatalogV1,
		".claude/skills/review-pr/SKILL.md": "# Review PR\n",
	})

	// Detach HEAD in the bare remote so ls-remote --symref has no symbolic HEAD.
	writeTestFile(t, filepath.Join(remote.bareDir, "HEAD"), commitA+"\n")

	store := openTestStore(t)
	rc := RepoConfig{
		Name:   "org/detached",
		GitURL: remote.bareDir,
	}
	syncer := NewSyncer(store, t.TempDir(), []RepoConfig{rc}, testLogger())

	_, err := syncer.SyncRepo(context.Background(), rc)
	if err == nil {
		t.Fatal("SyncRepo() succeeded with detached remote HEAD, want error")
	}
	if !strings.Contains(err.Error(), "default_branch") || !strings.Contains(err.Error(), "broker.toml") {
		t.Errorf("error = %q, want guidance to set default_branch in broker.toml", err.Error())
	}
}

func TestSyncOnceAndRun(t *testing.T) {
	remote := newTestRemote(t, "trunk")
	commitA := remote.commitAndPush(t, "trunk", "commit A", map[string]string{
		".agent-pool/catalog.json":          validCatalogV1,
		".claude/skills/review-pr/SKILL.md": "# Review PR\n",
	})

	store := openTestStore(t)
	repos := []RepoConfig{
		{
			Name:   "org/good",
			GitURL: remote.bareDir,
		},
		{
			Name:   "org/bad",
			GitURL: filepath.Join(t.TempDir(), "does-not-exist.git"),
		},
	}
	syncer := NewSyncer(store, t.TempDir(), repos, testLogger())

	errs := syncer.SyncOnce(context.Background())
	if _, ok := errs["org/good"]; ok {
		t.Errorf("SyncOnce() unexpected error for org/good: %v", errs["org/good"])
	}
	if _, ok := errs["org/bad"]; !ok {
		t.Error("SyncOnce() expected error for org/bad, got none")
	}

	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() {
		defer close(done)
		syncer.Run(ctx, 20*time.Millisecond)
	}()

	time.Sleep(50 * time.Millisecond)
	cancel()

	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("Run() did not stop after context cancellation")
	}

	stored, err := store.GetCatalog(context.Background(), "org/good")
	if err != nil {
		t.Fatalf("store.GetCatalog(org/good) failed: %v", err)
	}
	if stored.CatalogSha != commitA {
		t.Errorf("stored.CatalogSha = %q, want %q", stored.CatalogSha, commitA)
	}
}
