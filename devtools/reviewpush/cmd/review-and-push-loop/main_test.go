package main

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/reviewpush"
)

func setupTestGitRepo(t *testing.T, extraCommits int) (repoDir, remoteDir string) {
	t.Helper()
	tmp := t.TempDir()
	remoteDir = filepath.Join(tmp, "origin.git")
	repoDir = filepath.Join(tmp, "repo")

	runGit(t, tmp, "init", "--bare", "--initial-branch=main", remoteDir)
	runGit(t, tmp, "clone", remoteDir, repoDir)
	runGit(t, repoDir, "config", "user.email", "test@example.com")
	runGit(t, repoDir, "config", "user.name", "Test User")

	if err := os.WriteFile(filepath.Join(repoDir, "README.md"), []byte("init\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	runGit(t, repoDir, "add", "README.md")
	runGit(t, repoDir, "commit", "-m", "initial commit")
	runGit(t, repoDir, "push", "-u", "origin", "main")
	runGit(t, repoDir, "remote", "set-head", "origin", "main")

	for i := 1; i <= extraCommits; i++ {
		name := filepath.Join(repoDir, "file-"+string(rune('a'+i-1))+".txt")
		if err := os.WriteFile(name, []byte("change\n"), 0o600); err != nil {
			t.Fatal(err)
		}
		runGit(t, repoDir, "add", name)
		runGit(t, repoDir, "commit", "-m", "commit "+string(rune('0'+i)))
	}
	return repoDir, remoteDir
}

func runGit(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v in %s failed: %v\n%s", args, dir, err, string(out))
	}
	return strings.TrimSpace(string(out))
}

func writeReviewerScript(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "reviewer.sh")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, 0o700); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestCLICustomReviewerEndToEnd(t *testing.T) {
	t.Run("pushes multiple commits sequentially with --json", func(t *testing.T) {
		repoDir, _ := setupTestGitRepo(t, 2)
		reviewer := writeReviewerScript(t, `#!/bin/sh
cat >/dev/null
printf '{"status":"PUSH","summary":"ok","multi_commits":false,"push_target":"%s","blocked_reason":null}\n' "$REVIEWPUSH_TARGET_COMMIT"
`)
		var stdout, stderr bytes.Buffer
		code := run([]string{"--repo", repoDir, "--base", "main", "--json", "--reviewer-cmd", reviewer}, &stdout, &stderr)
		if code != 0 {
			t.Fatalf("exit=%d stderr=%s stdout=%s", code, stderr.String(), stdout.String())
		}
		var report reviewpush.RunReport
		if err := json.Unmarshal(stdout.Bytes(), &report); err != nil {
			t.Fatalf("decode json: %v (%s)", err, stdout.String())
		}
		if report.Status != "DONE" || len(report.PushedCommits) != 2 || len(report.Iterations) != 2 {
			t.Fatalf("unexpected report: %+v", report)
		}
		ahead := runGit(t, repoDir, "rev-list", "--count", "origin/main..HEAD")
		if ahead != "0" {
			t.Fatalf("expected 0 commits ahead after push, got %s", ahead)
		}
	})

	t.Run("multi_commits true pushes through HEAD in one pass", func(t *testing.T) {
		repoDir, _ := setupTestGitRepo(t, 2)
		headSHA := runGit(t, repoDir, "rev-parse", "HEAD")
		reviewer := writeReviewerScript(t, `#!/bin/sh
cat >/dev/null
printf '{"status":"PUSH","summary":"batch ok","multi_commits":true,"push_target":"`+headSHA+`","blocked_reason":null}\n'
`)
		var stdout, stderr bytes.Buffer
		code := run([]string{"--repo", repoDir, "--base", "main", "--json", "--reviewer-cmd", reviewer}, &stdout, &stderr)
		if code != 0 {
			t.Fatalf("exit=%d stderr=%s stdout=%s", code, stderr.String(), stdout.String())
		}
		var report reviewpush.RunReport
		if err := json.Unmarshal(stdout.Bytes(), &report); err != nil {
			t.Fatalf("decode json: %v (%s)", err, stdout.String())
		}
		if len(report.Iterations) != 1 || len(report.PushedCommits) != 1 || report.PushedCommits[0] != headSHA {
			t.Fatalf("unexpected report: %+v", report)
		}
	})

	t.Run("blocked stops without pushing", func(t *testing.T) {
		repoDir, _ := setupTestGitRepo(t, 1)
		reviewer := writeReviewerScript(t, `#!/bin/sh
cat >/dev/null
printf '{"status":"BLOCKED","summary":"hard blocker","multi_commits":false,"push_target":null,"blocked_reason":"secret leaked"}\n'
`)
		var stdout, stderr bytes.Buffer
		code := run([]string{"--repo", repoDir, "--base", "main", "--json", "--reviewer-cmd", reviewer}, &stdout, &stderr)
		if code != 2 {
			t.Fatalf("exit=%d want 2 stdout=%s stderr=%s", code, stdout.String(), stderr.String())
		}
		var report reviewpush.RunReport
		if err := json.Unmarshal(stdout.Bytes(), &report); err != nil {
			t.Fatalf("decode json: %v (%s)", err, stdout.String())
		}
		if report.Status != reviewpush.StatusBlocked || report.Reason != "secret leaked" {
			t.Fatalf("unexpected report: %+v", report)
		}
		ahead := runGit(t, repoDir, "rev-list", "--count", "origin/main..HEAD")
		if ahead != "1" {
			t.Fatalf("expected 1 commit still ahead, got %s", ahead)
		}
	})

	t.Run("non-zero reviewer exit rejects push", func(t *testing.T) {
		repoDir, _ := setupTestGitRepo(t, 1)
		reviewer := writeReviewerScript(t, `#!/bin/sh
echo "boom" >&2
exit 1
`)
		var stdout, stderr bytes.Buffer
		code := run([]string{"--repo", repoDir, "--base", "main", "--json", "--reviewer-cmd", reviewer}, &stdout, &stderr)
		if code != 2 {
			t.Fatalf("exit=%d want 2 stdout=%s", code, stdout.String())
		}
		ahead := runGit(t, repoDir, "rev-list", "--count", "origin/main..HEAD")
		if ahead != "1" {
			t.Fatalf("expected 1 commit still ahead, got %s", ahead)
		}
	})

	t.Run("malformed reviewer output rejects push", func(t *testing.T) {
		repoDir, _ := setupTestGitRepo(t, 1)
		reviewer := writeReviewerScript(t, `#!/bin/sh
echo 'not-json'
`)
		var stdout, stderr bytes.Buffer
		code := run([]string{"--repo", repoDir, "--base", "main", "--json", "--reviewer-cmd", reviewer}, &stdout, &stderr)
		if code != 2 {
			t.Fatalf("exit=%d want 2 stdout=%s", code, stdout.String())
		}
		ahead := runGit(t, repoDir, "rev-list", "--count", "origin/main..HEAD")
		if ahead != "1" {
			t.Fatalf("expected 1 commit still ahead, got %s", ahead)
		}
	})

	t.Run("conflicting json and output flags rejected", func(t *testing.T) {
		var stdout, stderr bytes.Buffer
		code := run([]string{"--json", "--output", "compact"}, &stdout, &stderr)
		if code != 1 {
			t.Fatalf("exit=%d want 1 stderr=%s", code, stderr.String())
		}
	})
}
