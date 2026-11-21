package forkkeeper

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

type testFixture struct {
	t           *testing.T
	root        string
	upstreamGit string
	originGit   string
	useed       string
	fseed       string
	contractDir string
}

func ensureTestPATH(t *testing.T, extraDirs ...string) {
	t.Helper()
	cur := os.Getenv("PATH")
	parts := append([]string{}, extraDirs...)
	if cur != "" {
		parts = append(parts, cur)
	}
	parts = append(parts, "/opt/homebrew/bin", "/usr/local/bin", "/home/linuxbrew/.linuxbrew/bin")
	t.Setenv("PATH", strings.Join(parts, string(os.PathListSeparator)))
}

func isolateGitEnv(t *testing.T, baseDir string) {
	t.Helper()
	noHooks := filepath.Join(baseDir, "nohooks")
	if err := os.MkdirAll(noHooks, 0o755); err != nil {
		t.Fatal(err)
	}
	home := filepath.Join(baseDir, "home")
	if err := os.MkdirAll(home, 0o755); err != nil {
		t.Fatal(err)
	}
	gitconfig := filepath.Join(baseDir, "gitconfig")
	content := fmt.Sprintf(`[user]
	name = Fork Keeper Test
	email = fork-keeper@example.invalid
[core]
	hooksPath = %s
[init]
	defaultBranch = main
[advice]
	detachedHead = false
`, noHooks)
	if err := os.WriteFile(gitconfig, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("GIT_CONFIG_GLOBAL", gitconfig)
	t.Setenv("GIT_CONFIG_NOSYSTEM", "1")
	t.Setenv("HOME", home)
}

func newTestFixture(t *testing.T) *testFixture {
	t.Helper()
	ensureTestPATH(t)
	root := t.TempDir()
	isolateGitEnv(t, root)

	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	contractDir := filepath.Join(wd, "testdata", "claude-toolbox")
	if _, err := os.Stat(filepath.Join(contractDir, "review-range.sh")); err != nil {
		t.Fatalf("missing contract dir at %s: %v", contractDir, err)
	}

	fx := &testFixture{
		t:           t,
		root:        root,
		upstreamGit: filepath.Join(root, "upstream.git"),
		originGit:   filepath.Join(root, "origin.git"),
		useed:       filepath.Join(root, "useed"),
		fseed:       filepath.Join(root, "fseed"),
		contractDir: contractDir,
	}

	fx.mustRun("", "git", "init", "-q", "--bare", "-b", "main", fx.upstreamGit)
	fx.mustRun("", "git", "init", "-q", "--bare", "-b", "main", fx.originGit)
	fx.mustRun("", "git", "init", "-q", "-b", "main", fx.useed)
	fx.mustRun(fx.useed, "git", "remote", "add", "origin", fx.upstreamGit)
	if err := os.WriteFile(filepath.Join(fx.useed, "README"), []byte("base\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	fx.mustRun(fx.useed, "git", "add", "README")
	fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "U0")
	fx.mustRun(fx.useed, "git", "push", "-q", "origin", "main")
	fx.mustRun(fx.useed, "git", "push", "-q", fx.originGit, "main")
	fx.mustRun("", "git", "clone", "-q", fx.originGit, fx.fseed)
	return fx
}

func (fx *testFixture) mustRun(dir, name string, args ...string) string {
	fx.t.Helper()
	cmd := exec.Command(name, args...)
	if dir != "" {
		cmd.Dir = dir
	}
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		fx.t.Fatalf("%s %s (dir=%s): %v\nstdout: %s\nstderr: %s",
			name, strings.Join(args, " "), dir, err, stdout.String(), stderr.String())
	}
	return strings.TrimSpace(stdout.String())
}

func (fx *testFixture) ucommit(relPath, line string) string {
	fx.t.Helper()
	p := filepath.Join(fx.useed, relPath)
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		fx.t.Fatal(err)
	}
	f, err := os.OpenFile(p, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		fx.t.Fatal(err)
	}
	if _, err := fmt.Fprintln(f, line); err != nil {
		_ = f.Close()
		fx.t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		fx.t.Fatal(err)
	}
	fx.mustRun(fx.useed, "git", "add", relPath)
	fx.mustRun(fx.useed, "git", "commit", "-q", "-m", "upstream: "+relPath)
	fx.mustRun(fx.useed, "git", "push", "-q", "-f", "origin", "HEAD:main")
	return fx.mustRun(fx.useed, "git", "rev-parse", "HEAD")
}

func (fx *testFixture) fcommit(relPath, line string) string {
	fx.t.Helper()
	fx.mustRun(fx.fseed, "git", "pull", "-q", "--rebase", "origin", "main")
	p := filepath.Join(fx.fseed, relPath)
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		fx.t.Fatal(err)
	}
	f, err := os.OpenFile(p, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		fx.t.Fatal(err)
	}
	if _, err := fmt.Fprintln(f, line); err != nil {
		_ = f.Close()
		fx.t.Fatal(err)
	}
	if err := f.Close(); err != nil {
		fx.t.Fatal(err)
	}
	fx.mustRun(fx.fseed, "git", "add", relPath)
	fx.mustRun(fx.fseed, "git", "commit", "-q", "-m", "mine: "+relPath)
	fx.mustRun(fx.fseed, "git", "push", "-q", "origin", "HEAD:main")
	return fx.mustRun(fx.fseed, "git", "rev-parse", "HEAD")
}

func (fx *testFixture) recordHead(sha string) {
	fx.t.Helper()
	fx.mustRun(fx.useed, "git", "push", "-q", "-f", fx.originGit, sha+":refs/fork-keeper/upstream-head")
}

func (fx *testFixture) forkAtUpstream() {
	fx.t.Helper()
	fx.mustRun(fx.useed, "git", "push", "-q", "-f", fx.originGit, "HEAD:main")
}

func (fx *testFixture) oref(ref string) string {
	fx.t.Helper()
	//nolint:gosec // Test fixture git command.
	cmd := exec.Command("git", "--git-dir="+fx.originGit, "rev-parse", "-q", "--verify", ref)
	out, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

func (fx *testFixture) uref(ref string) string {
	fx.t.Helper()
	//nolint:gosec // Test fixture git command.
	cmd := exec.Command("git", "--git-dir="+fx.upstreamGit, "rev-parse", "-q", "--verify", ref)
	out, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

func (fx *testFixture) prepTripwireRepo() (workDir string, rangeInfo RangeInfo, candSHA string) {
	fx.t.Helper()
	workDir = filepath.Join(fx.root, "w")
	_ = os.RemoveAll(workDir)
	fx.mustRun("", "git", "init", "-q", workDir)
	fx.mustRun(workDir, "git", "fetch", "-q", fx.originGit,
		"+refs/heads/*:refs/remotes/origin/*",
		"+refs/fork-keeper/*:refs/fork-keeper/*")
	fx.mustRun(workDir, "git", "fetch", "-q", "--no-tags", fx.upstreamGit,
		"+refs/heads/*:refs/remotes/upstream/*",
		"+refs/tags/*:refs/upstream-tags/*")
	rawRange := fx.mustRun(workDir, "bash", filepath.Join(fx.contractDir, "review-range.sh"))
	if err := json.Unmarshal([]byte(rawRange), &rangeInfo); err != nil {
		fx.t.Fatalf("parse review-range output: %v\n%s", err, rawRange)
	}
	fx.mustRun(workDir, "git", "checkout", "-q", "-B", "cand", rangeInfo.UpstreamNew)
	for _, c := range rangeInfo.UserCommits {
		fx.mustRun(workDir, "git", "cherry-pick", c)
	}
	candSHA = fx.mustRun(workDir, "git", "rev-parse", "HEAD")
	return workDir, rangeInfo, candSHA
}
