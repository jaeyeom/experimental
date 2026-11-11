package checkout

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// newClone makes a bare origin with one commit on main, sets its HEAD,
// and clones it. It returns the clone and the origin.
func newClone(t *testing.T, env []string) (string, string) {
	t.Helper()
	seed := newRepo(t, env)
	origin := filepath.Join(t.TempDir(), "origin.git")
	gitIn(t, env, seed, "clone", "-q", "--bare", seed, origin)
	parent, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	clone := filepath.Join(parent, "clone")
	gitIn(t, env, parent, "clone", "-q", origin, clone)
	return clone, origin
}

func gitOut(t *testing.T, env []string, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...) //nolint:gosec // G204: test git in a temp repo
	cmd.Env = env
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("git %v: %v", args, err)
	}
	return strings.TrimSpace(string(out))
}

// pushUpstream adds a commit to origin's main from another clone.
func pushUpstream(t *testing.T, env []string, origin, file string) {
	t.Helper()
	parent := t.TempDir()
	other := filepath.Join(parent, "other")
	gitIn(t, env, parent, "clone", "-q", origin, other)
	if err := os.WriteFile(filepath.Join(other, file), []byte("upstream\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	gitIn(t, env, other, "add", file)
	gitIn(t, env, other, "-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-q", "-m", "upstream")
	gitIn(t, env, other, "push", "-q", "origin", "main")
}

func TestSwitchDefaultAndPull(t *testing.T) {
	env := gitEnv(t)
	clone, origin := newClone(t, env)
	gitIn(t, env, clone, "switch", "-q", "-c", "feature")
	pushUpstream(t, env, origin, "b.txt")

	branch, err := SwitchDefault(clone, env)
	if err != nil {
		t.Fatal(err)
	}
	if branch != "main" {
		t.Fatalf("default branch = %q", branch)
	}
	if got := gitOut(t, env, clone, "branch", "--show-current"); got != "main" {
		t.Fatalf("current branch = %q", got)
	}
	if err := Pull(clone, env); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(clone, "b.txt")); err != nil {
		t.Fatalf("pull did not bring the upstream commit: %v", err)
	}
}

func TestSwitchDefaultNeedsOriginHead(t *testing.T) {
	env := gitEnv(t)
	repo := newRepo(t, env)
	if _, err := SwitchDefault(repo, env); err == nil || !strings.Contains(err.Error(), "origin/HEAD") {
		t.Fatalf("no origin: err = %v", err)
	}
}

func TestPullFailsWhenOriginIsUnreachable(t *testing.T) {
	env := gitEnv(t)
	clone, origin := newClone(t, env)
	if err := os.Rename(origin, origin+".moved"); err != nil {
		t.Fatal(err)
	}
	if err := Pull(clone, env); err == nil {
		t.Fatal("pull from a missing origin succeeded")
	}
}

func TestPullAbortsAConflictedRebase(t *testing.T) {
	env := gitEnv(t)
	clone, origin := newClone(t, env)
	pushUpstream(t, env, origin, "a.txt")
	if err := os.WriteFile(filepath.Join(clone, "a.txt"), []byte("local\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	gitIn(t, env, clone, "-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-q", "-am", "local")

	if err := Pull(clone, env); err == nil {
		t.Fatal("conflicting pull succeeded")
	}
	tree, err := Inspect(clone, env)
	if err != nil {
		t.Fatal(err)
	}
	if tree.Reason() != "" {
		t.Fatalf("tree after a failed pull = %q", tree.Reason())
	}
}
