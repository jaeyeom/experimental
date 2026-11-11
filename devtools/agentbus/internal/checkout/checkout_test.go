package checkout

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// gitEnv keeps the user's git config out of the test.
func gitEnv(t *testing.T) []string {
	t.Helper()
	return append(os.Environ(),
		"HOME="+t.TempDir(),
		"GIT_CONFIG_NOSYSTEM=1",
		"GIT_CONFIG_GLOBAL="+os.DevNull,
	)
}

// NewRepo makes a git repo with one commit and returns its real path.
func newRepo(t *testing.T, env []string) string {
	t.Helper()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git is not installed")
	}
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	gitIn(t, env, dir, "init", "-q", "-b", "main")
	if err := os.WriteFile(filepath.Join(dir, "a.txt"), []byte("a\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	gitIn(t, env, dir, "add", "a.txt")
	gitIn(t, env, dir, "-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-q", "-m", "init")
	return dir
}

func gitIn(t *testing.T, env []string, dir string, args ...string) {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...) //nolint:gosec // G204: test git in a temp repo
	cmd.Env = env
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git %v: %v\n%s", args, err, out)
	}
}

func TestBranch(t *testing.T) {
	env := gitEnv(t)
	repo := newRepo(t, env)
	if got, err := Branch(repo, env); err != nil || got != "main" {
		t.Fatalf("Branch = %q, %v", got, err)
	}
	gitIn(t, env, repo, "switch", "-q", "--detach")
	if got, err := Branch(repo, env); err != nil || got != "detached HEAD" {
		t.Fatalf("Branch detached = %q, %v", got, err)
	}
}

func TestResolve(t *testing.T) {
	env := gitEnv(t)
	repo := newRepo(t, env)
	if err := os.MkdirAll(filepath.Join(repo, "sub"), 0o755); err != nil {
		t.Fatal(err)
	}
	other := t.TempDir()
	nested := filepath.Join(repo, "nested")
	if err := os.MkdirAll(nested, 0o755); err != nil {
		t.Fatal(err)
	}
	gitIn(t, env, nested, "init", "-q")

	for _, tt := range []struct {
		name, path, want string
	}{
		{"root", repo, repo},
		{"subdir", filepath.Join(repo, "sub"), repo},
		{"file", filepath.Join(repo, "a.txt"), repo},
		{"new file in a new dir", filepath.Join(repo, "new", "b.txt"), repo},
		{"git dir", filepath.Join(repo, ".git", "config"), repo},
		{"outside", other, ""},
		{"another repo inside", filepath.Join(nested, "x.txt"), ""},
	} {
		got, err := Resolve(tt.path, []string{repo}, env)
		if err != nil || got != tt.want {
			t.Errorf("%s: Resolve(%q) = %q, %v; want %q", tt.name, tt.path, got, err, tt.want)
		}
	}
}

func TestResolveOutsideNeedsNoGit(t *testing.T) {
	// No PATH: any git call fails, so an unguarded answer proves none ran.
	got, err := Resolve(t.TempDir(), []string{"/nonexistent/guarded"}, []string{"PATH="})
	if err != nil || got != "" {
		t.Fatalf("Resolve = %q, %v", got, err)
	}
	got, err = Resolve(t.TempDir(), nil, nil)
	if err != nil || got != "" {
		t.Fatalf("Resolve without guarded = %q, %v", got, err)
	}
}

func TestInspect(t *testing.T) {
	env := gitEnv(t)
	repo := newRepo(t, env)
	tree, err := Inspect(repo, env)
	if err != nil || tree.Reason() != "" {
		t.Fatalf("clean Inspect = %+v, %v", tree, err)
	}
	if err := os.WriteFile(filepath.Join(repo, "a.txt"), []byte("changed\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(repo, "new.txt"), []byte("n\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	tree, err = Inspect(repo, env)
	if err != nil || len(tree.Changed) != 2 || !strings.Contains(tree.Reason(), "2 changed files") {
		t.Fatalf("dirty Inspect = %+v (%q), %v", tree, tree.Reason(), err)
	}

	gitIn(t, env, repo, "checkout", "-q", "--", "a.txt")
	if err := os.Remove(filepath.Join(repo, "new.txt")); err != nil {
		t.Fatal(err)
	}
	gitDir := filepath.Join(repo, ".git")
	if err := os.WriteFile(filepath.Join(gitDir, "MERGE_HEAD"), []byte("0000000000000000000000000000000000000000\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	tree, err = Inspect(repo, env)
	if err != nil || tree.Operation != "merge" || !strings.Contains(tree.Reason(), "merge in progress") {
		t.Fatalf("merge Inspect = %+v, %v", tree, err)
	}
}
