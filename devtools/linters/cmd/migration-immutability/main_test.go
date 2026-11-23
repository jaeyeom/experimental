package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
)

const mig = "app/migrations/0001_init.py"

func gitIn(t *testing.T, dir string, args ...string) {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	cmd.Env = append(os.Environ(),
		"GIT_AUTHOR_NAME=t", "GIT_AUTHOR_EMAIL=t@example.com",
		"GIT_COMMITTER_NAME=t", "GIT_COMMITTER_EMAIL=t@example.com",
		"GIT_CONFIG_NOSYSTEM=1", "GIT_CONFIG_GLOBAL=/dev/null")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git %v: %v\n%s", args, err, out)
	}
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
}

func newRepo(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	gitIn(t, dir, "init", "-q", "-b", "main")
	writeFile(t, filepath.Join(dir, mig), "x\n")
	gitIn(t, dir, "add", "-A")
	gitIn(t, dir, "commit", "-q", "-m", "init")
	gitIn(t, dir, "update-ref", "refs/remotes/origin/main", "HEAD")
	gitIn(t, dir, "symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main")
	gitIn(t, dir, "checkout", "-q", "-b", "feat")
	return dir
}

func runCmd(args ...string) (int, string, string) {
	var stdout, stderr bytes.Buffer
	code := run(args, &stdout, &stderr, func(string) string { return "" })
	return code, stdout.String(), stderr.String()
}

func TestFlagErrors(t *testing.T) {
	for _, args := range [][]string{
		{"--update-baseline"},
		{"--reason", "x"},
		{"--issue", "#1"},
		{"--nope"},
		{"extra-arg"},
	} {
		if code, _, _ := runCmd(args...); code != 2 {
			t.Errorf("run(%q) = %d, want 2", args, code)
		}
	}
	if code, out, _ := runCmd("--version"); code != 0 || !strings.HasPrefix(out, "migration-immutability ") {
		t.Errorf("--version = %d %q", code, out)
	}
}

func TestPassBlockUpdate(t *testing.T) {
	dir := newRepo(t)
	if code, out, errOut := runCmd("--root", dir); code != 0 || !strings.Contains(out, "PASSED") {
		t.Fatalf("clean = %d\n%s%s", code, out, errOut)
	}

	writeFile(t, filepath.Join(dir, mig), "y\n")
	code, _, errOut := runCmd("--root", dir)
	if code != 1 || !strings.Contains(errOut, mig+" [django] modified") || !strings.Contains(errOut, "NEW migration") {
		t.Fatalf("modified = %d\n%s", code, errOut)
	}

	code, out, errOut := runCmd("--root", dir, "--update-baseline", "--reason", "sweep", "--issue", "#9")
	if code != 0 || !strings.Contains(out, "allowed: "+mig) {
		t.Fatalf("update = %d\n%s%s", code, out, errOut)
	}
	bf, err := baseline.Load(filepath.Join(dir, ".baselines", "migration-immutability.json"))
	if err != nil {
		t.Fatal(err)
	}
	if bf.Checker != "migration-immutability" || len(bf.Entries[mig].Transitions) != 1 {
		t.Errorf("baseline = %+v", bf)
	}

	if code, out, errOut := runCmd("--root", dir); code != 0 || !strings.Contains(out, "allowed: "+mig+" modified: baseline (sweep)") {
		t.Errorf("after update = %d\n%s%s", code, out, errOut)
	}
}

func TestEnvironmentErrors(t *testing.T) {
	dir := newRepo(t)
	if code, _, errOut := runCmd("--root", dir, "--base", "origin/missing"); code != 2 || !strings.Contains(errOut, "origin/missing") {
		t.Errorf("missing base = %d %q", code, errOut)
	}
	if code, _, errOut := runCmd("--root", dir, "--disable-pattern", "django"); code != 2 || !strings.Contains(errOut, "vacuously") {
		t.Errorf("vacuous = %d %q", code, errOut)
	}
	if code, _, errOut := runCmd("--root", dir, "--pattern", "bad"); code != 2 || !strings.Contains(errOut, "NAME=REGEXP") {
		t.Errorf("bad pattern = %d %q", code, errOut)
	}
	if code, _, _ := runCmd("--root", t.TempDir()); code != 2 {
		t.Errorf("not a repo = %d, want 2", code)
	}
}
