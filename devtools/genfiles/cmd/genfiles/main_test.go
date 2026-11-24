package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

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

func newRepo(t *testing.T, list string) string {
	t.Helper()
	dir := t.TempDir()
	gitIn(t, dir, "init", "-q", "-b", "main")
	writeFile(t, filepath.Join(dir, "Cargo.lock"), "x\n")
	writeFile(t, filepath.Join(dir, ".gitattributes"), "Cargo.lock linguist-generated=true\n")
	writeFile(t, filepath.Join(dir, "generated_files.yaml"), list)
	gitIn(t, dir, "add", "-A")
	return dir
}

func runCmd(args ...string) (int, string, string) {
	var stdout, stderr bytes.Buffer
	code := run(args, &stdout, &stderr)
	return code, stdout.String(), stderr.String()
}

func TestCheckCommand(t *testing.T) {
	dir := newRepo(t, "- pattern: Cargo.lock\n  run: [[cargo, generate-lockfile]]\n")
	if code, out, errOut := runCmd("check", "--root", filepath.Join(dir)); code != 0 || !strings.Contains(out, "PASSED (1 patterns, 1 files)") {
		t.Fatalf("clean = %d\n%s%s", code, out, errOut)
	}

	writeFile(t, filepath.Join(dir, "generated_files.yaml"), "- pattern: Cargo.lock\n  run: [[{var: path}]]\n")
	code, _, errOut := runCmd("check", "--root", dir)
	if code != 1 || !strings.Contains(errOut, "the program must be a literal string") {
		t.Fatalf("broken list = %d\n%s", code, errOut)
	}
}

func TestCheckListFlag(t *testing.T) {
	dir := newRepo(t, "- pattern: Cargo.lock\n")
	writeFile(t, filepath.Join(dir, "tools", "gen.yaml"), "- pattern: Cargo.lock\n")
	sub := filepath.Join(dir, "tools")
	// A relative --list is resolved from the repository root, not --root.
	if code, out, errOut := runCmd("check", "--root", sub, "--list", "tools/gen.yaml"); code != 0 {
		t.Fatalf("relative --list = %d\n%s%s", code, out, errOut)
	}
	if code, _, errOut := runCmd("check", "--root", dir, "--list", "missing.yaml"); code != 2 || !strings.Contains(errOut, "missing.yaml") {
		t.Errorf("missing list = %d %q", code, errOut)
	}
}

func TestUsage(t *testing.T) {
	if code, out, _ := runCmd("--version"); code != 0 || !strings.HasPrefix(out, "genfiles ") {
		t.Errorf("--version = %d %q", code, out)
	}
	for _, args := range [][]string{{}, {"nope"}, {"check", "--nope"}, {"check", "extra"}} {
		if code, _, _ := runCmd(args...); code != 2 {
			t.Errorf("run(%q) = %d, want 2", args, code)
		}
	}
	if code, _, _ := runCmd("check", "--root", t.TempDir()); code != 2 {
		t.Errorf("not a repo: want 2")
	}
}

func TestCheckNewlineFileName(t *testing.T) {
	dir := newRepo(t, "- pattern: Cargo.lock\n")
	writeFile(t, filepath.Join(dir, "a\nb", "Cargo.lock"), "x\n")
	gitIn(t, dir, "add", "-A")
	code, _, errOut := runCmd("check", "--root", dir)
	if code != 1 || !strings.Contains(errOut, `unsafe file name "a\nb/Cargo.lock"`) {
		t.Fatalf("newline name = %d\n%s", code, errOut)
	}
}

func TestCheckEmptyRepo(t *testing.T) {
	dir := t.TempDir()
	gitIn(t, dir, "init", "-q", "-b", "main")
	writeFile(t, filepath.Join(dir, "generated_files.yaml"), "- pattern: \"**\"\n")
	// The list itself is untracked, so nothing is tracked; ** must not
	// match a phantom empty path.
	code, _, errOut := runCmd("check", "--root", dir)
	if code != 1 || !strings.Contains(errOut, `stale pattern "**"`) {
		t.Fatalf("empty repo = %d\n%s", code, errOut)
	}
}
