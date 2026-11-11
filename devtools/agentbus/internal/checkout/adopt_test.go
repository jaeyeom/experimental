package checkout

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const testRef = "refs/lease-backup/20261110T090000Z-ops-alice"

func writeIn(t *testing.T, dir, name, text string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(text), 0o600); err != nil {
		t.Fatal(err)
	}
}

func stashList(t *testing.T, env []string, dir string) string {
	t.Helper()
	return gitOut(t, env, dir, "stash", "list")
}

// treeFiles lists the files in ref's tree with their content.
func treeFiles(t *testing.T, env []string, dir, ref string) map[string]string {
	t.Helper()
	out := map[string]string{}
	for _, name := range strings.Split(gitOut(t, env, dir, "ls-tree", "-r", "--name-only", ref), "\n") {
		if name != "" {
			out[name] = gitOut(t, env, dir, "show", ref+":"+name)
		}
	}
	return out
}

func TestSaveLeftoversKeepsModifiedStagedAndUntrackedFilesBeforeCleaning(t *testing.T) {
	env := gitEnv(t)
	clone, _ := newClone(t, env)
	gitIn(t, env, clone, "switch", "-q", "-c", "feature")
	writeIn(t, clone, "a.txt", "modified\n")
	writeIn(t, clone, "staged.txt", "staged\n")
	gitIn(t, env, clone, "add", "staged.txt")
	if err := os.MkdirAll(filepath.Join(clone, "sub"), 0o755); err != nil {
		t.Fatal(err)
	}
	writeIn(t, clone, "sub/untracked.txt", "untracked\n")
	writeIn(t, clone, ".git/info/exclude", "*.log\n")
	writeIn(t, clone, "build.log", "ignored\n")
	head := gitOut(t, env, clone, "rev-parse", "HEAD")
	stashBefore := stashList(t, env, clone)

	saved, err := SaveLeftovers(clone, testRef, "lease backup of local:"+clone+" from ops/alice", env)
	if err != nil {
		t.Fatal(err)
	}
	if len(saved) != 1 || saved[0] != testRef {
		t.Fatalf("saved = %v", saved)
	}
	// The backup holds every change; the checkout is not touched yet.
	files := treeFiles(t, env, clone, testRef)
	want := map[string]string{"a.txt": "modified", "staged.txt": "staged", "sub/untracked.txt": "untracked"}
	for name, text := range want {
		if files[name] != text {
			t.Errorf("backup %s = %q, want %q (tree %v)", name, files[name], text, files)
		}
	}
	if _, ok := files["build.log"]; ok {
		t.Errorf("backup holds an ignored file")
	}
	if parent := gitOut(t, env, clone, "rev-parse", testRef+"^1"); parent != head {
		t.Errorf("backup parent = %s, want HEAD %s", parent, head)
	}
	// The second parent is the index: staged.txt staged, a.txt not.
	if got := gitOut(t, env, clone, "show", testRef+"^2:staged.txt"); got != "staged" {
		t.Errorf("index parent staged.txt = %q", got)
	}
	if got := gitOut(t, env, clone, "show", testRef+"^2:a.txt"); got != "a" {
		t.Errorf("index parent a.txt = %q", got)
	}
	msg := gitOut(t, env, clone, "log", "-1", "--format=%B", testRef)
	for _, part := range []string{"lease backup of local:" + clone + " from ops/alice", "branch: feature", "a.txt", "sub/untracked.txt"} {
		if !strings.Contains(msg, part) {
			t.Errorf("backup message is missing %q:\n%s", part, msg)
		}
	}
	if got := readFile(t, filepath.Join(clone, "sub/untracked.txt")); got != "untracked\n" {
		t.Fatalf("SaveLeftovers changed the tree: %q", got)
	}
	if branches := gitOut(t, env, clone, "branch", "--list", "--format=%(refname)"); strings.Contains(branches, "lease-backup") {
		t.Fatalf("backup made a branch: %s", branches)
	}

	if err := Clean(clone, env); err != nil {
		t.Fatal(err)
	}
	if tree, err := Inspect(clone, env); err != nil || tree.Reason() != "" {
		t.Fatalf("after clean = %+v, %v", tree, err)
	}
	if _, err := os.Stat(filepath.Join(clone, "build.log")); err != nil {
		t.Fatalf("clean removed an ignored file: %v", err)
	}
	if got := stashList(t, env, clone); got != stashBefore {
		t.Fatalf("stash list changed: %q", got)
	}
	refs, err := Backups(clone, env)
	if err != nil || len(refs) != 1 || refs[0] != testRef {
		t.Fatalf("backups = %v, %v", refs, err)
	}
}

func TestSaveLeftoversOfACleanTreeSavesNothing(t *testing.T) {
	env := gitEnv(t)
	clone, _ := newClone(t, env)
	saved, err := SaveLeftovers(clone, testRef, "header", env)
	if err != nil || len(saved) != 0 {
		t.Fatalf("clean tree saved = %v, %v", saved, err)
	}
	if refs, _ := Backups(clone, env); len(refs) != 0 {
		t.Fatalf("backups = %v", refs)
	}
}

func TestSaveLeftoversNeverOverwritesABackup(t *testing.T) {
	env := gitEnv(t)
	clone, _ := newClone(t, env)
	writeIn(t, clone, "one.txt", "one\n")
	if _, err := SaveLeftovers(clone, testRef, "first", env); err != nil {
		t.Fatal(err)
	}
	writeIn(t, clone, "two.txt", "two\n")
	saved, err := SaveLeftovers(clone, testRef, "second", env)
	if err != nil || len(saved) != 1 || saved[0] != testRef+"-2" {
		t.Fatalf("second save = %v, %v", saved, err)
	}
	if files := treeFiles(t, env, clone, testRef); files["two.txt"] != "" {
		t.Fatalf("the first backup was overwritten: %v", files)
	}
}

func TestSaveLeftoversRecordsAndAbortsARebase(t *testing.T) {
	env := gitEnv(t)
	clone, _ := newClone(t, env)
	commit := func(text string) {
		writeIn(t, clone, "a.txt", text)
		gitIn(t, env, clone, "-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-q", "-am", text)
	}
	gitIn(t, env, clone, "switch", "-q", "-c", "feature")
	commit("feature\n")
	gitIn(t, env, clone, "switch", "-q", "main")
	commit("main\n")
	gitIn(t, env, clone, "switch", "-q", "feature")
	cmdErr := runGit(env, clone, "-c", "user.name=t", "-c", "user.email=t@example.com", "rebase", "main")
	if cmdErr == nil {
		t.Fatal("rebase did not stop on the conflict")
	}
	if tree, _ := Inspect(clone, env); tree.Operation != "rebase" {
		t.Fatalf("operation = %q", tree.Operation)
	}
	writeIn(t, clone, "resolution-notes.txt", "half done\n")
	stashBefore := stashList(t, env, clone)

	saved, err := SaveLeftovers(clone, testRef, "header", env)
	if err != nil || len(saved) == 0 || saved[0] != testRef {
		t.Fatalf("saved = %v, %v", saved, err)
	}
	msg := gitOut(t, env, clone, "log", "-1", "--format=%B", testRef)
	if !strings.Contains(msg, "operation: rebase") || !strings.Contains(msg, "refs/heads/feature") {
		t.Fatalf("backup message does not record the rebase:\n%s", msg)
	}
	files := treeFiles(t, env, clone, testRef)
	if files["resolution-notes.txt"] != "half done" || !strings.Contains(files["a.txt"], "<<<<<<<") {
		t.Fatalf("backup tree = %v", files)
	}
	if tree, _ := Inspect(clone, env); tree.Operation != "" {
		t.Fatalf("rebase still in progress after save: %+v", tree)
	}
	if err := Clean(clone, env); err != nil {
		t.Fatal(err)
	}
	if tree, _ := Inspect(clone, env); tree.Reason() != "" {
		t.Fatalf("after clean = %+v", tree)
	}
	if got := stashList(t, env, clone); got != stashBefore {
		t.Fatalf("stash list changed: %q", got)
	}
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

func runGit(env []string, dir string, args ...string) error {
	_, err := git(env, dir, args...)
	return err
}
