// Package checkout runs the lease's git work on the checkout's machine:
// which guarded checkout a path belongs to, whether its working tree is
// clean, and release's switch to the default branch and pull.
package checkout

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// gitTimeout bounds each local git call. pullTimeout bounds the pull,
// which talks to the remote.
const (
	gitTimeout  = 10 * time.Second
	pullTimeout = 2 * time.Minute
)

// Resolve returns the guarded checkout that path belongs to, or "" when
// path is outside every guarded checkout. A path outside every guarded
// directory is decided without running git. A path inside one belongs
// to it only when its git top-level is that checkout, so a separate
// repository or worktree nested inside it is not guarded. path need not
// exist yet. env is the environment git runs with.
func Resolve(path string, guarded []string, env []string) (string, error) {
	abs, err := filepath.Abs(path)
	if err != nil {
		return "", fmt.Errorf("resolve %s: %w", path, err)
	}
	resolved, existing := realPath(abs)
	root := ""
	for _, g := range guarded {
		gr, _ := realPath(filepath.Clean(g))
		if within(resolved, gr) && len(gr) > len(root) {
			root = gr
		}
	}
	if root == "" {
		return "", nil
	}
	if rel, _ := filepath.Rel(root, resolved); rel == ".git" || strings.HasPrefix(rel, ".git"+string(filepath.Separator)) {
		return root, nil
	}
	dir := existing
	if info, err := os.Stat(dir); err == nil && !info.IsDir() {
		dir = filepath.Dir(dir)
	}
	out, err := git(env, dir, "rev-parse", "--show-toplevel")
	if err != nil {
		return "", err
	}
	top, _ := realPath(strings.TrimSpace(out))
	if top != root {
		return "", nil
	}
	return root, nil
}

// Tree is the state of a working tree.
type Tree struct {
	// Changed are git status --porcelain lines: modified, staged, and
	// untracked files that are not ignored.
	Changed []string
	// Operation is rebase, merge, cherry-pick, or revert when one is in
	// progress.
	Operation string
}

// Reason says why the tree is not clean, or "" when it is.
func (t Tree) Reason() string {
	var parts []string
	if t.Operation != "" {
		parts = append(parts, t.Operation+" in progress")
	}
	switch n := len(t.Changed); n {
	case 0:
	case 1:
		parts = append(parts, "1 changed file")
	default:
		parts = append(parts, strconv.Itoa(n)+" changed files")
	}
	return strings.Join(parts, ", ")
}

// operations maps a file in the git dir to the operation it marks.
var operations = []struct{ file, name string }{
	{"rebase-merge", "rebase"},
	{"rebase-apply", "rebase"},
	{"MERGE_HEAD", "merge"},
	{"CHERRY_PICK_HEAD", "cherry-pick"},
	{"REVERT_HEAD", "revert"},
}

// Inspect reports whether root's working tree is clean.
func Inspect(root string, env []string) (Tree, error) {
	out, err := git(env, root, "status", "--porcelain", "--untracked-files=all")
	if err != nil {
		return Tree{}, err
	}
	var tree Tree
	for _, line := range strings.Split(out, "\n") {
		if line != "" {
			tree.Changed = append(tree.Changed, line)
		}
	}
	gitDir, err := git(env, root, "rev-parse", "--absolute-git-dir")
	if err != nil {
		return Tree{}, err
	}
	for _, op := range operations {
		if _, err := os.Stat(filepath.Join(strings.TrimSpace(gitDir), op.file)); err == nil {
			tree.Operation = op.name
			break
		}
	}
	return tree, nil
}

// SwitchDefault switches root to the default branch, the one
// origin/HEAD points at, and returns its name.
func SwitchDefault(root string, env []string) (string, error) {
	out, err := git(env, root, "symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD")
	if err != nil {
		return "", fmt.Errorf("find the default branch from origin/HEAD (set it with git remote set-head origin --auto): %w", err)
	}
	branch := strings.TrimPrefix(strings.TrimSpace(out), "origin/")
	if _, err := git(env, root, "switch", "--quiet", branch); err != nil {
		return "", err
	}
	return branch, nil
}

// Pull runs git pull --rebase --prune on the current branch. When the
// pull stops in a rebase, the rebase is aborted so the tree stays clean.
func Pull(root string, env []string) error {
	_, err := gitWithin(pullTimeout, env, root, "pull", "--quiet", "--rebase", "--prune")
	if err == nil {
		return nil
	}
	if tree, inspectErr := Inspect(root, env); inspectErr == nil && tree.Operation == "rebase" {
		if _, abortErr := git(env, root, "rebase", "--abort"); abortErr != nil {
			return errors.Join(err, abortErr)
		}
	}
	return err
}

func git(env []string, dir string, args ...string) (string, error) {
	return gitWithin(gitTimeout, env, dir, args...)
}

func gitWithin(timeout time.Duration, env []string, dir string, args ...string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	cmd := exec.CommandContext(ctx, "git", append([]string{"-C", dir}, args...)...) //nolint:gosec // G204: fixed git binary, no shell
	// Status must not take the index lock another process may hold, and
	// a pull must not wait on a credential prompt.
	cmd.Env = append(append([]string{}, env...), "GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return "", fmt.Errorf("git %s in %s: %s", args[0], dir, msg)
	}
	return string(out), nil
}

// realPath resolves symlinks in the longest existing prefix of abs and
// keeps the rest. It also returns that existing prefix, resolved.
func realPath(abs string) (string, string) {
	existing := abs
	var rest []string
	for {
		if resolved, err := filepath.EvalSymlinks(existing); err == nil {
			return filepath.Join(append([]string{resolved}, rest...)...), resolved
		} else if !errors.Is(err, os.ErrNotExist) {
			return abs, abs
		}
		parent := filepath.Dir(existing)
		if parent == existing {
			return abs, abs
		}
		rest = append([]string{filepath.Base(existing)}, rest...)
		existing = parent
	}
}

func within(path, root string) bool {
	return path == root || strings.HasPrefix(path, root+string(filepath.Separator))
}
