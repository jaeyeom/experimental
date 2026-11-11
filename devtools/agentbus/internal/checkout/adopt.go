package checkout

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// backupIdentity signs backup commits, so a checkout without a git
// identity can still be saved.
var backupIdentity = []string{
	"GIT_AUTHOR_NAME=agentbus lease", "GIT_AUTHOR_EMAIL=agentbus-lease@localhost",
	"GIT_COMMITTER_NAME=agentbus lease", "GIT_COMMITTER_EMAIL=agentbus-lease@localhost",
}

// SaveLeftovers saves root's uncommitted work before adopt cleans it.
// When the tree is dirty or an operation is in progress, it commits the
// whole working tree, untracked files included (ignored files are not
// touched by Clean and are not saved), to ref, with HEAD and the index
// as parents and header, the branch, the operation and the changed files
// in the message. Then it aborts the operation, and saves again if the
// abort left changes (a rebase's autostash). It never uses a branch or
// git stash, and never overwrites a ref: a taken name gets -2, -3, ...
// It returns the refs it wrote; none for a clean tree. It leaves the
// tree as it found it, apart from the aborted operation.
func SaveLeftovers(root, ref, header string, env []string) ([]string, error) {
	var saved []string
	for range 2 {
		tree, err := Inspect(root, env)
		if err != nil {
			return saved, err
		}
		if tree.Reason() == "" {
			return saved, nil
		}
		if len(saved) == 0 || len(tree.Changed) > 0 {
			name, err := backup(root, ref, header, tree, env)
			if err != nil {
				return saved, err
			}
			saved = append(saved, name)
		}
		if tree.Operation == "" {
			return saved, nil
		}
		if _, err := git(env, root, tree.Operation, "--abort"); err != nil {
			return saved, fmt.Errorf("abort the %s (your work is saved to %s): %w", tree.Operation, saved[len(saved)-1], err)
		}
	}
	return saved, nil
}

// Clean drops every change in root, tracked and untracked, keeping
// ignored files. Call it only after SaveLeftovers.
func Clean(root string, env []string) error {
	if _, err := git(env, root, "reset", "--hard", "--quiet"); err != nil {
		return err
	}
	_, err := git(env, root, "clean", "-d", "--force", "--quiet")
	return err
}

// Backups lists root's lease backup refs, oldest name first.
func Backups(root string, env []string) ([]string, error) {
	out, err := git(env, root, "for-each-ref", "--format=%(refname)", "--sort=refname", "refs/lease-backup/")
	if err != nil {
		return nil, err
	}
	return strings.Fields(out), nil
}

// backup commits root's working tree through a temporary index, so the
// real index and the tree are left alone, and points a new ref at it.
func backup(root, ref, header string, tree Tree, env []string) (string, error) {
	gitDir, err := git(env, root, "rev-parse", "--absolute-git-dir")
	if err != nil {
		return "", err
	}
	gitDir = strings.TrimSpace(gitDir)
	treeID, err := worktreeTree(root, gitDir, env)
	if err != nil {
		return "", err
	}
	var parents []string
	head, headErr := git(env, root, "rev-parse", "--verify", "--quiet", "HEAD")
	if headErr == nil {
		parents = append(parents, "-p", strings.TrimSpace(head))
	}
	// The index as a second parent keeps what was staged apart from the
	// tree. An index with conflicts cannot be written; the tree still
	// holds the conflicted files.
	if index, err := git(env, root, "write-tree"); err == nil {
		args := append([]string{"commit-tree", strings.TrimSpace(index)}, parents...)
		if id, err := git(append(env, backupIdentity...), root, append(args, "-m", "index")...); err == nil {
			parents = append(parents, "-p", strings.TrimSpace(id))
		}
	}
	msg := header + "\n\n" + describe(root, gitDir, tree, env)
	args := append([]string{"commit-tree", treeID}, parents...)
	commit, err := git(append(env, backupIdentity...), root, append(args, "-m", msg)...)
	if err != nil {
		return "", err
	}
	commit = strings.TrimSpace(commit)
	for i := 1; i <= 100; i++ {
		name := ref
		if i > 1 {
			name = ref + "-" + strconv.Itoa(i)
		}
		if _, err := git(env, root, "rev-parse", "--verify", "--quiet", name); err == nil {
			continue
		}
		// An empty old value creates the ref only if it does not exist.
		if _, err := git(env, root, "update-ref", "-m", "agentbus lease adopt", name, commit, ""); err != nil {
			return "", err
		}
		got, err := git(env, root, "rev-parse", "--verify", "--quiet", name+"^{tree}")
		if err != nil || strings.TrimSpace(got) != treeID {
			return "", fmt.Errorf("backup %s does not hold the saved tree %s", name, treeID)
		}
		return name, nil
	}
	return "", fmt.Errorf("no free backup ref after %s-100", ref)
}

// worktreeTree writes the whole working tree, untracked files included,
// as a tree through a copy of the index.
func worktreeTree(root, gitDir string, env []string) (string, error) {
	tmp, err := os.CreateTemp(gitDir, "lease-backup-index-*")
	if err != nil {
		return "", fmt.Errorf("temporary index: %w", err)
	}
	path := tmp.Name()
	defer os.Remove(path)
	src, err := os.Open(filepath.Join(gitDir, "index"))
	switch {
	case err == nil:
		_, err = io.Copy(tmp, src)
		src.Close()
		if cerr := tmp.Close(); err == nil {
			err = cerr
		}
		if err != nil {
			return "", fmt.Errorf("copy the index: %w", err)
		}
	case errors.Is(err, os.ErrNotExist):
		// No index yet: git starts an empty one where the file is absent.
		tmp.Close()
		os.Remove(path)
	default:
		tmp.Close()
		return "", fmt.Errorf("read the index: %w", err)
	}
	tmpEnv := append(append([]string{}, env...), "GIT_INDEX_FILE="+path)
	if _, err := git(tmpEnv, root, "add", "--all"); err != nil {
		return "", err
	}
	out, err := git(tmpEnv, root, "write-tree")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(out), nil
}

// describe is the backup message body: the branch, the operation in
// progress with its state, and the changed files.
func describe(root, gitDir string, tree Tree, env []string) string {
	var b strings.Builder
	if branch, err := git(env, root, "symbolic-ref", "--quiet", "--short", "HEAD"); err == nil {
		fmt.Fprintf(&b, "branch: %s\n", strings.TrimSpace(branch))
	} else if head, err := git(env, root, "rev-parse", "--verify", "--quiet", "HEAD"); err == nil {
		fmt.Fprintf(&b, "branch: none, HEAD detached at %s\n", strings.TrimSpace(head))
	}
	if tree.Operation != "" {
		fmt.Fprintf(&b, "operation: %s in progress, aborted after this backup\n", tree.Operation)
		for _, f := range operationFiles {
			if data, err := os.ReadFile(filepath.Join(gitDir, f)); err == nil {
				fmt.Fprintf(&b, "  %s: %s\n", f, strings.TrimSpace(string(data)))
			}
		}
	}
	if len(tree.Changed) > 0 {
		b.WriteString("changed:\n")
		for _, line := range tree.Changed {
			fmt.Fprintf(&b, "  %s\n", line)
		}
	}
	return b.String()
}

// operationFiles record an operation's state in the git dir.
var operationFiles = []string{
	"rebase-merge/head-name", "rebase-merge/onto", "rebase-merge/orig-head", "rebase-merge/msgnum", "rebase-merge/end",
	"rebase-apply/head-name", "rebase-apply/onto", "rebase-apply/orig-head", "rebase-apply/next", "rebase-apply/last",
	"MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD",
}
