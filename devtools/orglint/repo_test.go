package orglint

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// repoRoot returns the checkout root. Under Bazel the go.mod runfile is a
// symlink into the checkout, so resolving it finds the real tree (and the
// real .org files, which Bazel cannot glob across packages).
func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("getwd: %w", err)
	}
	for {
		mod := filepath.Join(dir, "go.mod")
		if _, err := os.Stat(mod); err == nil {
			resolved, err := filepath.EvalSymlinks(mod)
			if err != nil {
				return "", fmt.Errorf("resolve %s: %w", mod, err)
			}
			return filepath.Dir(resolved), nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", errors.New("go.mod not found")
		}
		dir = parent
	}
}

// orgFiles lists the .org files under root, skipping hidden directories,
// node_modules and Bazel output symlinks.
func orgFiles(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		name := d.Name()
		if d.IsDir() && path != root &&
			(strings.HasPrefix(name, ".") || name == "node_modules" || strings.HasPrefix(name, "bazel-")) {
			return filepath.SkipDir
		}
		if !d.IsDir() && strings.HasSuffix(name, ".org") {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk %s: %w", root, err)
	}
	return files, nil
}

// TestRepoOrgFilesLintClean lints every .org file in the repository with the
// Go linter in one pass. It never starts Emacs.
func TestRepoOrgFilesLintClean(t *testing.T) {
	root, err := repoRoot()
	if err != nil {
		t.Fatal(err)
	}
	files, err := orgFiles(root)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("no .org files found")
	}
	for _, name := range files {
		got, err := LintFile(name, Checkers())
		if err != nil {
			t.Error(err)
		}
		for _, f := range got {
			rel, _ := filepath.Rel(root, name)
			t.Errorf("%s:%d: %s: %s", rel, f.Line, f.Checker, f.Message)
		}
	}
}
