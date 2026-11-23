package memstore

import (
	"fmt"
	"os"
	"path/filepath"
)

// RejectGitWorkTree walks parent directories of path and returns an error if
// a .git directory or file is found, preventing vault data from being stored
// inside a git repository or worktree.
func RejectGitWorkTree(path string) error {
	if path == "" {
		return fmt.Errorf("vault path is empty")
	}
	abs, err := filepath.Abs(path)
	if err != nil {
		return fmt.Errorf("resolve path %q: %w", path, err)
	}
	for d := abs; ; d = filepath.Dir(d) {
		if _, err := os.Lstat(filepath.Join(d, ".git")); err == nil {
			return fmt.Errorf("path %s is inside the git work tree %s; choose a directory outside any repository", path, d)
		}
		if parent := filepath.Dir(d); parent == d {
			return nil
		}
	}
}

// EnsureSecureDir creates dir with mode 0700 and verifies its permissions.
func EnsureSecureDir(dir string) error {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create directory %s: %w", dir, err)
	}
	if err := os.Chmod(dir, 0o700); err != nil {
		return fmt.Errorf("chmod directory %s: %w", dir, err)
	}
	fi, err := os.Stat(dir)
	if err != nil {
		return fmt.Errorf("stat directory %s: %w", dir, err)
	}
	if !fi.IsDir() {
		return fmt.Errorf("%s is not a directory", dir)
	}
	if perm := fi.Mode().Perm(); perm != 0o700 {
		return fmt.Errorf("directory %s has mode %04o, want 0700", dir, perm)
	}
	return nil
}

// EnsureSecureFile ensures an existing file at path has mode 0600 and verifies
// its permissions.
func EnsureSecureFile(path string) error {
	fi, err := os.Stat(path)
	if err != nil {
		return fmt.Errorf("stat file %s: %w", path, err)
	}
	if fi.IsDir() {
		return fmt.Errorf("%s is a directory, want file", path)
	}
	if err := os.Chmod(path, 0o600); err != nil {
		return fmt.Errorf("chmod file %s: %w", path, err)
	}
	fi, err = os.Stat(path)
	if err != nil {
		return fmt.Errorf("stat file %s: %w", path, err)
	}
	if perm := fi.Mode().Perm(); perm != 0o600 {
		return fmt.Errorf("file %s has mode %04o, want 0600", path, perm)
	}
	return nil
}
