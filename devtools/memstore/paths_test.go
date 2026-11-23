package memstore

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestRejectGitWorkTree(t *testing.T) {
	t.Run("allows path outside git", func(t *testing.T) {
		dir := filepath.Join(t.TempDir(), "vaults", "shared.db")
		if err := RejectGitWorkTree(dir); err != nil {
			t.Errorf("RejectGitWorkTree(%q) = %v, want nil", dir, err)
		}
	})

	t.Run("rejects path inside git directory repo", func(t *testing.T) {
		repo := t.TempDir()
		if err := os.Mkdir(filepath.Join(repo, ".git"), 0o700); err != nil {
			t.Fatal(err)
		}
		target := filepath.Join(repo, "sub", "vault.db")
		err := RejectGitWorkTree(target)
		if err == nil {
			t.Fatalf("RejectGitWorkTree(%q) = nil, want error", target)
		}
		if !strings.Contains(err.Error(), "git") {
			t.Errorf("error = %q, want mention of git", err.Error())
		}
	})

	t.Run("rejects path inside git worktree with .git file", func(t *testing.T) {
		wt := t.TempDir()
		if err := os.WriteFile(filepath.Join(wt, ".git"), []byte("gitdir: /some/repo/.git/worktrees/wt\n"), 0o600); err != nil {
			t.Fatal(err)
		}
		target := filepath.Join(wt, "nested", "vault.db")
		if err := RejectGitWorkTree(target); err == nil {
			t.Fatalf("RejectGitWorkTree(%q) = nil, want error for .git file", target)
		}
	})

	t.Run("rejects empty path", func(t *testing.T) {
		if err := RejectGitWorkTree(""); err == nil {
			t.Fatal("RejectGitWorkTree(\"\") = nil, want error")
		}
	})
}

func TestEnsureSecureDir(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "vaults", "nested")
	if err := EnsureSecureDir(dir); err != nil {
		t.Fatalf("EnsureSecureDir: %v", err)
	}
	fi, err := os.Stat(dir)
	if err != nil {
		t.Fatal(err)
	}
	if !fi.IsDir() {
		t.Fatalf("%s is not a directory", dir)
	}
	if got := fi.Mode().Perm(); got != 0o700 {
		t.Errorf("mode = %o, want 0700", got)
	}

	// Tightens looser existing directory permissions.
	if err := os.Chmod(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := EnsureSecureDir(dir); err != nil {
		t.Fatalf("EnsureSecureDir existing: %v", err)
	}
	fi, err = os.Stat(dir)
	if err != nil {
		t.Fatal(err)
	}
	if got := fi.Mode().Perm(); got != 0o700 {
		t.Errorf("tightened mode = %o, want 0700", got)
	}
}

func TestEnsureSecureFile(t *testing.T) {
	path := filepath.Join(t.TempDir(), "vault.db")
	if err := EnsureSecureFile(path); err == nil {
		t.Fatal("EnsureSecureFile on missing file = nil, want error")
	}

	if err := os.WriteFile(path, []byte("data"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := EnsureSecureFile(path); err != nil {
		t.Fatalf("EnsureSecureFile: %v", err)
	}
	fi, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if got := fi.Mode().Perm(); got != 0o600 {
		t.Errorf("mode = %o, want 0600", got)
	}
}
