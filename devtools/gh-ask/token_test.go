package ghask

import (
	"encoding/hex"
	"os"
	"path/filepath"
	"testing"
)

func TestEnsureTokenCreatesAndReuses(t *testing.T) {
	path := filepath.Join(t.TempDir(), "sub", "token")
	first, err := EnsureToken(path)
	if err != nil {
		t.Fatalf("EnsureToken() error = %v", err)
	}
	raw, err := hex.DecodeString(first)
	if err != nil || len(raw) != TokenBytes {
		t.Fatalf("token %q decodes to %d bytes (err %v), want %d", first, len(raw), err, TokenBytes)
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Errorf("token mode = %o, want 600", perm)
	}
	second, err := EnsureToken(path)
	if err != nil {
		t.Fatalf("EnsureToken() second error = %v", err)
	}
	if second != first {
		t.Errorf("EnsureToken() = %q on second call, want existing %q", second, first)
	}
}

func TestReadToken(t *testing.T) {
	dir := t.TempDir()
	if _, err := ReadToken(filepath.Join(dir, "missing")); err == nil {
		t.Error("ReadToken(missing) error = nil")
	}

	loose := filepath.Join(dir, "loose")
	if err := os.WriteFile(loose, []byte("abcdef\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(loose, 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := ReadToken(loose); err == nil {
		t.Error("ReadToken(mode 0644) error = nil, want refusal")
	}

	empty := filepath.Join(dir, "empty")
	if err := os.WriteFile(empty, []byte("\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := ReadToken(empty); err == nil {
		t.Error("ReadToken(empty) error = nil")
	}

	good := filepath.Join(dir, "good")
	if err := os.WriteFile(good, []byte("abcdef\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	got, err := ReadToken(good)
	if err != nil || got != "abcdef" {
		t.Errorf("ReadToken() = %q, %v; want abcdef", got, err)
	}
}
