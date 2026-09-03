package cli

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestValidateOK(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "docs"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "docs", "a.md"), []byte("x\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "a.go"), []byte("package a\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg := filepath.Join(dir, "docsync.yml")
	if err := os.WriteFile(cfg, []byte("version: 1\nrules:\n  - match: [\"a.go\"]\n    docs:\n      - path: docs/a.md\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"validate", "--config", cfg}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	if strings.TrimSpace(stdout.String()) != "docsync.yml: ok" {
		t.Fatalf("stdout = %q", stdout.String())
	}
}

func TestValidateProblems(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "docs"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "docs", "ok.md"), []byte("x\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg := filepath.Join(dir, "docsync.yml")
	body := "version: 1\nrules:\n" +
		"  - match: [\"missing.go\"]\n    docs:\n      - path: docs/ok.md\n" +
		"  - match: [\"also-missing.go\"]\n    docs:\n      - path: docs/old-name.md\n"
	if err := os.WriteFile(cfg, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"validate", "--config", cfg}, &stdout, &stderr, nil)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want 2, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := stdout.String()
	if !strings.Contains(got, "problems") {
		t.Fatalf("stdout = %q", got)
	}
	if !strings.Contains(got, `docs/old-name.md" does not exist`) {
		t.Fatalf("stdout = %q", got)
	}
	if !strings.Contains(got, "matches no files currently tracked") {
		t.Fatalf("stdout = %q", got)
	}
}

func TestValidateWarningsOnly(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "docs"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "docs", "ok.md"), []byte("x\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg := filepath.Join(dir, "docsync.yml")
	if err := os.WriteFile(cfg, []byte("version: 1\nrules:\n  - match: [\"legacy/*.go\"]\n    docs:\n      - path: docs/ok.md\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"validate", "--config", cfg}, &stdout, &stderr, nil)
	if code != ExitDocsAffected {
		t.Fatalf("exit = %d, want 1, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	if !strings.Contains(stdout.String(), "warn") {
		t.Fatalf("stdout = %q", stdout.String())
	}
}

func TestValidateUnknownKey(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, "version: 1\nrules:\n  - match: [\"a.go\"]\n    sections: x\n    docs:\n      - path: docs/a.md\n")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"validate", "--config", cfg}, &stdout, &stderr, nil)
	if code != ExitUsage {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	if !strings.Contains(stderr.String(), "sections") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}
