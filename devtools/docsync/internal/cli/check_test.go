package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/docsync/internal/matcher"
	executor "github.com/jaeyeom/go-cmdexec"
)

const mappingYAML = `
version: 1
rules:
  - match: ["internal/auth/**/*.go"]
    docs:
      - path: docs/api-reference.md
        section: "## Authentication"
        why: "token TTL + claim shape"
  - match: ["config/schema.go"]
    docs:
      - path: docs/configuration.md
        why: "documented option list"
`

func TestCheckRequiresBaseOrFiles(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg}, &stdout, &stderr, nil)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "need --base or --files") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestCheckBaseAndFilesMutuallyExclusive(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg, "--base", "main", "--files", "a.go"}, &stdout, &stderr, nil)
	if code != ExitUsage {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	if !strings.Contains(stderr.String(), "cannot use --base with --files") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestCheckJSONAffected(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	root := filepath.Dir(cfg)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{
		"check", "--config", cfg, "--json",
		"--files", filepath.Join(root, "internal/auth/token.go"),
		"--files", filepath.Join(root, "config/schema.go"),
		"--files", filepath.Join(root, "README.md"),
	}, &stdout, &stderr, nil)
	if code != ExitDocsAffected {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	want, err := os.ReadFile(filepath.Join("testdata", "golden", "check-affected.json"))
	if err != nil {
		t.Fatal(err)
	}
	assertJSONEq(t, stdout.Bytes(), want)
}

func TestCheckJSONClear(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	root := filepath.Dir(cfg)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg, "--json", "--files", filepath.Join(root, "README.md")}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	want, err := os.ReadFile(filepath.Join("testdata", "golden", "check-clear.json"))
	if err != nil {
		t.Fatal(err)
	}
	assertJSONEq(t, stdout.Bytes(), want)
}

func TestCheckTextAffected(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	root := filepath.Dir(cfg)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{
		"check", "--config", cfg,
		"--files", filepath.Join(root, "internal/auth/token.go"),
		"--files", filepath.Join(root, "config/schema.go"),
	}, &stdout, &stderr, nil)
	if code != ExitDocsAffected {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := stdout.String()
	if !strings.Contains(got, "Affected docs (2):") {
		t.Fatalf("stdout = %q", got)
	}
	if !strings.Contains(got, "docs/api-reference.md  §Authentication") {
		t.Fatalf("stdout = %q", got)
	}
	if !strings.Contains(got, "← internal/auth/token.go (why: token TTL + claim shape)") {
		t.Fatalf("stdout = %q", got)
	}
	if !strings.Contains(got, "docs/configuration.md") {
		t.Fatalf("stdout = %q", got)
	}
}

func TestCheckTextClear(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	root := filepath.Dir(cfg)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg, "--files", filepath.Join(root, "README.md")}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	if strings.TrimSpace(stdout.String()) != "All clear — no docs implicated." {
		t.Fatalf("stdout = %q", stdout.String())
	}
}

func TestCheckExitZero(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	root := filepath.Dir(cfg)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg, "--exit-zero", "--files", filepath.Join(root, "config/schema.go")}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, want 0, stderr=%q", code, stderr.String())
	}
}

func TestCheckConfigNotFoundDiscovery(t *testing.T) {
	dir := t.TempDir()
	orig := startDirFn
	startDirFn = func() string { return dir }
	t.Cleanup(func() { startDirFn = orig })
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--files", "a.go"}, &stdout, &stderr, nil)
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitPrecondition, stderr.String())
	}
}

func TestCheckMissingConfigFlag(t *testing.T) {
	t.Parallel()
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", filepath.Join(t.TempDir(), "nope.yml"), "--files", "a.go"}, &stdout, &stderr, nil)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
}

func TestCheckBaseUsesGitArgv(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	root := filepath.Dir(cfg)
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("git", true)
	mock.ExpectCommandWithArgs("git", "diff", "--name-only", "main...HEAD").
		WillSucceed("internal/auth/token.go\n", 0).Build()
	mock.ExpectCommandWithArgs("git", "rev-parse", "--show-toplevel").
		WillSucceed(root+"\n", 0).Build()
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg, "--base", "main", "--json"}, &stdout, &stderr, mock)
	if code != ExitDocsAffected {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var got matcher.Result
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatal(err)
	}
	if len(got.Affected) != 1 || got.Affected[0].Path != "docs/api-reference.md" {
		t.Fatalf("affected = %+v", got.Affected)
	}
}

func TestCheckGitMissing(t *testing.T) {
	t.Parallel()
	cfg := writeDocsync(t, mappingYAML)
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("git", "diff", "--name-only", "main...HEAD").
		WillError(&executor.ExecutableNotFoundError{Command: "git"}).Build()
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg, "--base", "main"}, &stdout, &stderr, mock)
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want 3, stderr=%q", code, stderr.String())
	}
}

func TestCheckStdinDash(t *testing.T) {
	cfg := writeDocsync(t, mappingYAML)
	root := filepath.Dir(cfg)
	restore := swapStdin(t, filepath.Join(root, "config/schema.go")+"\n")
	defer restore()
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"check", "--config", cfg, "-"}, &stdout, &stderr, nil)
	if code != ExitDocsAffected {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
}

func TestRelativizeUserPath(t *testing.T) {
	t.Parallel()
	root := t.TempDir()
	outside := t.TempDir()
	insideCwd := filepath.Join(root, "internal")
	absInside := filepath.Join(root, "internal", "auth", "token.go")
	absOutside := filepath.Join(outside, "internal", "auth", "token.go")

	tests := []struct {
		name string
		cwd  string
		p    string
		want string
		ok   bool
	}{
		{name: "abs inside root", cwd: outside, p: absInside, want: "internal/auth/token.go", ok: true},
		{name: "abs outside root", cwd: outside, p: absOutside, ok: false},
		{name: "rel cwd-inside root", cwd: insideCwd, p: "auth/token.go", want: "internal/auth/token.go", ok: true},
		{name: "rel mapping-root-style from outside cwd dropped", cwd: outside, p: "internal/auth/token.go", ok: false},
		{name: "empty path", cwd: outside, p: "", ok: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, ok := relativizeUserPath(root, tc.cwd, tc.p)
			if ok != tc.ok || got != tc.want {
				t.Fatalf("relativizeUserPath(...) = %q, %v; want %q, %v", got, ok, tc.want, tc.ok)
			}
		})
	}
}

func writeDocsync(t *testing.T, body string) string {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "docsync.yml")
	if err := os.WriteFile(path, []byte(strings.TrimSpace(body)+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func assertJSONEq(t *testing.T, got, want []byte) {
	t.Helper()
	var g, w any
	if err := json.Unmarshal(got, &g); err != nil {
		t.Fatalf("got json: %v\n%s", err, got)
	}
	if err := json.Unmarshal(want, &w); err != nil {
		t.Fatalf("want json: %v\n%s", err, want)
	}
	gb, _ := json.Marshal(g)
	wb, _ := json.Marshal(w)
	if string(gb) != string(wb) {
		t.Fatalf("json mismatch\ngot:  %s\nwant: %s", got, want)
	}
}

func swapStdin(t *testing.T, body string) (restore func()) {
	t.Helper()
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	old := os.Stdin
	os.Stdin = r
	done := make(chan struct{})
	go func() {
		_, _ = io.WriteString(w, body)
		_ = w.Close()
		close(done)
	}()
	return func() {
		<-done
		os.Stdin = old
		_ = r.Close()
	}
}
