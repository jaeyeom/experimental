package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestRunListAndWrite(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	unformatted := filepath.Join(dir, "bad.go")
	orig := `package example

import (
	"strings"
	"github.com/example/pkg"
	"fmt"
)

func f() {
	fmt.Println(strings.ToUpper(pkg.Name))
}
`
	if err := os.WriteFile(unformatted, []byte(orig), 0o600); err != nil {
		t.Fatalf("write bad.go: %v", err)
	}

	var stdout, stderr bytes.Buffer
	code := run([]string{"-l", unformatted}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("expected exit 0 for -l, got %d (stderr: %s)", code, stderr.String())
	}
	if got := strings.TrimSpace(stdout.String()); got != unformatted {
		t.Fatalf("expected -l to report %q, got %q", unformatted, got)
	}

	// Verify -l did not modify the file.
	afterList, err := os.ReadFile(unformatted)
	if err != nil {
		t.Fatalf("read bad.go: %v", err)
	}
	if string(afterList) != orig {
		t.Fatalf("-l unexpectedly modified file")
	}

	// Fix with -w.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"-w", unformatted}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("expected exit 0 for -w, got %d (stderr: %s)", code, stderr.String())
	}

	// Verify -l now reports nothing.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"-l", unformatted}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("expected exit 0 for -l after -w, got %d (stderr: %s)", code, stderr.String())
	}
	if stdout.Len() != 0 {
		t.Fatalf("expected empty -l output after -w, got %q", stdout.String())
	}

	// Verify imports are grouped and sorted.
	fixed, err := os.ReadFile(unformatted)
	if err != nil {
		t.Fatalf("read fixed bad.go: %v", err)
	}
	want := `package example

import (
	"fmt"
	"strings"

	"github.com/example/pkg"
)

func f() {
	fmt.Println(strings.ToUpper(pkg.Name))
}
`
	if string(fixed) != want {
		t.Fatalf("formatted output mismatch:\ngot:\n%s\nwant:\n%s", string(fixed), want)
	}
}

func TestRunSyntaxError(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	broken := filepath.Join(dir, "broken.go")
	if err := os.WriteFile(broken, []byte("package example\nfunc (\n"), 0o600); err != nil {
		t.Fatalf("write broken.go: %v", err)
	}

	var stdout, stderr bytes.Buffer
	code := run([]string{"-l", broken}, &stdout, &stderr)
	if code == 0 {
		t.Fatalf("expected non-zero exit code for syntax error")
	}
	if stderr.Len() == 0 {
		t.Fatalf("expected error message on stderr")
	}
}
