package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestRunVersion(t *testing.T) {
	for _, flag := range []string{"--version", "-version"} {
		var stdout, stderr bytes.Buffer
		code := run([]string{flag}, &stdout, &stderr)
		if code != 0 || stderr.Len() != 0 {
			t.Fatalf("%s: code %d, stderr %q", flag, code, stderr.String())
		}
		out := strings.TrimSpace(stdout.String())
		if !strings.HasPrefix(out, "org-lint ") || !strings.Contains(out, "(") {
			t.Errorf("%s: stdout = %q, want 'org-lint <version> (<rev>)'", flag, out)
		}
	}
}

func TestRunHelpAndNoArgs(t *testing.T) {
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--help"}, &stdout, &stderr); code != 0 {
		t.Fatalf("--help: code %d, want 0", code)
	}
	if !strings.Contains(stdout.String(), "--version") {
		t.Errorf("--help missing --version:\n%s", stdout.String())
	}

	stdout.Reset()
	stderr.Reset()
	if code := run(nil, &stdout, &stderr); code != 1 {
		t.Fatalf("no args: code %d, want 1", code)
	}
}

func TestRunSkipIfNoEmacs(t *testing.T) {
	t.Setenv("PATH", t.TempDir())
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--skip-if-no-emacs", "README.org"}, &stdout, &stderr); code != 0 {
		t.Fatalf("--skip-if-no-emacs without emacs: code %d, stderr %q", code, stderr.String())
	}
}
