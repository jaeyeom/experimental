package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestHandleVersion(t *testing.T) {
	for _, flag := range []string{"--version", "-version"} {
		var stdout bytes.Buffer
		if !handleVersion([]string{flag}, &stdout) {
			t.Fatalf("handleVersion(%q) = false, want true", flag)
		}
		out := strings.TrimSpace(stdout.String())
		if !strings.HasPrefix(out, "pkgvar ") || !strings.Contains(out, "(") {
			t.Errorf("%s: stdout = %q, want 'pkgvar <version> (<rev>)'", flag, out)
		}
	}

	var stdout bytes.Buffer
	if handleVersion([]string{"./..."}, &stdout) {
		t.Fatal("handleVersion(./...) = true, want false")
	}
}
