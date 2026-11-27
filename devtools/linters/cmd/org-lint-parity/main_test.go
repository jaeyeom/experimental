package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestExecuteVersion(t *testing.T) {
	for _, flag := range []string{"--version", "-version"} {
		var stdout, stderr bytes.Buffer
		code := execute([]string{flag}, &stdout, &stderr)
		if code != 0 || stderr.Len() != 0 {
			t.Fatalf("%s: exit = %d, stderr = %q", flag, code, stderr.String())
		}
		out := strings.TrimSpace(stdout.String())
		if !strings.HasPrefix(out, "org-lint-parity ") || !strings.Contains(out, "(") {
			t.Errorf("%s: stdout = %q", flag, out)
		}
	}
}
