package main

import (
	"bytes"
	"path/filepath"
	"strings"
	"testing"
)

func TestRunCLIUsageAndFlags(t *testing.T) {
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--help"}, &stdout, &stderr); code != 0 {
		t.Fatalf("--help code = %d, want 0", code)
	}
	if !strings.Contains(stdout.String(), "--gh-bin") || !strings.Contains(stdout.String(), "--reviewer-cmd") {
		t.Fatalf("usage missing expected flags:\n%s", stdout.String())
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"help"}, &stdout, &stderr); code != 0 {
		t.Fatalf("help code = %d, want 0", code)
	}

	cases := []struct {
		name    string
		args    []string
		wantSub string
		wantRC  int
	}{
		{
			name:    "empty-gh-bin-flag",
			args:    []string{"--gh-bin", "   "},
			wantSub: "--gh-bin must not be empty",
			wantRC:  2,
		},
		{
			name:    "invalid-reviewer-cmd",
			args:    []string{"--reviewer-cmd", `"unclosed`},
			wantSub: "invalid --reviewer-cmd",
			wantRC:  2,
		},
		{
			name:    "unknown-flag",
			args:    []string{"--no-such-flag"},
			wantSub: "flag provided but not defined",
			wantRC:  2,
		},
		{
			name:    "missing-config-fails",
			args:    []string{"--config", filepath.Join(t.TempDir(), "missing.json")},
			wantSub: "no config",
			wantRC:  1,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var outBuf, errBuf bytes.Buffer
			code := run(tc.args, &outBuf, &errBuf)
			if code != tc.wantRC {
				t.Fatalf("run(%v) = %d, want %d (stderr=%s)", tc.args, code, tc.wantRC, errBuf.String())
			}
			if !strings.Contains(errBuf.String(), tc.wantSub) {
				t.Fatalf("stderr %q does not contain %q", errBuf.String(), tc.wantSub)
			}
		})
	}
}
