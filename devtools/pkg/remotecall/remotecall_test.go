package remotecall

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func writeScript(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "fake")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o755); err != nil { //nolint:gosec // test executable
		t.Fatal(err)
	}
	return path
}

func TestRunReportsOutputAndExitCode(t *testing.T) {
	bin := writeScript(t, "echo out; echo err >&2; exit 3\n")
	res, err := Run(context.Background(), bin)
	if err != nil {
		t.Fatal(err)
	}
	if res.Stdout != "out\n" || res.Stderr != "err\n" || res.Code != 3 || res.TimedOut {
		t.Fatalf("Run = %+v", res)
	}
}

func TestRunKillsTheProcessGroupAtTheDeadline(t *testing.T) {
	// The child sleep keeps stdout open. Killing only the shell would
	// leave Run waiting on the pipe.
	bin := writeScript(t, "sleep 30 &\nwait\n")
	ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel()
	start := time.Now()
	res, err := Run(ctx, bin)
	if err != nil {
		t.Fatal(err)
	}
	if !res.TimedOut {
		t.Fatalf("Run = %+v, want timed out", res)
	}
	if took := time.Since(start); took > 3*time.Second {
		t.Fatalf("Run took %s after a 200ms deadline", took)
	}
}

func TestRunMissingBinary(t *testing.T) {
	if _, err := Run(context.Background(), filepath.Join(t.TempDir(), "missing")); err == nil {
		t.Fatal("Run of a missing binary returned no error")
	}
}

func TestClassify(t *testing.T) {
	up := func(context.Context) bool { return true }
	down := func(context.Context) bool { return false }
	tests := []struct {
		name  string
		res   Result
		probe Probe
		want  string
	}{
		{"success", Result{Code: 0}, down, ""},
		{"deadline, host answers", Result{Code: -1, TimedOut: true}, up, CauseAuthLocked},
		{"deadline, host silent", Result{Code: -1, TimedOut: true}, down, CauseHostUnreachable},
		{"publickey", Result{Code: 255, Stderr: "user@m2: Permission denied (publickey)."}, up, CauseAuthLocked},
		{"publickey skips probe", Result{Code: 255, Stderr: "Permission denied (publickey)"}, nil, CauseAuthLocked},
		{"connection refused", Result{Code: 255, Stderr: "ssh: connect to host m2 port 22: Connection refused"}, nil, CauseHostUnreachable},
		{"disabled machine", Result{Code: 1, Stderr: "Error: machine 'm2' is disabled"}, nil, CauseHostUnreachable},
		{"other error, host answers", Result{Code: 1, Stderr: `{"error":{"code":"pane_not_found"}}`}, up, ""},
		{"other error, host silent", Result{Code: 1, Stderr: "broken pipe"}, down, CauseHostUnreachable},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := Classify(context.Background(), "m2", tt.res, tt.probe)
			if tt.want == "" {
				if err != nil {
					t.Fatalf("Classify = %v, want nil", err)
				}
				return
			}
			if err == nil || err.Cause != tt.want || err.Machine != "m2" {
				t.Fatalf("Classify = %#v, want cause %q", err, tt.want)
			}
			if want := "machine m2 unreachable: " + tt.want; err.Error() != want {
				t.Fatalf("Error() = %q, want %q", err.Error(), want)
			}
		})
	}
}
