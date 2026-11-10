package main

import (
	"bytes"
	"io"
	"strings"
	"testing"
)

func TestRunRejectsBadRetention(t *testing.T) {
	var stderr bytes.Buffer
	code := run(nil, []string{
		"XDG_RUNTIME_DIR=/tmp",
		"HOME=/Users/me",
		"AGENTBUS_LOG_RETENTION_DAYS=0",
	}, io.Discard, &stderr)
	if code == 0 {
		t.Fatal("run exited 0")
	}
	if !strings.Contains(stderr.String(), "AGENTBUS_LOG_RETENTION_DAYS") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestHelp(t *testing.T) {
	for _, arg := range []string{"--help", "-h"} {
		var stdout, stderr bytes.Buffer
		code := run([]string{arg}, []string{"HOME=/Users/me"}, &stdout, &stderr)
		if code != 0 || stderr.Len() != 0 {
			t.Fatalf("%s: code %d, stderr %q", arg, code, stderr.String())
		}
		for _, want := range []string{"Usage: agentbusd", "AGENTBUS_LOG_RETENTION_DAYS", "AGENTBUS_INLINE_BYTES", "XDG_RUNTIME_DIR"} {
			if !strings.Contains(stdout.String(), want) {
				t.Errorf("%s: stdout is missing %q", arg, want)
			}
		}
	}
}

func TestRunRejectsArguments(t *testing.T) {
	var stderr bytes.Buffer
	code := run([]string{"serve"}, []string{"HOME=/Users/me"}, io.Discard, &stderr)
	if code != 2 || !strings.Contains(stderr.String(), "agentbusd --help") {
		t.Fatalf("code %d, stderr %q", code, stderr.String())
	}
}
