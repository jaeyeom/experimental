package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestRunRejectsBadRetention(t *testing.T) {
	var stderr bytes.Buffer
	code := run([]string{
		"XDG_RUNTIME_DIR=/tmp",
		"HOME=/Users/me",
		"AGENTBUS_LOG_RETENTION_DAYS=0",
	}, &stderr)
	if code == 0 {
		t.Fatal("run exited 0")
	}
	if !strings.Contains(stderr.String(), "AGENTBUS_LOG_RETENTION_DAYS") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}
