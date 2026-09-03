package main

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
)

func TestRunVersionExitOK(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := run([]string{"version"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	var got map[string]string
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if got["version"] == "" {
		t.Fatalf("missing version in %s", stdout.String())
	}
}

func TestRunUnknownCommandExitUsage(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := run([]string{"not-a-command"}, &stdout, &stderr)
	if code != 2 {
		t.Fatalf("exit = %d, want 2, stderr=%q", code, stderr.String())
	}
	if !strings.Contains(stderr.String(), "unknown command") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}
