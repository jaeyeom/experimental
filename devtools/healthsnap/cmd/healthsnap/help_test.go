package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestHelpAndUsage(t *testing.T) {
	var stdout, stderr bytes.Buffer
	if code := run(nil, &stdout, &stderr, nil, appRuntime{}); code != 2 || !strings.Contains(stderr.String(), "usage: healthsnap") {
		t.Fatalf("no args: code=%d stderr=%q", code, stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"help"}, &stdout, &stderr, nil, appRuntime{}); code != 0 || !strings.Contains(stdout.String(), "healthsnap run") {
		t.Fatalf("help: code=%d stdout=%q", code, stdout.String())
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"help", "run"}, &stdout, &stderr, nil, appRuntime{}); code != 0 || !strings.Contains(stdout.String(), "--dry-run") {
		t.Fatalf("help run: code=%d stdout=%q", code, stdout.String())
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"help", "bogus"}, &stdout, &stderr, nil, appRuntime{}); code != 2 || !strings.Contains(stderr.String(), `unknown command "bogus"`) {
		t.Fatalf("help bogus: code=%d stderr=%q", code, stderr.String())
	}
}
