package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestRunFailsLoudlyWithoutRuntimeDir(t *testing.T) {
	var stderr bytes.Buffer
	code := run(nil, []string{"HOME=/Users/me"}, &stderr)
	if code == 0 {
		t.Fatal("run exited 0")
	}
	if !strings.Contains(stderr.String(), "XDG_RUNTIME_DIR") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestRunRejectsCommands(t *testing.T) {
	var stderr bytes.Buffer
	code := run([]string{"send"}, []string{"XDG_RUNTIME_DIR=/tmp", "HOME=/Users/me"}, &stderr)
	if code != 2 {
		t.Fatalf("run = %d, stderr %q", code, stderr.String())
	}
}
