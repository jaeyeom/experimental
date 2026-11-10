package main

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
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

func TestRunRejectsBadConfig(t *testing.T) {
	config := t.TempDir()
	if err := os.MkdirAll(filepath.Join(config, "agentbus"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(config, "agentbus", "config.toml"), []byte("machines = linux2\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var stderr bytes.Buffer
	code := run(nil, []string{
		"XDG_RUNTIME_DIR=/tmp",
		"HOME=/Users/me",
		"XDG_STATE_HOME=" + t.TempDir(),
		"XDG_CONFIG_HOME=" + config,
	}, io.Discard, &stderr)
	if code != 1 || !strings.Contains(stderr.String(), "machines must be a list of strings") {
		t.Fatalf("code %d, stderr %q", code, stderr.String())
	}
}
