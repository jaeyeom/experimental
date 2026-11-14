package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeSpec(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "spec.json")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestCheckApprovalWordFlag(t *testing.T) {
	path := writeSpec(t, `{"roles":{"coord/lead":{"decides":["ship the build"]}}}`)
	var stderr bytes.Buffer
	if code := run([]string{"check", path}, &stderr); code != 0 {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
	stderr.Reset()
	code := run([]string{"check", "--approval-word", "ship", path}, &stderr)
	if code != 1 || !strings.Contains(stderr.String(), `approval word "ship"`) {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
}

func TestCheckPrintsAllowWarning(t *testing.T) {
	path := writeSpec(t, `{"roles":{"coord/lead":{"decides":["merge PRs"]}},"allowApprovalWords":["merge"]}`)
	var stderr bytes.Buffer
	code := run([]string{"check", path}, &stderr)
	if code != 0 || !strings.Contains(stderr.String(), `warning: allowApprovalWords "merge"`) {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
}
