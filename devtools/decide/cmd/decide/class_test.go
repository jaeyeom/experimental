package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestAskClassesPrefixesAndRequireClassSwitch(t *testing.T) {
	state := t.TempDir()
	cfg := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", cfg)
	t.Setenv("HERDR_PANE_ID", "")

	// Missing --class files a decision with a deprecation warning.
	stdout, stderr, code := runCLI("ask", "--owner", "ada", "--kind", "relay", "--option", "a:Alpha", "--option", "b:Beta", "--rec", "a", "Pick one?")
	if code != 0 {
		t.Fatalf("ask without --class = %d\n%s", code, stderr)
	}
	if id := strings.TrimSpace(stdout); id != "D-001" {
		t.Fatalf("id = %q, want D-001", id)
	}
	if !strings.Contains(stderr, "warning") || !strings.Contains(stderr, "--class") {
		t.Fatalf("stderr = %q, want deprecation warning", stderr)
	}

	// With requireClass=true, missing --class is an error.
	cfgDir := filepath.Join(cfg, "decide")
	if err := os.MkdirAll(cfgDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(cfgDir, "config.json"), []byte(`{"requireClass": true}`), 0o600); err != nil {
		t.Fatal(err)
	}
	_, stderr, code = runCLI("ask", "--owner", "ada", "--kind", "relay", "--option", "a:Alpha", "--option", "b:Beta", "--rec", "a", "Pick one?")
	if code != 2 || !strings.Contains(stderr, "--class") {
		t.Fatalf("ask with requireClass=true = %d, stderr=%q, want exit 2", code, stderr)
	}

	// Approval requires verbatim, defaults to yes/no/edit, and stores --parent.
	_, stderr, code = runCLI("ask", "--class", "approval", "--owner", "ada", "Post?")
	if code == 0 || !strings.Contains(stderr, "verbatim") {
		t.Fatalf("approval without verbatim = %d, stderr=%q", code, stderr)
	}
	stdout, stderr, code = runCLI("ask", "--class", "approval", "--parent", "1", "--owner", "ada", "--verbatim", "hello world", "Post this?")
	if code != 0 {
		t.Fatalf("approval ask = %d\n%s", code, stderr)
	}
	if id := strings.TrimSpace(stdout); id != "A-002" {
		t.Fatalf("approval id = %q, want A-002", id)
	}
	got := showJSON(t, "2")
	if got.ID != "A-002" || got.Parent != "D-001" || !optionKeys(got.Options, "yes", "no", "edit") {
		t.Fatalf("approval item = %+v", got)
	}

	// Note refuses options, allows bare ask, and retag changes prefix.
	_, stderr, code = runCLI("ask", "--class", "N", "--owner", "ada", "--option", "a:A", "FYI")
	if code == 0 {
		t.Fatalf("note with option succeeded, stderr=%q", stderr)
	}
	stdout, stderr, code = runCLI("ask", "--class", "note", "--owner", "ada", "Build finished")
	if code != 0 {
		t.Fatalf("note ask = %d\n%s", code, stderr)
	}
	if id := strings.TrimSpace(stdout); id != "N-003" {
		t.Fatalf("note id = %q, want N-003", id)
	}
	stdout, stderr, code = runCLI("retag", "--json", "3", "task")
	if code != 0 {
		t.Fatalf("retag = %d\n%s", code, stderr)
	}
	if !strings.Contains(stdout, `"T-003"`) || !strings.Contains(stdout, `"task"`) {
		t.Fatalf("retag json = %s", stdout)
	}
}
