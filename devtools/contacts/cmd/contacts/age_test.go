package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestAgeCLI(t *testing.T) {
	ta := newTestApp(t)
	cfg := filepath.Join(ta.env["XDG_CONFIG_HOME"], "contacts", "config.toml")
	if err := os.MkdirAll(filepath.Dir(cfg), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfg, []byte("retention_days = 30\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	ta.mustRun("add", "Ada")
	for _, d := range []string{"2026-01-03", "2026-01-20"} {
		ta.mustRun("report", "ada", "--channel", "chat", "--date", d, "--link", "https://x/"+d, "--summary", "s", "--by", "ops")
	}
	if out := ta.mustRun("age", "--dry-run"); out != "would have compacted 2 events into 1 summaries\n" {
		t.Errorf("dry run printed %q", out)
	}
	if out := ta.mustRun("age"); out != "compacted 2 events into 1 summaries\n" {
		t.Errorf("age printed %q", out)
	}
	if out := ta.mustRun("age"); out != "nothing older than 30 days\n" {
		t.Errorf("second age printed %q", out)
	}
}
