package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func writeConfig(t *testing.T, dir string, tokenMode os.FileMode) string {
	t.Helper()
	token := filepath.Join(dir, "tokens")
	if err := os.WriteFile(token, []byte("xoxp-secret"), tokenMode); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(token, tokenMode); err != nil {
		t.Fatal(err)
	}
	cfg := filepath.Join(dir, "config.json")
	body := `{"tokenFile": "` + token + `", "dbPath": "` + filepath.Join(dir, "state", "slackwatch.db") + `"}`
	if err := os.WriteFile(cfg, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return cfg
}

func TestRunUsage(t *testing.T) {
	var stdout, stderr bytes.Buffer
	if code := run(nil, &stdout, &stderr); code != 2 {
		t.Errorf("no args code = %d, want 2", code)
	}
	if code := run([]string{"bogus"}, &stdout, &stderr); code != 2 {
		t.Errorf("unknown command code = %d, want 2", code)
	}
	stderr.Reset()
	if code := run([]string{"help"}, &stdout, &stderr); code != 0 {
		t.Errorf("help code = %d, want 0", code)
	}
	if !strings.Contains(stderr.String(), "slackwatch serve") {
		t.Errorf("usage = %q", stderr.String())
	}
}

func TestServeRejectsOpenTokenFile(t *testing.T) {
	dir := t.TempDir()
	cfg := writeConfig(t, dir, 0o644)
	var stdout, stderr bytes.Buffer
	code := run([]string{"--config", cfg, "serve"}, &stdout, &stderr)
	if code == 0 {
		t.Fatal("serve accepted a 0644 token file")
	}
	if !strings.Contains(stderr.String(), "readable by group or others") {
		t.Errorf("stderr = %q", stderr.String())
	}
	if strings.Contains(stderr.String(), "xoxp-secret") {
		t.Errorf("stderr leaks token: %q", stderr.String())
	}
	if _, err := os.Stat(filepath.Join(dir, "state", "slackwatch.db")); err == nil {
		t.Error("serve opened the db before checking the token file")
	}
}

func TestServeConfigFromEnv(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("SLACKWATCH_CONFIG", writeConfig(t, dir, 0o600))
	var stdout, stderr bytes.Buffer
	run([]string{"serve"}, &stdout, &stderr)
	if strings.Contains(stderr.String(), "readable by group or others") || strings.Contains(stderr.String(), "xoxp-secret") {
		t.Errorf("0600 token file rejected: %q", stderr.String())
	}
	if _, err := os.Stat(filepath.Join(dir, "state", "slackwatch.db")); err != nil {
		t.Errorf("serve did not open the db: %v", err)
	}
}
