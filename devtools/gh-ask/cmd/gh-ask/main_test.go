package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const configBody = `
repos = ["octo/demo"]
ask_command = ["asker", "{file}"]
id_pattern = 'thread-(\w+)'
followup_command = ["follower", "{id}", "{file}"]
`

func TestTokenCreatesAndPrints(t *testing.T) {
	home := t.TempDir()
	env := []string{"HOME=" + home}
	var out, errOut bytes.Buffer
	if code := run([]string{"token"}, &out, &errOut, env); code != 0 {
		t.Fatalf("token exit %d: %s", code, errOut.String())
	}
	token := strings.TrimSpace(out.String())
	if len(token) != 64 {
		t.Fatalf("token = %q, want 64 hex chars", token)
	}
	path := filepath.Join(home, ".config", "gh-ask", "token")
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Errorf("token mode = %o, want 600", perm)
	}
	out.Reset()
	if code := run([]string{"token"}, &out, &errOut, env); code != 0 || strings.TrimSpace(out.String()) != token {
		t.Errorf("second token = %q (exit %d), want %q", out.String(), code, token)
	}
}

func TestTokenUsesConfigTokenFile(t *testing.T) {
	dir := t.TempDir()
	cfg := filepath.Join(dir, "config.toml")
	if err := os.WriteFile(cfg, []byte(configBody+"token_file = \"secret\"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var out, errOut bytes.Buffer
	if code := run([]string{"token", "--config", cfg}, &out, &errOut, nil); code != 0 {
		t.Fatalf("token exit %d: %s", code, errOut.String())
	}
	data, err := os.ReadFile(filepath.Join(dir, "secret"))
	if err != nil {
		t.Fatal(err)
	}
	if strings.TrimSpace(string(data)) != strings.TrimSpace(out.String()) {
		t.Errorf("printed %q, file has %q", out.String(), data)
	}
}

func TestServeRefusesNonLoopback(t *testing.T) {
	dir := t.TempDir()
	cfg := filepath.Join(dir, "config.toml")
	if err := os.WriteFile(cfg, []byte("listen = \"0.0.0.0:8765\"\n"+configBody), 0o600); err != nil {
		t.Fatal(err)
	}
	var out, errOut bytes.Buffer
	if code := run([]string{"serve", "--config", cfg}, &out, &errOut, nil); code != 2 {
		t.Fatalf("serve exit %d, want 2", code)
	}
	if !strings.Contains(errOut.String(), "127.0.0.1") {
		t.Errorf("stderr = %q, want loopback refusal", errOut.String())
	}
}

func TestServeNeedsToken(t *testing.T) {
	dir := t.TempDir()
	cfg := filepath.Join(dir, "config.toml")
	if err := os.WriteFile(cfg, []byte("listen = \"127.0.0.1:0\"\n"+configBody), 0o600); err != nil {
		t.Fatal(err)
	}
	var out, errOut bytes.Buffer
	if code := run([]string{"serve", "--config", cfg}, &out, &errOut, nil); code != 1 {
		t.Fatalf("serve exit %d, want 1", code)
	}
	if !strings.Contains(errOut.String(), "gh-ask token") {
		t.Errorf("stderr = %q, want a hint to run gh-ask token", errOut.String())
	}
}

func TestUsage(t *testing.T) {
	var out, errOut bytes.Buffer
	if code := run(nil, &out, &errOut, nil); code != 2 {
		t.Errorf("no args exit %d, want 2", code)
	}
	if code := run([]string{"exec"}, &out, &errOut, nil); code != 2 {
		t.Errorf("unknown command exit %d, want 2", code)
	}
}
