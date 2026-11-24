package main

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
)

func TestLoadConfig(t *testing.T) {
	cfgPath := filepath.Join(t.TempDir(), "broker.toml")
	content := `
listen_addr = "127.0.0.1:9090"
db_path = "./custom.db"
repo_cache_dir = "./cache"
catalog_sync_interval = "30s"
lease_ttl = "45s"
worker_stale_after = "120s"
max_attempts = 3
max_concurrent_per_requester = 7

[[repos]]
name = "org/core-api"
git_url = "https://github.com/org/core-api.git"
`
	if err := os.WriteFile(cfgPath, []byte(content), 0o600); err != nil {
		t.Fatalf("WriteFile() error = %v", err)
	}

	cfg, err := LoadConfig(cfgPath)
	if err != nil {
		t.Fatalf("LoadConfig() error = %v", err)
	}
	if cfg.ListenAddr != "127.0.0.1:9090" || cfg.MaxAttempts != 3 || cfg.MaxConcurrentPerRequester != 7 {
		t.Fatalf("unexpected config: %+v", cfg)
	}
	if len(cfg.Repos) != 1 || cfg.Repos[0].Name != "org/core-api" {
		t.Fatalf("unexpected repos: %+v", cfg.Repos)
	}
}

func TestTokenCLIAndVersion(t *testing.T) {
	dbPath := filepath.Join(t.TempDir(), "broker.db")

	// version --json.
	var out, errBuf bytes.Buffer
	if code := run([]string{"version", "--json"}, &out, &errBuf); code != 0 {
		t.Fatalf("version --json exit = %d, stderr = %s", code, errBuf.String())
	}
	var ver map[string]string
	if err := json.Unmarshal(out.Bytes(), &ver); err != nil {
		t.Fatalf("unmarshal version JSON: %v", err)
	}
	if ver["version"] == "" {
		t.Fatalf("expected non-empty version")
	}

	// token create.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"token", "create", "--db", dbPath, "--role", "producer", "--name", "ci", "--repos", "org/a,org/b"}, &out, &errBuf); code != 0 {
		t.Fatalf("token create exit = %d, stderr = %s", code, errBuf.String())
	}
	plaintext := strings.TrimSpace(out.String())
	if !strings.HasPrefix(plaintext, "ap_prod_") {
		t.Fatalf("unexpected plaintext token: %q", plaintext)
	}

	// token ls --json.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"token", "ls", "--db", dbPath, "--json"}, &out, &errBuf); code != 0 {
		t.Fatalf("token ls --json exit = %d, stderr = %s", code, errBuf.String())
	}
	var tokens []broker.TokenRecord
	if err := json.Unmarshal(out.Bytes(), &tokens); err != nil {
		t.Fatalf("unmarshal token ls JSON: %v", err)
	}
	if len(tokens) != 1 || tokens[0].Name != "ci" || len(tokens[0].AllowedRepos) != 2 {
		t.Fatalf("unexpected token list: %+v", tokens)
	}

	// token revoke.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"token", "revoke", "--db", dbPath, "--id", tokens[0].ID}, &out, &errBuf); code != 0 {
		t.Fatalf("token revoke exit = %d, stderr = %s", code, errBuf.String())
	}

	// Verify revoked in token ls --json.
	out.Reset()
	errBuf.Reset()
	if code := run([]string{"token", "ls", "--db", dbPath, "--json"}, &out, &errBuf); code != 0 {
		t.Fatalf("token ls after revoke exit = %d, stderr = %s", code, errBuf.String())
	}
	tokens = nil
	if err := json.Unmarshal(out.Bytes(), &tokens); err != nil {
		t.Fatalf("unmarshal token ls JSON: %v", err)
	}
	if len(tokens) != 1 || tokens[0].RevokedAt == nil {
		t.Fatalf("expected token to be revoked: %+v", tokens)
	}
}
