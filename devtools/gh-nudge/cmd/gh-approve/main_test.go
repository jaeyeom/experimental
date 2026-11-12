package main

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
)

func TestNewAgentCache(t *testing.T) {
	cfg := config.ApproveConfig{AgentCacheTTL: 48 * time.Hour, AgentCacheMaxMB: 3}
	home := t.TempDir()
	xdg := t.TempDir()

	if cache := newAgentCache(cfg, true, home, xdg); cache != nil {
		t.Fatalf("--no-agent-cache cache = %+v", cache)
	}
	cache := newAgentCache(cfg, false, home, xdg)
	if cache == nil || cache.Dir != filepath.Join(xdg, "gh-approve", "agent") ||
		cache.TTL != 48*time.Hour || cache.MaxBytes != 3<<20 || cache.Now == nil {
		t.Fatalf("cache = %+v", cache)
	}
	cache = newAgentCache(cfg, false, home, "")
	if cache == nil || cache.Dir != filepath.Join(home, ".cache", "gh-approve", "agent") {
		t.Fatalf("default cache = %+v", cache)
	}
}

func TestOpenAuditLogAppendsUnderConfigDir(t *testing.T) {
	home := t.TempDir()
	want := filepath.Join(home, ".config", "gh-nudge", "approve-log.jsonl")
	for _, line := range []string{"one\n", "two\n"} {
		file, path, err := openAuditLog(home)
		if err != nil {
			t.Fatal(err)
		}
		if path != want {
			t.Fatalf("path = %q, want %q", path, want)
		}
		if _, err := file.WriteString(line); err != nil {
			t.Fatal(err)
		}
		if err := file.Close(); err != nil {
			t.Fatal(err)
		}
	}
	data, err := os.ReadFile(want)
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != "one\ntwo\n" {
		t.Fatalf("log = %q", data)
	}
}
