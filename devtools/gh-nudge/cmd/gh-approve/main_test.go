package main

import (
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/approve"
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

func TestRunOpenExitCodes(t *testing.T) {
	t.Run("print succeeds", func(t *testing.T) {
		var out, errOut bytes.Buffer
		code := runOpenWith([]string{"--print", "acme/widgets#12"}, &out, &errOut, approve.Open)
		if code != 0 {
			t.Fatalf("exit code = %d, want 0 (stderr=%q)", code, errOut.String())
		}
		if out.String() != "https://github.com/acme/widgets/pull/12\n" {
			t.Fatalf("stdout = %q", out.String())
		}
	})

	t.Run("malformed ref exits 2 with usage", func(t *testing.T) {
		var out, errOut bytes.Buffer
		code := runOpenWith([]string{"garbage"}, &out, &errOut, approve.Open)
		if code != 2 {
			t.Fatalf("exit code = %d, want 2", code)
		}
		if !strings.Contains(errOut.String(), "Usage: gh-approve open") {
			t.Fatalf("stderr = %q, want usage", errOut.String())
		}
	})

	t.Run("missing arg exits 2 with usage", func(t *testing.T) {
		var out, errOut bytes.Buffer
		code := runOpenWith(nil, &out, &errOut, approve.Open)
		if code != 2 {
			t.Fatalf("exit code = %d, want 2", code)
		}
		if !strings.Contains(errOut.String(), "Usage: gh-approve open") {
			t.Fatalf("stderr = %q, want usage", errOut.String())
		}
	})

	t.Run("help exits 0", func(t *testing.T) {
		var out, errOut bytes.Buffer
		code := runOpenWith([]string{"--help"}, &out, &errOut, approve.Open)
		if code != 0 {
			t.Fatalf("exit code = %d, want 0", code)
		}
	})

	t.Run("opener failure exits 1", func(t *testing.T) {
		var out, errOut bytes.Buffer
		code := runOpenWith([]string{"acme/widgets#12"}, &out, &errOut, func(approve.OpenOptions) error {
			return errors.New("opener failed")
		})
		if code != 1 {
			t.Fatalf("exit code = %d, want 1", code)
		}
		if !strings.Contains(errOut.String(), "opener failed") {
			t.Fatalf("stderr = %q", errOut.String())
		}
	})
}
