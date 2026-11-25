package secretwatch

import (
	"context"
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

func TestMain(m *testing.M) {
	if pklExec := resolvePklExec(); pklExec != "" {
		_ = os.Setenv("PKL_EXEC", pklExec)
	}
	os.Exit(m.Run())
}

// resolvePklExec finds the Bazel-managed pkl binary in runfiles so Pkl config
// tests run hermetically offline without relying on host PATH.
func resolvePklExec() string {
	if bin := os.Getenv("PKL_EXEC"); bin != "" {
		return bin
	}
	for _, key := range []string{"RUNFILES_DIR", "TEST_SRCDIR"} {
		root := os.Getenv(key)
		if root == "" {
			continue
		}
		matches, err := filepath.Glob(filepath.Join(root, "*pkl-cli-*", "file", "downloaded"))
		if err != nil {
			continue
		}
		for _, candidate := range matches {
			if info, err := os.Stat(candidate); err == nil && !info.IsDir() {
				return candidate
			}
		}
	}
	return ""
}

func exampleConfigPkl(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "pkl", "example.config.pkl"))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "secretwatch", "pkl", "example.config.pkl"))
	}
	for _, p := range candidates {
		if _, err := os.Stat(p); err == nil {
			return p
		}
	}
	t.Fatal("pkl/example.config.pkl not found")
	return ""
}

func TestLoadOptionalMissingConfig(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "missing.pkl")
	cfg, err := LoadConfig(context.Background(), missing, true)
	if err != nil {
		t.Fatalf("optional missing LoadConfig error = %v", err)
	}
	if len(cfg.Rules) != 0 || len(cfg.Notify) != 0 {
		t.Fatalf("unexpected non-default config: %+v", cfg)
	}
	if _, err := LoadConfig(context.Background(), missing, false); err == nil {
		t.Fatal("expected error for required missing config")
	}
}

func TestLoadExamplePklConfig(t *testing.T) {
	cfg, err := LoadConfig(context.Background(), exampleConfigPkl(t), false)
	if err != nil {
		t.Fatalf("LoadConfig(example.config.pkl) = %v", err)
	}
	if len(cfg.Notify) == 0 || cfg.Notify[0] != "notify-send" {
		t.Errorf("Notify = %v", cfg.Notify)
	}
	if len(cfg.Rules) != 1 || cfg.Rules[0].Name != "internal-api-token" {
		t.Errorf("Rules = %+v", cfg.Rules)
	}
	if len(cfg.Allowlist.Patterns) != 2 || len(cfg.Allowlist.Units) != 2 {
		t.Errorf("Allowlist = %+v", cfg.Allowlist)
	}
}

func TestLoadStandalonePklConfigAndValidationErrors(t *testing.T) {
	dir := t.TempDir()
	valid := filepath.Join(dir, "config.pkl")
	if err := os.WriteFile(valid, []byte(`
notify { "echo"; "{rule}"; "{prefix}" }
allowlist {
  units { "test.service" }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := LoadConfig(context.Background(), valid, false)
	if err != nil {
		t.Fatalf("LoadConfig standalone = %v", err)
	}
	if len(cfg.Notify) != 3 || len(cfg.Allowlist.Units) != 1 {
		t.Fatalf("unexpected standalone config: %+v", cfg)
	}

	invalidRegex := filepath.Join(dir, "bad_regex.pkl")
	if err := os.WriteFile(invalidRegex, []byte(`
rules {
  new {
    name = "bad"
    pattern = "[unclosed"
  }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := LoadConfig(context.Background(), invalidRegex, false); err == nil {
		t.Fatal("expected invalid regex error")
	}

	dupRule := filepath.Join(dir, "dup.pkl")
	if err := os.WriteFile(dupRule, []byte(`
rules {
  new {
    name = "github-token"
    pattern = "foo"
  }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := LoadConfig(context.Background(), dupRule, false); err == nil {
		t.Fatal("expected duplicate rule name error")
	}
}
