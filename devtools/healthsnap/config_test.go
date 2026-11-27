package healthsnap

import (
	"context"
	"os"
	"path/filepath"
	"runtime"
	"testing"
	"time"
)

func TestMain(m *testing.M) {
	if pklExec := resolvePklExec(); pklExec != "" {
		_ = os.Setenv("PKL_EXEC", pklExec)
	}
	os.Exit(m.Run())
}

// resolvePklExec locates the Bazel-bundled pkl binary in runfiles so Pkl tests
// run hermetically offline on macOS and Linux RBE without host pkl.
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
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "healthsnap", "pkl", "example.config.pkl"))
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
	if len(cfg.Checks) != 0 || cfg.KeepSnapshots != DefaultKeepSnapshots {
		t.Fatalf("unexpected default config: %+v", cfg)
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
	if cfg.KeepSnapshots != 48 || !cfg.Markdown {
		t.Errorf("KeepSnapshots=%d Markdown=%v", cfg.KeepSnapshots, cfg.Markdown)
	}
	if cfg.Notify.Recipient != "ops/inbox" || cfg.Notify.JudgmentRecipient != "ops/triage" {
		t.Errorf("Notify = %+v", cfg.Notify)
	}
	if len(cfg.Checks) != 9 {
		t.Fatalf("len(Checks) = %d, want 9", len(cfg.Checks))
	}
	if cfg.Checks[0].Parse.Mode != ParseJSONPath || cfg.Checks[0].TimeoutDuration() != 10*time.Second {
		t.Errorf("check[0] = %+v", cfg.Checks[0])
	}
	if cfg.Checks[3].IntervalDuration() != 6*time.Hour {
		t.Errorf("check[3] interval = %v, want 6h", cfg.Checks[3].IntervalDuration())
	}
	if cfg.Checks[3].Rules[0].Recipient != "ops/cron" {
		t.Errorf("check[3] rule recipient = %q, want ops/cron", cfg.Checks[3].Rules[0].Recipient)
	}
	if cfg.Checks[6].Rules[0].State != SeverityInfo {
		t.Errorf("check[6] rule[0] state = %q, want info", cfg.Checks[6].Rules[0].State)
	}
	if cfg.Checks[6].Rules[1].Clear != "value < 1.2" {
		t.Errorf("check[6] rule[1] clear = %q, want value < 1.2", cfg.Checks[6].Rules[1].Clear)
	}
}

func TestLoadStandalonePklAndValidation(t *testing.T) {
	dir := t.TempDir()
	valid := filepath.Join(dir, "valid.pkl")
	if err := os.WriteFile(valid, []byte(`
notify {
  command { "notify-send"; "{rule}"; "{message}" }
}
checks {
  new {
    id = "ping"
    argv { "true" }
  }
  new {
    id = "hb"
    kind = "file-age"
    path = "/tmp/hb"
    rules {
      new {
        id = "hb-warn"
        condition = "age > 2h"
        clear = "age < 1h"
        state = "warn"
        recipient = "ops/hb"
        command { "custom-notify"; "{recipient}"; "{message}" }
      }
      new {
        id = "hb-info"
        condition = "age > 1h"
        state = "info"
      }
    }
  }
  new {
    id = "single-rule"
    kind = "file-age"
    path = "/tmp/single"
    rules {
      new {
        condition = "age > 1h"
        state = "warn"
      }
    }
  }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := LoadConfig(context.Background(), valid, false)
	if err != nil {
		t.Fatalf("LoadConfig(valid) = %v", err)
	}
	if len(cfg.Checks) != 3 {
		t.Fatalf("len(Checks) = %d, want 3", len(cfg.Checks))
	}
	if cfg.Checks[0].Kind != KindCommand || cfg.Checks[0].Parse.Mode != ParseExit {
		t.Errorf("check[0] defaults not filled: %+v", cfg.Checks[0])
	}
	if len(cfg.Checks[0].Rules) != 1 || cfg.Checks[0].Rules[0].ID != "ping" || cfg.Checks[0].Rules[0].Condition != "exit != 0" {
		t.Errorf("check[0] implicit rule = %+v", cfg.Checks[0].Rules)
	}
	if cfg.Checks[1].Rules[0].Clear != "age < 1h" || cfg.Checks[1].Rules[0].Recipient != "ops/hb" || len(cfg.Checks[1].Rules[0].Command) != 3 {
		t.Errorf("check[1].Rules[0] = %+v", cfg.Checks[1].Rules[0])
	}
	if cfg.Checks[1].Rules[1].State != SeverityInfo {
		t.Errorf("check[1].Rules[1].State = %q, want info", cfg.Checks[1].Rules[1].State)
	}
	if cfg.Checks[2].Rules[0].ID != "single-rule" {
		t.Errorf("single rule id default = %q, want single-rule", cfg.Checks[2].Rules[0].ID)
	}

	invalidCases := []struct {
		name string
		pkl  string
	}{
		{
			name: "duplicate check id",
			pkl: `
checks {
  new { id = "dup"; argv { "true" } }
  new { id = "dup"; argv { "true" } }
}`,
		},
		{
			name: "duplicate rule id across checks",
			pkl: `
checks {
  new { id = "a"; argv { "true" }; rules { new { id = "r1"; condition = "exit != 0" } } }
  new { id = "b"; argv { "true" }; rules { new { id = "r1"; condition = "exit != 0" } } }
}`,
		},
		{
			name: "missing rule id when multiple rules",
			pkl: `
checks {
  new {
    id = "a"
    argv { "true" }
    rules {
      new { condition = "exit == 1"; state = "warn" }
      new { condition = "exit == 2"; state = "fail" }
    }
  }
}`,
		},
		{
			name: "invalid regex",
			pkl: `
checks {
  new {
    id = "a"
    argv { "echo" }
    parse { regex = "[unclosed" }
    rules { new { condition = "value > 0" } }
  }
}`,
		},
		{
			name: "invalid jsonpath",
			pkl: `
checks {
  new {
    id = "a"
    argv { "echo" }
    parse { jsonPath = "not_rooted" }
    rules { new { condition = "value > 0" } }
  }
}`,
		},
		{
			name: "invalid rule expression",
			pkl: `
checks {
  new {
    id = "a"
    argv { "true" }
    rules { new { condition = "(exit == 0" } }
  }
}`,
		},
		{
			name: "invalid rule clear expression",
			pkl: `
checks {
  new {
    id = "a"
    argv { "true" }
    rules { new { condition = "value > 10"; clear = "(value < 5" } }
  }
}`,
		},
		{
			name: "invalid rule state",
			pkl: `
checks {
  new {
    id = "a"
    argv { "true" }
    rules { new { condition = "exit != 0"; state = "debug" } }
  }
}`,
		},
		{
			name: "empty arg in rule command",
			pkl: `
checks {
  new {
    id = "a"
    argv { "true" }
    rules { new { condition = "exit != 0"; command { "notify"; "" } } }
  }
}`,
		},
		{
			name: "file-age without path or rules",
			pkl: `
checks {
  new { id = "hb"; kind = "file-age" }
}`,
		},
		{
			name: "unknown host metric",
			pkl: `
checks {
  new {
    id = "h"
    kind = "host"
    metric = "gpu"
    rules { new { condition = "value > 0" } }
  }
}`,
		},
		{
			name: "unknown pkl field rejected",
			pkl: `
bogusField = 123
`,
		},
	}

	for _, tc := range invalidCases {
		t.Run(tc.name, func(t *testing.T) {
			p := filepath.Join(dir, "bad.pkl")
			if err := os.WriteFile(p, []byte(tc.pkl), 0o600); err != nil {
				t.Fatal(err)
			}
			if _, err := LoadConfig(context.Background(), p, false); err == nil {
				t.Fatalf("expected validation/load error for %s", tc.name)
			}
		})
	}
}
