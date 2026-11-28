package main

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/chorong-sh/chorong/pkg/toolcli"
)

func TestHealthsnapToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
			t.Setenv("XDG_STATE_HOME", filepath.Join(homeDir, ".local", "state"))
			exe := filepath.Join(t.TempDir(), "healthsnap")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newHealthsnapToolWith(os.Environ(), func(file string) (string, error) {
				if file == "healthsnap" {
					return exe, nil
				}
				return "/bin/" + file, nil
			})
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			return tool
		},
	})
}

func TestDoctorConfiguredChecksAndJudgmentWarning(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, ".config", "healthsnap", "config.pkl")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, []byte(`
notify {
  recipient = "ops/inbox"
}
checks {
  new {
    id = "c1"
    argv { "agentbus"; "health" }
    rules {
      new {
        id = "r1"
        condition = "exit != 0"
        judgment = true
      }
    }
  }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}

	env := []string{
		"HOME=" + home,
		"XDG_CONFIG_HOME=" + filepath.Join(home, ".config"),
		"XDG_STATE_HOME=" + filepath.Join(home, ".local", "state"),
	}
	tool := newHealthsnapToolWith(env, func(file string) (string, error) {
		return "/usr/bin/" + file, nil
	})
	var out, errOut bytes.Buffer
	code, ok := tool.Handle(context.Background(), []string{"doctor", "--offline", "--json"}, &out, &errOut)
	if !ok || code != 1 {
		t.Fatalf("doctor exit = %d (ok=%v), want 1; out=%s err=%s", code, ok, out.String(), errOut.String())
	}
	if !strings.Contains(out.String(), "judgment-routing") {
		t.Errorf("expected judgment-routing warning in doctor output:\n%s", out.String())
	}
}

func TestDoctorChecksPerRuleCommandsAndRecipients(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, ".config", "healthsnap", "config.pkl")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, []byte(`
checks {
  new {
    id = "c1"
    argv { "my-check" }
    rules {
      new {
        id = "r1"
        condition = "exit != 0"
        judgment = true
        recipient = "agent/a"
      }
      new {
        id = "r2"
        condition = "exit == 2"
        command { "missing-rule-notifier"; "{message}" }
      }
    }
  }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}

	env := []string{
		"HOME=" + home,
		"XDG_CONFIG_HOME=" + filepath.Join(home, ".config"),
		"XDG_STATE_HOME=" + filepath.Join(home, ".local", "state"),
	}
	tool := newHealthsnapToolWith(env, func(file string) (string, error) {
		if file == "missing-rule-notifier" {
			return "", os.ErrNotExist
		}
		return "/usr/bin/" + file, nil
	})
	var out, errOut bytes.Buffer
	code, ok := tool.Handle(context.Background(), []string{"doctor", "--offline", "--json"}, &out, &errOut)
	if !ok || code != 2 {
		t.Fatalf("doctor exit = %d (ok=%v), want 2; out=%s err=%s", code, ok, out.String(), errOut.String())
	}
	if !strings.Contains(out.String(), "missing-rule-notifier") {
		t.Errorf("expected missing-rule-notifier in doctor output:\n%s", out.String())
	}
	if !strings.Contains(out.String(), `"name": "judgment-routing"`) || !strings.Contains(out.String(), `"detail": "per-rule routing"`) {
		t.Errorf("expected judgment-routing ok with per-rule routing:\n%s", out.String())
	}
}
