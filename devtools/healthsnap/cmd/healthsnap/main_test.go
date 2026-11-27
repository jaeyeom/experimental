package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/healthsnap"
)

func TestMain(m *testing.M) {
	if pklExec := resolvePklExec(); pklExec != "" {
		_ = os.Setenv("PKL_EXEC", pklExec)
	}
	os.Exit(m.Run())
}

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

func TestCLIRunAndStatusLifecycle(t *testing.T) {
	tmp := t.TempDir()
	stateDir := filepath.Join(tmp, "state")
	cfgPath := filepath.Join(tmp, "config.pkl")
	if err := os.WriteFile(cfgPath, []byte(`
notify {
  recipient = "ops/inbox"
  judgmentRecipient = "ops/model"
}
checks {
  new {
    id = "bus-health"
    argv { "agentbus"; "health"; "--json" }
    parse {
      mode = "jsonpath"
      jsonPath = "$.daemon"
    }
    rules {
      new {
        id = "bus-down"
        condition = #"value != "up""#
        state = "fail"
        judgment = true
      }
    }
  }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}

	env := []string{
		"HOME=" + tmp,
		"XDG_STATE_HOME=" + stateDir,
	}

	// Status before any snapshot exits 2.
	var stdout, stderr bytes.Buffer
	if code := run([]string{"status", "--config", cfgPath}, &stdout, &stderr, env, appRuntime{}); code != 2 {
		t.Fatalf("status before run = %d, want 2", code)
	}

	now := time.Date(2026, 11, 27, 6, 0, 0, 0, time.UTC)
	daemonState := "down"
	var sent [][]string
	rt := appRuntime{
		deps: healthsnap.CheckDeps{
			Now: func() time.Time { return now },
			RunCmd: func(context.Context, healthsnap.CmdRequest) (healthsnap.CmdResult, error) {
				return healthsnap.CmdResult{Stdout: `{"daemon":"` + daemonState + `"}`}, nil
			},
		},
		notify: func(_ context.Context, argv, _ []string) error {
			sent = append(sent, argv)
			return nil
		},
	}

	// Run 1 (down): exits 1, writes JSON snapshot, routes alert to ops/model.
	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"run", "--config", cfgPath, "--json", "--markdown"}, &stdout, &stderr, env, rt); code != 2 {
		t.Fatalf("combining --json and --markdown on stdout should exit 2, got %d", code)
	}

	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"run", "--config", cfgPath, "--json"}, &stdout, &stderr, env, rt); code != 1 {
		t.Fatalf("run (down) exit = %d, want 1; stderr=%s", code, stderr.String())
	}
	var snap healthsnap.Snapshot
	if err := json.Unmarshal(stdout.Bytes(), &snap); err != nil {
		t.Fatalf("decode run json: %v\n%s", err, stdout.String())
	}
	if snap.State != healthsnap.SeverityFail || len(sent) != 1 || sent[0][2] != "ops/model" {
		t.Fatalf("unexpected snap=%+v sent=%v", snap, sent)
	}

	// Status reads the persisted snapshot.
	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"status", "--config", cfgPath, "--markdown"}, &stdout, &stderr, env, rt); code != 1 {
		t.Fatalf("status (fail) exit = %d, want 1; stderr=%s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "| bus-health |") {
		t.Errorf("unexpected status markdown:\n%s", stdout.String())
	}

	// Run 2 (up): exits 0, clears alert once.
	daemonState = "up"
	now = now.Add(time.Hour)
	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"run", "--config", cfgPath}, &stdout, &stderr, env, rt); code != 0 {
		t.Fatalf("run (up) exit = %d, want 0; stderr=%s", code, stderr.String())
	}
	if len(sent) != 2 || !strings.Contains(sent[1][3], "CLEAR bus-down") {
		t.Fatalf("expected clear alert, sent=%v", sent)
	}

	// Status now exits 0.
	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"status", "--config", cfgPath, "--json"}, &stdout, &stderr, env, rt); code != 0 {
		t.Fatalf("status (ok) exit = %d, want 0; stderr=%s", code, stderr.String())
	}
}

func TestCLIErrorExitCodes(t *testing.T) {
	tmp := t.TempDir()
	env := []string{"HOME=" + tmp, "XDG_STATE_HOME=" + filepath.Join(tmp, "state")}
	cfgPath := filepath.Join(tmp, "config.pkl")
	if err := os.WriteFile(cfgPath, []byte(`
notify { recipient = "ops/inbox" }
checks {
  new { id = "c"; argv { "cmd" } }
}
`), 0o600); err != nil {
		t.Fatal(err)
	}

	var out, errOut bytes.Buffer
	if code := run([]string{"run", "--config", filepath.Join(tmp, "missing.pkl")}, &out, &errOut, env, appRuntime{}); code != 2 {
		t.Fatalf("missing config exit = %d, want 2", code)
	}

	rt := appRuntime{
		deps: healthsnap.CheckDeps{
			Now: time.Now,
			RunCmd: func(context.Context, healthsnap.CmdRequest) (healthsnap.CmdResult, error) {
				return healthsnap.CmdResult{ExitCode: 1}, nil
			},
		},
		notify: func(context.Context, []string, []string) error {
			return errors.New("notify failed")
		},
	}
	out.Reset()
	errOut.Reset()
	if code := run([]string{"run", "--config", cfgPath}, &out, &errOut, env, rt); code != 2 {
		t.Fatalf("notify error exit = %d, want 2", code)
	}
}
