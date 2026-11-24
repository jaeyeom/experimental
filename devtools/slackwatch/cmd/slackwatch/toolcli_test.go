package main

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

func TestSlackwatchToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
			t.Setenv("SLACKWATCH_CONFIG", "")
			exe := filepath.Join(t.TempDir(), "slackwatch")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newSlackwatchTool("")
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "slackwatch" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}

func TestDoctorWarnsOnStaleDaemonBinary(t *testing.T) {
	dir := t.TempDir()
	tokenPath := filepath.Join(dir, "tokens.json")
	if err := os.WriteFile(tokenPath, []byte(`{"appToken":"xapp-1","userToken":"xoxp-1"}`), 0o600); err != nil {
		t.Fatal(err)
	}
	dbPath := filepath.Join(dir, "slackwatch.db")
	cfgPath := filepath.Join(dir, "config.json")
	cfgBytes, err := json.Marshal(map[string]any{
		"tokenFile": tokenPath,
		"dbPath":    dbPath,
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, cfgBytes, 0o600); err != nil {
		t.Fatal(err)
	}
	store, err := slackwatch.OpenStore(dbPath)
	if err != nil {
		t.Fatal(err)
	}
	now := time.Now()
	if err := store.WriteStatus(slackwatch.Status{
		PID:               os.Getpid(),
		StartedAt:         now,
		HeartbeatAt:       now,
		HeartbeatInterval: slackwatch.Duration{Duration: time.Minute},
		Connection:        slackwatch.ConnConnected,
		BinaryInfo:        "old-build-info-from-previous-version",
	}); err != nil {
		t.Fatal(err)
	}
	store.Close()

	tool := newSlackwatchTool(cfgPath)
	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(context.Background(), []string{"doctor", "--offline", "--json"}, &out, &errBuf)
	if !ok {
		t.Fatalf("doctor not handled: %s", errBuf.String())
	}
	var doc toolcli.DoctorOutput
	if err := json.Unmarshal(out.Bytes(), &doc); err != nil {
		t.Fatalf("decode doctor json: %v (code=%d out=%s)", err, code, out.String())
	}
	var found bool
	for _, c := range doc.Checks {
		if c.Name == "daemon-binary" {
			found = true
			if c.Status != toolcli.StatusWarn {
				t.Errorf("daemon-binary status = %s, want warn (detail=%q)", c.Status, c.Detail)
			}
			if !strings.Contains(c.Detail, "slackwatch binary updated; restart needed") {
				t.Errorf("daemon-binary detail = %q", c.Detail)
			}
		}
	}
	if !found {
		t.Fatalf("doctor output missing daemon-binary check: %+v", doc.Checks)
	}
}
