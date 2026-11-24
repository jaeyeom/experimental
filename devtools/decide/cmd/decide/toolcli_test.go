package main

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestDecideToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
			t.Setenv("DECIDE_HUB", "")
			exe := filepath.Join(t.TempDir(), "decide")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newDecideTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "decide" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}

func TestDecideExtraDoctorChecks(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")

	hubRes := checkDecideHub(context.Background(), true)
	if hubRes.Status != toolcli.StatusOK {
		t.Fatalf("expected ok for unconfigured hub, got %+v", hubRes)
	}

	t.Setenv("DECIDE_HUB", "example-hub")
	hubOffline := checkDecideHub(context.Background(), true)
	if hubOffline.Status != toolcli.StatusWarn {
		t.Fatalf("expected warn for offline configured hub, got %+v", hubOffline)
	}

	notifyRes := checkDecideNotifyTarget(context.Background(), true)
	if notifyRes.Status != toolcli.StatusWarn {
		t.Fatalf("expected warn for unconfigured notify target, got %+v", notifyRes)
	}
}

func TestCheckDecideAgentbus(t *testing.T) {
	t.Run("found on PATH", func(t *testing.T) {
		bin := t.TempDir()
		bus := filepath.Join(bin, "agentbus")
		if err := os.WriteFile(bus, []byte("#!/bin/sh\n"), 0o755); err != nil { //nolint:gosec // test executable
			t.Fatal(err)
		}
		t.Setenv("PATH", bin)
		t.Setenv("XDG_CONFIG_HOME", t.TempDir())

		got := checkDecideAgentbus(context.Background(), true)
		if got.Status != toolcli.StatusOK || got.Name != "agentbus" || got.Detail != bus {
			t.Fatalf("got %+v, want StatusOK with detail %q", got, bus)
		}
	})

	cases := []struct {
		name       string
		configJSON string
		want       toolcli.CheckStatus
	}{
		{
			name: "missing and no config",
			want: toolcli.StatusWarn,
		},
		{
			name:       "missing and non-agentbus config",
			configJSON: `{"notifyPane":"p1","targets":[{"name":"a","pane":"w1:p1"}]}`,
			want:       toolcli.StatusWarn,
		},
		{
			name:       "missing with recipients.agentbus",
			configJSON: `{"recipients":{"agentbus":true}}`,
			want:       toolcli.StatusFail,
		},
		{
			name:       "missing with checkouts",
			configJSON: `{"checkouts":[{"resource":"repo","path":"/tmp/repo"}]}`,
			want:       toolcli.StatusFail,
		},
		{
			name:       "missing with agentbus target",
			configJSON: `{"targets":[{"name":"ops","pane":"agentbus:ops"}]}`,
			want:       toolcli.StatusFail,
		},
		{
			name:       "missing with agentbus notifyPane",
			configJSON: `{"notifyPane":"agentbus:ops"}`,
			want:       toolcli.StatusFail,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PATH", t.TempDir())
			xdg := t.TempDir()
			t.Setenv("XDG_CONFIG_HOME", xdg)
			if tc.configJSON != "" {
				dir := filepath.Join(xdg, "decide")
				if err := os.MkdirAll(dir, 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(filepath.Join(dir, "config.json"), []byte(tc.configJSON), 0o600); err != nil {
					t.Fatal(err)
				}
			}

			got := checkDecideAgentbus(context.Background(), true)
			if got.Status != tc.want || got.Name != "agentbus" {
				t.Fatalf("got %+v, want status %q", got, tc.want)
			}
		})
	}
}

func TestDecideDoctorFlagsStaleSkill(t *testing.T) {
	homeDir := t.TempDir()
	workDir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
	t.Setenv("DECIDE_HUB", "")

	exe := filepath.Join(t.TempDir(), "decide")
	if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
		t.Fatal(err)
	}
	tool := newDecideTool()
	tool.HomeDir = func() (string, error) { return homeDir, nil }
	tool.WorkDir = func() (string, error) { return workDir, nil }
	tool.Executable = func() (string, error) { return exe, nil }
	tool.LookPath = func(file string) (string, error) {
		if file == "decide" {
			return exe, nil
		}
		return "/bin/" + file, nil
	}

	// Install an older stamped skill for claude.
	skillPath := filepath.Join(homeDir, ".claude", "skills", "decide", "SKILL.md")
	if err := os.MkdirAll(filepath.Dir(skillPath), 0o755); err != nil {
		t.Fatal(err)
	}
	oldSkill := "---\nname: decide\ntoolcli-tool: decide\ntoolcli-version: v0.0.1\ntoolcli-hash: sha256:0000\n---\nold\n"
	if err := os.WriteFile(skillPath, []byte(oldSkill), 0o600); err != nil {
		t.Fatal(err)
	}

	var stdout, stderr bytes.Buffer
	code, handled := tool.Handle(context.Background(), []string{"doctor", "--offline", "--json"}, &stdout, &stderr)
	if !handled || code != 1 {
		t.Fatalf("doctor stale skill = (%d, %v), stderr=%s, stdout=%s", code, handled, stderr.String(), stdout.String())
	}
	var out toolcli.DoctorOutput
	if err := json.Unmarshal(stdout.Bytes(), &out); err != nil {
		t.Fatalf("unmarshal doctor json: %v\n%s", err, stdout.String())
	}
	var found bool
	for _, c := range out.Checks {
		if c.Name == "skills" {
			found = true
			if c.Status != toolcli.StatusWarn || !strings.Contains(c.Detail, "stale") {
				t.Fatalf("skills check = %+v, want warn with stale", c)
			}
		}
	}
	if !found {
		t.Fatalf("missing skills check in %+v", out.Checks)
	}

	// Updating with skill install makes doctor report skills ok.
	stdout.Reset()
	stderr.Reset()
	code, handled = tool.Handle(context.Background(), []string{"skill", "install", "--agent", "claude"}, &stdout, &stderr)
	if !handled || code != 0 {
		t.Fatalf("skill install = (%d, %v), stderr=%s", code, handled, stderr.String())
	}
	stdout.Reset()
	stderr.Reset()
	_, handled = tool.Handle(context.Background(), []string{"doctor", "--offline", "--json"}, &stdout, &stderr)
	if !handled {
		t.Fatal("doctor not handled")
	}
	if err := json.Unmarshal(stdout.Bytes(), &out); err != nil {
		t.Fatalf("unmarshal doctor json: %v", err)
	}
	for _, c := range out.Checks {
		if c.Name == "skills" && c.Status != toolcli.StatusOK {
			t.Fatalf("skills check after install = %+v, want ok", c)
		}
	}
}
