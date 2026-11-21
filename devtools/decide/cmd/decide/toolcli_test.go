package main

import (
	"context"
	"os"
	"path/filepath"
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
