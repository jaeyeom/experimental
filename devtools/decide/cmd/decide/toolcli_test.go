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
