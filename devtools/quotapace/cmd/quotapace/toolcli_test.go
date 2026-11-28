package main

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/chorong-sh/chorong/pkg/toolcli"
)

func TestQuotapaceToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("XDG_DATA_HOME", filepath.Join(homeDir, ".local", "share"))
			exe := filepath.Join(t.TempDir(), "quotapace")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newQuotapaceTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "quotapace" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}
