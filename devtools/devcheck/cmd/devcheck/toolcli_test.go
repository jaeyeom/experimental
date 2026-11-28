package main

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/chorong-sh/chorong/pkg/toolcli"
)

func TestDevcheckToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			exe := filepath.Join(t.TempDir(), "devcheck")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newDevcheckTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "devcheck" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			if tool.ModulePath != "github.com/jaeyeom/experimental" {
				t.Fatalf("ModulePath = %q, want %q", tool.ModulePath, "github.com/jaeyeom/experimental")
			}
			return tool
		},
	})
}
