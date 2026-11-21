package cli

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestDocsyncToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			exe := filepath.Join(t.TempDir(), "docsync")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newDocsyncTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "docsync" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}
