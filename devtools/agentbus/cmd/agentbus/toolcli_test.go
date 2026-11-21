package main

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestAgentbusToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
			t.Setenv("XDG_RUNTIME_DIR", shortRuntime(t))
			t.Setenv("XDG_STATE_HOME", t.TempDir())
			exe := filepath.Join(t.TempDir(), "agentbus")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newAgentbusTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "agentbus" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}

func TestShouldHandleToolCLI(t *testing.T) {
	cases := []struct {
		args []string
		want bool
	}{
		{nil, false},
		{[]string{"version"}, true},
		{[]string{"version", "--json"}, true},
		{[]string{"update", "--check"}, true},
		{[]string{"skill"}, true},
		{[]string{"skill", "status", "--json"}, true},
		{[]string{"doctor"}, true},
		{[]string{"doctor", "extra"}, true},
		{[]string{"doctor", "--sandbox"}, true},
		{[]string{"doctor", "--json", "--sandbox"}, true},
		{[]string{"doctor", "--offline", "--sandbox"}, true},
		{[]string{"doctor", "--json"}, true},
		{[]string{"doctor", "--offline"}, true},
		{[]string{"doctor", "--offline", "--json"}, true},
		{[]string{"send", "ops/inbox", "hi"}, false},
	}
	for _, tc := range cases {
		if got := shouldHandleToolCLI(tc.args); got != tc.want {
			t.Errorf("shouldHandleToolCLI(%v) = %v, want %v", tc.args, got, tc.want)
		}
	}
}
