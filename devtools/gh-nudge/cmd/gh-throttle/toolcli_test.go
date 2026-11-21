package main

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestGHThrottleToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			exe := filepath.Join(t.TempDir(), "gh-throttle")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newGHThrottleTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "gh-throttle" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}

func TestGHThrottleSymlinkGuard(t *testing.T) {
	ctx := context.Background()
	for _, cmd := range []string{"version", "update", "doctor", "skill"} {
		var out, errBuf bytes.Buffer
		if _, ok := handleToolCLI(ctx, "/usr/local/bin/gh", []string{cmd}, &out, &errBuf); ok {
			t.Errorf("invoked as gh: expected %q to pass through, but toolcli handled it", cmd)
		}
	}

	var out, errBuf bytes.Buffer
	code, ok := handleToolCLI(ctx, "/usr/local/bin/gh-throttle", []string{"version", "--json"}, &out, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("invoked as gh-throttle: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
}
