package main

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	"github.com/chorong-sh/chorong/pkg/toolcli"
)

func TestReviewPushToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("GIT_BIN", "")
			t.Setenv("CODEX_BIN", "")
			exe := filepath.Join(t.TempDir(), "review-and-push-loop")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newReviewPushTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "review-and-push-loop" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}

func TestRunDispatchesToolCLI(t *testing.T) {
	var out, errBuf bytes.Buffer
	if code := run([]string{"version", "--json"}, &out, &errBuf); code != 0 {
		t.Fatalf("run(version --json) = %d, stderr=%s", code, errBuf.String())
	}
	if out.Len() == 0 {
		t.Fatal("expected JSON version output")
	}
}

func TestDoctorChecksCustomReviewerCommand(t *testing.T) {
	t.Setenv("REVIEWPUSH_REVIEWER_CMD", "/opt/bin/custom-reviewer --strict")
	tool := newReviewPushTool()
	foundCustom := false
	for _, cmd := range tool.RequiredCommands {
		if cmd == "codex" {
			t.Fatalf("did not expect codex in RequiredCommands when REVIEWPUSH_REVIEWER_CMD is set: %v", tool.RequiredCommands)
		}
		if cmd == "/opt/bin/custom-reviewer" {
			foundCustom = true
		}
	}
	if !foundCustom {
		t.Fatalf("expected /opt/bin/custom-reviewer in RequiredCommands, got %v", tool.RequiredCommands)
	}
}
