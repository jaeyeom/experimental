package main

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestForkKeeperToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			cfgDir := filepath.Join(homeDir, ".config", "fork-keeper")
			if err := os.MkdirAll(cfgDir, 0o750); err != nil {
				t.Fatal(err)
			}
			cfgFile := filepath.Join(cfgDir, "forks.json")
			if err := os.WriteFile(cfgFile, []byte(`{"forks":[]}`), 0o600); err != nil {
				t.Fatal(err)
			}
			t.Setenv("FORK_KEEPER_CONFIG", cfgFile)
			scriptsDir := filepath.Join(homeDir, "scripts")
			if err := os.MkdirAll(scriptsDir, 0o750); err != nil {
				t.Fatal(err)
			}
			for _, name := range []string{"review-range.sh", "validate-verdict.sh"} {
				if err := os.WriteFile(filepath.Join(scriptsDir, name), []byte("#!/bin/sh\n"), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			t.Setenv("FORK_KEEPER_SCRIPTS", scriptsDir)

			exe := filepath.Join(t.TempDir(), "fork-keeper")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newForkKeeperTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "fork-keeper" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}

func TestForkKeeperExtraDoctorChecks(t *testing.T) {
	missingDir := filepath.Join(t.TempDir(), "missing")
	t.Setenv("FORK_KEEPER_SCRIPTS", missingDir)
	res := checkHelperScripts(context.Background(), true)
	if res.Status != toolcli.StatusFail {
		t.Fatalf("expected fail for missing scripts dir, got %+v", res)
	}

	scriptsDir := t.TempDir()
	for _, name := range []string{"review-range.sh", "validate-verdict.sh"} {
		if err := os.WriteFile(filepath.Join(scriptsDir, name), []byte("#!/bin/sh\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	t.Setenv("FORK_KEEPER_SCRIPTS", scriptsDir)
	res = checkHelperScripts(context.Background(), true)
	if res.Status != toolcli.StatusOK {
		t.Fatalf("expected ok when helper scripts exist, got %+v", res)
	}
}
