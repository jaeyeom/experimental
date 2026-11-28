package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"github.com/chorong-sh/chorong/pkg/toolcli"
)

func TestInstallCheckToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			t.Setenv("HOME", homeDir)
			t.Setenv("XDG_CONFIG_HOME", filepath.Join(homeDir, ".config"))
			exe := filepath.Join(t.TempDir(), "install-check")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			lookPath := func(file string) (string, error) {
				if file == "install-check" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			tool := newInstallCheckToolWith([]string{
				"HOME=" + homeDir,
				"XDG_CONFIG_HOME=" + filepath.Join(homeDir, ".config"),
			}, lookPath)
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			return tool
		},
	})
}

func TestInstallCheckDoctorStandaloneAndAgentbus(t *testing.T) {
	runDoctor := func(t *testing.T, cfgBody string, missingBins map[string]bool, explicitMissing bool) (int, toolcli.DoctorOutput) {
		t.Helper()
		home := t.TempDir()
		cfgPath := filepath.Join(home, "config.toml")
		if !explicitMissing {
			if err := os.WriteFile(cfgPath, []byte(cfgBody), 0o600); err != nil {
				t.Fatal(err)
			}
		}
		exe := filepath.Join(t.TempDir(), "install-check")
		if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
			t.Fatal(err)
		}
		tool := newInstallCheckToolWith([]string{"HOME=" + home}, func(file string) (string, error) {
			if file == "install-check" {
				return exe, nil
			}
			if missingBins[file] {
				return "", errors.New("not found")
			}
			return "/usr/bin/" + file, nil
		})
		tool.HomeDir = func() (string, error) { return home, nil }
		tool.WorkDir = func() (string, error) { return home, nil }
		tool.Executable = func() (string, error) { return exe, nil }

		var out, errBuf bytes.Buffer
		code, ok := tool.Handle(context.Background(), []string{"doctor", "--offline", "--json", "--config", cfgPath}, &out, &errBuf)
		if !ok {
			t.Fatal("doctor not handled")
		}
		var doc toolcli.DoctorOutput
		if err := json.Unmarshal(out.Bytes(), &doc); err != nil {
			t.Fatalf("decode doctor json: %v\nstdout=%s\nstderr=%s", err, out.String(), errBuf.String())
		}
		return code, doc
	}

	t.Run("command mode passes without agentbus installed", func(t *testing.T) {
		cfg := `notify_command = ["my-notifier"]
gh_bin = "gh-throttle"
[[repo]]
url = "https://github.com/o/r"
issues = [1]
`
		code, doc := runDoctor(t, cfg, map[string]bool{"agentbus": true}, false)
		if code == 2 {
			t.Fatalf("expected standalone command mode to pass without agentbus, got code=2: %+v", doc)
		}
	})

	t.Run("stdout mode passes without agentbus installed", func(t *testing.T) {
		cfg := `notify_mode = "stdout"
[[repo]]
url = "https://github.com/o/r"
issues = [1]
`
		code, doc := runDoctor(t, cfg, map[string]bool{"agentbus": true}, false)
		if code == 2 {
			t.Fatalf("expected stdout mode to pass without agentbus, got code=2: %+v", doc)
		}
	})

	t.Run("agentbus mode fails if agentbus missing", func(t *testing.T) {
		cfg := `notify_name = "installer"
[[repo]]
url = "https://github.com/o/r"
issues = [1]
`
		code, doc := runDoctor(t, cfg, map[string]bool{"agentbus": true}, false)
		if code != 2 {
			t.Fatalf("expected code=2 when agentbus missing, got %d: %+v", code, doc)
		}
	})

	t.Run("explicit missing config fails", func(t *testing.T) {
		code, doc := runDoctor(t, "", nil, true)
		if code != 2 {
			t.Fatalf("expected code=2 for missing explicit config, got %d: %+v", code, doc)
		}
	})
}
