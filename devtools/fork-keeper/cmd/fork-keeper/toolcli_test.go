package main

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/chorong-sh/chorong/pkg/toolcli"
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
			t.Setenv("FORK_KEEPER_SCRIPTS", "")
			t.Setenv("FORK_KEEPER_REVIEWER_CMD", "")

			exe := filepath.Join(t.TempDir(), "fork-keeper")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := newForkKeeperToolWith([]string{
				"HOME=" + homeDir,
				"FORK_KEEPER_CONFIG=" + cfgFile,
			}, func(file string) (string, error) {
				if file == "fork-keeper" {
					return exe, nil
				}
				return "/bin/" + file, nil
			})
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			return tool
		},
	})
}

func TestForkKeeperExtraDoctorChecks(t *testing.T) {
	lookPathFound := func(allowed ...string) func(string) (string, error) {
		set := make(map[string]bool, len(allowed))
		for _, a := range allowed {
			set[a] = true
		}
		return func(name string) (string, error) {
			if set[name] {
				return "/usr/local/bin/" + name, nil
			}
			return "", errors.New("not found")
		}
	}

	t.Run("standalone-no-reviewer-ok", func(t *testing.T) {
		res := extraDoctorChecks(
			[]string{"HOME=/tmp/h", "FORK_KEEPER_SANDBOX=none"},
			lookPathFound("gh-throttle"),
			func() (string, bool) { return "", false },
		)
		if len(res) != 3 {
			t.Fatalf("got %d checks, want 3", len(res))
		}
		// Without config, gh resolves to "gh" which is missing here -> fail.
		if res[0].Name != "gh-bin" || res[0].Status != toolcli.StatusFail {
			t.Fatalf("expected gh-bin fail, got %+v", res[0])
		}
		if res[1].Name != "sandbox" || res[1].Status != toolcli.StatusOK {
			t.Fatalf("expected sandbox ok, got %+v", res[1])
		}
		if res[2].Name != "reviewer" || res[2].Status != toolcli.StatusOK || !strings.Contains(res[2].Detail, "standalone") {
			t.Fatalf("expected standalone reviewer ok, got %+v", res[2])
		}
	})

	t.Run("configured-gh-bin-and-reviewer", func(t *testing.T) {
		dir := t.TempDir()
		cfgPath := filepath.Join(dir, "forks.json")
		cfgBody := `{"gh_bin":"gh-throttle","forks":[{"name":"demo","repo":"me/demo","upstream_url":"https://github.com/up/demo.git","reviewer_cmd":["codex","exec"]}]}`
		if err := os.WriteFile(cfgPath, []byte(cfgBody), 0o600); err != nil {
			t.Fatal(err)
		}

		res := extraDoctorChecks(
			[]string{"HOME=" + dir, "FORK_KEEPER_SANDBOX=none"},
			lookPathFound("gh-throttle", "codex"),
			func() (string, bool) { return cfgPath, true },
		)
		for _, r := range res {
			if r.Status != toolcli.StatusOK {
				t.Errorf("check %s = %+v, want ok", r.Name, r)
			}
		}

		resMissing := extraDoctorChecks(
			[]string{"HOME=" + dir, "FORK_KEEPER_SANDBOX=bogus"},
			lookPathFound("gh-throttle"),
			func() (string, bool) { return cfgPath, true },
		)
		if resMissing[1].Status != toolcli.StatusFail {
			t.Errorf("expected sandbox fail for bogus mode, got %+v", resMissing[1])
		}
		if resMissing[2].Status != toolcli.StatusFail || !strings.Contains(resMissing[2].Detail, "codex") {
			t.Errorf("expected reviewer fail for missing codex, got %+v", resMissing[2])
		}
	})

	t.Run("missing-default-config-is-ok", func(t *testing.T) {
		dir := t.TempDir()
		missingCfg := filepath.Join(dir, "forks.json")
		tool := newForkKeeperToolWith(
			[]string{"HOME=" + dir, "FORK_KEEPER_CONFIG=" + missingCfg, "FORK_KEEPER_SANDBOX=none"},
			lookPathFound("git", "gh", "fork-keeper"),
		)
		if err := tool.CheckConfig(); err != nil {
			t.Fatalf("CheckConfig on missing default path returned error: %v", err)
		}
	})
}

func TestForkKeeperToolModulePath(t *testing.T) {
	tool := newForkKeeperTool()
	const want = "github.com/jaeyeom/experimental"
	if tool.ModulePath != want {
		t.Fatalf("ModulePath = %q, want %q", tool.ModulePath, want)
	}
}
