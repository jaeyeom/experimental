package cli

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestFactoryToolCLIConformance(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			exe := filepath.Join(t.TempDir(), "factory")
			if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
				t.Fatal(err)
			}
			tool := *factoryTool()
			tool.Installer = inst
			tool.HomeDir = func() (string, error) { return homeDir, nil }
			tool.WorkDir = func() (string, error) { return workDir, nil }
			tool.Executable = func() (string, error) { return exe, nil }
			tool.LookPath = func(file string) (string, error) {
				if file == "factory" {
					return exe, nil
				}
				return "/bin/" + file, nil
			}
			return tool
		},
	})
}

func TestGoExecRunner(t *testing.T) {
	if _, err := goExecRunner(nil)(context.Background(), nil, "version"); err == nil {
		t.Fatal("expected error for nil executor")
	}

	mockExec := executor.NewMockExecutor()
	mockExec.ExpectCommandWithArgs("go", "install", "pkg@main").WillSucceed("ok\n", 0).Once().Build()
	out, err := goExecRunner(mockExec)(context.Background(), map[string]string{"GOBIN": "/tmp/bin"}, "install", "pkg@main")
	if err != nil {
		t.Fatal(err)
	}
	if string(out) != "ok\n" {
		t.Fatalf("unexpected output: %q", string(out))
	}
	execs := mockExec.Executions()
	if len(execs) != 1 || execs[0].Env["GOBIN"] != "/tmp/bin" {
		t.Fatalf("unexpected executions: %+v", execs)
	}
	if err := mockExec.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}

	failExec := executor.NewMockExecutor()
	failExec.ExpectCommandWithArgs("go", "list").WillFail("bad target\n", 1).Once().Build()
	if _, err := goExecRunner(failExec)(context.Background(), nil, "list"); err == nil || !strings.Contains(err.Error(), "bad target") {
		t.Fatalf("expected stderr error, got %v", err)
	}

	errExec := executor.NewMockExecutor()
	errExec.SetDefaultBehavior(nil, errors.New("boom"))
	if _, err := goExecRunner(errExec)(context.Background(), nil, "list"); err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected boom error, got %v", err)
	}
}

func TestFactoryToolInstallerLDFlags(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("go", "list", "-m", "-json", factoryModulePath+"@main").
		WillSucceed(`{"Version":"v0.1.0","Origin":{"Hash":"deadbeef1234"}}`, 0).Once().Build()
	mock.ExpectCommandWithArgs(
		"go", "install",
		"-ldflags", "-X github.com/jaeyeom/experimental/devtools/pkg/version.Version=v0.1.0",
		factoryMainPackage+"@main",
	).WillSucceed("", 0).Once().Build()

	useExec(t, mock)
	inst, ok := factoryTool().Installer.(toolcli.GoInstaller)
	if !ok {
		t.Fatalf("Installer = %T, want toolcli.GoInstaller", factoryTool().Installer)
	}
	if err := inst.Install(context.Background(), factoryMainPackage, "main", "/tmp/bin"); err != nil {
		t.Fatalf("Install() error = %v", err)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}
