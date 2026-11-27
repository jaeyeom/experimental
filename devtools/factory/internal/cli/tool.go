package cli

import (
	"context"
	"errors"
	"fmt"
	"strings"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

// Test hooks for toolcli integration; nil in production.
var (
	testToolInstaller  toolcli.Installer
	testToolHomeDir    func() (string, error)
	testToolWorkDir    func() (string, error)
	testToolExecutable func() (string, error)
	testToolLookPath   func(string) (string, error)
	testToolBuildInfo  func() toolcli.BuildInfo
)

const (
	factoryModulePath  = "github.com/jaeyeom/experimental"
	factoryMainPackage = "github.com/jaeyeom/experimental/devtools/factory/cmd/factory"
)

func factoryTool() *toolcli.Tool {
	exec := newCommandExec()
	env := config.OSEnv()
	noColor := envVal(env, "NO_COLOR") != "" || envVal(env, "TERM") == "dumb"

	inst := testToolInstaller
	if inst == nil {
		inst = toolcli.GoInstaller{
			RunEnv:     goExecRunner(exec),
			ModulePath: factoryModulePath,
			LDFlags: func(rev toolcli.RevisionInfo) string {
				if rev.Version == "" {
					return ""
				}
				return "-X github.com/jaeyeom/experimental/devtools/pkg/version.Version=" + rev.Version
			},
		}
	}

	return &toolcli.Tool{
		Name:        "factory",
		ModulePath:  factoryModulePath,
		MainPackage: factoryMainPackage,
		SkillText:   skillText,
		SkillFiles: []toolcli.SkillFile{
			{Path: "references/setup.md", Content: setupReferenceText},
		},
		RequiredCommands: []string{"herdr", "gh", "git"},
		Installer:        inst,
		ReadBuildInfo:    testToolBuildInfo,
		HomeDir:          testToolHomeDir,
		WorkDir:          testToolWorkDir,
		Executable:       testToolExecutable,
		LookPath:         testToolLookPath,
		NoColor:          noColor,
	}
}

func goExecRunner(exec executor.Executor) func(ctx context.Context, env map[string]string, args ...string) ([]byte, error) {
	return func(ctx context.Context, env map[string]string, args ...string) ([]byte, error) {
		if exec == nil {
			return nil, errors.New("go installer: expected an executor, found nil")
		}
		res, err := exec.Execute(ctx, executor.ToolConfig{
			Command: "go",
			Args:    args,
			Env:     env,
		})
		if err != nil {
			return nil, err //nolint:wrapcheck // GoInstaller wraps with command context
		}
		if res == nil {
			return nil, errors.New("go installer: expected execution result, found nil")
		}
		if res.ExitCode != 0 {
			msg := strings.TrimSpace(res.Stderr)
			if msg == "" {
				return nil, fmt.Errorf("exit %d", res.ExitCode)
			}
			return nil, errors.New(msg)
		}
		return []byte(res.Output), nil
	}
}

func envVal(env config.Env, key string) string {
	val, _ := env.LookupEnv(key)
	return strings.TrimSpace(val)
}
