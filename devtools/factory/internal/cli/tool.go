package cli

import (
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/toolcli"
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
			Exec:       exec,
			ModulePath: factoryModulePath,
			LDFlags: func(rev toolcli.RevisionInfo) string {
				ver := rev.Version
				if ver == "" {
					ver = devVersion
				}
				commitHash := rev.Revision
				if commitHash == "" {
					commitHash = unknownCommit
				}
				return fmt.Sprintf(
					"-X github.com/jaeyeom/experimental/devtools/pkg/version.Version=%s -X github.com/jaeyeom/experimental/devtools/factory/internal/cli.version=%s -X github.com/jaeyeom/experimental/devtools/factory/internal/cli.commit=%s",
					ver, ver, commitHash,
				)
			},
		}
	}

	return &toolcli.Tool{
		Name:             "factory",
		ModulePath:       factoryModulePath,
		MainPackage:      factoryMainPackage,
		SkillText:        skillText,
		RequiredCommands: []string{"herdr", "gh", "git"},
		Exec:             exec,
		Installer:        inst,
		ReadBuildInfo:    currentToolBuildInfo,
		HomeDir:          testToolHomeDir,
		WorkDir:          testToolWorkDir,
		Executable:       testToolExecutable,
		LookPath:         testToolLookPath,
		NoColor:          noColor,
	}
}

func envVal(env config.Env, key string) string {
	val, _ := env.LookupEnv(key)
	return strings.TrimSpace(val)
}
