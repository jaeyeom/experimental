package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	forkkeeper "github.com/jaeyeom/experimental/devtools/fork-keeper"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newForkKeeperTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "fork-keeper",
		MainPackage: "github.com/jaeyeom/experimental/devtools/fork-keeper/cmd/fork-keeper",
		SkillText:   skillText,
		ConfigPath: func() (string, bool) {
			p := forkkeeper.DefaultConfigPath()
			return p, p != ""
		},
		CheckConfig: func() error {
			if _, err := forkkeeper.LoadConfig(forkkeeper.DefaultConfigPath()); err != nil {
				return fmt.Errorf("load fork-keeper config: %w", err)
			}
			return nil
		},
		RequiredCommands: []string{"git", "jq", "gh"},
		ExtraDoctorChecks: []toolcli.DoctorCheck{
			checkHelperScripts,
		},
	}
}

func checkHelperScripts(_ context.Context, _ bool) toolcli.CheckResult {
	dir := forkkeeper.DefaultScriptsDir()
	for _, name := range []string{"review-range.sh", "validate-verdict.sh"} {
		p := filepath.Join(dir, name)
		if _, err := os.Stat(p); err != nil {
			return toolcli.CheckResult{
				Name:   "helper-scripts",
				Status: toolcli.StatusFail,
				Detail: fmt.Sprintf("missing %s", p),
				Hint:   "set FORK_KEEPER_SCRIPTS or clone jaeyeom/claude-toolbox into ~/.cache/claude-toolbox",
			}
		}
	}
	return toolcli.CheckResult{
		Name:   "helper-scripts",
		Status: toolcli.StatusOK,
		Detail: "review-range.sh and validate-verdict.sh found in " + dir,
	}
}
