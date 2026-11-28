package main

import (
	"errors"
	"fmt"
	"os"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/worker"
)

func newAgentPoolTool(deps cliDeps) toolcli.Tool {
	deps = deps.resolved()
	return toolcli.Tool{
		Name:          "agent-pool",
		MainPackage:   "github.com/jaeyeom/experimental/devtools/agent-pool/cmd/agent-pool",
		DaemonService: "agent-pool worker",
		Companions: []toolcli.Companion{
			{
				Name:          "agent-poold",
				Package:       "github.com/jaeyeom/experimental/devtools/agent-pool/cmd/agent-poold",
				DaemonService: "agent-poold",
			},
		},
		SkillText: skillText,
		ConfigPath: func() (string, bool) {
			p := worker.DefaultConfigPath()
			if _, err := os.Stat(p); err == nil {
				return p, true
			}
			return p, false
		},
		CheckConfig: func() error {
			p := worker.DefaultConfigPath()
			if _, err := worker.LoadConfig(p); err != nil {
				if errors.Is(err, os.ErrNotExist) {
					return nil
				}
				return fmt.Errorf("load worker config: %w", err)
			}
			return nil
		},
		ExtraDoctorChecks: []toolcli.DoctorCheck{
			doctorCheckCplt(deps),
			doctorCheckPlatformSandbox(deps),
			doctorCheckClaude(deps),
			doctorCheckClaudeUsage(deps),
			doctorCheckGitHubAuth(deps),
			doctorCheckBrokerHealthz(deps),
		},
		HomeDir:    deps.homeDir,
		Executable: deps.executable,
		LookPath:   deps.lookPath,
	}
}
