package main

import (
	"fmt"
	"os"
	"slices"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newAgentbusTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "agentbus",
		MainPackage: "github.com/jaeyeom/experimental/devtools/agentbus/cmd/agentbus",
		Companions: []toolcli.Companion{
			{
				Name:          "agentbusd",
				Package:       "github.com/jaeyeom/experimental/devtools/agentbus/cmd/agentbusd",
				DaemonService: "agentbusd",
			},
		},
		SkillText: skillText,
		ConfigPath: func() (string, bool) {
			p, err := config.Path(os.Environ())
			return p, err == nil
		},
		CheckConfig: func() error {
			if _, err := config.Load(os.Environ()); err != nil {
				return fmt.Errorf("load agentbus config: %w", err)
			}
			return nil
		},
		RequiredCommands: []string{"herdr"},
	}
}

func shouldHandleToolCLI(args []string) bool {
	if len(args) == 0 {
		return false
	}
	switch args[0] {
	case "version", "update", "skill":
		return true
	case "doctor":
		hasProtocolFlag := slices.Contains(args[1:], "--json") || slices.Contains(args[1:], "--offline")
		hasSandbox := slices.Contains(args[1:], "--sandbox")
		return hasProtocolFlag && !hasSandbox
	default:
		return false
	}
}
