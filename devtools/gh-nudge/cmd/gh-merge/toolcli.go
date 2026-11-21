package main

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newGHMergeTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "gh-merge",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-merge",
		SkillText:        skillText,
		RequiredCommands: []string{"gh"},
		ConfigPath: func() (string, bool) {
			p, err := resolveMergeConfigPath("")
			return p, err == nil && p != ""
		},
		CheckConfig: func() error {
			p, err := resolveMergeConfigPath("")
			if err != nil {
				return fmt.Errorf("resolve gh-merge config: %w", err)
			}
			if _, err := config.LoadConfig(p); err != nil {
				return fmt.Errorf("load gh-merge config: %w", err)
			}
			return nil
		},
	}
}
