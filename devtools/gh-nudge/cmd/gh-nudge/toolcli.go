package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func defaultGHNudgeConfigPath() (string, bool) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", false
	}
	base := filepath.Join(home, ".config", "gh-nudge", "config")
	if _, err := os.Stat(base + ".pkl"); err == nil {
		return base + ".pkl", true
	}
	return base + ".yaml", true
}

func newGHNudgeTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "gh-nudge",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-nudge",
		SkillText:        skillText,
		ConfigPath:       defaultGHNudgeConfigPath,
		RequiredCommands: []string{"gh"},
		CheckConfig: func() error {
			if _, err := config.LoadConfig(""); err != nil {
				return fmt.Errorf("load gh-nudge config: %w", err)
			}
			return nil
		},
	}
}
