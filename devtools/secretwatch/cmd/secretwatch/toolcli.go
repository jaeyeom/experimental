package main

import (
	"context"
	"fmt"
	"os"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/secretwatch"
)

func newSecretwatchTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "secretwatch",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/secretwatch/cmd/secretwatch",
		DaemonService:    "secretwatch check --follow",
		SkillText:        skillText,
		RequiredCommands: []string{"journalctl"},
		ConfigPath: func() (string, bool) {
			paths, err := secretwatch.LocatePaths(os.Environ())
			if err != nil {
				return "", false
			}
			return paths.ConfigFile, paths.ConfigFile != ""
		},
		CheckConfig: func() error {
			paths, err := secretwatch.LocatePaths(os.Environ())
			if err != nil {
				return fmt.Errorf("locate secretwatch paths: %w", err)
			}
			if _, err := secretwatch.LoadConfig(context.Background(), paths.ConfigFile, true); err != nil {
				return fmt.Errorf("load secretwatch config: %w", err)
			}
			return nil
		},
	}
}
