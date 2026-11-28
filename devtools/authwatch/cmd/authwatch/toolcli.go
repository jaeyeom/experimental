package main

import (
	"fmt"
	"os"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/authwatch"
)

func newAuthwatchTool() toolcli.Tool {
	return toolcli.Tool{
		Name:          "authwatch",
		MainPackage:   "github.com/jaeyeom/experimental/devtools/authwatch/cmd/authwatch",
		DaemonService: "authwatch run",
		SkillText:     skillText,
		ConfigPath: func() (string, bool) {
			paths, err := authwatch.LocatePaths(os.Environ())
			if err != nil {
				return "", false
			}
			return paths.ConfigFile, paths.ConfigFile != ""
		},
		CheckConfig: func() error {
			paths, err := authwatch.LocatePaths(os.Environ())
			if err != nil {
				return fmt.Errorf("locate authwatch paths: %w", err)
			}
			if _, err := authwatch.LoadConfig(paths.ConfigFile, true); err != nil {
				return fmt.Errorf("load authwatch config: %w", err)
			}
			return nil
		},
	}
}
