package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/diskwatch"
)

func newDiskwatchTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "diskwatch",
		MainPackage: "github.com/jaeyeom/experimental/devtools/diskwatch/cmd/diskwatch",
		SkillText:   skillText,
		ConfigPath: func() (string, bool) {
			paths, err := diskwatch.LocatePaths(os.Environ())
			if err != nil {
				return "", false
			}
			return paths.ConfigFile, paths.ConfigFile != ""
		},
		CheckConfig: func() error {
			paths, err := diskwatch.LocatePaths(os.Environ())
			if err != nil {
				return fmt.Errorf("locate diskwatch paths: %w", err)
			}
			if _, err := diskwatch.LoadConfig(paths.ConfigFile); err != nil && !errors.Is(err, fs.ErrNotExist) {
				return fmt.Errorf("load diskwatch config: %w", err)
			}
			return nil
		},
	}
}
