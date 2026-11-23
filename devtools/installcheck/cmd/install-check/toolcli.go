package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"

	"github.com/jaeyeom/experimental/devtools/installcheck"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newInstallCheckTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "install-check",
		MainPackage: "github.com/jaeyeom/experimental/devtools/installcheck/cmd/install-check",
		SkillText:   skillText,
		ConfigPath: func() (string, bool) {
			paths, err := installcheck.LocatePaths(os.Environ())
			if err != nil {
				return "", false
			}
			return paths.ConfigFile, paths.ConfigFile != ""
		},
		CheckConfig: func() error {
			paths, err := installcheck.LocatePaths(os.Environ())
			if err != nil {
				return fmt.Errorf("locate install-check paths: %w", err)
			}
			if _, err := loadConfig(paths.ConfigFile); err != nil && !errors.Is(err, fs.ErrNotExist) {
				return fmt.Errorf("load install-check config: %w", err)
			}
			return nil
		},
	}
}
