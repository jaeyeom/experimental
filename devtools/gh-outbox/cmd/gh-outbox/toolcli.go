package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"

	ghoutbox "github.com/jaeyeom/experimental/devtools/gh-outbox"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newGHOutboxTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "gh-outbox",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/gh-outbox/cmd/gh-outbox",
		DaemonService:    "gh-outbox poll --watch",
		SkillText:        skillText,
		RequiredCommands: []string{"gh", "decide", "agentbus"},
		ConfigPath: func() (string, bool) {
			p, err := ghoutbox.DefaultConfigPath(os.Environ())
			return p, err == nil
		},
		CheckConfig: func() error {
			if _, err := ghoutbox.LoadConfig("", os.Environ()); err != nil && !errors.Is(err, fs.ErrNotExist) {
				return fmt.Errorf("load gh-outbox config: %w", err)
			}
			return nil
		},
	}
}
