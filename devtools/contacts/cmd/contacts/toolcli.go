package main

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/contacts"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newContactsTool(configFlag string, getenv func(string) string) toolcli.Tool {
	return toolcli.Tool{
		Name:        "contacts",
		MainPackage: "github.com/jaeyeom/experimental/devtools/contacts/cmd/contacts",
		SkillText:   skillText,
		ConfigPath: func() (string, bool) {
			p, err := contacts.ConfigPath(configFlag, getenv)
			return p, err == nil
		},
		CheckConfig: func() error {
			p, err := contacts.ConfigPath(configFlag, getenv)
			if err != nil {
				return fmt.Errorf("contacts config path: %w", err)
			}
			if _, err := contacts.LoadConfig(p, getenv); err != nil {
				return fmt.Errorf("load contacts config: %w", err)
			}
			return nil
		},
	}
}
