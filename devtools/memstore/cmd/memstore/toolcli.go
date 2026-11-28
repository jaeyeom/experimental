package main

import (
	"fmt"

	"github.com/chorong-sh/chorong/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/memstore"
)

func newMemstoreTool(configFlag string, getenv func(string) string) toolcli.Tool {
	return toolcli.Tool{
		Name:        "memstore",
		ModulePath:  "github.com/jaeyeom/experimental",
		MainPackage: "github.com/jaeyeom/experimental/devtools/memstore/cmd/memstore",
		SkillText:   skillText,
		ConfigPath: func() (string, bool) {
			p, err := memstore.ConfigPath(configFlag, getenv)
			return p, err == nil
		},
		CheckConfig: func() error {
			p, err := memstore.ConfigPath(configFlag, getenv)
			if err != nil {
				return fmt.Errorf("memstore config path: %w", err)
			}
			if _, err := memstore.LoadConfig(p, getenv); err != nil {
				return fmt.Errorf("load memstore config: %w", err)
			}
			return nil
		},
	}
}
