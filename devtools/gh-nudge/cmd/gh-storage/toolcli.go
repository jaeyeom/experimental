package main

import (
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newGHStorageTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "gh-storage",
		MainPackage: "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-storage",
		SkillText:   skillText,
	}
}
