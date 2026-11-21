package main

import (
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newQuotapaceTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "quotapace",
		MainPackage: "github.com/jaeyeom/experimental/devtools/quotapace/cmd/quotapace",
		SkillText:   skillText,
	}
}
