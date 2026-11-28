package main

import (
	"github.com/chorong-sh/chorong/pkg/toolcli"
)

func newDevcheckTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "devcheck",
		ModulePath:  "github.com/jaeyeom/experimental",
		MainPackage: "github.com/jaeyeom/experimental/devtools/devcheck/cmd/devcheck",
		SkillText:   skillText,
	}
}
