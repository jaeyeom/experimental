package main

import (
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newOrgctlTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "orgctl",
		MainPackage: "github.com/jaeyeom/experimental/devtools/orgctl/cmd/orgctl",
		SkillText:   skillText,
	}
}
