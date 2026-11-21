package main

import (
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newGHCodeownersTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "gh-codeowners",
		MainPackage: "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-codeowners",
		SkillText:   skillText,
	}
}
