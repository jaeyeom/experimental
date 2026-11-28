package main

import (
	"github.com/chorong-sh/chorong/pkg/toolcli"
)

func newGHPrReviewTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "gh-pr-review",
		ModulePath:       "github.com/jaeyeom/experimental",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-pr-review",
		SkillText:        skillText,
		RequiredCommands: []string{"gh", "git"},
	}
}
