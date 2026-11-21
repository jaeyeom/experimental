package main

import (
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newGHPrReviewTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "gh-pr-review",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-pr-review",
		SkillText:        skillText,
		RequiredCommands: []string{"gh", "git"},
	}
}
