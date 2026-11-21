package main

import (
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newClaudeUsageTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "claude-usage",
		MainPackage: "github.com/jaeyeom/experimental/devtools/claude-usage/cmd/claude-usage",
		SkillText:   skillText,
	}
}
