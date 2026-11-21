package main

import (
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newReviewPushTool() toolcli.Tool {
	return toolcli.Tool{
		Name:        "review-and-push-loop",
		MainPackage: "github.com/jaeyeom/experimental/devtools/reviewpush/cmd/review-and-push-loop",
		SkillText:   skillText,
		RequiredCommands: []string{
			envOrDefault("GIT_BIN", "git"),
			envOrDefault("CODEX_BIN", "codex"),
		},
	}
}
