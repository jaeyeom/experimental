package main

import (
	"os"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	"github.com/jaeyeom/experimental/devtools/reviewpush"
)

func newReviewPushTool() toolcli.Tool {
	reviewerBin := envOrDefault("CODEX_BIN", "codex")
	if raw := os.Getenv("REVIEWPUSH_REVIEWER_CMD"); raw != "" {
		if argv, err := reviewpush.SplitCommand(raw); err == nil && len(argv) > 0 {
			reviewerBin = argv[0]
		}
	}
	return toolcli.Tool{
		Name:        "review-and-push-loop",
		MainPackage: "github.com/jaeyeom/experimental/devtools/reviewpush/cmd/review-and-push-loop",
		SkillText:   skillText,
		RequiredCommands: []string{
			envOrDefault("GIT_BIN", "git"),
			reviewerBin,
		},
	}
}
