package main

import (
	"context"
	"io"
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func newGHThrottleTool() toolcli.Tool {
	return toolcli.Tool{
		Name:             "gh-throttle",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-throttle",
		SkillText:        skillText,
		RequiredCommands: []string{"gh"},
	}
}

func handleToolCLI(ctx context.Context, arg0 string, args []string, stdout, stderr io.Writer) (int, bool) {
	if filepath.Base(arg0) == "gh" {
		return 0, false
	}
	return newGHThrottleTool().Handle(ctx, args, stdout, stderr)
}
