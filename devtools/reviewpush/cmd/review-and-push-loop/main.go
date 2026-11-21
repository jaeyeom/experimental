// Command review-and-push-loop reviews and pushes the oldest commit ahead of
// the remote base branch in a fresh Codex process, then repeats until there
// are no local commits left to review or the workflow is blocked.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"strconv"

	"github.com/jaeyeom/experimental/devtools/reviewpush"
	executor "github.com/jaeyeom/go-cmdexec"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	if code, ok := newReviewPushTool().Handle(context.Background(), args, stdout, stderr); ok {
		return code
	}

	var cfg reviewpush.Config
	fs := flag.NewFlagSet("review-and-push-loop", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() {
		fmt.Fprintf(stderr, `Usage of %s:
  review-and-push-loop reviews local commits ahead of the remote base branch
  and pushes them one at a time after a read-only Codex review pass.

  The loop fetches origin, finds the oldest unpushed local commit, asks Codex
  to report PUSH or BLOCKED as JSON, validates the proposed push target, then
  performs the git push itself. Codex is instructed not to edit files or push.

  Base branch detection uses origin/HEAD, then main/master/trunk. Use -base
  when working against a different branch or bootstrapping an empty remote.

Environment:
  GIT_BIN                 git executable to run (default "git")
  CODEX_BIN               codex executable to run (default "codex")
  CODEX_TIMEOUT_SECONDS   per-review timeout in seconds (default "300")

Flags:
`, os.Args[0])
		fs.PrintDefaults()
	}

	fs.StringVar(&cfg.RepoDir, "repo", ".", "Repository directory")
	fs.StringVar(&cfg.BaseBranchOverride, "base", "", "Override base branch detection")
	fs.IntVar(&cfg.MaxIterations, "max-iterations", 100, "Maximum loop iterations")
	fs.StringVar(&cfg.OutputMode, "output", reviewpush.OutputModeHuman, "Output mode: human or compact")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return 0
		}
		return 2
	}

	if cfg.MaxIterations <= 0 {
		fmt.Fprintf(stderr, "Error: --max-iterations must be greater than zero\n")
		return 1
	}
	if cfg.OutputMode != reviewpush.OutputModeHuman && cfg.OutputMode != reviewpush.OutputModeCompact {
		fmt.Fprintf(stderr, "Error: --output must be %q or %q\n", reviewpush.OutputModeHuman, reviewpush.OutputModeCompact)
		return 1
	}

	cfg.GitBin = envOrDefault("GIT_BIN", "git")
	cfg.CodexBin = envOrDefault("CODEX_BIN", "codex")

	timeoutStr := envOrDefault("CODEX_TIMEOUT_SECONDS", "300")
	timeout, err := strconv.Atoi(timeoutStr)
	if err != nil || timeout < 0 {
		fmt.Fprintf(stderr, "Error: CODEX_TIMEOUT_SECONDS must be a non-negative integer\n")
		return 1
	}
	cfg.CodexTimeoutSecs = timeout

	exec := executor.NewBasicExecutor()
	codex := &reviewpush.RealCodexRunner{
		CodexBin:     cfg.CodexBin,
		TimeoutSecs:  cfg.CodexTimeoutSecs,
		Stderr:       stderr,
		StreamStderr: cfg.OutputMode != reviewpush.OutputModeCompact,
	}

	runner := reviewpush.NewRunner(cfg, exec, codex)
	return runner.Run(context.Background())
}

func envOrDefault(key, defaultVal string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return defaultVal
}
