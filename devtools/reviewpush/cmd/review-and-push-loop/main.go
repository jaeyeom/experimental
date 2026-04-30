// Command review-and-push-loop reviews and pushes the oldest commit ahead of
// the remote base branch in a fresh Codex process, then repeats until there
// are no local commits left to review or the workflow is blocked.
package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"strconv"

	"github.com/jaeyeom/experimental/devtools/reviewpush"
	executor "github.com/jaeyeom/go-cmdexec"
)

func main() {
	var cfg reviewpush.Config

	flag.Usage = func() {
		out := flag.CommandLine.Output()
		fmt.Fprintf(out, `Usage of %s:
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
		flag.PrintDefaults()
	}

	flag.StringVar(&cfg.RepoDir, "repo", ".", "Repository directory")
	flag.StringVar(&cfg.BaseBranchOverride, "base", "", "Override base branch detection")
	flag.IntVar(&cfg.MaxIterations, "max-iterations", 100, "Maximum loop iterations")
	flag.StringVar(&cfg.OutputMode, "output", reviewpush.OutputModeHuman, "Output mode: human or compact")
	flag.Parse()

	if cfg.MaxIterations <= 0 {
		fmt.Fprintf(os.Stderr, "Error: --max-iterations must be greater than zero\n")
		os.Exit(1)
	}
	if cfg.OutputMode != reviewpush.OutputModeHuman && cfg.OutputMode != reviewpush.OutputModeCompact {
		fmt.Fprintf(os.Stderr, "Error: --output must be %q or %q\n", reviewpush.OutputModeHuman, reviewpush.OutputModeCompact)
		os.Exit(1)
	}

	cfg.GitBin = envOrDefault("GIT_BIN", "git")
	cfg.CodexBin = envOrDefault("CODEX_BIN", "codex")

	timeoutStr := envOrDefault("CODEX_TIMEOUT_SECONDS", "300")
	timeout, err := strconv.Atoi(timeoutStr)
	if err != nil || timeout < 0 {
		fmt.Fprintf(os.Stderr, "Error: CODEX_TIMEOUT_SECONDS must be a non-negative integer\n")
		os.Exit(1)
	}
	cfg.CodexTimeoutSecs = timeout

	exec := executor.NewBasicExecutor()
	codex := &reviewpush.RealCodexRunner{
		CodexBin:     cfg.CodexBin,
		TimeoutSecs:  cfg.CodexTimeoutSecs,
		Stderr:       os.Stderr,
		StreamStderr: cfg.OutputMode != reviewpush.OutputModeCompact,
	}

	runner := reviewpush.NewRunner(cfg, exec, codex)
	exitCode := runner.Run(context.Background())
	os.Exit(exitCode)
}

func envOrDefault(key, defaultVal string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return defaultVal
}
