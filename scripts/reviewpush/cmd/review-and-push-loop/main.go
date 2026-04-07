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

	"github.com/jaeyeom/experimental/scripts/reviewpush"
	executor "github.com/jaeyeom/go-cmdexec"
)

func main() {
	var cfg reviewpush.Config

	flag.StringVar(&cfg.RepoDir, "repo", ".", "Repository directory")
	flag.StringVar(&cfg.BaseBranchOverride, "base", "", "Override base branch detection")
	flag.IntVar(&cfg.MaxIterations, "max-iterations", 100, "Maximum loop iterations")
	flag.Parse()

	if cfg.MaxIterations <= 0 {
		fmt.Fprintf(os.Stderr, "Error: --max-iterations must be greater than zero\n")
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
		CodexBin:    cfg.CodexBin,
		TimeoutSecs: cfg.CodexTimeoutSecs,
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
