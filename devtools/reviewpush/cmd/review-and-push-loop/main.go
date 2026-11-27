// Command review-and-push-loop reviews and pushes the oldest commit ahead of
// the remote base branch in a fresh read-only reviewer process, then repeats
// until there are no local commits left to review or the workflow is blocked.
package main

import (
	"context"
	"encoding/json"
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
	var jsonFlag bool
	fs := flag.NewFlagSet("review-and-push-loop", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() {
		fmt.Fprintf(stderr, `Usage of %s:
  review-and-push-loop reviews local commits ahead of the remote base branch
  and pushes them one at a time after a read-only review pass.

  The loop fetches origin, finds the oldest unpushed local commit, runs a
  read-only reviewer (Codex CLI by default, or any command via --reviewer-cmd
  or REVIEWPUSH_REVIEWER_CMD) expecting PUSH or BLOCKED as JSON, validates the
  verdict and push target, then performs git push itself.

  Base branch detection uses origin/HEAD, then main/master/trunk. Use --base
  when working against a different branch or bootstrapping an empty remote.

Environment:
  GIT_BIN                      git executable to run (default "git")
  REVIEWPUSH_REVIEWER_CMD      custom reviewer command (overrides CODEX_BIN)
  REVIEWPUSH_TIMEOUT_SECONDS   per-review timeout in seconds (falls back to CODEX_TIMEOUT_SECONDS, default "300")
  CODEX_BIN                    codex executable to run when no custom reviewer is set (default "codex")
  CODEX_TIMEOUT_SECONDS        legacy per-review timeout in seconds (default "300")

Flags:
`, os.Args[0])
		fs.PrintDefaults()
	}

	fs.StringVar(&cfg.RepoDir, "repo", ".", "Repository directory")
	fs.StringVar(&cfg.BaseBranchOverride, "base", "", "Override base branch detection")
	fs.IntVar(&cfg.MaxIterations, "max-iterations", 100, "Maximum loop iterations")
	fs.StringVar(&cfg.OutputMode, "output", reviewpush.OutputModeHuman, "Output mode: human, compact, or json")
	fs.BoolVar(&jsonFlag, "json", false, "Emit a single structured JSON summary on stdout (shorthand for --output json)")
	fs.StringVar(&cfg.ReviewerCmd, "reviewer-cmd", "", "Custom reviewer command (reads prompt on stdin, emits verdict JSON on stdout)")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return 0
		}
		return 2
	}

	if jsonFlag {
		if cfg.OutputMode != reviewpush.OutputModeHuman && cfg.OutputMode != reviewpush.OutputModeJSON {
			return configError(stdout, stderr, false, fmt.Sprintf("--json cannot be combined with --output %s", cfg.OutputMode))
		}
		cfg.OutputMode = reviewpush.OutputModeJSON
	}

	isJSON := cfg.OutputMode == reviewpush.OutputModeJSON
	switch cfg.OutputMode {
	case reviewpush.OutputModeHuman, reviewpush.OutputModeCompact, reviewpush.OutputModeJSON:
	default:
		return configError(stdout, stderr, isJSON, fmt.Sprintf("--output must be %q, %q, or %q", reviewpush.OutputModeHuman, reviewpush.OutputModeCompact, reviewpush.OutputModeJSON))
	}

	if cfg.MaxIterations <= 0 {
		return configError(stdout, stderr, isJSON, "--max-iterations must be greater than zero")
	}

	cfg.GitBin = envOrDefault("GIT_BIN", "git")
	cfg.CodexBin = envOrDefault("CODEX_BIN", "codex")
	if cfg.ReviewerCmd == "" {
		cfg.ReviewerCmd = os.Getenv("REVIEWPUSH_REVIEWER_CMD")
	}

	timeout, err := parseTimeoutSeconds()
	if err != nil {
		return configError(stdout, stderr, isJSON, err.Error())
	}
	cfg.CodexTimeoutSecs = timeout

	reviewer, err := buildReviewer(cfg, stderr)
	if err != nil {
		return configError(stdout, stderr, isJSON, err.Error())
	}

	exec := executor.NewBasicExecutor()
	runner := reviewpush.NewRunner(cfg, exec, reviewer)
	runner.Stdout = stdout
	runner.Stderr = stderr
	return runner.Run(context.Background())
}

func buildReviewer(cfg reviewpush.Config, stderr io.Writer) (reviewpush.Reviewer, error) {
	streamStderr := cfg.OutputMode == reviewpush.OutputModeHuman
	if cfg.ReviewerCmd != "" {
		argv, err := reviewpush.SplitCommand(cfg.ReviewerCmd)
		if err != nil {
			return nil, fmt.Errorf("invalid reviewer command: %w", err)
		}
		return &reviewpush.CommandReviewer{
			Command:      argv,
			TimeoutSecs:  cfg.CodexTimeoutSecs,
			Stderr:       stderr,
			StreamStderr: streamStderr,
		}, nil
	}
	return &reviewpush.RealCodexRunner{
		CodexBin:     cfg.CodexBin,
		TimeoutSecs:  cfg.CodexTimeoutSecs,
		Stderr:       stderr,
		StreamStderr: streamStderr,
	}, nil
}

func parseTimeoutSeconds() (int, error) {
	if raw := os.Getenv("REVIEWPUSH_TIMEOUT_SECONDS"); raw != "" {
		n, err := strconv.Atoi(raw)
		if err != nil || n < 0 {
			return 0, errors.New("REVIEWPUSH_TIMEOUT_SECONDS must be a non-negative integer")
		}
		return n, nil
	}
	if raw := os.Getenv("CODEX_TIMEOUT_SECONDS"); raw != "" {
		n, err := strconv.Atoi(raw)
		if err != nil || n < 0 {
			return 0, errors.New("CODEX_TIMEOUT_SECONDS must be a non-negative integer")
		}
		return n, nil
	}
	return 300, nil
}

func configError(stdout, stderr io.Writer, asJSON bool, msg string) int {
	if asJSON {
		enc := json.NewEncoder(stdout)
		enc.SetEscapeHTML(false)
		_ = enc.Encode(reviewpush.RunReport{
			Status:   "ERROR",
			Reason:   msg,
			ExitCode: 1,
		})
		return 1
	}
	fmt.Fprintf(stderr, "Error: %s\n", msg)
	return 1
}

func envOrDefault(key, defaultVal string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return defaultVal
}
