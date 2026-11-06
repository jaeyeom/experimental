// Package main implements the gh-merge command-line tool.
//
// gh-merge is a utility for finding and merging GitHub pull requests that have
// no pending review requests and are in a mergeable state. It can automatically
// merge these PRs and optionally delete the source branch after merging.
//
// Usage:
//
//	# List mergeable PRs without taking action
//	gh-merge list
//
//	# Merge all eligible PRs
//	gh-merge merge
//
//	# Merge and delete branches after merging
//	gh-merge merge -d
//
//	# Dry run to preview what would be merged
//	gh-merge merge --dry-run
//
//	# Use a custom config file
//	gh-merge --config /path/to/config.pkl
//
//	# Enqueue one pull request on the merge queue
//	gh-merge queue acme/widgets#12
//
//	# Enqueue pull request 12 in the current checkout
//	gh-merge queue 12
//
//	# Preview an enqueue. A draft is refused and nothing is enqueued.
//	gh-merge --dry-run queue 12
//
//	# Watch until each pull request merges, is ejected, or times out
//	gh-merge watch acme/widgets#12
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	executor "github.com/jaeyeom/go-cmdexec"
)

var (
	configPath            string
	dryRun                bool
	deleteBranch          bool
	verbose               bool
	force                 bool
	includeDownstack      bool
	jsonOutput            bool
	allowNoRequiredChecks bool
)

const (
	defaultWatchTimeout  = 30 * time.Minute
	defaultWatchInterval = 15 * time.Second
)

func registerFlags() {
	flag.StringVar(&configPath, "config", "", "Path to config file")
	flag.BoolVar(&dryRun, "dry-run", false, "Dry run mode (don't actually merge or enqueue)")
	flag.BoolVar(&deleteBranch, "d", false, "Delete branch after merge")
	flag.BoolVar(&verbose, "v", false, "Verbose output")
	flag.BoolVar(&force, "force", false, "Merge even if CI checks have failed or are pending")
	flag.BoolVar(&includeDownstack, "include-downstack", false, "Also enqueue open pull requests below a stack")
	flag.BoolVar(&jsonOutput, "json", false, "Print queue and watch results as JSON")
	flag.BoolVar(&allowNoRequiredChecks, "allow-no-required-checks", false, "Queue even when required_checks is empty or the repository is not configured")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [options] [command]\n\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nCommands:\n")
		fmt.Fprintf(os.Stderr, "  list    List mergeable PRs (default)\n")
		fmt.Fprintf(os.Stderr, "  merge   Merge eligible PRs\n")
		fmt.Fprintf(os.Stderr, "  queue   %s\n", queueCommandHelp())
		fmt.Fprintf(os.Stderr, "  watch   %s\n", watchCommandHelp())
	}
}

// setupLogging configures the logger with the appropriate level.
func setupLogging() *slog.Logger {
	logLevel := slog.LevelInfo
	if verbose {
		logLevel = slog.LevelDebug
	}
	logger := slog.New(slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
		Level: logLevel,
	}))
	slog.SetDefault(logger)
	return logger
}

// printCIStatus prints CI check status details for a pull request.
func printCIStatus(pr *models.PullRequest) {
	switch pr.ChecksStatus() {
	case models.ChecksPass:
		fmt.Printf("         CI: PASS (%d/%d checks passed)\n", len(pr.StatusCheckRollup), len(pr.StatusCheckRollup))
	case models.ChecksFail:
		failed := pr.FailedChecks()
		fmt.Printf("         CI: FAIL (%d failed)\n", len(failed))
		for _, check := range failed {
			fmt.Printf("           - %q %s\n", check.Name, check.Conclusion)
		}
	case models.ChecksPending:
		pending := pr.PendingChecks()
		fmt.Printf("         CI: PENDING (%d checks still running)\n", len(pending))
	case models.ChecksNone:
		fmt.Printf("         CI: NONE (no checks configured)\n")
	}
}

// mergeClient fetches mergeable pull requests and merges one of them.
type mergeClient interface {
	GetMergeablePullRequests() ([]models.PullRequest, error)
	MergePullRequest(prURL string, deleteBranch bool) error
}

// listMergeablePRs lists all PRs that are ready to merge.
func listMergeablePRs(githubClient mergeClient, cfg *config.Config) error {
	slog.Info("Finding PRs with no review requests in the current repository",
		"block_merge_labels", cfg.Settings.BlockMergeLabels)

	prs, err := githubClient.GetMergeablePullRequests()
	if err != nil {
		return fmt.Errorf("failed to get PRs: %w", err)
	}

	if len(prs) == 0 {
		fmt.Println("No PRs with no review requests found.")
		return nil
	}

	fmt.Printf("Found %d PRs with no review requests:\n\n", len(prs))

	for i := range prs {
		pr := &prs[i]
		fmt.Printf("  %s\n", pr.Title)
		fmt.Printf("         URL: %s\n", pr.URL)
		fmt.Printf("         Branch: %s\n", pr.HeadRefName)
		printCIStatus(pr)
		if label, blocked := pr.BlocksMerge(cfg.Settings.BlockMergeLabels); blocked {
			fmt.Printf("         Blocked: label %q prevents merge\n", label)
		}
		fmt.Println()
	}

	return nil
}

// mergeResult tracks the outcome of attempting to merge a single PR.
type mergeResult int

const (
	mergeResultMerged mergeResult = iota
	mergeResultSkippedFail
	mergeResultSkippedPending
	mergeResultSkippedLabel
	mergeResultDryRun
	mergeResultError
)

// tryMergePR attempts to merge a single PR, checking block labels and CI status first.
// It prints status and returns the outcome. A block label is not overridden by --force.
func tryMergePR(githubClient mergeClient, pr *models.PullRequest, blockLabels []string) mergeResult {
	fmt.Printf("  %s\n", pr.Title)
	fmt.Printf("         URL: %s\n", pr.URL)
	fmt.Printf("         Branch: %s\n", pr.HeadRefName)
	printCIStatus(pr)

	if label, blocked := pr.BlocksMerge(blockLabels); blocked {
		fmt.Printf("         Skipping: label %q blocks merge\n\n", label)
		return mergeResultSkippedLabel
	}

	ciStatus := pr.ChecksStatus()
	if ciStatus == models.ChecksFail && !force {
		fmt.Printf("         Skipping: CI checks failed — blocking merge (use --force to override)\n\n")
		return mergeResultSkippedFail
	}
	if ciStatus == models.ChecksPending && !force {
		fmt.Printf("         Skipping: CI checks still in progress\n\n")
		return mergeResultSkippedPending
	}
	if force && (ciStatus == models.ChecksFail || ciStatus == models.ChecksPending) {
		fmt.Printf("         Warning: merging despite CI status %s (--force)\n", ciStatus)
	}

	if dryRun {
		branchAction := "keep"
		if deleteBranch {
			branchAction = "delete"
		}
		fmt.Printf("         Would merge PR and %s branch\n", branchAction)
		return mergeResultDryRun
	}

	if err := githubClient.MergePullRequest(pr.URL, deleteBranch); err != nil {
		fmt.Printf("         Error merging: %v\n", err)
		return mergeResultError
	}

	successMsg := "Successfully added to the merge queue"
	if deleteBranch {
		successMsg += " and deleted branch"
	}
	fmt.Printf("         %s\n\n", successMsg)
	return mergeResultMerged
}

// mergePRs merges all eligible PRs.
func mergePRs(githubClient mergeClient, cfg *config.Config) error {
	slog.Info("Finding and merging PRs with no review requests",
		"dry_run", dryRun,
		"delete_branch", deleteBranch,
		"force", force,
		"block_merge_labels", cfg.Settings.BlockMergeLabels)

	prs, err := githubClient.GetMergeablePullRequests()
	if err != nil {
		return fmt.Errorf("failed to get PRs: %w", err)
	}

	if len(prs) == 0 {
		fmt.Println("No PRs with no review requests found.")
		return nil
	}

	fmt.Printf("Found %d PRs with no review requests:\n\n", len(prs))

	var merged, skippedFail, skippedPending, skippedLabel int
	for i := range prs {
		switch tryMergePR(githubClient, &prs[i], cfg.Settings.BlockMergeLabels) {
		case mergeResultMerged:
			merged++
		case mergeResultSkippedFail:
			skippedFail++
		case mergeResultSkippedPending:
			skippedPending++
		case mergeResultSkippedLabel:
			skippedLabel++
		}
	}

	if dryRun {
		fmt.Printf("Found %d PRs with no review requests (dry run, no changes made).\n", len(prs))
	} else {
		fmt.Printf("Successfully added to the merge queue %d of %d PRs.\n", merged, len(prs))
	}
	if skippedFail > 0 || skippedPending > 0 {
		fmt.Printf("Skipped: %d due to CI failure, %d due to pending CI.\n", skippedFail, skippedPending)
	}
	if skippedLabel > 0 {
		fmt.Printf("Skipped: %d due to a block label.\n", skippedLabel)
	}

	return nil
}

// resolveMergeConfigPath picks the gh-merge config file.
// -config wins, then GH_MERGE_CONFIG, then ~/.config/gh-merge/config.pkl
// when that file exists, then the gh-nudge default.
func resolveMergeConfigPath(flagPath string) (string, error) {
	if flagPath != "" {
		return flagPath, nil
	}
	if env := os.Getenv("GH_MERGE_CONFIG"); env != "" {
		return env, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("failed to get user home directory: %w", err)
	}
	mergePkl := filepath.Join(home, ".config", "gh-merge", "config.pkl")
	if configFileExists(mergePkl) {
		return mergePkl, nil
	}
	base := filepath.Join(home, ".config", "gh-nudge", "config")
	if configFileExists(base + ".pkl") {
		return base + ".pkl", nil
	}
	return base + ".yaml", nil
}

func configFileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func main() {
	registerFlags()
	flag.Parse()

	// Setup logging
	setupLogging()

	// Load configuration. gh-merge picks the path; gh-nudge's default stays put.
	path, err := resolveMergeConfigPath(configPath)
	if err != nil {
		slog.Error("Failed to resolve configuration path", "error", err)
		os.Exit(1)
	}
	cfg, err := config.LoadConfig(path)
	if err != nil {
		slog.Error("Failed to load configuration", "error", err)
		os.Exit(1)
	}
	cfg.Path = path

	// Initialize GitHub client
	ctx := context.Background()
	exec := executor.NewBasicExecutor()
	githubClient := github.NewClient(ctx, exec)

	// Determine command
	args := flag.Args()
	command := "list"
	if len(args) > 0 {
		command = args[0]
	}

	// Execute command
	var cmdErr error
	switch command {
	case "list":
		cmdErr = listMergeablePRs(githubClient, cfg)
	case "merge":
		cmdErr = mergePRs(githubClient, cfg)
	case "queue":
		if len(args) != 2 {
			fmt.Fprintf(os.Stderr, "Usage: %s %s\n", os.Args[0], queueUsage())
			os.Exit(1)
		}
		cmdErr = queuePullRequest(githubClient, cfg, args[1], includeDownstack)
	case "watch":
		cmdErr = watchPullRequests(githubClient, args[1:], watchOptions{
			timeout:  defaultWatchTimeout,
			interval: defaultWatchInterval,
			asJSON:   jsonOutput,
		})
		if errors.Is(cmdErr, errWatchUsage) {
			fmt.Fprintf(os.Stderr, "Usage: %s %s\n", os.Args[0], watchUsage())
			os.Exit(1)
		}
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
		flag.Usage()
		os.Exit(1)
	}

	if cmdErr != nil {
		slog.Error("Command failed", "command", command, "error", cmdErr)
		os.Exit(1)
	}
}
