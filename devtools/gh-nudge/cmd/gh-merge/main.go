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
package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	executor "github.com/jaeyeom/go-cmdexec"
)

// mergeOptions holds the flags that change merge behavior.
type mergeOptions struct {
	dryRun       bool
	deleteBranch bool
	force        bool
}

// setupLogging configures the logger with the appropriate level.
func setupLogging(verbose bool) *slog.Logger {
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
func tryMergePR(githubClient mergeClient, pr *models.PullRequest, blockLabels []string, opts mergeOptions) mergeResult {
	fmt.Printf("  %s\n", pr.Title)
	fmt.Printf("         URL: %s\n", pr.URL)
	fmt.Printf("         Branch: %s\n", pr.HeadRefName)
	printCIStatus(pr)

	if label, blocked := pr.BlocksMerge(blockLabels); blocked {
		fmt.Printf("         Skipping: label %q blocks merge\n\n", label)
		return mergeResultSkippedLabel
	}

	ciStatus := pr.ChecksStatus()
	if ciStatus == models.ChecksFail && !opts.force {
		fmt.Printf("         Skipping: CI checks failed — blocking merge (use --force to override)\n\n")
		return mergeResultSkippedFail
	}
	if ciStatus == models.ChecksPending && !opts.force {
		fmt.Printf("         Skipping: CI checks still in progress\n\n")
		return mergeResultSkippedPending
	}
	if opts.force && (ciStatus == models.ChecksFail || ciStatus == models.ChecksPending) {
		fmt.Printf("         Warning: merging despite CI status %s (--force)\n", ciStatus)
	}

	if opts.dryRun {
		branchAction := "keep"
		if opts.deleteBranch {
			branchAction = "delete"
		}
		fmt.Printf("         Would merge PR and %s branch\n", branchAction)
		return mergeResultDryRun
	}

	if err := githubClient.MergePullRequest(pr.URL, opts.deleteBranch); err != nil {
		fmt.Printf("         Error merging: %v\n", err)
		return mergeResultError
	}

	successMsg := "Successfully added to the merge queue"
	if opts.deleteBranch {
		successMsg += " and deleted branch"
	}
	fmt.Printf("         %s\n\n", successMsg)
	return mergeResultMerged
}

// mergePRs merges all eligible PRs.
func mergePRs(githubClient mergeClient, cfg *config.Config, opts mergeOptions) error {
	slog.Info("Finding and merging PRs with no review requests",
		"dry_run", opts.dryRun,
		"delete_branch", opts.deleteBranch,
		"force", opts.force,
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
		switch tryMergePR(githubClient, &prs[i], cfg.Settings.BlockMergeLabels, opts) {
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

	if opts.dryRun {
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

func main() {
	var configPath string
	var opts mergeOptions
	var verbose bool
	flag.StringVar(&configPath, "config", "", "Path to config file")
	flag.BoolVar(&opts.dryRun, "dry-run", false, "Dry run mode (don't actually merge)")
	flag.BoolVar(&opts.deleteBranch, "d", false, "Delete branch after merge")
	flag.BoolVar(&verbose, "v", false, "Verbose output")
	flag.BoolVar(&opts.force, "force", false, "Merge even if CI checks have failed or are pending")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [options] [command]\n\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nCommands:\n")
		fmt.Fprintf(os.Stderr, "  list    List mergeable PRs (default)\n")
		fmt.Fprintf(os.Stderr, "  merge   Merge eligible PRs\n")
	}
	flag.Parse()

	// Setup logging
	setupLogging(verbose)

	// Load configuration
	cfg, err := config.LoadConfig(configPath)
	if err != nil {
		slog.Error("Failed to load configuration", "error", err)
		os.Exit(1)
	}

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
		cmdErr = mergePRs(githubClient, cfg, opts)
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
