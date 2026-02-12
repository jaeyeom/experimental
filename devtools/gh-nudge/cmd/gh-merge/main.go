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
	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

var (
	configPath   string
	dryRun       bool
	deleteBranch bool
	verbose      bool
)

func init() {
	flag.StringVar(&configPath, "config", "", "Path to config file")
	flag.BoolVar(&dryRun, "dry-run", false, "Dry run mode (don't actually merge)")
	flag.BoolVar(&deleteBranch, "d", false, "Delete branch after merge")
	flag.BoolVar(&verbose, "v", false, "Verbose output")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [options] [command]\n\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nCommands:\n")
		fmt.Fprintf(os.Stderr, "  list    List mergeable PRs (default)\n")
		fmt.Fprintf(os.Stderr, "  merge   Merge eligible PRs\n")
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

// listMergeablePRs lists all PRs that are ready to merge.
func listMergeablePRs(githubClient *github.Client, cfg *config.Config) error {
	_ = cfg // TODO: use config for filtering/display options
	slog.Info("Finding PRs with no review requests in the current repository")

	prs, err := githubClient.GetMergeablePullRequests()
	if err != nil {
		return fmt.Errorf("failed to get PRs: %w", err)
	}

	if len(prs) == 0 {
		fmt.Println("No PRs with no review requests found.")
		return nil
	}

	fmt.Printf("Found %d PRs with no review requests:\n\n", len(prs))

	for _, pr := range prs {
		fmt.Printf("  %s\n", pr.Title)
		fmt.Printf("         URL: %s\n", pr.URL)
		fmt.Printf("         Branch: %s\n", pr.HeadRefName)
		fmt.Println()
	}

	return nil
}

// mergePRs merges all eligible PRs.
func mergePRs(githubClient *github.Client, cfg *config.Config) error {
	_ = cfg // TODO: use config for merge strategies/rules
	slog.Info("Finding and merging PRs with no review requests",
		"dry_run", dryRun,
		"delete_branch", deleteBranch)

	prs, err := githubClient.GetMergeablePullRequests()
	if err != nil {
		return fmt.Errorf("failed to get PRs: %w", err)
	}

	if len(prs) == 0 {
		fmt.Println("No PRs with no review requests found.")
		return nil
	}

	fmt.Printf("Found %d PRs with no review requests:\n\n", len(prs))

	mergedPRs := 0
	for _, pr := range prs {
		fmt.Printf("  %s\n", pr.Title)
		fmt.Printf("         URL: %s\n", pr.URL)
		fmt.Printf("         Branch: %s\n", pr.HeadRefName)

		if dryRun {
			branchAction := "keep"
			if deleteBranch {
				branchAction = "delete"
			}
			fmt.Printf("         Would merge PR and %s branch\n", branchAction)
			continue
		}

		err := githubClient.MergePullRequest(pr.URL, deleteBranch)
		if err != nil {
			fmt.Printf("         Error merging: %v\n", err)
			continue
		}

		successMsg := "Successfully added to the merge queue"
		if deleteBranch {
			successMsg += " and deleted branch"
		}
		fmt.Printf("         %s\n", successMsg)
		mergedPRs++
		fmt.Println()
	}

	if dryRun {
		fmt.Printf("Found %d PRs with no review requests (dry run, no changes made).\n", len(prs))
	} else {
		fmt.Printf("Successfully added to the merge queue %d of %d PRs.\n", mergedPRs, len(prs))
	}

	return nil
}

func main() {
	flag.Parse()

	// Setup logging
	setupLogging()

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
		cmdErr = mergePRs(githubClient, cfg)
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
