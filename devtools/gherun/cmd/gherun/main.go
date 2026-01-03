// Package main provides a CLI tool for running Gherkin tests with Perplexity Comet Browser.
package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/browser"
	"github.com/jaeyeom/experimental/devtools/gherun/internal/gherkin"
	"github.com/jaeyeom/experimental/devtools/gherun/internal/github"
	"github.com/jaeyeom/experimental/devtools/gherun/internal/runner"
	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

func main() {
	var (
		parallel     int
		verbose      bool
		pollInterval time.Duration
		issueTitle   string
	)

	flag.IntVar(&parallel, "parallel", 3, "Maximum parallel browser tabs")
	flag.BoolVar(&verbose, "verbose", false, "Enable verbose logging")
	flag.DurationVar(&pollInterval, "poll-interval", 90*time.Second, "GitHub issue poll interval")
	flag.StringVar(&issueTitle, "title", "Gherkin Test Suite", "GitHub issue title")
	flag.Parse()

	featureFiles := flag.Args()
	if len(featureFiles) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: gherun [flags] <feature-files...>")
		fmt.Fprintln(os.Stderr, "\nFlags:")
		flag.PrintDefaults()
		os.Exit(1)
	}

	// Set up logging
	logLevel := slog.LevelInfo
	if verbose {
		logLevel = slog.LevelDebug
	}
	logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: logLevel}))
	slog.SetDefault(logger)

	// Handle BUILD_WORKING_DIRECTORY for Bazel
	if wd := os.Getenv("BUILD_WORKING_DIRECTORY"); wd != "" {
		if err := os.Chdir(wd); err != nil {
			logger.Error("Failed to change to working directory", "dir", wd, "error", err)
			os.Exit(1)
		}
	}

	// Create dependencies
	ctx := context.Background()
	exec := executor.NewBasicExecutor()

	parser := gherkin.NewParser()
	ghClient := github.NewClient(ctx, exec)
	issueManager := github.NewIssueManager(ctx, ghClient)
	launcher := browser.NewLauncher(exec)
	promptBuilder := browser.NewPromptBuilder()

	config := runner.Config{
		MaxParallel:  parallel,
		PollInterval: pollInterval,
		IssueTitle:   issueTitle,
		Verbose:      verbose,
	}

	r := runner.NewRunner(parser, issueManager, launcher, promptBuilder, config, logger)

	// Run
	summary, err := r.Run(ctx, featureFiles)
	if err != nil {
		logger.Error("Test run failed", "error", err)
		os.Exit(1)
	}

	// Print summary
	printSummary(summary)

	// Exit with appropriate code
	if summary.Failed > 0 {
		os.Exit(1)
	}
}

func printSummary(summary *runner.Summary) {
	fmt.Println()
	fmt.Println("========================================")
	fmt.Println("           TEST SUMMARY")
	fmt.Println("========================================")
	fmt.Printf("Total:    %d\n", summary.Total)
	fmt.Printf("Passed:   %d\n", summary.Passed)
	fmt.Printf("Failed:   %d\n", summary.Failed)
	fmt.Printf("Duration: %s\n", summary.Duration.Round(time.Second))
	fmt.Printf("Issue:    %s\n", summary.IssueURL)
	fmt.Println("========================================")

	if summary.Failed == 0 {
		fmt.Println("All tests passed!")
	} else {
		fmt.Printf("%d test(s) failed.\n", summary.Failed)
	}
}
