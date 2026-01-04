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
	"github.com/jaeyeom/experimental/devtools/gherun/internal/vars"
	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

// stringSliceFlag allows a flag to be specified multiple times.
type stringSliceFlag []string

func (s *stringSliceFlag) String() string {
	return fmt.Sprintf("%v", *s)
}

func (s *stringSliceFlag) Set(value string) error {
	*s = append(*s, value)
	return nil
}

func main() {
	var (
		parallel     int
		verbose      bool
		pollInterval time.Duration
		issueTitle   string
		varFlags     stringSliceFlag
		envFile      string
	)

	flag.IntVar(&parallel, "parallel", 1, "Maximum parallel browser tabs")
	flag.BoolVar(&verbose, "verbose", false, "Enable verbose logging")
	flag.DurationVar(&pollInterval, "poll-interval", 90*time.Second, "GitHub issue poll interval")
	flag.StringVar(&issueTitle, "title", "Gherkin Test Suite", "GitHub issue title")
	flag.Var(&varFlags, "var", "Template variable in KEY=VALUE format (can be repeated)")
	flag.StringVar(&envFile, "env-file", "", "Load template variables from file")
	flag.Parse()

	featureFiles := flag.Args()
	if len(featureFiles) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: gherun [flags] <feature-files...>")
		fmt.Fprintln(os.Stderr, "\nFlags:")
		flag.PrintDefaults()
		fmt.Fprintln(os.Stderr, "\nTemplate Variables:")
		fmt.Fprintln(os.Stderr, "  Use {{VAR_NAME}} in .feature files to reference variables.")
		fmt.Fprintln(os.Stderr, "  Example: gherun --var BASE_URL=https://example.com login.feature")
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

	// Parse template variables
	templateVars, err := parseTemplateVars(varFlags, envFile, logger)
	if err != nil {
		logger.Error("Failed to parse template variables", "error", err)
		os.Exit(1)
	}

	// Create dependencies
	ctx := context.Background()
	exec := executor.NewBasicExecutor()

	var parser gherkin.Parser
	if len(templateVars) > 0 {
		parser = gherkin.NewParserWithVars(templateVars)
		logger.Debug("Using parser with template variables", "count", len(templateVars))
	} else {
		parser = gherkin.NewParser()
	}
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

// parseTemplateVars parses template variables from flags and env file.
// Flag variables take precedence over env file variables.
func parseTemplateVars(varFlags []string, envFile string, logger *slog.Logger) (vars.Vars, error) {
	var result vars.Vars

	// Load from env file first (if specified)
	if envFile != "" {
		fileVars, err := vars.LoadEnvFile(envFile)
		if err != nil {
			return nil, fmt.Errorf("loading env file %s: %w", envFile, err)
		}
		result = fileVars
		logger.Debug("Loaded variables from env file", "file", envFile, "count", len(fileVars))
	}

	// Parse flag variables (override env file)
	if len(varFlags) > 0 {
		flagVars, err := vars.ParseFlags(varFlags)
		if err != nil {
			return nil, fmt.Errorf("parsing --var flags: %w", err)
		}
		if result == nil {
			result = flagVars
		} else {
			result = vars.Merge(result, flagVars)
		}
		logger.Debug("Parsed flag variables", "count", len(flagVars))
	}

	return result, nil
}
