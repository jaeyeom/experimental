// Package main provides the gh-codeowners command-line utility.
//
// gh-codeowners is a tool for identifying code owners for files in a repository
// based on GitHub's CODEOWNERS file rules. It uses a section-based matching approach
// where the last matching rule from each section contributes to a file's owners.
//
// Usage:
//
//	# Find owners for specific files using the default CODEOWNERS location
//	gh-codeowners file1.go path/to/file2.js
//
//	# Specify a custom CODEOWNERS file location
//	gh-codeowners --codeowners-file=/path/to/CODEOWNERS file1.go
//
//	# Process files from stdin (one file per line)
//	git diff --name-only | gh-codeowners
//
// Output Format:
//
// The output is a simple tab-separated format where each line shows a file and one owner.
// If a file has multiple owners, it will appear on multiple lines (once for each owner):
//
//	file1.go        @myorg/api-reviewers
//	file1.go        @myorg/go-readability-reviewers
//	file2.js        @myorg/frontend-reviewers
//
// This format makes it easy to process with standard Unix tools like grep, awk, and sort.
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log/slog"
	"os"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/codeowners"
)

var (
	codeownersFile string
	verbose        bool
	version        bool
)

const (
	// Version of the gh-codeowners tool.
	Version = "0.1.0"
)

func init() {
	flag.StringVar(&codeownersFile, "codeowners-file", "CODEOWNERS", "Path to CODEOWNERS file")
	flag.BoolVar(&verbose, "verbose", false, "Show verbose output")
	flag.BoolVar(&version, "version", false, "Display version information")
}

// setupLogger configures the application logger based on verbosity level.
func setupLogger() *slog.Logger {
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

// processFiles reads files from the given input and processes them with the codeowners parser.
func processFiles(input io.Reader, owners *codeowners.Codeowners, output io.Writer) error {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		file := scanner.Text()
		if file == "" {
			continue
		}

		// Clean the file path to ensure consistent matching
		file = filepath.Clean(file)
		slog.Debug("Processing file", "file", file)

		// Get owners for the file
		fileOwners := owners.OwnersFor(file)
		if len(fileOwners) == 0 {
			slog.Debug("No owners found", "file", file)
			continue
		}

		// Output each owner on a separate line
		for _, owner := range fileOwners {
			fmt.Fprintf(output, "%s\t%s\n", file, owner)
		}
	}

	if err := scanner.Err(); err != nil {
		return fmt.Errorf("error reading input: %w", err)
	}

	return nil
}

func run() error {
	// Handle version flag
	if version {
		fmt.Printf("gh-codeowners version %s\n", Version)
		return nil
	}

	// Open CODEOWNERS file
	codeownersPath := codeownersFile
	file, err := os.Open(codeownersPath)
	if err != nil {
		return fmt.Errorf("failed to open CODEOWNERS file %s: %w", codeownersPath, err)
	}
	defer file.Close()

	// Parse CODEOWNERS file
	slog.Debug("Parsing CODEOWNERS file", "path", codeownersPath)
	owners := codeowners.ParseSections(file)

	// Determine input source: stdin or command-line arguments
	var input io.Reader
	args := flag.Args()
	if len(args) > 0 {
		// Use command-line arguments as input
		slog.Debug("Using command-line arguments as input", "count", len(args))
		fileList := append([]string(nil), args...)
		input = strings.NewReader(strings.Join(fileList, "\n"))
	} else {
		// Use stdin as input
		slog.Debug("Reading file paths from stdin")
		input = os.Stdin
	}

	// Process files and output results
	err = processFiles(input, owners, os.Stdout)
	if err != nil {
		return fmt.Errorf("error processing files: %w", err)
	}

	slog.Debug("Processing completed")
	return nil
}

func main() {
	flag.Parse()

	// Set up logging
	setupLogger()

	if err := run(); err != nil {
		slog.Error("Command failed", "error", err)
		os.Exit(1)
	}
}
