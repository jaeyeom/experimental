package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"

	pkgversion "github.com/chorong-sh/chorong/pkg/version"
)

type Config struct {
	OldKey      string
	NewKey      string
	PlanFile    string
	DocsDir     string
	ShowVersion bool
}

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	config, exitCode, done := parseFlags(args, stderr)
	if done {
		return exitCode
	}
	if config.ShowVersion {
		fmt.Fprintln(stdout, pkgversion.Format("rename-jira-keys"))
		return 0
	}

	if config.OldKey == "" || config.NewKey == "" {
		switch {
		case config.OldKey == "" && config.NewKey == "":
			fmt.Fprintf(stderr, "Error: Both --old and --new flags are required\n\n")
		case config.OldKey == "":
			fmt.Fprintf(stderr, "Error: --old flag is required\n\n")
		default:
			fmt.Fprintf(stderr, "Error: --new flag is required\n\n")
		}
		// Show full help information when required flags are missing
		printUsage(stderr)
		return 1
	}

	if err := runRename(config); err != nil {
		fmt.Fprintf(stderr, "Error: %v\n", err)
		return 1
	}
	return 0
}

func printUsage(stderr io.Writer) {
	_, _, _ = parseFlags([]string{"-h"}, stderr)
}

func parseFlags(args []string, stderr io.Writer) (Config, int, bool) {
	var config Config
	fs := flag.NewFlagSet("rename-jira-keys", flag.ContinueOnError)
	fs.SetOutput(stderr)

	fs.StringVar(&config.OldKey, "old", "", "Old Jira key to replace (required)")
	fs.StringVar(&config.OldKey, "old-key", "", "Old Jira key to replace")
	fs.StringVar(&config.NewKey, "new", "", "New Jira key to use (required)")
	fs.StringVar(&config.NewKey, "new-key", "", "New Jira key to use")
	fs.StringVar(&config.PlanFile, "p", "plan.md", "Specify the plan file (shorthand)")
	fs.StringVar(&config.PlanFile, "plan-file", "plan.md", "Specify the plan file")
	fs.StringVar(&config.DocsDir, "d", "docs/project", "Specify the docs directory (shorthand)")
	fs.StringVar(&config.DocsDir, "docs-dir", "docs/project", "Specify the docs directory")
	fs.BoolVar(&config.ShowVersion, "version", false, "Print version information and exit")

	fs.Usage = func() {
		fmt.Fprintf(stderr, "rename-jira-keys - A tool to rename Jira ticket keys in documentation files\n\n")
		fmt.Fprintf(stderr, "USAGE:\n")
		fmt.Fprintf(stderr, "  %s --old OLD-KEY --new NEW-KEY [OPTIONS]\n\n", os.Args[0])
		fmt.Fprintf(stderr, "BACKGROUND:\n")
		fmt.Fprintf(stderr, "This tool is useful when Jira tickets are renumbered, moved between projects,\n")
		fmt.Fprintf(stderr, "or when you need to update references across multiple documentation files.\n")
		fmt.Fprintf(stderr, "It ensures consistent renaming across your project documentation while safely\n")
		fmt.Fprintf(stderr, "handling file operations with proper error checking.\n\n")
		fmt.Fprintf(stderr, "OPERATIONS:\n")
		fmt.Fprintf(stderr, "This tool performs two main operations:\n")
		fmt.Fprintf(stderr, "1. Content Replacement - Replaces all occurrences of OLD-KEY with NEW-KEY in:\n")
		fmt.Fprintf(stderr, "   • Plan file (default: plan.md, or custom file specified with -p)\n")
		fmt.Fprintf(stderr, "   • All *.md files in docs directory (default: docs/project/, or custom with -d)\n")
		fmt.Fprintf(stderr, "   • Uses word boundary matching to prevent partial replacements\n")
		fmt.Fprintf(stderr, "2. File Renaming - Renames documentation files starting with OLD-KEY:\n")
		fmt.Fprintf(stderr, "   • From: docs/project/OLD-KEY*.md (e.g., OLD-KEY.md, OLD-KEY-description.md)\n")
		fmt.Fprintf(stderr, "   • To: docs/project/NEW-KEY*.md (preserving any suffix after OLD-KEY)\n")
		fmt.Fprintf(stderr, "   • Safely checks for existing files to prevent overwrites\n\n")
		fmt.Fprintf(stderr, "SAFETY FEATURES:\n")
		fmt.Fprintf(stderr, "• Word boundary matching prevents partial matches (PROJ-123 won't match MYPROJ-123)\n")
		fmt.Fprintf(stderr, "• Checks for existing destination files before renaming\n")
		fmt.Fprintf(stderr, "• Provides detailed feedback on all operations performed\n")
		fmt.Fprintf(stderr, "• Gracefully handles missing files with warnings\n\n")
		fmt.Fprintf(stderr, "OPTIONS:\n")
		fs.PrintDefaults()
		fmt.Fprintf(stderr, "\nEXAMPLES:\n")
		fmt.Fprintf(stderr, "  # Basic usage - rename PROJ-123 to PROJ-456 in default locations\n")
		fmt.Fprintf(stderr, "  %s --old PROJ-123 --new PROJ-456\n\n", os.Args[0])
		fmt.Fprintf(stderr, "  # Custom paths - specify different plan file and docs directory\n")
		fmt.Fprintf(stderr, "  %s --old DEV-400 --new DEV-900 -p project-plan.md -d documentation/tickets\n\n", os.Args[0])
		fmt.Fprintf(stderr, "  # Using short form options\n")
		fmt.Fprintf(stderr, "  %s -old OLD-123 -new NEW-456 --plan-file roadmap.md --docs-dir ./docs/issues\n\n", os.Args[0])
		fmt.Fprintf(stderr, "COMMON USE CASES:\n")
		fmt.Fprintf(stderr, "• Jira project migration: OLDPROJ-123 → NEWPROJ-123\n")
		fmt.Fprintf(stderr, "• Ticket renumbering: PROJ-123 → PROJ-456\n")
		fmt.Fprintf(stderr, "• Bulk documentation updates after project restructuring\n")
	}

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return config, 0, true
		}
		return config, 2, true
	}

	return config, 0, false
}

func runRename(config Config) error {
	fmt.Printf("Renaming Jira key from '%s' to '%s'\n", config.OldKey, config.NewKey)
	fmt.Printf("Plan file: %s\n", config.PlanFile)
	fmt.Printf("Docs directory: %s\n", config.DocsDir)
	fmt.Println("==================================================")

	filesUpdated := 0

	// Step 1: Replace OLD_KEY with NEW_KEY in file contents
	switch err := replaceInFile(config.PlanFile, config.OldKey, config.NewKey); {
	case err == nil:
		filesUpdated++
	case errors.Is(err, fs.ErrNotExist):
		fmt.Printf("Warning: %s not found\n", config.PlanFile)
	default:
		fmt.Printf("Error processing %s: %v\n", config.PlanFile, err)
	}

	// Process all markdown files in docs directory
	pattern := filepath.Join(config.DocsDir, "*.md")
	matches, err := filepath.Glob(pattern)
	switch {
	case err != nil:
		fmt.Printf("Error finding markdown files in %s: %v\n", config.DocsDir, err)
	case len(matches) == 0:
		fmt.Printf("Warning: No markdown files found in %s/\n", config.DocsDir)
	default:
		for _, file := range matches {
			if err := replaceInFile(file, config.OldKey, config.NewKey); err != nil {
				fmt.Printf("Error processing %s: %v\n", file, err)
			} else {
				filesUpdated++
			}
		}
	}

	// Step 2: Rename the specific documentation file
	if err := renameDocFile(config.DocsDir, config.OldKey, config.NewKey); err != nil {
		fmt.Printf("File rename warning: %v\n", err)
	}

	fmt.Println()
	fmt.Printf("Summary: Updated %d files\n", filesUpdated)
	fmt.Println("Operation completed successfully!")
	return nil
}

func renameDocFile(docsDir, oldKey, newKey string) error {
	// Find all .md files in the docs directory
	pattern := filepath.Join(docsDir, "*.md")
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return fmt.Errorf("error searching for markdown files in %s: %v", docsDir, err)
	}

	if len(matches) == 0 {
		return fmt.Errorf("no markdown files found in %s", docsDir)
	}

	foundMatches := false

	// Process each file and check if it matches our precise pattern
	for _, oldDocFile := range matches {
		filename := filepath.Base(oldDocFile)

		// Check if filename starts with oldKey followed by either:
		// 1. .md (exact match: PROJ-1.md)
		// 2. - (dash-separated: PROJ-1-description.md)
		if filename == oldKey+".md" || (len(filename) > len(oldKey)+1 && filename[:len(oldKey)+1] == oldKey+"-") {
			foundMatches = true

			// Extract the suffix after the oldKey (e.g., ".md" or "-description.md")
			suffix := filename[len(oldKey):]
			newDocFile := filepath.Join(docsDir, newKey+suffix)

			if _, err := os.Stat(newDocFile); err == nil {
				return fmt.Errorf("%s already exists. Cannot rename %s", newDocFile, oldDocFile)
			}

			if err := os.Rename(oldDocFile, newDocFile); err != nil {
				fmt.Printf("Error renaming %s: %v\n", oldDocFile, err)
				continue
			}

			fmt.Printf("Renamed %s to %s\n", oldDocFile, newDocFile)
		}
	}

	if !foundMatches {
		return fmt.Errorf("no files found matching pattern %s.md or %s-*.md", oldKey, oldKey)
	}

	return nil
}

func replaceInFile(filename, oldKey, newKey string) error {
	// Read the file
	content, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("failed to read file %s: %w", filename, err)
	}

	originalContent := string(content)

	// Replace all occurrences using regex to match word boundaries
	// This prevents partial matches within other words
	pattern := regexp.QuoteMeta(oldKey)
	re := regexp.MustCompile(`\b` + pattern + `\b`)
	newContent := re.ReplaceAllString(originalContent, newKey)

	// Check if any changes were made
	if originalContent == newContent {
		fmt.Printf("No changes needed in %s\n", filename)
		return fmt.Errorf("no changes made")
	}

	// Write the updated content back to the file
	if err := os.WriteFile(filename, []byte(newContent), 0o600); err != nil {
		return fmt.Errorf("failed to write file %s: %w", filename, err)
	}

	fmt.Printf("Updated %s\n", filename)
	return nil
}
