// Package main provides a tool to fix markdown links by resolving them to
// local files or absolute URLs.
//
// The tool searches for a .envrc file (direnv format) in the directory of the
// file being processed and parent directories. Environment variables override
// .envrc values.
//
// Configuration (via .envrc file or environment variables):
//   - MDLINK_BASE_URL: Base URL for external links (e.g., "https://code.claude.com/docs")
//   - MDLINK_LOCAL_PREFIX: Prefix in links that maps to the local directory (e.g., "/en")
//   - MDLINK_SUFFIX_DROP: Suffix to remove from the link path (default: "")
//   - MDLINK_SUFFIX_ADD: Suffix to add for local file check (default: ".md")
//
// Usage:
//
//	mdlinkfix file.md
//
// Or with environment variables (override .env):
//
//	MDLINK_BASE_URL="https://example.com" mdlinkfix file.md
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/jaeyeom/experimental/text/markdown/linkfix"
)

var (
	dryRun   = flag.Bool("dry-run", false, "Print changes without modifying files")
	verbose  = flag.Bool("verbose", false, "Print verbose output")
	validate = flag.Bool("validate", false, "Validate local links and report broken ones")
)

// loadConfig loads configuration from .envrc file and environment variables.
// Environment variables take precedence over .envrc file values.
func loadConfig(startDir string) (linkfix.Config, string) {
	// Load from .envrc file first
	cfg, envPath := linkfix.LoadConfigFromEnvrc(startDir)

	// Environment variables override .envrc file values
	if v := os.Getenv("MDLINK_BASE_URL"); v != "" {
		cfg.BaseURL = v
	}
	if v := os.Getenv("MDLINK_LOCAL_PREFIX"); v != "" {
		cfg.LocalPrefix = v
	}
	if v := os.Getenv("MDLINK_SUFFIX_DROP"); v != "" {
		cfg.SuffixDrop = v
	}
	if v := os.Getenv("MDLINK_SUFFIX_ADD"); v != "" {
		cfg.SuffixAdd = v
	}

	return cfg, envPath
}

// ProcessFile processes a single markdown file.
// It loads configuration from .envrc file in the file's directory tree.
// Returns the list of broken links found (if validation is enabled).
func ProcessFile(filePath string) ([]linkfix.BrokenLink, error) {
	// Get original file info to preserve permissions
	fileInfo, err := os.Stat(filePath)
	if err != nil {
		return nil, fmt.Errorf("stat file: %w", err)
	}

	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("reading file: %w", err)
	}

	fileDir := filepath.Dir(filePath)

	// Load config based on file's directory (searches up for .envrc)
	cfg, envPath := loadConfig(fileDir)

	if *verbose && envPath != "" {
		fmt.Printf("Loading config from: %s\n", envPath)
	}

	if *verbose {
		fmt.Printf("Config: BaseURL=%q, LocalPrefix=%q, SuffixDrop=%q, SuffixAdd=%q\n",
			cfg.BaseURL, cfg.LocalPrefix, cfg.SuffixDrop, cfg.SuffixAdd)
	}

	newContent, changes := linkfix.ProcessContent(cfg, string(content), fileDir)

	// Print changes if verbose or dry-run
	if (*verbose || *dryRun) && len(changes) > 0 {
		for _, c := range changes {
			fmt.Printf("  %s -> %s\n", c.OldPath, c.NewPath)
		}
	}

	// If validate mode, check for broken links after processing
	var brokenLinks []linkfix.BrokenLink
	if *validate {
		brokenLinks = linkfix.FindBrokenLinks(cfg, newContent, fileDir, filePath)
	}

	if len(changes) == 0 {
		if *verbose {
			fmt.Printf("%s: no changes\n", filePath)
		}
		return brokenLinks, nil
	}

	if *dryRun {
		fmt.Printf("%s: %d link(s) would be changed\n", filePath, len(changes))
		return brokenLinks, nil
	}

	// #nosec G306 -- preserve original file permissions for documentation files
	if err := os.WriteFile(filePath, []byte(newContent), fileInfo.Mode().Perm()); err != nil {
		return brokenLinks, fmt.Errorf("writing file: %w", err)
	}

	fmt.Printf("%s: %d link(s) changed\n", filePath, len(changes))
	return brokenLinks, nil
}

func main() {
	flag.Parse()

	if flag.NArg() == 0 {
		fmt.Fprintln(os.Stderr, "Usage: mdlinkfix [options] <file.md> [file2.md ...]")
		fmt.Fprintln(os.Stderr, "\nConfiguration is loaded from .envrc file in the file's directory or parents.")
		fmt.Fprintln(os.Stderr, "Environment variables override .envrc values.")
		fmt.Fprintln(os.Stderr, "\nConfiguration variables:")
		fmt.Fprintln(os.Stderr, "  MDLINK_BASE_URL     Base URL for external links")
		fmt.Fprintln(os.Stderr, "  MDLINK_LOCAL_PREFIX Prefix in links that maps to local directory")
		fmt.Fprintln(os.Stderr, "  MDLINK_SUFFIX_DROP  Suffix to remove from link path")
		fmt.Fprintln(os.Stderr, "  MDLINK_SUFFIX_ADD   Suffix to add for local file check (default: .md)")
		fmt.Fprintln(os.Stderr, "\nOptions:")
		flag.PrintDefaults()
		os.Exit(1)
	}

	exitCode := 0
	var allBrokenLinks []linkfix.BrokenLink

	for _, filePath := range flag.Args() {
		brokenLinks, err := ProcessFile(filePath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error processing %s: %v\n", filePath, err)
			exitCode = 1
		}
		allBrokenLinks = append(allBrokenLinks, brokenLinks...)
	}

	// Report broken links if validation is enabled
	if *validate && len(allBrokenLinks) > 0 {
		fmt.Fprintln(os.Stderr, "\nBroken links found:")
		for _, bl := range allBrokenLinks {
			fmt.Fprintf(os.Stderr, "  %s:%d: [%s](%s)\n", bl.FilePath, bl.Line, bl.Text, bl.Path)
		}
		fmt.Fprintf(os.Stderr, "\nTotal: %d broken link(s)\n", len(allBrokenLinks))
		exitCode = 1
	}

	os.Exit(exitCode)
}
