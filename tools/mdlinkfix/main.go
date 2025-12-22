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
	"bufio"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

var (
	// linkPattern matches markdown links: [text](url)
	// It captures the text, the URL, and any optional title.
	linkPattern = regexp.MustCompile(`\[([^\]]*)\]\(([^)\s]+)(?:\s+"[^"]*")?\)`)

	dryRun   = flag.Bool("dry-run", false, "Print changes without modifying files")
	verbose  = flag.Bool("verbose", false, "Print verbose output")
	validate = flag.Bool("validate", false, "Validate local links and report broken ones")
)

// BrokenLink represents a link that couldn't be resolved to an existing file.
type BrokenLink struct {
	Text     string // Link text
	Path     string // Link path
	Line     int    // Line number (1-indexed)
	FilePath string // File containing the broken link
}

// Config holds the configuration for link resolution.
type Config struct {
	BaseURL     string // Base URL for external links
	LocalPrefix string // Prefix in links that maps to local directory
	SuffixDrop  string // Suffix to remove from link path
	SuffixAdd   string // Suffix to add for local file check
	envDir      string // Directory where .env was found (for relative path resolution)
}

// findEnvFile searches for a .envrc file (direnv format) starting from startDir
// and walking up the directory tree. Returns the path to the .envrc file and
// the directory containing it, or empty strings if not found.
func findEnvFile(startDir string) (envPath, envDir string) {
	dir := startDir
	for {
		envPath := filepath.Join(dir, ".envrc")
		if _, err := os.Stat(envPath); err == nil {
			return envPath, dir
		}

		parent := filepath.Dir(dir)
		if parent == dir {
			// Reached root
			return "", ""
		}
		dir = parent
	}
}

// parseEnvFile reads a .envrc file and returns a map of key-value pairs.
// It supports simple KEY=value format (and export KEY=value for direnv compat).
// Lines starting with # are comments.
// Values can be optionally quoted with single or double quotes.
func parseEnvFile(path string) (map[string]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("opening env file: %w", err)
	}
	defer file.Close()

	env := make(map[string]string)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		// Skip empty lines and comments
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		// Handle direnv's "export KEY=value" format
		line = strings.TrimPrefix(line, "export ")

		// Split on first =
		idx := strings.Index(line, "=")
		if idx == -1 {
			continue
		}

		key := strings.TrimSpace(line[:idx])
		value := strings.TrimSpace(line[idx+1:])

		// Remove surrounding quotes if present
		if len(value) >= 2 {
			if (value[0] == '"' && value[len(value)-1] == '"') ||
				(value[0] == '\'' && value[len(value)-1] == '\'') {
				value = value[1 : len(value)-1]
			}
		}

		env[key] = value
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("scanning env file: %w", err)
	}

	return env, nil
}

// LoadConfig loads configuration from a .env file (if found) and environment
// variables. Environment variables take precedence over .env file values.
// The startDir parameter specifies where to start searching for .env files.
func LoadConfig(startDir string) Config {
	cfg := Config{}

	// Try to find and load .env file
	if envPath, envDir := findEnvFile(startDir); envPath != "" {
		if *verbose {
			fmt.Printf("Loading config from: %s\n", envPath)
		}
		cfg.envDir = envDir
		if envVars, err := parseEnvFile(envPath); err == nil {
			cfg.BaseURL = envVars["MDLINK_BASE_URL"]
			cfg.LocalPrefix = envVars["MDLINK_LOCAL_PREFIX"]
			cfg.SuffixDrop = envVars["MDLINK_SUFFIX_DROP"]
			cfg.SuffixAdd = envVars["MDLINK_SUFFIX_ADD"]
		}
	}

	// Environment variables override .env file values
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

	// Set defaults
	if cfg.SuffixAdd == "" {
		cfg.SuffixAdd = ".md"
	}

	return cfg
}

// tryURLToLocal attempts to convert a full URL back to a local file path.
// It strips the BaseURL prefix and checks if the resulting path exists locally.
// If the local file exists, it returns the local path; otherwise, it returns the original URL.
func tryURLToLocal(cfg Config, linkPath, fileDir, baseDir string) (string, bool) {
	// Strip the base URL to get the path portion
	urlPath := strings.TrimPrefix(linkPath, cfg.BaseURL)

	// Handle anchor fragments in the URL
	anchor := ""
	if idx := strings.Index(urlPath, "#"); idx != -1 {
		anchor = urlPath[idx:]
		urlPath = urlPath[:idx]
	}

	// Check if the path has our local prefix
	if cfg.LocalPrefix != "" && strings.HasPrefix(urlPath, cfg.LocalPrefix) {
		// Remove the prefix to get the relative path
		relativePath := strings.TrimPrefix(urlPath, cfg.LocalPrefix)
		relativePath = strings.TrimPrefix(relativePath, "/")

		// Apply suffix transformations
		if cfg.SuffixDrop != "" && strings.HasSuffix(relativePath, cfg.SuffixDrop) {
			relativePath = strings.TrimSuffix(relativePath, cfg.SuffixDrop)
		}

		localPath := relativePath + cfg.SuffixAdd

		// Check if the local file exists relative to baseDir
		fullPath := filepath.Join(baseDir, localPath)
		if _, err := os.Stat(fullPath); err == nil {
			// Local file exists - convert URL to local path
			// Calculate relative path from fileDir to the local file
			if fileDir != baseDir {
				relPath, err := filepath.Rel(fileDir, fullPath)
				if err == nil {
					localPath = relPath
				}
			}
			return localPath + anchor, true
		}
	}

	// Local file doesn't exist, keep the original URL
	return linkPath, false
}

// tryLocalPrefixResolution attempts to resolve a link that starts with the local prefix.
// Returns the resolved path, whether it was changed, and whether resolution was attempted.
func tryLocalPrefixResolution(cfg Config, linkPath, fileDir, baseDir string) (string, bool, bool) {
	if cfg.LocalPrefix == "" || !strings.HasPrefix(linkPath, cfg.LocalPrefix) {
		return "", false, false
	}

	// Remove the prefix to get the relative path
	relativePath := strings.TrimPrefix(linkPath, cfg.LocalPrefix)
	relativePath = strings.TrimPrefix(relativePath, "/")

	// Apply suffix transformations
	if cfg.SuffixDrop != "" && strings.HasSuffix(relativePath, cfg.SuffixDrop) {
		relativePath = strings.TrimSuffix(relativePath, cfg.SuffixDrop)
	}

	localPath := relativePath + cfg.SuffixAdd

	// Check if the local file exists relative to baseDir
	fullPath := filepath.Join(baseDir, localPath)
	if _, err := os.Stat(fullPath); err == nil {
		// Local file exists - calculate relative path from fileDir
		if fileDir != baseDir {
			if relPath, err := filepath.Rel(fileDir, fullPath); err == nil {
				localPath = relPath
			}
		}
		return localPath, linkPath != localPath, true
	}

	// Local file doesn't exist, fall back to URL if base URL is set
	if cfg.BaseURL != "" {
		newURL := cfg.BaseURL + linkPath
		return newURL, linkPath != newURL, true
	}

	return linkPath, false, true
}

// ResolveLink resolves a link to either a local file path or an absolute URL.
// It returns the resolved link and whether it was changed.
// The fileDir parameter is the directory of the file being processed.
// Local files are searched relative to cfg.envDir (if set) or fileDir.
func ResolveLink(cfg Config, linkPath, fileDir string) (string, bool) {
	// Skip anchor-only links
	if strings.HasPrefix(linkPath, "#") {
		return linkPath, false
	}

	// Skip mailto links
	if strings.HasPrefix(linkPath, "mailto:") {
		return linkPath, false
	}

	// Determine the base directory for local file lookup
	baseDir := fileDir
	if cfg.envDir != "" {
		baseDir = cfg.envDir
	}

	// Try to convert full URLs back to local links if they match the base URL
	if cfg.BaseURL != "" && strings.HasPrefix(linkPath, cfg.BaseURL) {
		return tryURLToLocal(cfg, linkPath, fileDir, baseDir)
	}

	// Skip other absolute URLs (http://, https://, etc.)
	if strings.Contains(linkPath, "://") {
		return linkPath, false
	}

	// Check if this link starts with the local prefix
	if result, changed, handled := tryLocalPrefixResolution(cfg, linkPath, fileDir, baseDir); handled {
		return result, changed
	}

	// If no local prefix match but we have a base URL for absolute paths
	if strings.HasPrefix(linkPath, "/") && cfg.BaseURL != "" {
		newURL := cfg.BaseURL + linkPath
		return newURL, linkPath != newURL
	}

	return linkPath, false
}

// ValidateLocalLink checks if a local link (relative path) exists.
// Returns true if the link is valid (file exists or it's not a local file link).
func ValidateLocalLink(cfg Config, linkPath, fileDir string) bool {
	// Skip anchor-only links (always valid)
	if strings.HasPrefix(linkPath, "#") {
		return true
	}

	// Skip mailto links (always valid)
	if strings.HasPrefix(linkPath, "mailto:") {
		return true
	}

	// Skip absolute URLs (not local links)
	if strings.Contains(linkPath, "://") {
		return true
	}

	// Handle anchor fragments
	pathWithoutAnchor := linkPath
	if idx := strings.Index(linkPath, "#"); idx != -1 {
		pathWithoutAnchor = linkPath[:idx]
	}

	// Skip empty paths (anchor-only after stripping)
	if pathWithoutAnchor == "" {
		return true
	}

	// Determine the base directory for local file lookup
	baseDir := fileDir
	if cfg.envDir != "" {
		baseDir = cfg.envDir
	}

	// For paths starting with the local prefix, check if file exists
	if cfg.LocalPrefix != "" && strings.HasPrefix(pathWithoutAnchor, cfg.LocalPrefix) {
		relativePath := strings.TrimPrefix(pathWithoutAnchor, cfg.LocalPrefix)
		relativePath = strings.TrimPrefix(relativePath, "/")

		if cfg.SuffixDrop != "" && strings.HasSuffix(relativePath, cfg.SuffixDrop) {
			relativePath = strings.TrimSuffix(relativePath, cfg.SuffixDrop)
		}

		localPath := relativePath + cfg.SuffixAdd
		fullPath := filepath.Join(baseDir, localPath)
		_, err := os.Stat(fullPath)
		return err == nil
	}

	// For relative paths (not starting with /), check relative to fileDir
	if !strings.HasPrefix(pathWithoutAnchor, "/") {
		fullPath := filepath.Join(fileDir, pathWithoutAnchor)
		_, err := os.Stat(fullPath)
		return err == nil
	}

	// For absolute paths starting with / but not matching local prefix,
	// we can't validate them without more context
	return true
}

// FindBrokenLinks finds all broken local links in the content.
func FindBrokenLinks(cfg Config, content, fileDir, filePath string) []BrokenLink {
	var broken []BrokenLink
	lines := strings.Split(content, "\n")

	for lineNum, line := range lines {
		matches := linkPattern.FindAllStringSubmatch(line, -1)
		for _, match := range matches {
			if len(match) < 3 {
				continue
			}
			text := match[1]
			linkPath := match[2]

			if !ValidateLocalLink(cfg, linkPath, fileDir) {
				broken = append(broken, BrokenLink{
					Text:     text,
					Path:     linkPath,
					Line:     lineNum + 1, // 1-indexed
					FilePath: filePath,
				})
			}
		}
	}

	return broken
}

// ProcessContent processes markdown content and resolves all links.
func ProcessContent(cfg Config, content, fileDir string) (string, int) {
	changesCount := 0

	result := linkPattern.ReplaceAllStringFunc(content, func(match string) string {
		submatches := linkPattern.FindStringSubmatch(match)
		if len(submatches) < 3 {
			return match
		}

		text := submatches[1]
		linkPath := submatches[2]

		newPath, changed := ResolveLink(cfg, linkPath, fileDir)
		if changed {
			changesCount++
			return fmt.Sprintf("[%s](%s)", text, newPath)
		}

		return match
	})

	return result, changesCount
}

// ProcessFile processes a single markdown file.
// It loads configuration from .env file in the file's directory tree.
// Returns the list of broken links found (if validation is enabled).
func ProcessFile(filePath string) ([]BrokenLink, error) {
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

	// Load config based on file's directory (searches up for .env)
	cfg := LoadConfig(fileDir)

	if *verbose {
		fmt.Printf("Config: BaseURL=%q, LocalPrefix=%q, SuffixDrop=%q, SuffixAdd=%q\n",
			cfg.BaseURL, cfg.LocalPrefix, cfg.SuffixDrop, cfg.SuffixAdd)
	}

	newContent, changesCount := ProcessContent(cfg, string(content), fileDir)

	// If validate mode, check for broken links after processing
	var brokenLinks []BrokenLink
	if *validate {
		brokenLinks = FindBrokenLinks(cfg, newContent, fileDir, filePath)
	}

	if changesCount == 0 {
		if *verbose {
			fmt.Printf("%s: no changes\n", filePath)
		}
		return brokenLinks, nil
	}

	if *dryRun {
		fmt.Printf("%s: %d link(s) would be changed\n", filePath, changesCount)
		if *verbose {
			fmt.Println("--- New content ---")
			fmt.Println(newContent)
			fmt.Println("--- End ---")
		}
		return brokenLinks, nil
	}

	// #nosec G306 -- preserve original file permissions for documentation files
	if err := os.WriteFile(filePath, []byte(newContent), fileInfo.Mode().Perm()); err != nil {
		return brokenLinks, fmt.Errorf("writing file: %w", err)
	}

	fmt.Printf("%s: %d link(s) changed\n", filePath, changesCount)
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
	var allBrokenLinks []BrokenLink

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
