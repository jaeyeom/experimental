// Package linkfix provides functionality for fixing markdown links by resolving
// them to local files or absolute URLs.
//
// The package can resolve links based on configurable prefixes and suffixes.
package linkfix

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

var (
	// LinkPattern matches markdown links: [text](url)
	// It captures the text, the URL, and any optional title.
	LinkPattern = regexp.MustCompile(`\[([^\]]*)\]\(([^)\s]+)(?:\s+"[^"]*")?\)`)

	// codeBlockPattern matches fenced code block delimiters (``` or ~~~)
	// with optional language identifier. Captures the fence characters.
	codeBlockPattern = regexp.MustCompile("^\\s*(`{3,}|~{3,})")
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
	SuffixAdd   string // Suffix to add for local file check (default: ".md")
	BaseDir     string // Base directory for local file lookup (if different from file's directory)
}

// LinkChange represents a change made to a link during processing.
type LinkChange struct {
	OldPath string // Original link path
	NewPath string // New link path after resolution
	Line    int    // Line number (1-indexed)
}

// suffixAdd returns the suffix to add, defaulting to ".md" if empty.
func suffixAdd(cfg Config) string {
	if cfg.SuffixAdd == "" {
		return ".md"
	}
	return cfg.SuffixAdd
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

		localPath := relativePath + suffixAdd(cfg)

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

	localPath := relativePath + suffixAdd(cfg)

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
// Local files are searched relative to cfg.BaseDir (if set) or fileDir.
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
	if cfg.BaseDir != "" {
		baseDir = cfg.BaseDir
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
	if cfg.BaseDir != "" {
		baseDir = cfg.BaseDir
	}

	// For paths starting with the local prefix, check if file exists
	if cfg.LocalPrefix != "" && strings.HasPrefix(pathWithoutAnchor, cfg.LocalPrefix) {
		relativePath := strings.TrimPrefix(pathWithoutAnchor, cfg.LocalPrefix)
		relativePath = strings.TrimPrefix(relativePath, "/")

		if cfg.SuffixDrop != "" && strings.HasSuffix(relativePath, cfg.SuffixDrop) {
			relativePath = strings.TrimSuffix(relativePath, cfg.SuffixDrop)
		}

		localPath := relativePath + suffixAdd(cfg)
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

// isCodeBlockDelimiter checks if a line is a fenced code block delimiter.
// Returns true and the fence type (` or ~) if it's a delimiter, false otherwise.
func isCodeBlockDelimiter(line string) (bool, byte) {
	match := codeBlockPattern.FindStringSubmatch(line)
	if len(match) < 2 {
		return false, 0
	}
	return true, match[1][0]
}

// FindBrokenLinks finds all broken local links in the content.
// It skips links inside fenced code blocks (``` or ~~~).
func FindBrokenLinks(cfg Config, content, fileDir, filePath string) []BrokenLink {
	var broken []BrokenLink
	lines := strings.Split(content, "\n")

	inCodeBlock := false
	var codeBlockFence byte

	for lineNum, line := range lines {
		// Check for code block delimiter
		if isDelim, fenceChar := isCodeBlockDelimiter(line); isDelim {
			if !inCodeBlock {
				// Entering a code block
				inCodeBlock = true
				codeBlockFence = fenceChar
			} else if fenceChar == codeBlockFence {
				// Exiting the code block (same fence type)
				inCodeBlock = false
				codeBlockFence = 0
			}
			continue
		}

		// Skip lines inside code blocks
		if inCodeBlock {
			continue
		}

		matches := LinkPattern.FindAllStringSubmatch(line, -1)
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
// It skips links inside fenced code blocks (``` or ~~~).
// Returns the processed content and a list of changes made.
func ProcessContent(cfg Config, content, fileDir string) (string, []LinkChange) {
	var changes []LinkChange
	lines := strings.Split(content, "\n")

	inCodeBlock := false
	var codeBlockFence byte

	for i, line := range lines {
		lineNum := i + 1 // 1-indexed

		// Check for code block delimiter
		if isDelim, fenceChar := isCodeBlockDelimiter(line); isDelim {
			if !inCodeBlock {
				// Entering a code block
				inCodeBlock = true
				codeBlockFence = fenceChar
			} else if fenceChar == codeBlockFence {
				// Exiting the code block (same fence type)
				inCodeBlock = false
				codeBlockFence = 0
			}
			continue
		}

		// Skip lines inside code blocks
		if inCodeBlock {
			continue
		}

		// Process links in this line
		newLine := LinkPattern.ReplaceAllStringFunc(line, func(match string) string {
			submatches := LinkPattern.FindStringSubmatch(match)
			if len(submatches) < 3 {
				return match
			}

			text := submatches[1]
			linkPath := submatches[2]

			newPath, changed := ResolveLink(cfg, linkPath, fileDir)
			if changed {
				changes = append(changes, LinkChange{
					OldPath: linkPath,
					NewPath: newPath,
					Line:    lineNum,
				})
				return fmt.Sprintf("[%s](%s)", text, newPath)
			}

			return match
		})

		lines[i] = newLine
	}

	return strings.Join(lines, "\n"), changes
}
