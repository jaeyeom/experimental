// Package codeowners provides functionality for parsing and working with GitHub
// CODEOWNERS files. It supports section-based parsing and owner resolution for
// files based on glob patterns.
package codeowners

import (
	"bufio"
	"io"
	"path/filepath"
	"strings"
)

// Rule represents a single CODEOWNERS rule that maps a file pattern to a list
// of owners.
type Rule struct {
	// File pattern (supports glob syntax including **)
	Pattern string

	// List of owners (users or teams) for files matching the pattern
	Owners []string
}

// Section represents a group of CODEOWNERS rules, typically separated by ##
// comments. Each section can have different rules that are processed
// independently.
type Section struct {
	Rules []Rule // List of rules in this section
}

// Codeowners represents a parsed CODEOWNERS file with support for multiple
// sections. It provides methods to find owners for specific files based on the
// defined rules.
type Codeowners struct {
	// List of sections containing ownership rules
	Sections []Section
}

// ParseSections parses a CODEOWNERS file with section-based logic. It reads
// from the provided io.Reader and returns a Codeowners structure. Sections are
// separated by lines starting with ## (double hash comments). Empty lines and
// single # comments are ignored. Each rule line should contain a file pattern
// followed by one or more owners.
func ParseSections(r io.Reader) *Codeowners {
	scanner := bufio.NewScanner(r)
	var sections []Section
	var current Section
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			if strings.HasPrefix(line, "##") {
				// Section boundary
				if len(current.Rules) > 0 {
					sections = append(sections, current)
					current = Section{}
				}
			}
			continue
		}
		fields := strings.Fields(line)
		if len(fields) < 2 {
			continue
		}
		pattern := fields[0]
		owners := fields[1:]
		current.Rules = append(current.Rules, Rule{Pattern: pattern, Owners: owners})
	}
	if len(current.Rules) > 0 {
		sections = append(sections, current)
	}
	return &Codeowners{Sections: sections}
}

// OwnersFor returns the union of the last-matching owners from each section for
// the given file. For each section, it finds the last rule that matches the
// file and includes those owners. The result is a deduplicated list of all
// owners from the last-matching rule in each section. If no rules match the
// file, an empty slice is returned.
func (c *Codeowners) OwnersFor(file string) []string {
	ownerSet := make(map[string]struct{})
	for _, section := range c.Sections {
		var lastOwners []string
		for _, rule := range section.Rules {
			if matchPattern(rule.Pattern, file) {
				lastOwners = rule.Owners // Always overwrite, so only the last match in the section is kept
			}
		}
		for _, o := range lastOwners {
			ownerSet[o] = struct{}{}
		}
	}
	owners := make([]string, 0, len(ownerSet))
	for o := range ownerSet {
		owners = append(owners, o)
	}
	return owners
}

// matchPattern matches CODEOWNERS-style globs, including ** for any directory
// depth and **/*.ext for extension matches at any depth.
//
// Supported patterns:
//   - Exact matches: "path/to/file.go"
//   - Extension matches: "*.go" (root only), "**/*.go" (any depth)
//   - Directory matches: "path/**" (any files under path)
//   - Complex patterns: "path/**/subdir/*.ext"
//
// The function normalizes paths using forward slashes for cross-platform
// compatibility.
func matchPattern(pattern, file string) bool {
	pattern = filepath.ToSlash(strings.TrimPrefix(pattern, "/"))
	file = filepath.ToSlash(strings.TrimPrefix(file, "/"))

	// Exact match
	if pattern == file {
		return true
	}

	// Special case for patterns like myorg/api/**/*.proto
	if strings.Contains(pattern, "/**/") {
		parts := strings.Split(pattern, "/**/")
		if len(parts) == 2 && strings.HasPrefix(file, parts[0]) {
			// Check if the suffix part matches the file extension
			if strings.HasPrefix(parts[1], "*.") {
				ext := parts[1][1:] // Get the extension including the dot
				return strings.HasSuffix(file, ext)
			}
			// For other patterns, check if the file ends with the suffix
			return strings.HasSuffix(file, parts[1])
		}
	}

	// Handle patterns with '**' anywhere (e.g., myorg/api/**/*.proto)
	if strings.Contains(pattern, "**") {
		parts := strings.Split(pattern, "**")
		if len(parts) == 2 {
			prefix := parts[0]
			suffix := parts[1]
			// Check if file starts with prefix and ends with suffix
			if strings.HasPrefix(file, prefix) && strings.HasSuffix(file, suffix) {
				return true
			}
		}
	}

	// Handle patterns like **/*.go or **/foo.go
	if strings.HasPrefix(pattern, "**/") {
		sub := pattern[3:]
		if ok, _ := filepath.Match(sub, filepath.Base(file)); ok {
			return true
		}
		if ok, _ := filepath.Match(sub, file); ok {
			return true
		}
	}

	// Handle patterns like *.go (matches files in root only)
	if strings.HasPrefix(pattern, "*.") {
		return filepath.Ext(file) == pattern[1:]
	}

	// Fallback to filepath.Match for single * and ?
	ok, _ := filepath.Match(pattern, file)
	return ok
}
