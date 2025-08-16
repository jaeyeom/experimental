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

// Owner represents a code owner that can be a user, team, or email address.
type Owner interface {
	String() string
}

// User represents a GitHub user owner (e.g., @username).
type User struct {
	// GitHub username
	Name string
}

// String returns the CODEOWNERS format for a user (@username).
func (u User) String() string {
	return "@" + u.Name
}

// Team represents a GitHub team owner (e.g., @org/team).
type Team struct {
	Org  string // GitHub organization name
	Name string // Team name within the organization
}

// String returns the CODEOWNERS format for a team (@org/team).
func (t Team) String() string {
	return "@" + t.Org + "/" + t.Name
}

// Email represents an email address owner (e.g., user@example.com).
type Email struct {
	// Email address
	Address string
}

// String returns the CODEOWNERS format for an email (user@example.com).
func (e Email) String() string {
	return e.Address
}

// ParseOwner parses an owner string from CODEOWNERS format into the appropriate Owner type.
// It handles three formats:
//   - @username -> User{Name: "username"}
//   - @org/team -> Team{Org: "org", Name: "team"}
//   - user@example.com -> Email{Address: "user@example.com"}
func ParseOwner(s string) Owner {
	s = strings.TrimSpace(s)

	// Handle email addresses (contains @ but doesn't start with @)
	if strings.Contains(s, "@") && !strings.HasPrefix(s, "@") {
		return Email{Address: s}
	}

	// Handle GitHub users and teams (starts with @)
	if strings.HasPrefix(s, "@") {
		ownerPart := s[1:] // Remove the @ prefix

		// Check if it's a team (contains /)
		if strings.Contains(ownerPart, "/") {
			parts := strings.SplitN(ownerPart, "/", 2)
			if len(parts) == 2 {
				return Team{Org: parts[0], Name: parts[1]}
			}
		}

		// It's a user
		return User{Name: ownerPart}
	}

	// Fallback: treat as user without @ prefix
	return User{Name: s}
}

// Rule represents a single CODEOWNERS rule that maps a file pattern to a list
// of owners.
type Rule struct {
	// File pattern (supports glob syntax including **)
	Pattern string

	// List of owners (users, teams, or emails) for files matching the pattern
	Owners []Owner
}

// Section represents a group of CODEOWNERS rules, typically separated by ##
// comments. Each section can have different rules that are processed
// independently.
type Section struct {
	// List of rules in this section
	Rules []Rule
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
		ownerStrings := fields[1:]

		// Parse owner strings into Owner types
		owners := make([]Owner, len(ownerStrings))
		for i, ownerStr := range ownerStrings {
			owners[i] = ParseOwner(ownerStr)
		}

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
func (c *Codeowners) OwnersFor(file string) []Owner {
	ownerSet := make(map[string]Owner)
	for _, section := range c.Sections {
		var lastOwners []Owner
		for _, rule := range section.Rules {
			if matchPattern(rule.Pattern, file) {
				lastOwners = rule.Owners // Always overwrite, so only the last match in the section is kept
			}
		}
		for _, o := range lastOwners {
			ownerSet[o.String()] = o
		}
	}
	owners := make([]Owner, 0, len(ownerSet))
	for _, o := range ownerSet {
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

	// Handle recursive patterns with **
	if strings.Contains(pattern, "**") {
		return matchRecursivePattern(pattern, file)
	}

	// Handle simple patterns without **
	return matchSimplePattern(pattern, file)
}

// matchRecursivePattern handles patterns containing **.
func matchRecursivePattern(pattern, file string) bool {
	// Split pattern on ** to handle multiple ** segments
	parts := strings.Split(pattern, "**")

	// For patterns like **/suffix, prefix/**, or prefix/**/suffix
	if len(parts) == 2 {
		prefix := parts[0]
		suffix := parts[1]

		// Remove trailing / from prefix and leading / from suffix
		prefix = strings.TrimSuffix(prefix, "/")
		suffix = strings.TrimPrefix(suffix, "/")

		// Check prefix match
		if prefix != "" && !strings.HasPrefix(file, prefix+"/") && file != prefix {
			return false
		}

		// Check suffix match
		if suffix != "" {
			// If suffix starts with /, it's a path-based match
			if strings.Contains(suffix, "/") {
				return strings.HasSuffix(file, "/"+suffix) || strings.HasSuffix(file, suffix)
			}
			// Otherwise, it's a filename pattern match
			fileBase := filepath.Base(file)
			if matched, _ := filepath.Match(suffix, fileBase); matched {
				return true
			}
		} else {
			// Pattern ends with ** (like "dir/**")
			// Should match files under the directory, but not the directory itself
			return prefix == "" || strings.HasPrefix(file, prefix+"/")
		}
	}

	// Handle more complex patterns with multiple **
	if len(parts) > 2 {
		return matchComplexRecursivePattern(pattern, file)
	}

	return false
}

// matchComplexRecursivePattern handles patterns with multiple ** segments.
func matchComplexRecursivePattern(pattern, file string) bool {
	// For now, use a simplified approach for complex patterns
	// This could be improved with a more sophisticated algorithm
	parts := strings.Split(pattern, "**")

	currentFile := file
	for i, part := range parts {
		part = strings.Trim(part, "/")
		if part == "" {
			continue
		}

		if i == len(parts)-1 {
			// Last part - check if it matches the end
			if strings.Contains(part, "/") {
				return strings.HasSuffix(currentFile, part)
			}
			// Filename pattern
			fileBase := filepath.Base(currentFile)
			matched, _ := filepath.Match(part, fileBase)
			return matched
		}
		// Find this part somewhere in the remaining file path
		if idx := strings.Index(currentFile, part); idx >= 0 {
			currentFile = currentFile[idx+len(part):]
		} else {
			return false
		}
	}

	return true
}

// matchSimplePattern handles patterns without **.
func matchSimplePattern(pattern, file string) bool {
	// For patterns like *.go, they should only match files at the same directory level
	if strings.Contains(pattern, "*") || strings.Contains(pattern, "?") {
		// If pattern contains no /, it should only match the basename
		if !strings.Contains(pattern, "/") {
			// Only match if file is also at root level (no /)
			if strings.Contains(file, "/") {
				return false
			}
			matched, _ := filepath.Match(pattern, file)
			return matched
		}
		// Pattern contains /, use normal matching
		matched, _ := filepath.Match(pattern, file)
		return matched
	}

	// Exact string match (no wildcards)
	return pattern == file
}
