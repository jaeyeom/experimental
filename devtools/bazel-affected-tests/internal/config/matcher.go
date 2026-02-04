package config

import (
	"path/filepath"
	"strings"
)

// MatchPattern matches a file path against a glob pattern.
// Supports:
// - ** recursive matching (e.g., **/BUILD matches foo/bar/BUILD)
// - * single-level matching (e.g., *.bzl matches defs.bzl)
// - exact matches (e.g., WORKSPACE matches only WORKSPACE).
func MatchPattern(pattern, file string) bool {
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
