// Package detector provides directory scanning and traversal functionality.
package detector

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// ScanOptions configures the directory scanning behavior.
type ScanOptions struct {
	// MaxDepth limits how deep to traverse (0 = unlimited)
	MaxDepth int
	// FollowSymlinks determines if symbolic links should be followed
	FollowSymlinks bool
	// IgnorePatterns contains glob patterns for files/directories to ignore
	IgnorePatterns []string
	// IncludeHidden determines if hidden files (starting with .) should be included
	IncludeHidden bool
}

// DefaultScanOptions returns sensible default scanning options.
func DefaultScanOptions() ScanOptions {
	return ScanOptions{
		MaxDepth:       5, // Reasonable depth for most projects
		FollowSymlinks: false,
		IgnorePatterns: []string{
			"node_modules",
			".git",
			"vendor",
			"__pycache__",
			"*.pyc",
			".DS_Store",
			"bazel-*",
			".bazel",
			"target", // Rust/Java build output
			"dist",
			"build",
			".vscode",
			".idea",
			"*.tmp",
			"*.temp",
		},
		IncludeHidden: false,
	}
}

// Scanner handles directory traversal and file discovery.
type Scanner struct {
	options ScanOptions
}

// NewScanner creates a new scanner with the given options.
func NewScanner(options ScanOptions) *Scanner {
	return &Scanner{
		options: options,
	}
}

// ScanResult contains the results of a directory scan.
type ScanResult struct {
	// Files contains all discovered files with their relative paths
	Files []string
	// Directories contains all discovered directories with their relative paths
	Directories []string
	// Root is the root path that was scanned
	Root string
	// Errors contains any errors encountered during scanning
	Errors []error
}

// Scan traverses the given root path and returns all discovered files and directories.
func (s *Scanner) Scan(rootPath string) (*ScanResult, error) {
	// Ensure the root path exists and is a directory
	info, err := os.Stat(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to stat root path %s: %w", rootPath, err)
	}

	if !info.IsDir() {
		return nil, fmt.Errorf("root path %s is not a directory", rootPath)
	}

	// Resolve to absolute path for consistency
	absRoot, err := filepath.Abs(rootPath)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve absolute path for %s: %w", rootPath, err)
	}

	result := &ScanResult{
		Files:       []string{},
		Directories: []string{},
		Root:        absRoot,
		Errors:      []error{},
	}

	// Perform the traversal
	err = s.walkDirectory(absRoot, "", 0, result)
	if err != nil {
		return result, fmt.Errorf("failed to walk directory %s: %w", absRoot, err)
	}

	return result, nil
}

// walkDirectory recursively walks through directories, respecting the scan options.
func (s *Scanner) walkDirectory(absRoot, relPath string, depth int, result *ScanResult) error {
	// Check depth limit
	if s.options.MaxDepth > 0 && depth > s.options.MaxDepth {
		return nil
	}

	currentPath := filepath.Join(absRoot, relPath)

	// Read directory contents
	entries, err := os.ReadDir(currentPath)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Errorf("failed to read directory %s: %w", currentPath, err))
		return nil // Continue with other directories
	}

	for _, entry := range entries {
		name := entry.Name()
		entryRelPath := filepath.Join(relPath, name)
		entryAbsPath := filepath.Join(absRoot, entryRelPath)

		// Skip hidden files if not included
		if !s.options.IncludeHidden && strings.HasPrefix(name, ".") {
			continue
		}

		// Check ignore patterns
		if s.shouldIgnore(name, entryRelPath) {
			continue
		}

		// Handle symbolic links
		if entry.Type()&os.ModeSymlink != 0 {
			if !s.options.FollowSymlinks {
				continue
			}

			// Resolve symlink and check if it's valid
			target, err := os.Readlink(entryAbsPath)
			if err != nil {
				result.Errors = append(result.Errors, fmt.Errorf("failed to read symlink %s: %w", entryAbsPath, err))
				continue
			}

			// Make sure we don't follow symlinks that point outside the root
			if !filepath.IsAbs(target) {
				target = filepath.Join(filepath.Dir(entryAbsPath), target)
			}

			cleanTarget, err := filepath.EvalSymlinks(target)
			if err != nil {
				result.Errors = append(result.Errors, fmt.Errorf("failed to evaluate symlink target %s: %w", target, err))
				continue
			}

			// Check if target is within root (prevent infinite loops)
			if !strings.HasPrefix(cleanTarget, absRoot) {
				continue
			}
		}

		if entry.IsDir() {
			// Add directory to results
			result.Directories = append(result.Directories, entryRelPath)

			// Recursively scan subdirectory
			err := s.walkDirectory(absRoot, entryRelPath, depth+1, result)
			if err != nil {
				result.Errors = append(result.Errors, err)
			}
		} else {
			// Add file to results
			result.Files = append(result.Files, entryRelPath)
		}
	}

	return nil
}

// shouldIgnore checks if a file or directory should be ignored based on patterns.
func (s *Scanner) shouldIgnore(name, relPath string) bool {
	for _, pattern := range s.options.IgnorePatterns {
		// Check against filename
		if matched, _ := filepath.Match(pattern, name); matched {
			return true
		}

		// Check against relative path
		if matched, _ := filepath.Match(pattern, relPath); matched {
			return true
		}

		// Check if any part of the path matches the pattern
		pathParts := strings.Split(relPath, string(filepath.Separator))
		for _, part := range pathParts {
			if matched, _ := filepath.Match(pattern, part); matched {
				return true
			}
		}
	}

	return false
}

// GetFilesByExtension filters the scan results to return only files with specific extensions.
func (sr *ScanResult) GetFilesByExtension(extensions ...string) []string {
	var filtered []string

	extMap := make(map[string]bool)
	for _, ext := range extensions {
		// Ensure extension starts with a dot
		if !strings.HasPrefix(ext, ".") {
			ext = "." + ext
		}
		extMap[strings.ToLower(ext)] = true
	}

	for _, file := range sr.Files {
		ext := strings.ToLower(filepath.Ext(file))
		if extMap[ext] {
			filtered = append(filtered, file)
		}
	}

	return filtered
}

// GetFilesByPattern filters the scan results to return only files matching specific patterns.
func (sr *ScanResult) GetFilesByPattern(patterns ...string) []string {
	var filtered []string

	for _, file := range sr.Files {
		filename := filepath.Base(file)
		for _, pattern := range patterns {
			if matched, _ := filepath.Match(pattern, filename); matched {
				filtered = append(filtered, file)
				break
			}
		}
	}

	return filtered
}

// HasFile checks if a specific file exists in the scan results.
func (sr *ScanResult) HasFile(filename string) bool {
	for _, file := range sr.Files {
		if filepath.Base(file) == filename {
			return true
		}
	}
	return false
}

// HasPattern checks if any files match the given pattern.
func (sr *ScanResult) HasPattern(pattern string) bool {
	for _, file := range sr.Files {
		if matched, _ := filepath.Match(pattern, filepath.Base(file)); matched {
			return true
		}
	}
	return false
}
