package cache

import (
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Cache manages caching of Bazel query results.
type Cache struct {
	dir   string
	debug bool
}

// NewCache creates a new cache instance.
func NewCache(dir string, debug bool) *Cache {
	if dir == "" {
		homeDir, _ := os.UserHomeDir()
		dir = filepath.Join(homeDir, ".cache", "bazel-affected-tests")
	}
	return &Cache{dir: dir, debug: debug}
}

func (c *Cache) debugf(format string, args ...interface{}) {
	if c.debug {
		fmt.Printf("DEBUG: "+format+"\n", args...)
	}
}

// GetCacheKey computes a cache key based on BUILD files.
func (c *Cache) GetCacheKey() (string, error) {
	// Find all BUILD, WORKSPACE, and MODULE files
	var buildFiles []string
	err := filepath.Walk(".", func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // Skip files we can't access
		}
		if info.IsDir() {
			return nil
		}
		name := info.Name()
		if name == "BUILD" || name == "BUILD.bazel" ||
			name == "WORKSPACE" || name == "WORKSPACE.bazel" ||
			name == "MODULE" || name == "MODULE.bazel" {
			buildFiles = append(buildFiles, path)
		}
		return nil
	})
	if err != nil {
		return "", fmt.Errorf("failed to walk directory: %w", err)
	}

	// Sort files for consistent hashing
	sort.Strings(buildFiles)

	// Compute hash of all BUILD files
	h := sha256.New()
	for _, file := range buildFiles {
		// Include file path in hash
		fmt.Fprintf(h, "%s\n", file)

		// Include file content in hash
		f, err := os.Open(file)
		if err != nil {
			continue // Skip files we can't read
		}
		_, _ = io.Copy(h, f)
		f.Close()
	}

	return fmt.Sprintf("%x", h.Sum(nil)), nil
}

// Get retrieves cached results for a package.
func (c *Cache) Get(cacheKey, pkg string) ([]string, bool) {
	cacheFile := c.getCacheFile(cacheKey, pkg)
	data, err := os.ReadFile(cacheFile)
	if err != nil {
		return nil, false
	}

	var tests []string
	if err := json.Unmarshal(data, &tests); err != nil {
		c.debugf("Failed to unmarshal cache for %s: %v", pkg, err)
		return nil, false
	}

	c.debugf("Cache hit for package %s", pkg)
	return tests, true
}

// Set stores results in cache for a package.
func (c *Cache) Set(cacheKey, pkg string, tests []string) error {
	// Create cache directory if it doesn't exist
	cacheDir := filepath.Join(c.dir, cacheKey)
	if err := os.MkdirAll(cacheDir, 0o755); err != nil {
		return fmt.Errorf("failed to create cache directory: %w", err)
	}

	cacheFile := c.getCacheFile(cacheKey, pkg)
	data, err := json.Marshal(tests)
	if err != nil {
		return fmt.Errorf("failed to marshal tests: %w", err)
	}

	if err := os.WriteFile(cacheFile, data, 0o600); err != nil {
		return fmt.Errorf("failed to write cache file: %w", err)
	}
	return nil
}

// Clear removes all cached data.
func (c *Cache) Clear() error {
	if err := os.RemoveAll(c.dir); err != nil {
		return fmt.Errorf("failed to clear cache: %w", err)
	}
	return nil
}

// getCacheFile returns the cache file path for a package.
func (c *Cache) getCacheFile(cacheKey, pkg string) string {
	// Replace // and : with safe characters for filenames
	safePkg := strings.ReplaceAll(pkg, "//", "")
	safePkg = strings.ReplaceAll(safePkg, "/", "__")
	safePkg = strings.ReplaceAll(safePkg, ":", "__")
	if safePkg == "" {
		safePkg = "root"
	}
	return filepath.Join(c.dir, cacheKey, safePkg+".json")
}
