package main

import (
	"flag"
	"fmt"
	"os"
	"sort"

	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/cache"
	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/git"
	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/query"
)

func main() {
	var (
		debug      = flag.Bool("debug", false, "Enable debug output")
		cacheDir   = flag.String("cache-dir", "", "Cache directory (default: $HOME/.cache/bazel-affected-tests)")
		clearCache = flag.Bool("clear-cache", false, "Clear the cache and exit")
		noCache    = flag.Bool("no-cache", false, "Disable caching")
	)
	flag.Parse()

	// Set debug from environment if not set via flag
	if !*debug && os.Getenv("DEBUG") != "" {
		*debug = true
	}

	// Initialize cache
	c := cache.NewCache(*cacheDir, *debug)

	// Handle cache clearing
	if *clearCache {
		if err := c.Clear(); err != nil {
			fmt.Fprintf(os.Stderr, "Error clearing cache: %v\n", err)
			os.Exit(1)
		}
		if *debug {
			fmt.Println("Cache cleared successfully")
		}
		os.Exit(0)
	}

	// Get staged files
	stagedFiles, err := git.GetStagedFiles()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting staged files: %v\n", err)
		os.Exit(1)
	}

	if len(stagedFiles) == 0 {
		// No staged files, no tests to run
		os.Exit(0)
	}

	debugf := func(format string, args ...interface{}) {
		if *debug {
			fmt.Printf("DEBUG: "+format+"\n", args...)
		}
	}

	debugf("Found %d staged files", len(stagedFiles))

	// Get cache key
	var cacheKey string
	if !*noCache {
		cacheKey, err = c.GetCacheKey()
		if err != nil {
			debugf("Failed to compute cache key: %v", err)
			*noCache = true
		} else {
			debugf("Cache key: %s", cacheKey)
		}
	}

	// Find packages for staged files
	packageMap := make(map[string]bool)
	for _, file := range stagedFiles {
		debugf("Processing file: %s", file)
		if pkg, found := query.FindBazelPackage(file); found {
			debugf("  Found package: %s", pkg)
			packageMap[pkg] = true
		} else {
			debugf("  No Bazel package found for this file")
		}
	}

	// Convert to slice
	var packages []string
	for pkg := range packageMap {
		packages = append(packages, pkg)
	}

	// Initialize querier
	querier := query.NewBazelQuerier(*debug)

	// Process packages and collect tests
	allTestsMap := make(map[string]bool)

	for _, pkg := range packages {
		var tests []string
		cacheHit := false

		// Try to get from cache
		if !*noCache {
			if cachedTests, found := c.Get(cacheKey, pkg); found {
				tests = cachedTests
				cacheHit = true
			}
		}

		// If not in cache, query Bazel
		if !cacheHit {
			pkgTests, err := querier.FindAffectedTests([]string{pkg})
			if err != nil {
				debugf("Error querying tests for package %s: %v", pkg, err)
				continue
			}
			tests = pkgTests

			// Store in cache
			if !*noCache {
				if err := c.Set(cacheKey, pkg, tests); err != nil {
					debugf("Failed to cache results for %s: %v", pkg, err)
				}
			}
		}

		// Add to all tests
		for _, test := range tests {
			allTestsMap[test] = true
		}
	}

	// Always check for format tests (will be filtered based on file types)
	formatTests, err := querier.FindAffectedTests([]string{"//tools/format"})
	if err == nil {
		for _, test := range formatTests {
			allTestsMap[test] = true
		}
	}

	// Convert to slice
	var allTests []string
	for test := range allTestsMap {
		allTests = append(allTests, test)
	}

	// Filter format tests based on file types
	filter := query.NewFormatTestFilter(stagedFiles, *debug)
	filteredTests := filter.Filter(allTests)

	// Sort and output
	sort.Strings(filteredTests)
	for _, test := range filteredTests {
		fmt.Println(test)
	}
}
