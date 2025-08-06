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
	config := parseFlags()

	c := cache.NewCache(config.cacheDir, config.debug)

	if config.clearCache {
		handleCacheClear(c, config.debug)
		return
	}

	stagedFiles, err := getStagedFiles()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting staged files: %v\n", err)
		os.Exit(1)
	}

	if len(stagedFiles) == 0 {
		os.Exit(0)
	}

	debugf := createDebugFunc(config.debug)
	debugf("Found %d staged files", len(stagedFiles))

	cacheKey := getCacheKey(c, config.noCache, debugf)
	packages := findPackages(stagedFiles, debugf)

	querier := query.NewBazelQuerier(config.debug)
	allTests := collectAllTests(packages, querier, c, cacheKey, config.noCache, debugf)

	// Filter and output results
	filter := query.NewFormatTestFilter(stagedFiles, config.debug)
	filteredTests := filter.Filter(allTests)
	outputResults(filteredTests)
}

type config struct {
	debug      bool
	cacheDir   string
	clearCache bool
	noCache    bool
}

func parseFlags() config {
	var cfg config
	flag.BoolVar(&cfg.debug, "debug", false, "Enable debug output")
	flag.StringVar(&cfg.cacheDir, "cache-dir", "", "Cache directory (default: $HOME/.cache/bazel-affected-tests)")
	flag.BoolVar(&cfg.clearCache, "clear-cache", false, "Clear the cache and exit")
	flag.BoolVar(&cfg.noCache, "no-cache", false, "Disable caching")
	flag.Parse()

	// Set debug from environment if not set via flag
	if !cfg.debug && os.Getenv("DEBUG") != "" {
		cfg.debug = true
	}

	return cfg
}

func handleCacheClear(c *cache.Cache, debug bool) {
	if err := c.Clear(); err != nil {
		fmt.Fprintf(os.Stderr, "Error clearing cache: %v\n", err)
		os.Exit(1)
	}
	if debug {
		fmt.Println("Cache cleared successfully")
	}
	os.Exit(0)
}

func getStagedFiles() ([]string, error) {
	files, err := git.GetStagedFiles()
	if err != nil {
		return nil, fmt.Errorf("getting staged files: %w", err)
	}
	return files, nil
}

func createDebugFunc(debug bool) func(string, ...interface{}) {
	return func(format string, args ...interface{}) {
		if debug {
			fmt.Printf("DEBUG: "+format+"\n", args...)
		}
	}
}

func getCacheKey(c *cache.Cache, noCache bool, debugf func(string, ...interface{})) string {
	if noCache {
		return ""
	}

	cacheKey, err := c.GetCacheKey()
	if err != nil {
		debugf("Failed to compute cache key: %v", err)
		return ""
	}

	debugf("Cache key: %s", cacheKey)
	return cacheKey
}

func findPackages(stagedFiles []string, debugf func(string, ...interface{})) []string {
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

	var packages []string
	for pkg := range packageMap {
		packages = append(packages, pkg)
	}
	return packages
}

func collectAllTests(packages []string, querier *query.BazelQuerier, c *cache.Cache, cacheKey string, noCache bool, debugf func(string, ...interface{})) []string {
	allTestsMap := make(map[string]bool)

	// Process packages
	for _, pkg := range packages {
		tests := getPackageTests(pkg, querier, c, cacheKey, noCache, debugf)
		for _, test := range tests {
			allTestsMap[test] = true
		}
	}

	// Always check for format tests
	formatTests, err := querier.FindAffectedTests([]string{"//tools/format"})
	if err == nil {
		for _, test := range formatTests {
			allTestsMap[test] = true
		}
	}

	var allTests []string
	for test := range allTestsMap {
		allTests = append(allTests, test)
	}
	return allTests
}

func getPackageTests(pkg string, querier *query.BazelQuerier, c *cache.Cache, cacheKey string, noCache bool, debugf func(string, ...interface{})) []string {
	if !noCache && cacheKey != "" {
		if cachedTests, found := c.Get(cacheKey, pkg); found {
			return cachedTests
		}
	}

	tests, err := querier.FindAffectedTests([]string{pkg})
	if err != nil {
		debugf("Error querying tests for package %s: %v", pkg, err)
		return nil
	}

	// Store in cache
	if !noCache && cacheKey != "" {
		if err := c.Set(cacheKey, pkg, tests); err != nil {
			debugf("Failed to cache results for %s: %v", pkg, err)
		}
	}

	return tests
}

func outputResults(tests []string) {
	sort.Strings(tests)
	for _, test := range tests {
		fmt.Println(test)
	}
}
