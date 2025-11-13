package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"sort"

	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/cache"
	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/git"
	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/query"
	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

func main() {
	config := parseFlags()

	if config.debug {
		slog.SetLogLoggerLevel(slog.LevelDebug)
	}

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

	slog.Debug("Staged files found", "count", len(stagedFiles))

	cacheKey := getCacheKey(c, config.noCache)
	packages := findPackages(stagedFiles)

	querier := query.NewBazelQuerier(config.debug)
	allTests := collectAllTests(packages, querier, c, cacheKey, config.noCache)

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
	ctx := context.Background()
	exec := executor.NewBasicExecutor()
	files, err := git.GetStagedFiles(ctx, exec)
	if err != nil {
		return nil, fmt.Errorf("getting staged files: %w", err)
	}
	return files, nil
}

func getCacheKey(c *cache.Cache, noCache bool) string {
	if noCache {
		return ""
	}

	cacheKey, err := c.GetCacheKey()
	if err != nil {
		slog.Debug("Failed to compute cache key", "error", err)
		return ""
	}

	slog.Debug("Cache key computed", "key", cacheKey)
	return cacheKey
}

func findPackages(stagedFiles []string) []string {
	packageMap := make(map[string]bool)
	for _, file := range stagedFiles {
		slog.Debug("Processing file", "file", file)
		if pkg, found := query.FindBazelPackage(file); found {
			slog.Debug("Found package", "package", pkg)
			packageMap[pkg] = true
		} else {
			slog.Debug("No Bazel package found for file", "file", file)
		}
	}

	var packages []string
	for pkg := range packageMap {
		packages = append(packages, pkg)
	}
	return packages
}

func collectAllTests(packages []string, querier *query.BazelQuerier, c *cache.Cache, cacheKey string, noCache bool) []string {
	allTestsMap := make(map[string]bool)

	// Process packages
	for _, pkg := range packages {
		tests := getPackageTests(pkg, querier, c, cacheKey, noCache)
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

func getPackageTests(pkg string, querier *query.BazelQuerier, c *cache.Cache, cacheKey string, noCache bool) []string {
	if !noCache && cacheKey != "" {
		if cachedTests, found := c.Get(cacheKey, pkg); found {
			return cachedTests
		}
	}

	tests, err := querier.FindAffectedTests([]string{pkg})
	if err != nil {
		slog.Debug("Error querying tests for package", "package", pkg, "error", err)
		return nil
	}

	// Store in cache
	if !noCache && cacheKey != "" {
		if err := c.Set(cacheKey, pkg, tests); err != nil {
			slog.Debug("Failed to cache results", "package", pkg, "error", err)
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
