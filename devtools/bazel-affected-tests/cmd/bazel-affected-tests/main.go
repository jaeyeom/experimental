// Deprecated: Use github.com/jaeyeom/bazel-affected-tests instead.
package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"sort"

	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/cache"
	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/config"
	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/git"
	"github.com/jaeyeom/experimental/devtools/bazel-affected-tests/internal/query"
	executor "github.com/jaeyeom/go-cmdexec"
)

func main() {
	fmt.Fprintln(os.Stderr, "WARNING: This version of bazel-affected-tests is deprecated.")
	fmt.Fprintln(os.Stderr, "Please switch to: go install github.com/jaeyeom/bazel-affected-tests/cmd/bazel-affected-tests@latest")

	cfg := parseFlags()

	if cfg.debug {
		slog.SetLogLoggerLevel(slog.LevelDebug)
	}

	c := cache.NewCache(cfg.cacheDir, cfg.debug)

	if cfg.clearCache {
		handleCacheClear(c, cfg.debug)
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

	packages := findPackages(stagedFiles)
	if len(packages) == 0 {
		slog.Debug("No Bazel packages found for staged files")
		os.Exit(0)
	}

	cacheKey := getCacheKey(c, cfg.noCache)

	querier := query.NewBazelQuerier(cfg.debug)
	allTests := collectAllTests(packages, querier, c, cacheKey, cfg.noCache)

	// Filter and output results
	filter := query.NewFormatTestFilter(stagedFiles, cfg.debug)
	filteredTests := filter.Filter(allTests)

	// Load config and add pattern-matched targets
	repoCfg, err := config.LoadConfig()
	if err != nil {
		slog.Warn("Failed to load config", "error", err)
	}

	var configTargets []string
	if repoCfg != nil {
		configTargets = repoCfg.MatchTargets(stagedFiles)
		slog.Debug("Config targets matched", "count", len(configTargets))
	}

	// Merge and output
	outputResults(filteredTests, configTargets)
}

type cliConfig struct {
	debug      bool
	cacheDir   string
	clearCache bool
	noCache    bool
}

func parseFlags() cliConfig {
	var cfg cliConfig
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

func outputResults(tests []string, configTargets []string) {
	// Merge and deduplicate
	allTargets := make(map[string]bool)
	for _, test := range tests {
		allTargets[test] = true
	}
	for _, target := range configTargets {
		allTargets[target] = true
	}

	// Convert to sorted slice
	result := make([]string, 0, len(allTargets))
	for target := range allTargets {
		result = append(result, target)
	}
	sort.Strings(result)

	for _, target := range result {
		fmt.Println(target)
	}
}
