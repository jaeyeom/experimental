// Package query provides Bazel query operations for finding affected tests.
package query

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

// BazelQuerier executes Bazel queries.
type BazelQuerier struct {
	executor    executor.Executor
	failOnError bool // If true, return errors from query failures; if false, log and continue
}

// NewBazelQuerier creates a new BazelQuerier.
func NewBazelQuerier(debug bool) *BazelQuerier {
	if debug {
		slog.SetLogLoggerLevel(slog.LevelDebug)
	}
	failOnError := os.Getenv("BAZEL_AFFECTED_TESTS_FAIL_ON_ERROR") == "true" || os.Getenv("BAZEL_AFFECTED_TESTS_FAIL_ON_ERROR") == "1"
	return &BazelQuerier{
		executor:    executor.NewBasicExecutor(),
		failOnError: failOnError,
	}
}

// NewBazelQuerierWithExecutor creates a new BazelQuerier with a custom executor.
// This is primarily useful for testing.
func NewBazelQuerierWithExecutor(exec executor.Executor, debug bool) *BazelQuerier {
	if debug {
		slog.SetLogLoggerLevel(slog.LevelDebug)
	}
	failOnError := os.Getenv("BAZEL_AFFECTED_TESTS_FAIL_ON_ERROR") == "true" || os.Getenv("BAZEL_AFFECTED_TESTS_FAIL_ON_ERROR") == "1"
	return &BazelQuerier{
		executor:    exec,
		failOnError: failOnError,
	}
}

// FindAffectedTests finds test targets affected by changes to the given packages.
func (q *BazelQuerier) FindAffectedTests(packages []string) ([]string, error) {
	if len(packages) == 0 {
		return nil, nil
	}

	// Deduplicate packages
	uniquePackages := make(map[string]bool)
	for _, pkg := range packages {
		uniquePackages[pkg] = true
	}

	var allTests []string
	testsSet := make(map[string]bool)

	// Process each unique package
	for pkg := range uniquePackages {
		slog.Debug("Processing package", "package", pkg)

		// Get tests in the same package
		samePackageTests, err := q.query(fmt.Sprintf("kind('.*_test rule', %s:*)", pkg))
		if err != nil {
			if q.failOnError {
				return nil, fmt.Errorf("failed to query same package tests for %s: %w", pkg, err)
			}
			slog.Warn("Error querying same package tests, continuing...", "package", pkg, "error", err)
		} else {
			slog.Debug("Same package tests found", "count", len(samePackageTests))
			for _, test := range samePackageTests {
				testsSet[test] = true
			}
		}

		// Get external test dependencies
		externalTests, err := q.query(fmt.Sprintf("rdeps(//..., %s:*) intersect kind('.*_test rule', //...)", pkg))
		if err != nil {
			if q.failOnError {
				return nil, fmt.Errorf("failed to query external test deps for %s: %w", pkg, err)
			}
			slog.Warn("Error querying external test deps, continuing...", "package", pkg, "error", err)
		} else {
			slog.Debug("External test deps found", "count", len(externalTests))
			for _, test := range externalTests {
				testsSet[test] = true
			}
		}
	}

	// Always include format tests (will be filtered later based on file types)
	formatTests, err := q.query("//tools/format:* intersect kind('.*_test rule', //...)")
	if err != nil {
		if q.failOnError {
			return nil, fmt.Errorf("failed to query format tests: %w", err)
		}
		slog.Warn("Error querying format tests, continuing...", "error", err)
	} else {
		slog.Debug("Format test targets found", "count", len(formatTests))
		for _, test := range formatTests {
			testsSet[test] = true
		}
	}

	// Convert set to slice
	for test := range testsSet {
		allTests = append(allTests, test)
	}

	return allTests, nil
}

// query executes a single bazel query.
func (q *BazelQuerier) query(queryStr string) ([]string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	result, err := q.executor.Execute(ctx, executor.ToolConfig{
		Command:        "bazel",
		Args:           []string{"query", "--noblock_for_lock", queryStr},
		Timeout:        30 * time.Second,
		CommandBuilder: &executor.ShellCommandBuilder{},
	})
	if err != nil {
		return nil, fmt.Errorf("bazel query failed: %w", err)
	}

	// Check for lock contention - bazel exits with code 45 when another command is running
	if result.ExitCode == 45 || strings.Contains(result.Stderr, "Another command is running") {
		return nil, fmt.Errorf("another bazel command is running; wait for it to complete or run 'bazel shutdown'")
	}

	// Bazel query may return non-zero exit code for empty results
	if result.ExitCode != 0 && result.Stderr == "" {
		return nil, nil
	}

	if result.ExitCode != 0 {
		return nil, fmt.Errorf("bazel query failed with exit code %d: %s", result.ExitCode, result.Stderr)
	}

	if len(result.Output) == 0 {
		return nil, nil
	}

	lines := strings.Split(strings.TrimSpace(result.Output), "\n")
	var results []string
	for _, line := range lines {
		if line != "" {
			results = append(results, line)
		}
	}
	return results, nil
}
