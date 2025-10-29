package query

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

// BazelQuerier executes Bazel queries.
type BazelQuerier struct {
	executor executor.Executor
	debug    bool
}

// NewBazelQuerier creates a new BazelQuerier.
func NewBazelQuerier(debug bool) *BazelQuerier {
	return &BazelQuerier{
		executor: executor.NewBasicExecutor(),
		debug:    debug,
	}
}

// NewBazelQuerierWithExecutor creates a new BazelQuerier with a custom executor.
// This is primarily useful for testing.
func NewBazelQuerierWithExecutor(exec executor.Executor, debug bool) *BazelQuerier {
	return &BazelQuerier{
		executor: exec,
		debug:    debug,
	}
}

func (q *BazelQuerier) debugf(format string, args ...interface{}) {
	if q.debug {
		fmt.Printf("DEBUG: "+format+"\n", args...)
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
		q.debugf("Processing package: %s", pkg)

		// Get tests in the same package
		samePackageTests, err := q.query(fmt.Sprintf("kind('.*_test rule', %s:*)", pkg))
		if err != nil {
			q.debugf("Error querying same package tests: %v", err)
		} else {
			q.debugf("  Same package tests: %d", len(samePackageTests))
			for _, test := range samePackageTests {
				testsSet[test] = true
			}
		}

		// Get external test dependencies
		externalTests, err := q.query(fmt.Sprintf("rdeps(//..., %s:*) intersect kind('.*_test rule', //...)", pkg))
		if err != nil {
			q.debugf("Error querying external test deps: %v", err)
		} else {
			q.debugf("  External test deps: %d", len(externalTests))
			for _, test := range externalTests {
				testsSet[test] = true
			}
		}
	}

	// Always include format tests (will be filtered later based on file types)
	formatTests, err := q.query("//tools/format:* intersect kind('.*_test rule', //...)")
	if err != nil {
		q.debugf("Error querying format tests: %v", err)
	} else {
		q.debugf("Format test targets: %d", len(formatTests))
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
		Command: "bazel",
		Args:    []string{"query", queryStr},
		Timeout: 30 * time.Second,
	})
	if err != nil {
		return nil, fmt.Errorf("bazel query failed: %w", err)
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
