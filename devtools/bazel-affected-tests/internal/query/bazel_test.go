package query

import (
	"context"
	"errors"
	"sort"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

func TestNewBazelQuerier(t *testing.T) {
	q := NewBazelQuerier(false)
	if q == nil {
		t.Fatal("NewBazelQuerier returned nil")
	}
	if q.executor == nil {
		t.Fatal("BazelQuerier.executor is nil")
	}
	if q.debug {
		t.Error("Expected debug to be false")
	}
}

func TestNewBazelQuerierWithExecutor(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, true)

	if q == nil {
		t.Fatal("NewBazelQuerierWithExecutor returned nil")
	}
	if q.executor != mockExec {
		t.Error("Expected custom executor")
	}
	if !q.debug {
		t.Error("Expected debug to be true")
	}
}

func TestFindAffectedTests_EmptyPackages(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	tests, err := q.FindAffectedTests([]string{})
	if err != nil {
		t.Fatalf("FindAffectedTests failed: %v", err)
	}

	if len(tests) != 0 {
		t.Errorf("Expected 0 tests, got %d", len(tests))
	}

	// Verify no commands were executed
	history := mockExec.GetCallHistory()
	if len(history) != 0 {
		t.Errorf("Expected no command executions, got %d", len(history))
	}
}

func TestFindAffectedTests_SinglePackageWithTests(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// Mock same-package tests query
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/foo:*)").
		WillSucceed("//pkg/foo:foo_test\n//pkg/foo:bar_test", 0).
		Once().
		Build()

	// Mock external test deps query
	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/foo:*) intersect kind('.*_test rule', //...)").
		WillSucceed("//other/pkg:other_test", 0).
		Once().
		Build()

	// Mock format tests query
	mockExec.ExpectCommandWithArgs("bazel", "query", "//tools/format:* intersect kind('.*_test rule', //...)").
		WillSucceed("//tools/format:format_test", 0).
		Once().
		Build()

	tests, err := q.FindAffectedTests([]string{"//pkg/foo"})
	if err != nil {
		t.Fatalf("FindAffectedTests failed: %v", err)
	}

	expectedTests := []string{
		"//pkg/foo:foo_test",
		"//pkg/foo:bar_test",
		"//other/pkg:other_test",
		"//tools/format:format_test",
	}

	if len(tests) != len(expectedTests) {
		t.Errorf("Expected %d tests, got %d: %v", len(expectedTests), len(tests), tests)
	}

	// Verify all expected tests are present (order doesn't matter)
	testSet := make(map[string]bool)
	for _, test := range tests {
		testSet[test] = true
	}
	for _, expected := range expectedTests {
		if !testSet[expected] {
			t.Errorf("Expected test %s not found in results", expected)
		}
	}
}

func TestFindAffectedTests_MultiplePackages(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// Package 1 queries
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/foo:*)").
		WillSucceed("//pkg/foo:foo_test", 0).
		Build()
	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/foo:*) intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()

	// Package 2 queries
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/bar:*)").
		WillSucceed("//pkg/bar:bar_test", 0).
		Build()
	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/bar:*) intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()

	// Format tests
	mockExec.ExpectCommandWithArgs("bazel", "query", "//tools/format:* intersect kind('.*_test rule', //...)").
		WillSucceed("//tools/format:format_test", 0).
		Build()

	tests, err := q.FindAffectedTests([]string{"//pkg/foo", "//pkg/bar"})
	if err != nil {
		t.Fatalf("FindAffectedTests failed: %v", err)
	}

	expectedTests := []string{
		"//pkg/foo:foo_test",
		"//pkg/bar:bar_test",
		"//tools/format:format_test",
	}

	if len(tests) != len(expectedTests) {
		t.Errorf("Expected %d tests, got %d: %v", len(expectedTests), len(tests), tests)
	}
}

func TestFindAffectedTests_DeduplicatePackages(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// Should only execute once for the duplicate package
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/foo:*)").
		WillSucceed("//pkg/foo:test", 0).
		Once().
		Build()
	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/foo:*) intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Once().
		Build()

	// Format tests
	mockExec.ExpectCommandWithArgs("bazel", "query", "//tools/format:* intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()

	tests, err := q.FindAffectedTests([]string{"//pkg/foo", "//pkg/foo", "//pkg/foo"})
	if err != nil {
		t.Fatalf("FindAffectedTests failed: %v", err)
	}

	if len(tests) != 1 {
		t.Errorf("Expected 1 test, got %d", len(tests))
	}

	// Verify only one execution per unique package
	if err := mockExec.AssertExpectationsMet(); err != nil {
		t.Errorf("Mock expectations not met: %v", err)
	}
}

func TestFindAffectedTests_EmptyQueryResults(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// All queries return empty results
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/foo:*)").
		WillSucceed("", 0).
		Build()
	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/foo:*) intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()
	mockExec.ExpectCommandWithArgs("bazel", "query", "//tools/format:* intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()

	tests, err := q.FindAffectedTests([]string{"//pkg/foo"})
	if err != nil {
		t.Fatalf("FindAffectedTests failed: %v", err)
	}

	if len(tests) != 0 {
		t.Errorf("Expected 0 tests for empty results, got %d", len(tests))
	}
}

func TestFindAffectedTests_QueryErrorHandling(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// First query succeeds
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/foo:*)").
		WillSucceed("//pkg/foo:test", 0).
		Build()

	// Second query fails but is ignored (error handling in code)
	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/foo:*) intersect kind('.*_test rule', //...)").
		WillFail("query error", 1).
		Build()

	// Format tests succeed
	mockExec.ExpectCommandWithArgs("bazel", "query", "//tools/format:* intersect kind('.*_test rule', //...)").
		WillSucceed("//tools/format:test", 0).
		Build()

	tests, err := q.FindAffectedTests([]string{"//pkg/foo"})
	if err != nil {
		t.Fatalf("FindAffectedTests should not fail on query errors: %v", err)
	}

	// Should still get results from successful queries
	if len(tests) < 2 {
		t.Errorf("Expected at least 2 tests despite one query failing, got %d", len(tests))
	}
}

func TestFindAffectedTests_BazelEmptyResultExitCode(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// Bazel returns non-zero exit code but no stderr (empty result)
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/foo:*)").
		WillReturn(&executor.ExecutionResult{
			Command:   "bazel",
			Args:      []string{"query", "kind('.*_test rule', //pkg/foo:*)"},
			Output:    "",
			Stderr:    "", // No stderr means empty result, not error
			ExitCode:  1,
			StartTime: time.Now(),
			EndTime:   time.Now(),
		}, nil).
		Build()

	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/foo:*) intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()

	mockExec.ExpectCommandWithArgs("bazel", "query", "//tools/format:* intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()

	tests, err := q.FindAffectedTests([]string{"//pkg/foo"})
	if err != nil {
		t.Fatalf("FindAffectedTests should handle empty result exit codes: %v", err)
	}

	if len(tests) != 0 {
		t.Errorf("Expected 0 tests for empty results, got %d", len(tests))
	}
}

func TestFindAffectedTests_DeduplicateTestTargets(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// Same test appears in multiple queries
	mockExec.ExpectCommandWithArgs("bazel", "query", "kind('.*_test rule', //pkg/foo:*)").
		WillSucceed("//pkg/foo:shared_test", 0).
		Build()

	mockExec.ExpectCommandWithArgs("bazel", "query", "rdeps(//..., //pkg/foo:*) intersect kind('.*_test rule', //...)").
		WillSucceed("//pkg/foo:shared_test\n//other:test", 0).
		Build()

	mockExec.ExpectCommandWithArgs("bazel", "query", "//tools/format:* intersect kind('.*_test rule', //...)").
		WillSucceed("", 0).
		Build()

	tests, err := q.FindAffectedTests([]string{"//pkg/foo"})
	if err != nil {
		t.Fatalf("FindAffectedTests failed: %v", err)
	}

	// Should deduplicate shared_test
	if len(tests) != 2 {
		t.Errorf("Expected 2 unique tests, got %d: %v", len(tests), tests)
	}

	testSet := make(map[string]int)
	for _, test := range tests {
		testSet[test]++
	}

	for test, count := range testSet {
		if count > 1 {
			t.Errorf("Test %s appears %d times, should be deduplicated", test, count)
		}
	}
}

func TestQuery_ExecutorError(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// Mock executor error (not exit code)
	mockExec.ExpectCommandWithArgs("bazel", "query", "//...").
		WillError(errors.New("executor error")).
		Build()

	results, err := q.query("//...")
	if err == nil {
		t.Fatal("Expected error from query")
	}

	if results != nil {
		t.Errorf("Expected nil results on error, got %v", results)
	}

	if err.Error() != "bazel query failed: executor error" {
		t.Errorf("Unexpected error message: %v", err)
	}
}

func TestQuery_NonZeroExitWithStderr(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	mockExec.ExpectCommandWithArgs("bazel", "query", "invalid query").
		WillReturn(&executor.ExecutionResult{
			Command:   "bazel",
			Args:      []string{"query", "invalid query"},
			Output:    "",
			Stderr:    "ERROR: Invalid query syntax",
			ExitCode:  1,
			StartTime: time.Now(),
			EndTime:   time.Now(),
		}, nil).
		Build()

	results, err := q.query("invalid query")
	if err == nil {
		t.Fatal("Expected error for non-zero exit with stderr")
	}

	if results != nil {
		t.Errorf("Expected nil results on error, got %v", results)
	}

	if !contains(err.Error(), "exit code 1") {
		t.Errorf("Expected error to mention exit code: %v", err)
	}
}

func TestQuery_MultilineOutput(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	output := "//pkg/foo:test1\n//pkg/foo:test2\n//pkg/bar:test3\n"
	mockExec.ExpectCommandWithArgs("bazel", "query", "//...").
		WillSucceed(output, 0).
		Build()

	results, err := q.query("//...")
	if err != nil {
		t.Fatalf("Query failed: %v", err)
	}

	expected := []string{
		"//pkg/foo:test1",
		"//pkg/foo:test2",
		"//pkg/bar:test3",
	}

	if len(results) != len(expected) {
		t.Errorf("Expected %d results, got %d: %v", len(expected), len(results), results)
	}

	sort.Strings(results)
	sort.Strings(expected)

	for i, exp := range expected {
		if i >= len(results) || results[i] != exp {
			t.Errorf("Result[%d]: expected %s, got %s", i, exp, results[i])
		}
	}
}

func TestQuery_EmptyLinesFiltered(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	output := "//pkg/foo:test1\n\n\n//pkg/foo:test2\n\n"
	mockExec.ExpectCommandWithArgs("bazel", "query", "//...").
		WillSucceed(output, 0).
		Build()

	results, err := q.query("//...")
	if err != nil {
		t.Fatalf("Query failed: %v", err)
	}

	if len(results) != 2 {
		t.Errorf("Expected 2 results (empty lines filtered), got %d: %v", len(results), results)
	}
}

func TestQuery_Timeout(t *testing.T) {
	mockExec := executor.NewMockExecutor()
	q := NewBazelQuerierWithExecutor(mockExec, false)

	// Verify timeout is set correctly
	var capturedConfig executor.ToolConfig
	mockExec.ExpectCustom(func(_ context.Context, cfg executor.ToolConfig) bool {
		capturedConfig = cfg
		return cfg.Command == "bazel" && len(cfg.Args) == 2
	}).WillSucceed("//test:target", 0).Build()

	_, err := q.query("//...")
	if err != nil {
		t.Fatalf("Query failed: %v", err)
	}

	if capturedConfig.Timeout != 30*time.Second {
		t.Errorf("Expected timeout of 30s, got %v", capturedConfig.Timeout)
	}
}

// Helper function.
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > len(substr) && (s[:len(substr)] == substr || s[len(s)-len(substr):] == substr || containsMiddle(s, substr)))
}

func containsMiddle(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
