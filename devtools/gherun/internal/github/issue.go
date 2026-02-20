package github

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gherun/internal/gherkin"
)

// Package-level compiled regexes for better performance.
var (
	featurePattern = regexp.MustCompile(`- \*\*([^*]+)\*\*`)
	passedPattern  = regexp.MustCompile(`- \[([xX ])\] PASSED`)
	failedPattern  = regexp.MustCompile(`- \[([xX ])\] FAILED`)
)

// TestResult represents the outcome of a single test.
type TestResult string

const (
	TestResultPending TestResult = "pending"
	TestResultPassed  TestResult = "passed"
	TestResultFailed  TestResult = "failed"
)

// TestProgress tracks the status of all tests.
type TestProgress struct {
	Total     int
	Completed int
	Passed    int
	Failed    int
	Features  map[string]TestResult // Feature ID -> result
}

// IssueManager handles test suite issue creation and monitoring.
type IssueManager struct {
	client *Client
	ctx    context.Context
}

// NewIssueManager creates a new IssueManager.
func NewIssueManager(ctx context.Context, client *Client) *IssueManager {
	return &IssueManager{client: client, ctx: ctx}
}

// CreateSuiteIssue creates a GitHub issue with checkboxes for all features.
func (m *IssueManager) CreateSuiteIssue(features []*gherkin.Feature, title string) (*Issue, error) {
	body := BuildSuiteIssueBody(features, title)
	return m.client.CreateIssue(m.ctx, title, body)
}

// GetTestProgress parses issue body to determine test progress.
func (m *IssueManager) GetTestProgress(issueNumber int) (*TestProgress, error) {
	issue, err := m.client.GetIssue(m.ctx, issueNumber)
	if err != nil {
		return nil, err
	}

	return ParseProgress(issue.Body), nil
}

// BuildSuiteIssueBody generates the markdown body for a test suite issue.
func BuildSuiteIssueBody(features []*gherkin.Feature, title string) string {
	var body strings.Builder
	fmt.Fprintf(&body, "## Test Suite: %s\n\n", title)

	for _, f := range features {
		fmt.Fprintf(&body, "- **%s** - %s\n", f.ID, f.Name)
		body.WriteString("    - [ ] PASSED\n")
		body.WriteString("    - [ ] FAILED\n")
	}

	return body.String()
}

// ParseProgress parses issue body for checkbox states.
// It looks for patterns like:
// - **feature-id** - Feature Name
//
//   - [x] PASSED
//   - [ ] FAILED
//
// If both PASSED and FAILED are checked, the feature is counted as FAILED.
func ParseProgress(body string) *TestProgress {
	progress := &TestProgress{
		Features: make(map[string]TestResult),
	}

	lines := strings.Split(body, "\n")
	var currentFeatureID string

	for _, line := range lines {
		// Check for feature line
		if matches := featurePattern.FindStringSubmatch(line); len(matches) > 1 {
			currentFeatureID = matches[1]
			progress.Total++
			progress.Features[currentFeatureID] = TestResultPending
			continue
		}

		// Check for PASSED checkbox (only if feature is still pending)
		if currentFeatureID != "" && progress.Features[currentFeatureID] == TestResultPending {
			if matches := passedPattern.FindStringSubmatch(line); len(matches) > 1 {
				if matches[1] == "x" || matches[1] == "X" {
					progress.Features[currentFeatureID] = TestResultPassed
					progress.Completed++
					progress.Passed++
				}
				continue
			}
		}

		// Check for FAILED checkbox (can override PASSED if both checked)
		if currentFeatureID != "" {
			if matches := failedPattern.FindStringSubmatch(line); len(matches) > 1 {
				if matches[1] == "x" || matches[1] == "X" {
					// If already counted as passed, adjust counters
					if progress.Features[currentFeatureID] == TestResultPassed {
						progress.Passed--
					} else {
						progress.Completed++
					}
					progress.Features[currentFeatureID] = TestResultFailed
					progress.Failed++
				}
			}
		}
	}

	return progress
}
