package github

import (
	"testing"
)

func TestGetPendingPullRequests(t *testing.T) {
	// This is a basic test structure
	// In a real implementation, we would mock the gh command execution
	// For now, we'll just test the function signature and basic error handling

	t.Run("should handle command execution errors", func(t *testing.T) {
		client := NewClient(&mockExecutor{shouldFail: true})
		_, err := client.GetPendingPullRequests()
		if err == nil {
			t.Error("expected error when gh command fails, got nil")
		}
	})

	t.Run("should parse valid JSON response", func(t *testing.T) {
		// Sample JSON response based on the example provided
		sampleJSON := `[
			{
				"title": "Test PR",
				"url": "https://github.com/org/repo/pull/1",
				"files": [
					{
						"additions": 10,
						"deletions": 5,
						"path": "path/to/file.go"
					}
				],
				"reviewRequests": [
					{
						"__typename": "User",
						"login": "testuser"
					}
				]
			}
		]`

		client := NewClient(&mockExecutor{output: sampleJSON})
		prs, err := client.GetPendingPullRequests()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		if len(prs) != 1 {
			t.Errorf("expected 1 PR, got %d", len(prs))
		}

		if prs[0].Title != "Test PR" {
			t.Errorf("expected PR title 'Test PR', got '%s'", prs[0].Title)
		}

		if len(prs[0].ReviewRequests) != 1 {
			t.Errorf("expected 1 review request, got %d", len(prs[0].ReviewRequests))
		}

		if prs[0].ReviewRequests[0].Login != "testuser" {
			t.Errorf("expected reviewer 'testuser', got '%s'", prs[0].ReviewRequests[0].Login)
		}
	})
}

// mockExecutor is a test helper that mocks command execution.
type mockExecutor struct {
	output     string
	shouldFail bool
}

func (m *mockExecutor) Execute(cmd string, args ...string) (string, error) {
	if m.shouldFail {
		return "", &mockError{message: "command failed"}
	}
	return m.output, nil
}

type mockError struct {
	message string
}

func (e *mockError) Error() string {
	return e.message
}
