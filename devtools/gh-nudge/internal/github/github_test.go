package github

import (
	"strings" // Added strings import
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
		// Sample JSON response based on the example provided, now including baseRefName and headRefName
		sampleJSON := `[
			{
				"title": "Test PR 1",
				"url": "https://github.com/org/repo/pull/1",
				"baseRefName": "main",
				"headRefName": "feature/pr-1",
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
						"login": "testuser1"
					}
				],
				"mergeable": "MERGEABLE"
			},
			{
				"title": "Test PR 2",
				"url": "https://github.com/org/repo/pull/2",
				"baseRefName": "develop",
				"headRefName": "bugfix/pr-2",
				"files": [],
				"reviewRequests": [
					{
						"__typename": "Team",
						"login": "my-team"
					}
				],
				"mergeable": "CONFLICTING"
			}
		]`

		client := NewClient(&mockExecutor{output: sampleJSON})
		prs, err := client.GetPendingPullRequests()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		if len(prs) != 2 {
			t.Fatalf("expected 2 PRs, got %d", len(prs))
		}

		// Verify PR 1
		if prs[0].Title != "Test PR 1" {
			t.Errorf("expected PR title 'Test PR 1', got '%s'", prs[0].Title)
		}
		if prs[0].BaseRefName != "main" {
			t.Errorf("expected PR 0 BaseRefName 'main', got '%s'", prs[0].BaseRefName)
		}
		if prs[0].HeadRefName != "feature/pr-1" {
			t.Errorf("expected PR 0 HeadRefName 'feature/pr-1', got '%s'", prs[0].HeadRefName)
		}
		if prs[0].Mergeable != "MERGEABLE" {
			t.Errorf("expected PR 0 Mergeable 'MERGEABLE', got '%s'", prs[0].Mergeable)
		}
		if len(prs[0].ReviewRequests) != 1 {
			t.Errorf("expected 1 review request for PR 0, got %d", len(prs[0].ReviewRequests))
		}
		if prs[0].ReviewRequests[0].Login != "testuser1" {
			t.Errorf("expected reviewer 'testuser1' for PR 0, got '%s'", prs[0].ReviewRequests[0].Login)
		}

		// Verify PR 2
		if prs[1].Title != "Test PR 2" {
			t.Errorf("expected PR title 'Test PR 2', got '%s'", prs[1].Title)
		}
		if prs[1].BaseRefName != "develop" {
			t.Errorf("expected PR 1 BaseRefName 'develop', got '%s'", prs[1].BaseRefName)
		}
		if prs[1].HeadRefName != "bugfix/pr-2" {
			t.Errorf("expected PR 1 HeadRefName 'bugfix/pr-2', got '%s'", prs[1].HeadRefName)
		}
		if prs[1].Mergeable != "CONFLICTING" {
			t.Errorf("expected PR 1 Mergeable 'CONFLICTING', got '%s'", prs[1].Mergeable)
		}
		if len(prs[1].ReviewRequests) != 1 {
			t.Errorf("expected 1 review request for PR 1, got %d", len(prs[1].ReviewRequests))
		}
		if prs[1].ReviewRequests[0].Login != "my-team" { // Assuming Team login is stored in Login field
			t.Errorf("expected reviewer 'my-team' for PR 1, got '%s'", prs[1].ReviewRequests[0].Login)
		}
	})
}

// mockExecutor is a test helper that mocks command execution.
type mockExecutor struct {
	output     string
	shouldFail bool
	executeFn  func(cmd string, args ...string) (string, error) // More flexible mocking
}

func (m *mockExecutor) Execute(cmd string, args ...string) (string, error) {
	if m.executeFn != nil {
		return m.executeFn(cmd, args...)
	}
	if m.shouldFail {
		// For gh api errors, output might still be relevant (e.g. JSON error message from API)
		// So, return m.output along with the error.
		return m.output, &mockError{message: "command failed"}
	}
	return m.output, nil
}

type mockError struct {
	message string
}

func (e *mockError) Error() string {
	return e.message
}

func TestGetFileContent(t *testing.T) {
	t.Run("should successfully fetch and decode file content", func(t *testing.T) {
		// "Hello, world!" base64 encoded
		encodedContent := "SGVsbG8sIHdvcmxkIQ=="
		jsonResponse := `{
			"content": "` + encodedContent + `",
			"encoding": "base64"
		}`
		client := NewClient(&mockExecutor{output: jsonResponse})
		content, err := client.GetFileContent("owner", "repo", "path/to/file.txt", "main")
		if err != nil {
			t.Fatalf("GetFileContent failed: %v", err)
		}
		if content != "Hello, world!" {
			t.Errorf("Expected content 'Hello, world!', got '%s'", content)
		}
	})

	t.Run("should handle file not found error from gh command", func(t *testing.T) {
		client := NewClient(&mockExecutor{output: `{"message": "Not Found"}`, shouldFail: true}) // Output needed for current GetFileContent error check
		_, err := client.GetFileContent("owner", "repo", "path/to/nonexistent.txt", "main")
		if err == nil {
			t.Fatal("Expected an error for file not found, got nil")
		}
		// Check if the error message indicates "file not found"
		if !strings.Contains(err.Error(), "file not found") && !strings.Contains(err.Error(), "Not Found") {
			t.Errorf("Expected error to indicate 'file not found', but got: %v", err)
		}
	})

	t.Run("should handle gh command execution error", func(t *testing.T) {
		client := NewClient(&mockExecutor{shouldFail: true, output: "some generic gh error"})
		_, err := client.GetFileContent("owner", "repo", "path/to/file.txt", "main")
		if err == nil {
			t.Fatal("Expected an error for gh command failure, got nil")
		}
		expectedErrorSubString := `failed to execute gh api command for file content: repos/owner/repo/contents/path/to/file.txt`
		if !strings.Contains(err.Error(), expectedErrorSubString) {
			t.Errorf("Expected error message to contain '%s', got '%s'", expectedErrorSubString, err.Error())
		}
	})

	t.Run("should handle base64 decoding error", func(t *testing.T) {
		jsonResponse := `{
			"content": "This is not valid base64!@#",
			"encoding": "base64"
		}`
		client := NewClient(&mockExecutor{output: jsonResponse})
		_, err := client.GetFileContent("owner", "repo", "path/to/file.txt", "main")
		if err == nil {
			t.Fatal("Expected a base64 decoding error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to decode base64 content") {
			t.Errorf("Expected error to be about base64 decoding, got: %v", err)
		}
	})

	t.Run("should handle JSON parsing error", func(t *testing.T) {
		invalidJsonResponse := `{"content": "valid", "encoding": "base64"` // Missing closing brace
		client := NewClient(&mockExecutor{output: invalidJsonResponse})
		_, err := client.GetFileContent("owner", "repo", "path/to/file.txt", "main")
		if err == nil {
			t.Fatal("Expected a JSON parsing error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to parse gh api JSON output") {
			t.Errorf("Expected error to be about JSON parsing, got: %v", err)
		}
	})

	t.Run("should handle unexpected encoding type", func(t *testing.T) {
		jsonResponse := `{
			"content": "SGVsbG8sIHdvcmxkIQ==",
			"encoding": "utf-8"
		}`
		client := NewClient(&mockExecutor{output: jsonResponse})
		_, err := client.GetFileContent("owner", "repo", "path/to/file.txt", "main")
		if err == nil {
			t.Fatal("Expected an error for unexpected encoding type, got nil")
		}
		if !strings.Contains(err.Error(), "unexpected content encoding") {
			t.Errorf("Expected error to be about unexpected encoding, got: %v", err)
		}
	})
}
