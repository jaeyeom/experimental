package ghtest

import (
	"context"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/executor"
)

func TestGHMockExecutor(t *testing.T) {
	mock := NewGHMockExecutor()

	// gh should be available
	if !mock.IsAvailable("gh") {
		t.Error("gh should be available in mock")
	}
}

func TestGHMockExecutorExpectAuthToken(t *testing.T) {
	mock := NewGHMockExecutor()
	mock.ExpectAuthToken("ghp_test_token_12345")

	output, err := executor.Output(context.Background(), mock.Executor(), "gh", "auth", "token")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if string(output) != "ghp_test_token_12345" {
		t.Errorf("expected token, got %q", string(output))
	}
}

func TestGHMockExecutorExpectGHCommand(t *testing.T) {
	mock := NewGHMockExecutor()
	mock.ExpectGHCommand("repo view").WillReturn(`{"name":"test-repo","owner":{"login":"owner"}}`)

	output, err := executor.Output(context.Background(), mock.Executor(), "gh", "repo", "view", "--json", "name,owner")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if string(output) != `{"name":"test-repo","owner":{"login":"owner"}}` {
		t.Errorf("unexpected output: %s", string(output))
	}
}

func TestGHMockExecutorExpectAPICall(t *testing.T) {
	mock := NewGHMockExecutor()

	response := map[string]any{
		"number": 42,
		"title":  "Test PR",
	}
	if err := mock.ExpectAPICall("/repos/owner/repo/pulls/42", response); err != nil {
		t.Fatalf("failed to set up mock: %v", err)
	}

	output, err := executor.Output(context.Background(), mock.Executor(), "gh", "api", "/repos/owner/repo/pulls/42")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should contain the JSON response
	if len(output) == 0 {
		t.Error("expected non-empty output")
	}
}

func TestGHMockExecutorWillFail(t *testing.T) {
	mock := NewGHMockExecutor()
	mock.ExpectGHCommand("pr merge").WillFail("not mergeable", 1)

	_, err := executor.Output(context.Background(), mock.Executor(), "gh", "pr", "merge", "123")
	if err == nil {
		t.Fatal("expected error")
	}
}

func TestSampleFixtures(t *testing.T) {
	// Test that fixtures return valid data
	issue := SampleIssue(1, "Test Issue", "Body")
	if issue["number"] != 1 {
		t.Errorf("expected number=1, got %v", issue["number"])
	}

	pr := SamplePR(42, "Test PR")
	if pr["number"] != 42 {
		t.Errorf("expected number=42, got %v", pr["number"])
	}

	repo := SampleRepo("owner", "repo")
	if repo["name"] != "repo" {
		t.Errorf("expected name='repo', got %v", repo["name"])
	}

	user := SampleUser("testuser")
	if user["login"] != "testuser" {
		t.Errorf("expected login='testuser', got %v", user["login"])
	}
}
