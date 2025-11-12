package github

import (
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// TestNewClient tests the behavior of creating a new GitHub client.
func TestNewClient(t *testing.T) {
	t.Run("creates client with provided executor", func(t *testing.T) {
		executor := &mockExecutor{output: "test"}
		client := NewClientWithExecutor(executor)
		if client == nil {
			t.Fatal("expected client to be created, got nil")
		}
	})
}

// TestGetPendingPullRequests tests the behavior of fetching pending pull requests.
func TestGetPendingPullRequests(t *testing.T) {
	t.Run("returns error when gh command fails", func(t *testing.T) {
		client := NewClientWithExecutor(&mockExecutor{shouldFail: true})
		prs, err := client.GetPendingPullRequests()
		if err == nil {
			t.Error("expected error when gh command fails, got nil")
		}
		if prs != nil {
			t.Errorf("expected nil PRs on error, got %v", prs)
		}
		if !strings.Contains(err.Error(), "failed to execute gh command") {
			t.Errorf("expected error to mention command execution, got: %v", err)
		}
	})

	t.Run("returns error when JSON is invalid", func(t *testing.T) {
		client := NewClientWithExecutor(&mockExecutor{output: "invalid json"})
		prs, err := client.GetPendingPullRequests()
		if err == nil {
			t.Error("expected error when JSON is invalid, got nil")
		}
		if prs != nil {
			t.Errorf("expected nil PRs on parse error, got %v", prs)
		}
		if !strings.Contains(err.Error(), "failed to parse") {
			t.Errorf("expected error to mention parsing, got: %v", err)
		}
	})

	t.Run("parses single PR with all fields", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "Add new feature",
				"url": "https://github.com/org/repo/pull/123",
				"headRefName": "feature/new-feature",
				"mergeable": "MERGEABLE",
				"files": [
					{
						"path": "src/main.go",
						"additions": 50,
						"deletions": 10
					}
				],
				"reviewRequests": [
					{
						"__typename": "User",
						"login": "reviewer1"
					}
				]
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetPendingPullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 1 {
			t.Fatalf("expected 1 PR, got %d", len(prs))
		}

		pr := prs[0]
		if pr.Title != "Add new feature" {
			t.Errorf("expected title 'Add new feature', got %q", pr.Title)
		}
		if pr.URL != "https://github.com/org/repo/pull/123" {
			t.Errorf("expected specific URL, got %q", pr.URL)
		}
		if pr.HeadRefName != "feature/new-feature" {
			t.Errorf("expected headRefName 'feature/new-feature', got %q", pr.HeadRefName)
		}
		if pr.Mergeable != "MERGEABLE" {
			t.Errorf("expected mergeable 'MERGEABLE', got %q", pr.Mergeable)
		}
		if len(pr.Files) != 1 {
			t.Fatalf("expected 1 file, got %d", len(pr.Files))
		}
		if pr.Files[0].Path != "src/main.go" {
			t.Errorf("expected file path 'src/main.go', got %q", pr.Files[0].Path)
		}
		if len(pr.ReviewRequests) != 1 {
			t.Fatalf("expected 1 review request, got %d", len(pr.ReviewRequests))
		}
		if pr.ReviewRequests[0].Login != "reviewer1" {
			t.Errorf("expected reviewer 'reviewer1', got %q", pr.ReviewRequests[0].Login)
		}
	})

	t.Run("parses multiple PRs", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "First PR",
				"url": "https://github.com/org/repo/pull/1",
				"files": [],
				"reviewRequests": []
			},
			{
				"title": "Second PR",
				"url": "https://github.com/org/repo/pull/2",
				"files": [],
				"reviewRequests": []
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetPendingPullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 2 {
			t.Fatalf("expected 2 PRs, got %d", len(prs))
		}

		if prs[0].Title != "First PR" {
			t.Errorf("expected first PR title 'First PR', got %q", prs[0].Title)
		}
		if prs[1].Title != "Second PR" {
			t.Errorf("expected second PR title 'Second PR', got %q", prs[1].Title)
		}
	})

	t.Run("handles empty PR list", func(t *testing.T) {
		client := NewClientWithExecutor(&mockExecutor{output: "[]"})
		prs, err := client.GetPendingPullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 0 {
			t.Errorf("expected empty PR list, got %d PRs", len(prs))
		}
	})

	t.Run("parses PR with team review requests", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "Team Review PR",
				"url": "https://github.com/org/repo/pull/456",
				"files": [],
				"reviewRequests": [
					{
						"__typename": "Team",
						"name": "Core Team",
						"slug": "core-team"
					}
				]
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetPendingPullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 1 {
			t.Fatalf("expected 1 PR, got %d", len(prs))
		}

		if len(prs[0].ReviewRequests) != 1 {
			t.Fatalf("expected 1 review request, got %d", len(prs[0].ReviewRequests))
		}

		rr := prs[0].ReviewRequests[0]
		if rr.Type != "Team" {
			t.Errorf("expected type 'Team', got %q", rr.Type)
		}
		if rr.Name != "Core Team" {
			t.Errorf("expected name 'Core Team', got %q", rr.Name)
		}
		if rr.Slug != "core-team" {
			t.Errorf("expected slug 'core-team', got %q", rr.Slug)
		}
	})

	t.Run("parses PR with multiple files", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "Multi-file PR",
				"url": "https://github.com/org/repo/pull/789",
				"files": [
					{
						"path": "file1.go",
						"additions": 10,
						"deletions": 5
					},
					{
						"path": "file2.go",
						"additions": 20,
						"deletions": 15
					}
				],
				"reviewRequests": []
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetPendingPullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 1 {
			t.Fatalf("expected 1 PR, got %d", len(prs))
		}

		if len(prs[0].Files) != 2 {
			t.Fatalf("expected 2 files, got %d", len(prs[0].Files))
		}

		file1 := prs[0].Files[0]
		if file1.Path != "file1.go" || file1.Additions != 10 || file1.Deletions != 5 {
			t.Errorf("unexpected file1 data: %+v", file1)
		}

		file2 := prs[0].Files[1]
		if file2.Path != "file2.go" || file2.Additions != 20 || file2.Deletions != 15 {
			t.Errorf("unexpected file2 data: %+v", file2)
		}
	})
}

// TestGetMergeablePullRequests tests the behavior of filtering mergeable PRs.
func TestGetMergeablePullRequests(t *testing.T) {
	t.Run("returns error when GetPendingPullRequests fails", func(t *testing.T) {
		client := NewClientWithExecutor(&mockExecutor{shouldFail: true})
		prs, err := client.GetMergeablePullRequests()
		if err == nil {
			t.Error("expected error when underlying call fails, got nil")
		}
		if prs != nil {
			t.Errorf("expected nil PRs on error, got %v", prs)
		}
	})

	t.Run("filters out PRs with pending review requests", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "PR with reviews",
				"url": "https://github.com/org/repo/pull/1",
				"mergeable": "MERGEABLE",
				"files": [],
				"reviewRequests": [{"__typename": "User", "login": "reviewer"}]
			},
			{
				"title": "PR without reviews",
				"url": "https://github.com/org/repo/pull/2",
				"mergeable": "MERGEABLE",
				"files": [],
				"reviewRequests": []
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetMergeablePullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 1 {
			t.Fatalf("expected 1 mergeable PR, got %d", len(prs))
		}

		if prs[0].Title != "PR without reviews" {
			t.Errorf("expected PR without reviews to be returned, got %q", prs[0].Title)
		}
	})

	t.Run("filters out PRs that are not mergeable", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "Conflicted PR",
				"url": "https://github.com/org/repo/pull/1",
				"mergeable": "CONFLICTING",
				"files": [],
				"reviewRequests": []
			},
			{
				"title": "Mergeable PR",
				"url": "https://github.com/org/repo/pull/2",
				"mergeable": "MERGEABLE",
				"files": [],
				"reviewRequests": []
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetMergeablePullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 1 {
			t.Fatalf("expected 1 mergeable PR, got %d", len(prs))
		}

		if prs[0].Title != "Mergeable PR" {
			t.Errorf("expected mergeable PR to be returned, got %q", prs[0].Title)
		}
	})

	t.Run("returns empty list when no PRs are mergeable", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "PR 1",
				"url": "https://github.com/org/repo/pull/1",
				"mergeable": "MERGEABLE",
				"files": [],
				"reviewRequests": [{"__typename": "User", "login": "reviewer"}]
			},
			{
				"title": "PR 2",
				"url": "https://github.com/org/repo/pull/2",
				"mergeable": "CONFLICTING",
				"files": [],
				"reviewRequests": []
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetMergeablePullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 0 {
			t.Errorf("expected empty list, got %d PRs", len(prs))
		}
	})

	t.Run("returns all PRs when all are mergeable and have no review requests", func(t *testing.T) {
		sampleJSON := `[
			{
				"title": "PR 1",
				"url": "https://github.com/org/repo/pull/1",
				"mergeable": "MERGEABLE",
				"files": [],
				"reviewRequests": []
			},
			{
				"title": "PR 2",
				"url": "https://github.com/org/repo/pull/2",
				"mergeable": "MERGEABLE",
				"files": [],
				"reviewRequests": []
			}
		]`

		client := NewClientWithExecutor(&mockExecutor{output: sampleJSON})
		prs, err := client.GetMergeablePullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 2 {
			t.Errorf("expected 2 mergeable PRs, got %d", len(prs))
		}
	})

	t.Run("handles empty PR list from upstream", func(t *testing.T) {
		client := NewClientWithExecutor(&mockExecutor{output: "[]"})
		prs, err := client.GetMergeablePullRequests()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(prs) != 0 {
			t.Errorf("expected empty list, got %d PRs", len(prs))
		}
	})
}

// TestGetPullRequestDetails tests the behavior of fetching PR details.
func TestGetPullRequestDetails(t *testing.T) {
	client := NewClientWithExecutor(&mockExecutor{})

	t.Run("returns PR unchanged (placeholder implementation)", func(t *testing.T) {
		pr := models.PullRequest{
			Title: "Test PR",
			URL:   "https://github.com/org/repo/pull/1",
		}

		result, err := client.GetPullRequestDetails(pr)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if result.Title != pr.Title {
			t.Errorf("expected title %q, got %q", pr.Title, result.Title)
		}
		if result.URL != pr.URL {
			t.Errorf("expected URL %q, got %q", pr.URL, result.URL)
		}
	})

	t.Run("preserves all PR fields", func(t *testing.T) {
		pr := models.PullRequest{
			Title:       "Complex PR",
			URL:         "https://github.com/org/repo/pull/123",
			HeadRefName: "feature/test",
			Mergeable:   "MERGEABLE",
			Files: []models.File{
				{Path: "test.go", Additions: 10, Deletions: 5},
			},
			ReviewRequests: []models.ReviewRequest{
				{Type: "User", Login: "reviewer1"},
			},
		}

		result, err := client.GetPullRequestDetails(pr)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(result.Files) != len(pr.Files) {
			t.Errorf("expected %d files, got %d", len(pr.Files), len(result.Files))
		}
		if len(result.ReviewRequests) != len(pr.ReviewRequests) {
			t.Errorf("expected %d review requests, got %d", len(pr.ReviewRequests), len(result.ReviewRequests))
		}
	})
}

// TestMergePullRequest tests the behavior of merging pull requests.
func TestMergePullRequest(t *testing.T) {
	t.Run("merges PR without deleting branch", func(t *testing.T) {
		executor := &mockExecutor{captureArgs: true}
		client := NewClientWithExecutor(executor)

		err := client.MergePullRequest("https://github.com/org/repo/pull/123", false)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if !executor.wasCalled {
			t.Fatal("expected executor to be called")
		}
		if executor.lastCmd != "gh" {
			t.Errorf("expected command 'gh', got %q", executor.lastCmd)
		}

		expectedArgs := []string{"pr", "merge", "https://github.com/org/repo/pull/123"}
		if !sliceEqual(executor.lastArgs, expectedArgs) {
			t.Errorf("expected args %v, got %v", expectedArgs, executor.lastArgs)
		}
	})

	t.Run("merges PR with branch deletion", func(t *testing.T) {
		executor := &mockExecutor{captureArgs: true}
		client := NewClientWithExecutor(executor)

		err := client.MergePullRequest("https://github.com/org/repo/pull/456", true)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if !executor.wasCalled {
			t.Fatal("expected executor to be called")
		}

		expectedArgs := []string{"pr", "merge", "https://github.com/org/repo/pull/456", "--delete-branch"}
		if !sliceEqual(executor.lastArgs, expectedArgs) {
			t.Errorf("expected args %v, got %v", expectedArgs, executor.lastArgs)
		}
	})

	t.Run("returns error when merge fails", func(t *testing.T) {
		executor := &mockExecutor{shouldFail: true, output: "merge conflict detected"}
		client := NewClientWithExecutor(executor)

		err := client.MergePullRequest("https://github.com/org/repo/pull/789", false)
		if err == nil {
			t.Fatal("expected error when merge fails, got nil")
		}

		if !strings.Contains(err.Error(), "failed to merge PR") {
			t.Errorf("expected error to mention merge failure, got: %v", err)
		}
		if !strings.Contains(err.Error(), "https://github.com/org/repo/pull/789") {
			t.Errorf("expected error to include PR URL, got: %v", err)
		}
		if !strings.Contains(err.Error(), "merge conflict detected") {
			t.Errorf("expected error to include command output, got: %v", err)
		}
	})

	t.Run("handles different PR URL formats", func(t *testing.T) {
		executor := &mockExecutor{captureArgs: true}
		client := NewClientWithExecutor(executor)

		urls := []string{
			"https://github.com/org/repo/pull/1",
			"https://github.com/different-org/different-repo/pull/999",
			"1", // PR number only
		}

		for _, url := range urls {
			executor.wasCalled = false
			err := client.MergePullRequest(url, false)
			if err != nil {
				t.Errorf("unexpected error for URL %q: %v", url, err)
			}
			if !executor.wasCalled {
				t.Errorf("expected executor to be called for URL %q", url)
			}
		}
	})
}

// mockExecutor is a test helper that mocks command execution.
type mockExecutor struct {
	output      string
	shouldFail  bool
	captureArgs bool
	wasCalled   bool
	lastCmd     string
	lastArgs    []string
}

func (m *mockExecutor) Execute(cmd string, args ...string) (string, error) {
	if m.captureArgs {
		m.wasCalled = true
		m.lastCmd = cmd
		m.lastArgs = args
	}

	if m.shouldFail {
		return m.output, errors.New("command failed")
	}
	return m.output, nil
}

func (m *mockExecutor) ExecuteWithStdin(_ /* stdin */, cmd string, args ...string) (string, error) {
	// For testing purposes, just delegate to Execute
	return m.Execute(cmd, args...)
}

// Helper function to compare slices.
func sliceEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
