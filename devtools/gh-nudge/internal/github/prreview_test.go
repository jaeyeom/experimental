package github

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// mockCommandExecutor is a mock implementation of CommandExecutor for testing.
type mockCommandExecutor struct {
	// executeFunc is called when Execute is invoked
	executeFunc func(cmd string, args ...string) (string, error)
	// executeWithStdinFunc is called when ExecuteWithStdin is invoked
	executeWithStdinFunc func(stdin, cmd string, args ...string) (string, error)

	// callHistory records all calls made
	executeCalls         []executeCall
	executeWithStdinCall []executeWithStdinCall
}

type executeCall struct {
	cmd  string
	args []string
}

type executeWithStdinCall struct {
	stdin string
	cmd   string
	args  []string
}

func newMockCommandExecutor() *mockCommandExecutor {
	return &mockCommandExecutor{
		executeCalls:         make([]executeCall, 0),
		executeWithStdinCall: make([]executeWithStdinCall, 0),
	}
}

func (m *mockCommandExecutor) Execute(cmd string, args ...string) (string, error) {
	m.executeCalls = append(m.executeCalls, executeCall{cmd: cmd, args: args})
	if m.executeFunc != nil {
		return m.executeFunc(cmd, args...)
	}
	return "", nil
}

func (m *mockCommandExecutor) ExecuteWithStdin(stdin, cmd string, args ...string) (string, error) {
	m.executeWithStdinCall = append(m.executeWithStdinCall, executeWithStdinCall{
		stdin: stdin,
		cmd:   cmd,
		args:  args,
	})
	if m.executeWithStdinFunc != nil {
		return m.executeWithStdinFunc(stdin, cmd, args...)
	}
	return "", nil
}

func TestNewPRReviewClient(t *testing.T) {
	client := &Client{}
	prReviewClient := NewPRReviewClient(client)

	if prReviewClient == nil {
		t.Fatal("expected non-nil PRReviewClient")
		return
	}

	if prReviewClient.client != client {
		t.Error("expected client to be set correctly")
	}
}

func TestParsePatchToDiffHunks(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)

	tests := []struct {
		name     string
		filename string
		patch    string
		sha      string
		want     int // expected number of hunks
	}{
		{
			name:     "empty patch",
			filename: "test.go",
			patch:    "",
			sha:      "abc123",
			want:     0,
		},
		{
			name:     "single hunk with additions",
			filename: "test.go",
			patch: `@@ -1,3 +1,4 @@
 package main
+import "fmt"

 func main() {`,
			sha:  "abc123",
			want: 1, // RIGHT side only
		},
		{
			name:     "single hunk with deletions",
			filename: "test.go",
			patch: `@@ -1,4 +1,3 @@
 package main
-import "fmt"

 func main() {`,
			sha:  "abc123",
			want: 2, // RIGHT and LEFT sides
		},
		{
			name:     "multiple hunks",
			filename: "test.go",
			patch: `@@ -1,3 +1,4 @@
 package main
+import "fmt"

 func main() {
@@ -10,2 +11,3 @@
 	fmt.Println("hello")
+	fmt.Println("world")
 }`,
			sha:  "abc123",
			want: 2, // Two hunks, both RIGHT side
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hunks := prc.parsePatchToDiffHunks(tt.filename, tt.patch, tt.sha)
			if len(hunks) != tt.want {
				t.Errorf("expected %d hunks, got %d", tt.want, len(hunks))
			}

			// Verify all hunks have the correct filename and SHA
			for _, hunk := range hunks {
				if hunk.Location.Path != tt.filename {
					t.Errorf("expected filename %q, got %q", tt.filename, hunk.Location.Path)
				}
				if hunk.SHA != tt.sha {
					t.Errorf("expected SHA %q, got %q", tt.sha, hunk.SHA)
				}
			}
		})
	}
}

func TestParseHunkHeader(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)

	tests := []struct {
		name          string
		line          string
		wantLeftLine  int
		wantRightLine int
	}{
		{
			name:          "basic hunk header",
			line:          "@@ -1,3 +1,4 @@",
			wantLeftLine:  1,
			wantRightLine: 1,
		},
		{
			name:          "different start lines",
			line:          "@@ -10,5 +15,8 @@",
			wantLeftLine:  10,
			wantRightLine: 15,
		},
		{
			name:          "single line change",
			line:          "@@ -42 +42 @@",
			wantLeftLine:  42,
			wantRightLine: 42,
		},
		{
			name:          "invalid format",
			line:          "@@ invalid @@",
			wantLeftLine:  0,
			wantRightLine: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			leftLine, rightLine := prc.parseHunkHeader(tt.line)
			if leftLine != tt.wantLeftLine {
				t.Errorf("expected left line %d, got %d", tt.wantLeftLine, leftLine)
			}
			if rightLine != tt.wantRightLine {
				t.Errorf("expected right line %d, got %d", tt.wantRightLine, rightLine)
			}
		})
	}
}

func TestParseLineNumber(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)

	tests := []struct {
		name   string
		part   string
		prefix string
		want   int
	}{
		{
			name:   "single line number with minus",
			part:   "-10,5",
			prefix: "-",
			want:   10,
		},
		{
			name:   "single line number with plus",
			part:   "+15,8",
			prefix: "+",
			want:   15,
		},
		{
			name:   "no comma",
			part:   "+42",
			prefix: "+",
			want:   42,
		},
		{
			name:   "wrong prefix",
			part:   "+10",
			prefix: "-",
			want:   0,
		},
		{
			name:   "invalid number",
			part:   "+abc",
			prefix: "+",
			want:   0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := prc.parseLineNumber(tt.part, tt.prefix)
			if got != tt.want {
				t.Errorf("expected %d, got %d", tt.want, got)
			}
		})
	}
}

func TestCreateBidirectionalHunks(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)

	tests := []struct {
		name      string
		hunks     []models.DiffHunk
		wantCount int
	}{
		{
			name: "hunk with deletions",
			hunks: []models.DiffHunk{
				{
					Location: models.NewFileLocation("test.go", models.NewLineRange(1, 5)),
					Side:     models.SideRight,
					Content:  "@@ -1,3 +1,4 @@\n package main\n-import \"fmt\"\n",
					SHA:      "abc123",
				},
			},
			wantCount: 2, // RIGHT + LEFT
		},
		{
			name: "hunk without deletions",
			hunks: []models.DiffHunk{
				{
					Location: models.NewFileLocation("test.go", models.NewLineRange(1, 5)),
					Side:     models.SideRight,
					Content:  "@@ -1,3 +1,4 @@\n package main\n+import \"fmt\"\n",
					SHA:      "abc123",
				},
			},
			wantCount: 1, // RIGHT only
		},
		{
			name: "multiple hunks with mixed content",
			hunks: []models.DiffHunk{
				{
					Location: models.NewFileLocation("test.go", models.NewLineRange(1, 5)),
					Side:     models.SideRight,
					Content:  "@@ -1,3 +1,4 @@\n package main\n-import \"fmt\"\n",
					SHA:      "abc123",
				},
				{
					Location: models.NewFileLocation("test.go", models.NewLineRange(10, 15)),
					Side:     models.SideRight,
					Content:  "@@ -10,2 +11,3 @@\n+	fmt.Println(\"hello\")\n",
					SHA:      "abc123",
				},
			},
			wantCount: 3, // First hunk: RIGHT + LEFT, Second hunk: RIGHT only
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := prc.createBidirectionalHunks(tt.hunks)
			if len(result) != tt.wantCount {
				t.Errorf("expected %d hunks, got %d", tt.wantCount, len(result))
			}

			// Verify that LEFT hunks are created correctly
			for i, hunk := range result {
				if hunk.Side == "LEFT" {
					// Verify that the original hunk had deletions
					originalHunk := tt.hunks[0] // Simplified - assumes first hunk
					if !strings.Contains(originalHunk.Content, "\n-") {
						t.Errorf("hunk %d has LEFT side but no deletions in original", i)
					}
				}
			}
		})
	}
}

func TestSubmitReview(t *testing.T) {
	tests := []struct {
		name    string
		review  models.PRReview
		wantErr bool
	}{
		{
			name: "review with single line comment",
			review: models.PRReview{
				Body:  "LGTM",
				Event: "APPROVE",
				Comments: []models.Comment{
					{
						Path: "test.go",
						Line: models.NewSingleLine(10),
						Body: "Nice fix!",
						Side: models.SideRight,
						SHA:  "abc123",
					},
				},
			},
			wantErr: false,
		},
		{
			name: "review with multi-line comment",
			review: models.PRReview{
				Body:  "Some improvements needed",
				Event: "REQUEST_CHANGES",
				Comments: []models.Comment{
					{
						Path: "test.go",
						Line: models.NewLineRange(10, 15),
						Body: "This whole section needs work",
						Side: models.SideRight,
						SHA:  "abc123",
					},
				},
			},
			wantErr: false,
		},
		{
			name: "review with no comments",
			review: models.PRReview{
				Body:     "Looks good overall",
				Event:    "COMMENT",
				Comments: []models.Comment{},
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Test that the review payload can be marshaled correctly
			//
			// TODO: Investigate why this does not use GitHubComment struct.
			githubComments := make([]map[string]interface{}, len(tt.review.Comments))
			for i, comment := range tt.review.Comments {
				githubComment := map[string]interface{}{
					"path": comment.Path,
					"body": comment.Body,
					"side": comment.Side,
				}

				if comment.IsMultiLine() {
					githubComment["start_line"] = comment.Line.StartLine
					githubComment["line"] = comment.Line.EndLine
				} else {
					githubComment["line"] = comment.Line.EndLine
				}

				if comment.SHA != "" {
					githubComment["commit_id"] = comment.SHA
				}

				githubComments[i] = githubComment
			}

			reviewPayload := map[string]interface{}{
				"comments": githubComments,
			}

			if tt.review.Body != "" {
				reviewPayload["body"] = tt.review.Body
			}

			if tt.review.Event != "" {
				reviewPayload["event"] = tt.review.Event
			}

			// Verify JSON can be marshaled
			_, err := json.Marshal(reviewPayload)
			if (err != nil) != tt.wantErr {
				t.Errorf("json.Marshal() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestConvertGitHubComment(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)

	now := time.Now()

	tests := []struct {
		name      string
		ghComment GitHubComment
		wantErr   bool
		checkFunc func(*testing.T, models.Comment)
	}{
		{
			name: "single line comment",
			ghComment: GitHubComment{
				ID:        123,
				Path:      "test.go",
				Line:      intPtr(10),
				Body:      "Nice fix!",
				Side:      models.SideRight,
				CommitID:  "abc123",
				CreatedAt: now,
				UpdatedAt: now,
				User: struct {
					Login string `json:"login"`
				}{Login: "testuser"},
			},
			wantErr: false,
			checkFunc: func(t *testing.T, c models.Comment) {
				if c.Path != "test.go" {
					t.Errorf("expected path 'test.go', got %q", c.Path)
				}
				if c.Line != models.NewSingleLine(10) {
					t.Errorf("expected line 10, got %d", c.Line.EndLine)
				}
				if c.Side != models.SideRight {
					t.Errorf("expected side 'RIGHT', got %q", c.Side)
				}
				if c.Source != "github" {
					t.Errorf("expected source 'github', got %q", c.Source)
				}
				if c.GitHubID == nil || *c.GitHubID != 123 {
					t.Errorf("expected GitHubID 123, got %v", c.GitHubID)
				}
			},
		},
		{
			name: "multi-line comment",
			ghComment: GitHubComment{
				ID:        456,
				Path:      "test.go",
				StartLine: intPtr(5),
				Line:      intPtr(10),
				Body:      "This whole section needs work",
				Side:      models.SideRight,
				CommitID:  "def456",
				CreatedAt: now,
				UpdatedAt: now,
				User: struct {
					Login string `json:"login"`
				}{Login: "testuser"},
			},
			wantErr: false,
			checkFunc: func(t *testing.T, c models.Comment) {
				if !c.IsMultiLine() {
					t.Error("expected multi-line comment")
				}
				if c.Line.StartLine != 5 {
					t.Errorf("expected start line 5, got %d", c.Line.StartLine)
				}
				if c.Line.EndLine != 10 {
					t.Errorf("expected line 10, got %d", c.Line.EndLine)
				}
			},
		},
		{
			name: "comment with original line",
			ghComment: GitHubComment{
				ID:           789,
				Path:         "test.go",
				OriginalLine: intPtr(15),
				Body:         "Outdated comment",
				Side:         "left",
				CommitID:     "ghi789",
				CreatedAt:    now,
				UpdatedAt:    now,
				User: struct {
					Login string `json:"login"`
				}{Login: "testuser"},
			},
			wantErr: false,
			checkFunc: func(t *testing.T, c models.Comment) {
				if c.Line.EndLine != 15 {
					t.Errorf("expected line 15, got %d", c.Line.EndLine)
				}
			},
		},
		{
			name: "comment with no line number",
			ghComment: GitHubComment{
				ID:        999,
				Path:      "test.go",
				Body:      "General comment",
				Side:      models.SideRight,
				CommitID:  "xyz999",
				CreatedAt: now,
				UpdatedAt: now,
				User: struct {
					Login string `json:"login"`
				}{Login: "testuser"},
			},
			wantErr: true,
		},
		{
			name: "comment with invalid side",
			ghComment: GitHubComment{
				ID:        111,
				Path:      "test.go",
				Line:      intPtr(5),
				Body:      "Comment with invalid side",
				Side:      models.SideUnspecified, // unspecified side should default to RIGHT
				CommitID:  "aaa111",
				CreatedAt: now,
				UpdatedAt: now,
				User: struct {
					Login string `json:"login"`
				}{Login: "testuser"},
			},
			wantErr: false,
			checkFunc: func(t *testing.T, c models.Comment) {
				// Should default to RIGHT
				if c.Side != models.SideRight {
					t.Errorf("expected side 'RIGHT' (default), got %q", c.Side)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment, err := prc.convertGitHubComment(tt.ghComment)
			if (err != nil) != tt.wantErr {
				t.Errorf("convertGitHubComment() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if !tt.wantErr && tt.checkFunc != nil {
				tt.checkFunc(t, comment)
			}

			// Common checks for all successful conversions
			if err == nil {
				if comment.Status != models.StatusUnresolved {
					t.Errorf("expected status 'unresolved', got %q", comment.Status)
				}
				if comment.LastSynced == nil {
					t.Error("expected LastSynced to be set")
				}
				if comment.SyncStatus != "synced" {
					t.Errorf("expected sync status 'synced', got %q", comment.SyncStatus)
				}
			}
		})
	}
}

func TestCreatePendingReview(t *testing.T) {
	// This test verifies that CreatePendingReview creates a review with no event
	comments := []models.Comment{
		{
			Path: "test.go",
			Line: models.NewSingleLine(10),
			Body: "Test comment",
			Side: models.SideRight,
			SHA:  "abc123",
		},
	}
	body := "Draft review"

	// We can't directly test the function without mocking gh CLI,
	// but we can verify the expected behavior by checking the review structure
	review := models.PRReview{
		Body:     body,
		Comments: comments,
		// Event is omitted
	}

	if review.Event != "" {
		t.Errorf("expected empty event for pending review, got %q", review.Event)
	}
	if review.Body != body {
		t.Errorf("expected body %q, got %q", body, review.Body)
	}
	if len(review.Comments) != len(comments) {
		t.Errorf("expected %d comments, got %d", len(comments), len(review.Comments))
	}
}

func TestUpdateLineNumbers(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)

	tests := []struct {
		name            string
		line            string
		rightLineNum    int
		leftLineNum     int
		wantRightLine   int
		wantLeftLine    int
		wantHunkEndLine int
	}{
		{
			name:            "addition line",
			line:            "+import \"fmt\"",
			rightLineNum:    10,
			leftLineNum:     5,
			wantRightLine:   11,
			wantLeftLine:    5,
			wantHunkEndLine: 10,
		},
		{
			name:            "deletion line",
			line:            "-import \"fmt\"",
			rightLineNum:    10,
			leftLineNum:     5,
			wantRightLine:   10,
			wantLeftLine:    6,
			wantHunkEndLine: 0, // EndLine not updated on deletions
		},
		{
			name:            "context line",
			line:            " package main",
			rightLineNum:    10,
			leftLineNum:     5,
			wantRightLine:   11,
			wantLeftLine:    6,
			wantHunkEndLine: 10,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hunk := &models.DiffHunk{
				Location: models.NewFileLocation("test.go", models.NewLineRange(1, 0)),
				Side:     models.SideRight,
			}

			gotRightLine, gotLeftLine := prc.updateLineNumbers(
				tt.line,
				tt.rightLineNum,
				tt.leftLineNum,
				hunk,
			)

			if gotRightLine != tt.wantRightLine {
				t.Errorf("expected right line %d, got %d", tt.wantRightLine, gotRightLine)
			}
			if gotLeftLine != tt.wantLeftLine {
				t.Errorf("expected left line %d, got %d", tt.wantLeftLine, gotLeftLine)
			}

			// Check if EndLine was updated correctly
			if strings.HasPrefix(tt.line, "+") || strings.HasPrefix(tt.line, " ") {
				if hunk.Location.Lines.EndLine != tt.wantHunkEndLine {
					t.Errorf("expected hunk end line %d, got %d", tt.wantHunkEndLine, hunk.Location.Lines.EndLine)
				}
			}
		})
	}
}

func TestCreateNewHunk(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)

	filename := "test.go"
	sha := "abc123"
	line := "@@ -1,3 +1,4 @@"
	rightLineNum := 10

	hunk := prc.createNewHunk(filename, sha, line, rightLineNum)

	if hunk.Location.Path != filename {
		t.Errorf("expected file %q, got %q", filename, hunk.Location.Path)
	}
	if hunk.SHA != sha {
		t.Errorf("expected SHA %q, got %q", sha, hunk.SHA)
	}
	if hunk.Location.Lines != models.NewSingleLine(rightLineNum) {
		t.Errorf("expected line %d, got %v", rightLineNum, hunk.Location.Lines)
	}
	if hunk.Side != models.SideRight {
		t.Errorf("expected side 'RIGHT', got %q", hunk.Side)
	}
	if !strings.Contains(hunk.Content, line) {
		t.Errorf("expected content to contain %q, got %q", line, hunk.Content)
	}
}

// Helper function to create int pointer.
func intPtr(i int) *int {
	return &i
}

// TestGetPRDiffBehavior tests the behavior of fetching PR diff with full mocking.
func TestGetPRDiffBehavior(t *testing.T) {
	repo := models.NewRepository("owner", "repo")
	prNumber := 42

	t.Run("successfully fetches and parses PR diff", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, args ...string) (string, error) {
			// Respond to /pulls/{number}/files request
			if len(args) >= 2 && strings.Contains(args[1], "/files") {
				files := []File{
					{
						Filename: "test.go",
						Patch: `@@ -1,3 +1,4 @@
 package main
+import "fmt"
 func main() {`,
						SHA:       "file123",
						Status:    "modified",
						Additions: 1,
					},
				}
				data, _ := json.Marshal(files)
				return string(data), nil
			}
			// Respond to /pulls/{number} request for PR info
			if len(args) >= 2 && strings.Contains(args[1], fmt.Sprintf("/pulls/%d", prNumber)) && !strings.Contains(args[1], "/files") {
				pr := PR{Number: prNumber, Title: "Test PR"}
				pr.Head.SHA = "head123"
				pr.Base.SHA = "base456"
				data, _ := json.Marshal(pr)
				return string(data), nil
			}
			return "", fmt.Errorf("unexpected call")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRDiff(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result.PRNumber != prNumber {
			t.Errorf("expected PR number %d, got %d", prNumber, result.PRNumber)
		}
		if result.CommitSHA != "head123" {
			t.Errorf("expected commit SHA head123, got %s", result.CommitSHA)
		}
		if result.BaseSHA != "base456" {
			t.Errorf("expected base SHA base456, got %s", result.BaseSHA)
		}
		if len(result.DiffHunks) == 0 {
			t.Error("expected at least one diff hunk")
		}
	})

	t.Run("returns error when PR files fetch fails", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "", fmt.Errorf("API error: not found")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetPRDiff(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to fetch PR files") {
			t.Errorf("unexpected error message: %v", err)
		}
	})

	t.Run("returns error when PR info fetch fails", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, args ...string) (string, error) {
			// Files request succeeds
			if strings.Contains(args[1], "/files") {
				return "[]", nil
			}
			// PR info request fails
			return "", fmt.Errorf("PR info error")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetPRDiff(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to get PR info") {
			t.Errorf("unexpected error message: %v", err)
		}
	})

	t.Run("handles PR with no files", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, args ...string) (string, error) {
			if strings.Contains(args[1], "/files") {
				return "[]", nil // Empty file list
			}
			pr := PR{Number: prNumber}
			pr.Head.SHA = "head123"
			pr.Base.SHA = "base456"
			data, _ := json.Marshal(pr)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRDiff(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result.DiffHunks) != 0 {
			t.Errorf("expected empty diff hunks for PR with no files, got %d", len(result.DiffHunks))
		}
	})

	t.Run("handles files with no patches", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, args ...string) (string, error) {
			if strings.Contains(args[1], "/files") {
				files := []File{
					{
						Filename: "binary.png",
						Patch:    "", // No patch for binary files
						SHA:      "bin123",
						Status:   "added",
					},
				}
				data, _ := json.Marshal(files)
				return string(data), nil
			}
			pr := PR{Number: prNumber}
			pr.Head.SHA = "head123"
			pr.Base.SHA = "base456"
			data, _ := json.Marshal(pr)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRDiff(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Files with no patch should result in no hunks
		if len(result.DiffHunks) != 0 {
			t.Errorf("expected no hunks for files without patches, got %d", len(result.DiffHunks))
		}
	})

	t.Run("preserves repository and PR context in result", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, args ...string) (string, error) {
			if strings.Contains(args[1], "/files") {
				return "[]", nil
			}
			pr := PR{Number: prNumber, Title: "Context Test"}
			pr.Head.SHA = "context123"
			pr.Base.SHA = "context456"
			data, _ := json.Marshal(pr)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRDiff(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result.Repository.Owner != "owner" || result.Repository.Name != "repo" {
			t.Errorf("repository not preserved: got %v", result.Repository)
		}
		if result.PRNumber != prNumber {
			t.Errorf("PR number not preserved: got %d", result.PRNumber)
		}
		if result.CommitSHA != "context123" || result.BaseSHA != "context456" {
			t.Errorf("commit SHAs not preserved: got %s, %s", result.CommitSHA, result.BaseSHA)
		}
		if result.CapturedAt.IsZero() {
			t.Error("timestamp not set")
		}
	})
}

// TestGetPRInfoBehavior tests the behavior of fetching PR information with mocking.
func TestGetPRInfoBehavior(t *testing.T) {
	repo := models.NewRepository("testowner", "testrepo")
	prNumber := 99

	t.Run("successfully fetches basic PR information", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			pr := PR{
				Number: prNumber,
				Title:  "Test Pull Request",
			}
			pr.Head.SHA = "headsha123"
			pr.Base.SHA = "basesha456"
			data, _ := json.Marshal(pr)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRInfo(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result.Number != prNumber {
			t.Errorf("expected number %d, got %d", prNumber, result.Number)
		}
		if result.Title != "Test Pull Request" {
			t.Errorf("expected title 'Test Pull Request', got %s", result.Title)
		}
		if result.Head.SHA != "headsha123" {
			t.Errorf("expected head SHA headsha123, got %s", result.Head.SHA)
		}
		if result.Base.SHA != "basesha456" {
			t.Errorf("expected base SHA basesha456, got %s", result.Base.SHA)
		}
	})

	t.Run("returns error when PR does not exist", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "", fmt.Errorf("HTTP 404: Not Found")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetPRInfo(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to fetch PR info") {
			t.Errorf("unexpected error message: %v", err)
		}
	})

	t.Run("returns error when API call fails", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "", fmt.Errorf("network error: connection refused")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetPRInfo(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
	})

	t.Run("returns error when JSON response is malformed", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "{invalid json", nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetPRInfo(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to parse PR info response") {
			t.Errorf("unexpected error message: %v", err)
		}
	})
}

// TestValidatePRAccessBehavior tests the behavior of validating PR access with mocking.
func TestValidatePRAccessBehavior(t *testing.T) {
	repo := models.NewRepository("owner", "repo")
	prNumber := 100

	t.Run("returns nil when PR is accessible", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			pr := PR{Number: prNumber, Title: "Accessible PR"}
			data, _ := json.Marshal(pr)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		err := prClient.ValidatePRAccess(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("returns error when PR is not accessible", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "", fmt.Errorf("HTTP 403: Forbidden")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		err := prClient.ValidatePRAccess(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
	})

	t.Run("delegates to GetPRInfo", func(t *testing.T) {
		mock := newMockCommandExecutor()
		callCount := 0
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			callCount++
			pr := PR{Number: prNumber}
			data, _ := json.Marshal(pr)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_ = prClient.ValidatePRAccess(repo, prNumber)
		if callCount != 1 {
			t.Errorf("expected GetPRInfo to be called once, got %d calls", callCount)
		}
	})
}

// TestSubmitReviewComprehensive tests the behavior of submitting reviews with full mocking.
func TestSubmitReviewComprehensive(t *testing.T) {
	repo := models.NewRepository("owner", "repo")
	prNumber := 50

	t.Run("submits review with single-line comments", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			comments := payload["comments"].([]interface{})
			comment := comments[0].(map[string]interface{})
			if _, hasStartLine := comment["start_line"]; hasStartLine {
				t.Error("single-line comment should not have start_line field")
			}
			if line, ok := comment["line"]; !ok || line != float64(10) {
				t.Errorf("expected line field with value 10, got %v", comment["line"])
			}
			return `{"id": 1}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		review := models.PRReview{
			Comments: []models.Comment{
				{
					Path: "test.go",
					Line: models.NewSingleLine(10),
					Body: "Single line comment",
					Side: models.SideRight,
				},
			},
		}

		err := prClient.SubmitReview(repo, prNumber, review)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("submits review with multi-line comments", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			comments := payload["comments"].([]interface{})
			comment := comments[0].(map[string]interface{})
			if startLine, ok := comment["start_line"]; !ok || startLine != float64(5) {
				t.Errorf("expected start_line field with value 5, got %v", comment["start_line"])
			}
			if line, ok := comment["line"]; !ok || line != float64(10) {
				t.Errorf("expected line field with value 10, got %v", comment["line"])
			}
			return `{"id": 2}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		review := models.PRReview{
			Comments: []models.Comment{
				{
					Path: "test.go",
					Line: models.NewLineRange(5, 10),
					Body: "Multi-line comment",
					Side: models.SideRight,
				},
			},
		}

		err := prClient.SubmitReview(repo, prNumber, review)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("includes commit SHA when provided", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			comments := payload["comments"].([]interface{})
			comment := comments[0].(map[string]interface{})
			if commitID, ok := comment["commit_id"]; !ok || commitID != "sha123" {
				t.Errorf("expected commit_id field with value sha123, got %v", comment["commit_id"])
			}
			return `{"id": 3}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		review := models.PRReview{
			Comments: []models.Comment{
				{
					Path: "test.go",
					Line: models.NewSingleLine(5),
					Body: "Comment with SHA",
					Side: models.SideRight,
					SHA:  "sha123",
				},
			},
		}

		err := prClient.SubmitReview(repo, prNumber, review)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("includes review body when provided", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			if body, ok := payload["body"]; !ok || body != "Great work!" {
				t.Errorf("expected body field with value 'Great work!', got %v", payload["body"])
			}
			return `{"id": 4}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		review := models.PRReview{
			Body: "Great work!",
			Comments: []models.Comment{
				{
					Path: "test.go",
					Line: models.NewSingleLine(1),
					Body: "Nice",
					Side: models.SideRight,
				},
			},
		}

		err := prClient.SubmitReview(repo, prNumber, review)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("includes review event when provided", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			if event, ok := payload["event"]; !ok || event != "APPROVE" {
				t.Errorf("expected event field with value 'APPROVE', got %v", payload["event"])
			}
			return `{"id": 5}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		review := models.PRReview{
			Event: "APPROVE",
			Comments: []models.Comment{
				{
					Path: "test.go",
					Line: models.NewSingleLine(1),
					Body: "LGTM",
					Side: models.SideRight,
				},
			},
		}

		err := prClient.SubmitReview(repo, prNumber, review)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("returns error when gh command fails", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(_ string, _ string, _ ...string) (string, error) {
			return "validation failed", fmt.Errorf("API error")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		review := models.PRReview{
			Comments: []models.Comment{
				{
					Path: "test.go",
					Line: models.NewSingleLine(1),
					Body: "test",
					Side: models.SideRight,
				},
			},
		}

		err := prClient.SubmitReview(repo, prNumber, review)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to submit review") {
			t.Errorf("unexpected error message: %v", err)
		}
	})

	t.Run("handles review with no comments", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			comments := payload["comments"].([]interface{})
			if len(comments) != 0 {
				t.Errorf("expected 0 comments, got %d", len(comments))
			}
			return `{"id": 6}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		review := models.PRReview{
			Body:     "Looks good overall",
			Event:    "COMMENT",
			Comments: []models.Comment{},
		}

		err := prClient.SubmitReview(repo, prNumber, review)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})
}

// TestGetPRCommentsBehavior tests the behavior of fetching PR comments with mocking.
func TestGetPRCommentsBehavior(t *testing.T) {
	repo := models.NewRepository("owner", "repo")
	prNumber := 60
	now := time.Now()

	t.Run("fetches and converts all PR comments", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			line := 10
			comments := []GitHubComment{
				{
					ID:        100,
					Path:      "main.go",
					Line:      &line,
					Body:      "Test comment",
					Side:      models.SideRight,
					CommitID:  "sha123",
					CreatedAt: now,
					UpdatedAt: now,
				},
			}
			data, _ := json.Marshal(comments)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRComments(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result) != 1 {
			t.Errorf("expected 1 comment, got %d", len(result))
		}
		if result[0].Path != "main.go" {
			t.Errorf("expected path main.go, got %s", result[0].Path)
		}
	})

	t.Run("returns error when API call fails", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "", fmt.Errorf("API error")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetPRComments(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to fetch PR comments") {
			t.Errorf("unexpected error: %v", err)
		}
	})

	t.Run("skips comments that fail conversion with warning", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			line := 5
			comments := []GitHubComment{
				{
					ID:        101,
					Path:      "good.go",
					Line:      &line,
					Body:      "Valid comment",
					Side:      models.SideRight,
					CommitID:  "sha",
					CreatedAt: now,
					UpdatedAt: now,
				},
				{
					ID:   102,
					Path: "bad.go",
					// Missing Line field - should be skipped
					Body:      "Invalid comment",
					Side:      models.SideRight,
					CommitID:  "sha",
					CreatedAt: now,
					UpdatedAt: now,
				},
			}
			data, _ := json.Marshal(comments)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRComments(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Should only get the valid comment
		if len(result) != 1 {
			t.Errorf("expected 1 valid comment, got %d", len(result))
		}
		if result[0].Path != "good.go" {
			t.Errorf("expected valid comment from good.go, got %s", result[0].Path)
		}
	})

	t.Run("handles empty comment list", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "[]", nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRComments(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result) != 0 {
			t.Errorf("expected empty list, got %d comments", len(result))
		}
	})

	t.Run("sets source and sync metadata on converted comments", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			line := 20
			comments := []GitHubComment{
				{
					ID:        200,
					Path:      "test.go",
					Line:      &line,
					Body:      "Comment",
					Side:      models.SideRight,
					CommitID:  "sha",
					CreatedAt: now,
					UpdatedAt: now,
				},
			}
			data, _ := json.Marshal(comments)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRComments(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result) != 1 {
			t.Fatalf("expected 1 comment, got %d", len(result))
		}
		if result[0].Source != "github" {
			t.Errorf("expected source 'github', got %s", result[0].Source)
		}
		if result[0].LastSynced == nil {
			t.Error("expected LastSynced to be set")
		}
		if result[0].SyncStatus != "synced" {
			t.Errorf("expected SyncStatus 'synced', got %s", result[0].SyncStatus)
		}
	})
}

// TestGetPRReviewCommentsBehavior tests the behavior of fetching review comments with mocking.
func TestGetPRReviewCommentsBehavior(t *testing.T) {
	repo := models.NewRepository("owner", "repo")
	prNumber := 70
	now := time.Now()

	t.Run("fetches comments from all reviews", func(t *testing.T) {
		mock := newMockCommandExecutor()
		callCount := 0
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			callCount++
			// First call: get reviews
			if callCount == 1 {
				reviews := []map[string]interface{}{
					{"id": float64(1)},
					{"id": float64(2)},
				}
				data, _ := json.Marshal(reviews)
				return string(data), nil
			}
			// Subsequent calls: return comments for each review
			line := 10
			comments := []GitHubComment{
				{
					ID:        300,
					Path:      "file.go",
					Line:      &line,
					Body:      "Review comment",
					Side:      models.SideRight,
					CommitID:  "sha",
					CreatedAt: now,
					UpdatedAt: now,
				},
			}
			data, _ := json.Marshal(comments)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRReviewComments(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Should have 2 comments (one from each review)
		if len(result) != 2 {
			t.Errorf("expected 2 comments, got %d", len(result))
		}
	})

	t.Run("returns error when GetExistingReviews fails", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "", fmt.Errorf("reviews API error")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetPRReviewComments(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to get reviews") {
			t.Errorf("unexpected error: %v", err)
		}
	})

	t.Run("handles PRs with no reviews", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "[]", nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetPRReviewComments(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result) != 0 {
			t.Errorf("expected empty list, got %d comments", len(result))
		}
	})
}

// TestCreatePendingReviewComprehensive tests the behavior of creating pending reviews with mocking.
func TestCreatePendingReviewComprehensive(t *testing.T) {
	repo := models.NewRepository("owner", "repo")
	prNumber := 80

	t.Run("creates review without event field", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			if _, hasEvent := payload["event"]; hasEvent {
				t.Error("pending review should not have event field")
			}
			return `{"id": 100}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		comments := []models.Comment{
			{
				Path: "test.go",
				Line: models.NewSingleLine(5),
				Body: "Pending comment",
				Side: models.SideRight,
			},
		}

		err := prClient.CreatePendingReview(repo, prNumber, comments, "Draft body")
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})

	t.Run("includes comments and body", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeWithStdinFunc = func(stdin, _ string, _ ...string) (string, error) {
			var payload map[string]interface{}
			if err := json.Unmarshal([]byte(stdin), &payload); err != nil {
				return "", fmt.Errorf("failed to parse payload: %w", err)
			}
			if body, ok := payload["body"]; !ok || body != "My draft" {
				t.Errorf("expected body 'My draft', got %v", payload["body"])
			}
			comments := payload["comments"].([]interface{})
			if len(comments) != 1 {
				t.Errorf("expected 1 comment, got %d", len(comments))
			}
			return `{"id": 101}`, nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		comments := []models.Comment{
			{
				Path: "file.go",
				Line: models.NewSingleLine(10),
				Body: "Test",
				Side: models.SideRight,
			},
		}

		err := prClient.CreatePendingReview(repo, prNumber, comments, "My draft")
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	})
}

// TestGetExistingReviewsBehavior tests the behavior of fetching existing reviews with mocking.
func TestGetExistingReviewsBehavior(t *testing.T) {
	repo := models.NewRepository("owner", "repo")
	prNumber := 90

	t.Run("fetches all reviews for a PR", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			reviews := []map[string]interface{}{
				{
					"id":    float64(1),
					"state": "APPROVED",
					"body":  "LGTM",
				},
				{
					"id":    float64(2),
					"state": "COMMENTED",
					"body":  "Some comments",
				},
			}
			data, _ := json.Marshal(reviews)
			return string(data), nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetExistingReviews(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result) != 2 {
			t.Errorf("expected 2 reviews, got %d", len(result))
		}
		if result[0]["state"] != "APPROVED" {
			t.Errorf("expected first review state APPROVED, got %v", result[0]["state"])
		}
	})

	t.Run("returns error when API call fails", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "", fmt.Errorf("network error")
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetExistingReviews(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to fetch existing reviews") {
			t.Errorf("unexpected error: %v", err)
		}
	})

	t.Run("returns error when JSON is malformed", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "[{invalid json", nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		_, err := prClient.GetExistingReviews(repo, prNumber)
		if err == nil {
			t.Fatal("expected error, got nil")
		}
		if !strings.Contains(err.Error(), "failed to parse reviews response") {
			t.Errorf("unexpected error: %v", err)
		}
	})

	t.Run("handles PRs with no reviews", func(t *testing.T) {
		mock := newMockCommandExecutor()
		mock.executeFunc = func(_ string, _ ...string) (string, error) {
			return "[]", nil
		}

		client := NewClientWithExecutor(mock)
		prClient := NewPRReviewClient(client)

		result, err := prClient.GetExistingReviews(repo, prNumber)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result) != 0 {
			t.Errorf("expected empty list, got %d reviews", len(result))
		}
	})
}

// TestConvertGitHubCommentBehaviors tests comment conversion behaviors.
func TestConvertGitHubCommentBehaviors(t *testing.T) {
	client := &Client{}
	prc := NewPRReviewClient(client)
	now := time.Now()

	t.Run("normalizes side to uppercase", func(t *testing.T) {
		// Behavior: Side values are now typed - lowercase "left" gets converted to SideLeft constant
		ghComment := GitHubComment{
			ID:        123,
			Path:      "test.go",
			Line:      intPtr(10),
			Body:      "Test",
			Side:      models.SideLeft, // Now using typed constant
			CommitID:  "abc",
			CreatedAt: now,
			UpdatedAt: now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.Side != models.SideLeft {
			t.Errorf("expected side to be LEFT, got %q", comment.Side)
		}
	})

	t.Run("defaults invalid side to RIGHT", func(t *testing.T) {
		// Behavior: Unspecified side values should default to RIGHT for safety
		ghComment := GitHubComment{
			ID:        123,
			Path:      "test.go",
			Line:      intPtr(10),
			Body:      "Test",
			Side:      models.SideUnspecified, // unspecified defaults to RIGHT
			CommitID:  "abc",
			CreatedAt: now,
			UpdatedAt: now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.Side != models.SideRight {
			t.Errorf("expected unspecified side to default to RIGHT, got %q", comment.Side)
		}
	})

	t.Run("prefers Line over OriginalLine", func(t *testing.T) {
		// Behavior: When both Line and OriginalLine are present, Line should be used
		ghComment := GitHubComment{
			ID:           123,
			Path:         "test.go",
			Line:         intPtr(10),
			OriginalLine: intPtr(5),
			Body:         "Test",
			Side:         models.SideRight,
			CommitID:     "abc",
			CreatedAt:    now,
			UpdatedAt:    now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.Line.EndLine != 10 {
			t.Errorf("expected Line (10) to be preferred over OriginalLine (5), got %d", comment.Line.EndLine)
		}
	})

	t.Run("uses OriginalLine when Line is nil", func(t *testing.T) {
		// Behavior: Fallback to OriginalLine when Line is not present
		ghComment := GitHubComment{
			ID:           123,
			Path:         "test.go",
			OriginalLine: intPtr(15),
			Body:         "Test",
			Side:         models.SideLeft,
			CommitID:     "abc",
			CreatedAt:    now,
			UpdatedAt:    now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.Line.EndLine != 15 {
			t.Errorf("expected OriginalLine to be used when Line is nil, got %d", comment.Line.EndLine)
		}
	})

	t.Run("creates multi-line range when StartLine differs from Line", func(t *testing.T) {
		// Behavior: When StartLine is present and differs, create a multi-line range
		startLine := 5
		endLine := 10
		ghComment := GitHubComment{
			ID:        123,
			Path:      "test.go",
			StartLine: &startLine,
			Line:      &endLine,
			Body:      "Multi-line comment",
			Side:      models.SideRight,
			CommitID:  "abc",
			CreatedAt: now,
			UpdatedAt: now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if !comment.IsMultiLine() {
			t.Error("expected comment to be multi-line")
		}
		if comment.Line.StartLine != 5 || comment.Line.EndLine != 10 {
			t.Errorf("expected range 5-10, got %d-%d", comment.Line.StartLine, comment.Line.EndLine)
		}
	})

	t.Run("creates single-line range when StartLine equals Line", func(t *testing.T) {
		// Behavior: When StartLine equals Line, treat as single-line comment
		line := 10
		ghComment := GitHubComment{
			ID:        123,
			Path:      "test.go",
			StartLine: &line,
			Line:      &line,
			Body:      "Single-line comment",
			Side:      models.SideRight,
			CommitID:  "abc",
			CreatedAt: now,
			UpdatedAt: now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.IsMultiLine() {
			t.Error("expected comment to be single-line")
		}
		if comment.Line.StartLine != 10 || comment.Line.EndLine != 10 {
			t.Errorf("expected single line 10, got %d-%d", comment.Line.StartLine, comment.Line.EndLine)
		}
	})

	t.Run("sets OriginalRange when original position differs", func(t *testing.T) {
		// Behavior: When OriginalStartLine/OriginalLine differ from current position,
		// store the original range
		ghComment := GitHubComment{
			ID:                123,
			Path:              "test.go",
			StartLine:         intPtr(10),
			Line:              intPtr(15),
			OriginalStartLine: intPtr(5),
			OriginalLine:      intPtr(8),
			Body:              "Moved comment",
			Side:              models.SideRight,
			CommitID:          "abc",
			CreatedAt:         now,
			UpdatedAt:         now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.OriginalRange == nil {
			t.Fatal("expected OriginalRange to be set")
		}
		if comment.OriginalRange.StartLine != 5 || comment.OriginalRange.EndLine != 8 {
			t.Errorf("expected original range 5-8, got %d-%d",
				comment.OriginalRange.StartLine, comment.OriginalRange.EndLine)
		}
	})

	t.Run("sets status to unresolved by default", func(t *testing.T) {
		// Behavior: All converted comments should start as unresolved
		ghComment := GitHubComment{
			ID:        123,
			Path:      "test.go",
			Line:      intPtr(10),
			Body:      "Test",
			Side:      models.SideRight,
			CommitID:  "abc",
			CreatedAt: now,
			UpdatedAt: now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.Status != models.StatusUnresolved {
			t.Errorf("expected status to be unresolved, got %q", comment.Status)
		}
	})

	t.Run("sets GitHub ID and sync metadata", func(t *testing.T) {
		// Behavior: Converted comments should track their GitHub origin
		ghComment := GitHubComment{
			ID:        456,
			Path:      "test.go",
			Line:      intPtr(10),
			Body:      "Test",
			Side:      models.SideRight,
			CommitID:  "abc",
			CreatedAt: now,
			UpdatedAt: now,
		}

		comment, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment.GitHubID == nil || *comment.GitHubID != 456 {
			t.Errorf("expected GitHubID to be 456, got %v", comment.GitHubID)
		}
		if comment.Source != "github" {
			t.Errorf("expected source to be 'github', got %q", comment.Source)
		}
		if comment.LastSynced == nil {
			t.Error("expected LastSynced to be set")
		}
		if comment.SyncStatus != "synced" {
			t.Errorf("expected SyncStatus to be 'synced', got %q", comment.SyncStatus)
		}
	})

	t.Run("generates new local ID for comment", func(t *testing.T) {
		// Behavior: Each converted comment should get a new local ID (not GitHub ID)
		ghComment := GitHubComment{
			ID:        123,
			Path:      "test.go",
			Line:      intPtr(10),
			Body:      "Test",
			Side:      models.SideRight,
			CommitID:  "abc",
			CreatedAt: now,
			UpdatedAt: now,
		}

		comment1, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		comment2, err := prc.convertGitHubComment(ghComment)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if comment1.ID == comment2.ID {
			t.Error("expected different local IDs for different conversions")
		}
		if len(comment1.ID) != 40 {
			t.Errorf("expected 40-character ID, got %d characters", len(comment1.ID))
		}
	})
}

// TestGitHubCommentGetLineRange tests the GetLineRange method.
func TestGitHubCommentGetLineRange(t *testing.T) {
	intPtr := func(i int) *int { return &i }

	tests := []struct {
		name      string
		comment   GitHubComment
		want      models.LineRange
		wantError bool
		about     string
	}{
		{
			name: "Single line comment with Line field",
			comment: GitHubComment{
				Line: intPtr(42),
			},
			want:  models.NewSingleLine(42),
			about: "Should use Line field for single-line comment",
		},
		{
			name: "Multi-line comment with StartLine and Line",
			comment: GitHubComment{
				StartLine: intPtr(10),
				Line:      intPtr(20),
			},
			want:  models.NewLineRange(10, 20),
			about: "Should create multi-line range when StartLine differs from Line",
		},
		{
			name: "Single line with StartLine equal to Line",
			comment: GitHubComment{
				StartLine: intPtr(15),
				Line:      intPtr(15),
			},
			want:  models.NewSingleLine(15),
			about: "Should treat as single line when StartLine equals Line",
		},
		{
			name: "Falls back to OriginalLine when Line is nil",
			comment: GitHubComment{
				OriginalLine: intPtr(30),
			},
			want:  models.NewSingleLine(30),
			about: "Should use OriginalLine as fallback when Line is not set",
		},
		{
			name: "Prefers Line over OriginalLine",
			comment: GitHubComment{
				Line:         intPtr(50),
				OriginalLine: intPtr(40),
			},
			want:  models.NewSingleLine(50),
			about: "Should prefer Line field over OriginalLine when both are set",
		},
		{
			name: "Multi-line using StartLine and OriginalLine",
			comment: GitHubComment{
				StartLine:    intPtr(5),
				OriginalLine: intPtr(15),
			},
			want:  models.NewLineRange(5, 15),
			about: "Should create range using OriginalLine when Line is not set",
		},
		{
			name:      "Error when no line information",
			comment:   GitHubComment{},
			wantError: true,
			about:     "Should return error when neither Line nor OriginalLine is set",
		},
		{
			name: "Only StartLine is set (uses OriginalLine fallback path)",
			comment: GitHubComment{
				StartLine: intPtr(10),
			},
			wantError: true,
			about:     "Should error when only StartLine is set without end line",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := tt.comment.GetLineRange()

			if tt.wantError {
				if err == nil {
					t.Errorf("GetLineRange() expected error, got nil\nAbout: %s", tt.about)
				}
				return
			}

			if err != nil {
				t.Errorf("GetLineRange() unexpected error: %v\nAbout: %s", err, tt.about)
				return
			}

			if got != tt.want {
				t.Errorf("GetLineRange() = %v, want %v\nAbout: %s", got, tt.want, tt.about)
			}
		})
	}

	t.Run("Error message is informative", func(t *testing.T) {
		comment := GitHubComment{}
		_, err := comment.GetLineRange()

		if err == nil {
			t.Fatal("expected error when no line information")
		}

		errMsg := err.Error()
		if !strings.Contains(errMsg, "no line number") {
			t.Errorf("error message should mention 'no line number', got: %v", errMsg)
		}
	})
}

// TestGitHubCommentGetOriginalLineRange tests the GetOriginalLineRange method.
func TestGitHubCommentGetOriginalLineRange(t *testing.T) {
	intPtr := func(i int) *int { return &i }

	tests := []struct {
		name    string
		comment GitHubComment
		want    *models.LineRange
		about   string
	}{
		{
			name:    "Returns nil when OriginalLine is not set",
			comment: GitHubComment{},
			want:    nil,
			about:   "Should return nil when no original line information",
		},
		{
			name: "Returns nil when only Line is set",
			comment: GitHubComment{
				Line: intPtr(42),
			},
			want:  nil,
			about: "Should return nil when only current Line is set, not original",
		},
		{
			name: "Single line original range",
			comment: GitHubComment{
				OriginalLine: intPtr(25),
			},
			want:  ptrToLineRange(models.NewSingleLine(25)),
			about: "Should return single line range for OriginalLine",
		},
		{
			name: "Multi-line original range",
			comment: GitHubComment{
				OriginalStartLine: intPtr(10),
				OriginalLine:      intPtr(20),
			},
			want:  ptrToLineRange(models.NewLineRange(10, 20)),
			about: "Should return multi-line range when OriginalStartLine differs from OriginalLine",
		},
		{
			name: "Single line when OriginalStartLine equals OriginalLine",
			comment: GitHubComment{
				OriginalStartLine: intPtr(15),
				OriginalLine:      intPtr(15),
			},
			want:  ptrToLineRange(models.NewSingleLine(15)),
			about: "Should treat as single line when start equals end",
		},
		{
			name: "Ignores current Line when returning original",
			comment: GitHubComment{
				Line:         intPtr(50),
				OriginalLine: intPtr(30),
			},
			want:  ptrToLineRange(models.NewSingleLine(30)),
			about: "Should return original line info, not current Line",
		},
		{
			name: "OriginalStartLine without OriginalLine returns nil",
			comment: GitHubComment{
				OriginalStartLine: intPtr(10),
			},
			want:  nil,
			about: "Should return nil when OriginalStartLine is set but OriginalLine is not",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.comment.GetOriginalLineRange()

			if tt.want == nil {
				if got != nil {
					t.Errorf("GetOriginalLineRange() = %v, want nil\nAbout: %s", got, tt.about)
				}
				return
			}

			if got == nil {
				t.Errorf("GetOriginalLineRange() = nil, want %v\nAbout: %s", tt.want, tt.about)
				return
			}

			if *got != *tt.want {
				t.Errorf("GetOriginalLineRange() = %v, want %v\nAbout: %s", *got, *tt.want, tt.about)
			}
		})
	}

	t.Run("Returns pointer to different instance", func(t *testing.T) {
		// Behavior test: returned pointer should be independent
		comment := GitHubComment{
			OriginalLine: intPtr(42),
		}

		result1 := comment.GetOriginalLineRange()
		result2 := comment.GetOriginalLineRange()

		if result1 == nil || result2 == nil {
			t.Fatal("expected non-nil results")
			return
		}

		// Should have same value but different pointers
		if result1 == result2 {
			t.Error("GetOriginalLineRange() should return new pointer each time, not reuse same pointer")
		}

		if *result1 != *result2 {
			t.Errorf("GetOriginalLineRange() values differ: %v vs %v", *result1, *result2)
		}
	})
}

// ptrToLineRange is a helper to create a pointer to a LineRange.
func ptrToLineRange(lr models.LineRange) *models.LineRange {
	return &lr
}
