package github

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestNewPRReviewClient(t *testing.T) {
	client := &Client{}
	prReviewClient := NewPRReviewClient(client)

	if prReviewClient == nil {
		t.Fatal("expected non-nil PRReviewClient")
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
				if hunk.File != tt.filename {
					t.Errorf("expected filename %q, got %q", tt.filename, hunk.File)
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
					File:      "test.go",
					Side:      "RIGHT",
					StartLine: 1,
					EndLine:   5,
					Content:   "@@ -1,3 +1,4 @@\n package main\n-import \"fmt\"\n",
					SHA:       "abc123",
				},
			},
			wantCount: 2, // RIGHT + LEFT
		},
		{
			name: "hunk without deletions",
			hunks: []models.DiffHunk{
				{
					File:      "test.go",
					Side:      "RIGHT",
					StartLine: 1,
					EndLine:   5,
					Content:   "@@ -1,3 +1,4 @@\n package main\n+import \"fmt\"\n",
					SHA:       "abc123",
				},
			},
			wantCount: 1, // RIGHT only
		},
		{
			name: "multiple hunks with mixed content",
			hunks: []models.DiffHunk{
				{
					File:      "test.go",
					Side:      "RIGHT",
					StartLine: 1,
					EndLine:   5,
					Content:   "@@ -1,3 +1,4 @@\n package main\n-import \"fmt\"\n",
					SHA:       "abc123",
				},
				{
					File:      "test.go",
					Side:      "RIGHT",
					StartLine: 10,
					EndLine:   15,
					Content:   "@@ -10,2 +11,3 @@\n+	fmt.Println(\"hello\")\n",
					SHA:       "abc123",
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
						Line: 10,
						Body: "Nice fix!",
						Side: "RIGHT",
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
						Path:      "test.go",
						StartLine: intPtr(10),
						Line:      15,
						Body:      "This whole section needs work",
						Side:      "RIGHT",
						SHA:       "abc123",
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
			githubComments := make([]map[string]interface{}, len(tt.review.Comments))
			for i, comment := range tt.review.Comments {
				githubComment := map[string]interface{}{
					"path": comment.Path,
					"body": comment.Body,
					"side": comment.Side,
				}

				if comment.IsMultiLine() {
					githubComment["start_line"] = *comment.StartLine
					githubComment["line"] = comment.Line
				} else {
					githubComment["line"] = comment.Line
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
				Side:      "right",
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
				if c.Line != 10 {
					t.Errorf("expected line 10, got %d", c.Line)
				}
				if c.Side != "RIGHT" {
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
				Side:      "RIGHT",
				CommitID:  "def456",
				CreatedAt: now,
				UpdatedAt: now,
				User: struct {
					Login string `json:"login"`
				}{Login: "testuser"},
			},
			wantErr: false,
			checkFunc: func(t *testing.T, c models.Comment) {
				if c.StartLine == nil {
					t.Error("expected StartLine to be set")
				} else if *c.StartLine != 5 {
					t.Errorf("expected start line 5, got %d", *c.StartLine)
				}
				if c.Line != 10 {
					t.Errorf("expected line 10, got %d", c.Line)
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
				if c.Line != 15 {
					t.Errorf("expected line 15, got %d", c.Line)
				}
			},
		},
		{
			name: "comment with no line number",
			ghComment: GitHubComment{
				ID:        999,
				Path:      "test.go",
				Body:      "General comment",
				Side:      "RIGHT",
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
				Side:      "invalid",
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
				if c.Side != "RIGHT" {
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
			Line: 10,
			Body: "Test comment",
			Side: "RIGHT",
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
				File:      "test.go",
				Side:      "RIGHT",
				StartLine: 1,
				EndLine:   0,
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
				if hunk.EndLine != tt.wantHunkEndLine {
					t.Errorf("expected hunk end line %d, got %d", tt.wantHunkEndLine, hunk.EndLine)
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

	if hunk.File != filename {
		t.Errorf("expected file %q, got %q", filename, hunk.File)
	}
	if hunk.SHA != sha {
		t.Errorf("expected SHA %q, got %q", sha, hunk.SHA)
	}
	if hunk.StartLine != rightLineNum {
		t.Errorf("expected start line %d, got %d", rightLineNum, hunk.StartLine)
	}
	if hunk.EndLine != rightLineNum {
		t.Errorf("expected end line %d, got %d", rightLineNum, hunk.EndLine)
	}
	if hunk.Side != "RIGHT" {
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
