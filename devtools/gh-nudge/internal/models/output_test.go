package models

import (
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func TestJSONFormatter_FormatCommentsWithContext(t *testing.T) {
	formatter := NewJSONFormatter()

	tests := []struct {
		name     string
		comments []CommentWithLineContext
		wantErr  bool
	}{
		{
			name: "single comment with context",
			comments: []CommentWithLineContext{
				{
					Comment: Comment{
						ID:        "comment-1",
						Path:      "src/main.go",
						Line:      NewSingleLine(10),
						Body:      "Add error handling",
						Side:      SideRight,
						CreatedAt: time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
					},
					Context: &LineContext{
						Range: NewLineRange(8, 12),
						Lines: []string{
							"func main() {",
							"	result := process()",
							"	save(result)",
							"}",
							"",
						},
					},
				},
			},
			wantErr: false,
		},
		{
			name: "comment without context",
			comments: []CommentWithLineContext{
				{
					Comment: Comment{
						ID:        "comment-2",
						Path:      "src/utils.go",
						Line:      NewSingleLine(20),
						Body:      "Consider refactoring",
						Side:      SideLeft,
						CreatedAt: time.Date(2025, 1, 15, 11, 0, 0, 0, time.UTC),
					},
					Context: nil,
				},
			},
			wantErr: false,
		},
		{
			name:     "empty comments",
			comments: []CommentWithLineContext{},
			wantErr:  false,
		},
		{
			name: "multiple comments with mixed context",
			comments: []CommentWithLineContext{
				{
					Comment: Comment{
						ID:   "comment-3",
						Path: "file1.go",
						Line: NewSingleLine(5),
						Body: "Comment 1",
					},
					Context: &LineContext{
						Range: NewLineRange(3, 7),
						Lines: []string{"line3", "line4", "line5", "line6", "line7"},
					},
				},
				{
					Comment: Comment{
						ID:   "comment-4",
						Path: "file2.go",
						Line: NewSingleLine(10),
						Body: "Comment 2",
					},
					Context: nil,
				},
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := formatter.FormatCommentsWithContext(tt.comments)

			if (err != nil) != tt.wantErr {
				t.Errorf("FormatCommentsWithContext() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				return
			}

			// Verify it's valid JSON
			var parsed []CommentWithLineContext
			if err := json.Unmarshal([]byte(result), &parsed); err != nil {
				t.Errorf("Result is not valid JSON: %v\nGot: %s", err, result)
				return
			}

			// Verify the parsed data matches input
			if len(parsed) != len(tt.comments) {
				t.Errorf("Parsed comment count = %d, want %d", len(parsed), len(tt.comments))
			}

			for i, comment := range tt.comments {
				if i >= len(parsed) {
					break
				}

				if parsed[i].ID != comment.ID {
					t.Errorf("Comment[%d].ID = %s, want %s", i, parsed[i].ID, comment.ID)
				}

				if comment.Context != nil {
					if parsed[i].Context == nil {
						t.Errorf("Comment[%d].Context should not be nil", i)
					} else if parsed[i].Context.Range != comment.Context.Range {
						t.Errorf("Comment[%d].Context.Range = %v, want %v", i, parsed[i].Context.Range, comment.Context.Range)
					}
				}
			}
		})
	}
}

func TestTextFormatter_FormatCommentsWithContext(t *testing.T) {
	formatter := NewTextFormatter()

	tests := []struct {
		name           string
		comments       []CommentWithLineContext
		wantContains   []string
		wantNotContain []string
		wantErr        bool
	}{
		{
			name: "single comment with context",
			comments: []CommentWithLineContext{
				{
					Comment: Comment{
						ID:        "abc123",
						Path:      "src/main.go",
						Line:      NewSingleLine(10),
						Body:      "Add error handling",
						Side:      SideRight,
						CreatedAt: time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
					},
					Context: &LineContext{
						Range: NewLineRange(8, 12),
						Lines: []string{
							"func main() {",
							"	result := process()",
							"	save(result)",
							"}",
							"",
						},
					},
				},
			},
			wantContains: []string{
				"abc123",
				"src/main.go",
				"Line: 10",
				"RIGHT",
				"Code Context:",
				"     8 |",
				"     9 |",
				">   10 |",
				"save(result)",
				"Add error handling",
				"Total: 1 items",
			},
			wantErr: false,
		},
		{
			name: "comment without context",
			comments: []CommentWithLineContext{
				{
					Comment: Comment{
						ID:        "def456",
						Path:      "src/utils.go",
						Line:      NewSingleLine(20),
						Body:      "Consider refactoring",
						Side:      SideLeft,
						CreatedAt: time.Date(2025, 1, 15, 11, 0, 0, 0, time.UTC),
					},
					Context: nil,
				},
			},
			wantContains: []string{
				"def456",
				"src/utils.go",
				"Line: 20",
				"LEFT",
				"Consider refactoring",
				"Total: 1 items",
			},
			wantNotContain: []string{
				"Code Context:",
			},
			wantErr: false,
		},
		{
			name:     "empty comments",
			comments: []CommentWithLineContext{},
			wantContains: []string{
				"No comments found",
				"Total: 0 items",
			},
			wantErr: false,
		},
		{
			name: "multi-line comment with context",
			comments: []CommentWithLineContext{
				{
					Comment: Comment{
						ID:        "ghi789",
						Path:      "test.go",
						Line:      NewLineRange(23, 25),
						Body:      "Refactor this block",
						Side:      SideRight,
						CreatedAt: time.Date(2025, 1, 15, 12, 0, 0, 0, time.UTC),
					},
					Context: &LineContext{
						Range: NewLineRange(21, 27),
						Lines: []string{"line21", "line22", "line23", "line24", "line25", "line26", "line27"},
					},
				},
			},
			wantContains: []string{
				"ghi789",
				"Line: 23-25",
				"Code Context:",
				">   25 |", // End line is highlighted (comment.Line)
				"Refactor this block",
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := formatter.FormatCommentsWithContext(tt.comments)

			if (err != nil) != tt.wantErr {
				t.Errorf("FormatCommentsWithContext() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				return
			}

			for _, want := range tt.wantContains {
				if !strings.Contains(result, want) {
					t.Errorf("Result missing expected string %q\nGot:\n%s", want, result)
				}
			}

			for _, notWant := range tt.wantNotContain {
				if strings.Contains(result, notWant) {
					t.Errorf("Result contains unexpected string %q\nGot:\n%s", notWant, result)
				}
			}
		})
	}
}

func TestTextFormatter_FormatCommentsWithContext_MultipleComments(t *testing.T) {
	formatter := NewTextFormatter()

	comments := []CommentWithLineContext{
		{
			Comment: Comment{
				ID:        "comment-1",
				Path:      "file1.go",
				Line:      NewSingleLine(10),
				Body:      "First comment",
				Side:      SideRight,
				CreatedAt: time.Date(2025, 1, 15, 10, 0, 0, 0, time.UTC),
			},
			Context: &LineContext{
				Range: NewLineRange(8, 12),
				Lines: []string{"line8", "line9", "line10", "line11", "line12"},
			},
		},
		{
			Comment: Comment{
				ID:        "comment-2",
				Path:      "file2.go",
				Line:      NewSingleLine(20),
				Body:      "Second comment",
				Side:      SideLeft,
				CreatedAt: time.Date(2025, 1, 15, 11, 0, 0, 0, time.UTC),
			},
			Context: &LineContext{
				Range: NewLineRange(18, 22),
				Lines: []string{"line18", "line19", "line20", "line21", "line22"},
			},
		},
	}

	result, err := formatter.FormatCommentsWithContext(comments)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}

	// Verify both comments are present (Note: FormatIDShort truncates to 8 chars)
	if !strings.Contains(result, "Comment ID: comment-") {
		t.Errorf("Result missing comment ID header\nGot:\n%s", result)
	}

	// Verify total count
	if !strings.Contains(result, "Total: 2 items") {
		t.Error("Result missing correct total count")
	}

	// Verify both contexts are present
	if !strings.Contains(result, "line10") {
		t.Error("Result missing first context")
	}
	if !strings.Contains(result, "line20") {
		t.Error("Result missing second context")
	}

	// Verify separators between comments (there should be newlines between entries)
	commentCount := strings.Count(result, "Comment ID:")
	if commentCount != 2 {
		t.Errorf("Expected 2 comment headers, got %d", commentCount)
	}
}

func TestFormatLineContext_Integration(t *testing.T) {
	context := &LineContext{
		Range: NewLineRange(10, 14),
		Lines: []string{
			"func calculate(x int) int {",
			"	result := x * 2",
			"	if result > 100 {",
			"		return 100",
			"	}",
		},
	}

	result := FormatLineContext(context, 12)

	expectedLines := []string{
		"    10 | func calculate(x int) int {",
		"    11 | 	result := x * 2",
		">   12 | 	if result > 100 {",
		"    13 | 		return 100",
		"    14 | 	}",
	}

	for _, expected := range expectedLines {
		if !strings.Contains(result, expected) {
			t.Errorf("Result missing expected line:\n%q\nGot:\n%s", expected, result)
		}
	}
}

// TestJSONFormatter_FormatSubmitResult tests JSON formatting of submit results.
func TestJSONFormatter_FormatSubmitResult(t *testing.T) {
	formatter := NewJSONFormatter()

	tests := []struct {
		name    string
		result  SubmitResult
		wantErr bool
	}{
		{
			name: "successful submission with all fields",
			result: SubmitResult{
				Status:           "success",
				PRNumber:         123,
				Repository:       Repository{Owner: "owner", Name: "repo"},
				Comments:         5,
				SubmittedAt:      time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
				PostSubmitAction: "clear",
				File:             "src/main.go",
			},
			wantErr: false,
		},
		{
			name: "submission without file",
			result: SubmitResult{
				Status:           "success",
				PRNumber:         456,
				Repository:       Repository{Owner: "org", Name: "project"},
				Comments:         10,
				SubmittedAt:      time.Date(2025, 1, 16, 14, 0, 0, 0, time.UTC),
				PostSubmitAction: "archive",
			},
			wantErr: false,
		},
		{
			name: "submission with zero comments",
			result: SubmitResult{
				Status:           "success",
				PRNumber:         789,
				Repository:       Repository{Owner: "user", Name: "test"},
				Comments:         0,
				SubmittedAt:      time.Date(2025, 1, 17, 9, 0, 0, 0, time.UTC),
				PostSubmitAction: "none",
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output, err := formatter.FormatSubmitResult(tt.result)
			if (err != nil) != tt.wantErr {
				t.Errorf("FormatSubmitResult() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				return
			}

			// Verify it's valid JSON
			var parsed SubmitResult
			if err := json.Unmarshal([]byte(output), &parsed); err != nil {
				t.Errorf("Result is not valid JSON: %v", err)
				return
			}

			// Verify key fields are preserved
			if parsed.Status != tt.result.Status {
				t.Errorf("Status = %s, want %s", parsed.Status, tt.result.Status)
			}
			if parsed.PRNumber != tt.result.PRNumber {
				t.Errorf("PRNumber = %d, want %d", parsed.PRNumber, tt.result.PRNumber)
			}
			if parsed.Comments != tt.result.Comments {
				t.Errorf("Comments = %d, want %d", parsed.Comments, tt.result.Comments)
			}
			if parsed.PostSubmitAction != tt.result.PostSubmitAction {
				t.Errorf("PostSubmitAction = %s, want %s", parsed.PostSubmitAction, tt.result.PostSubmitAction)
			}
			if parsed.File != tt.result.File {
				t.Errorf("File = %s, want %s", parsed.File, tt.result.File)
			}
		})
	}
}

// TestJSONFormatter_FormatComments tests JSON formatting of comment lists.
func TestJSONFormatter_FormatComments(t *testing.T) {
	formatter := NewJSONFormatter()

	tests := []struct {
		name     string
		comments []Comment
		wantErr  bool
	}{
		{
			name: "multiple comments",
			comments: []Comment{
				{
					ID:        "comment-1",
					Path:      "file1.go",
					Line:      NewSingleLine(10),
					Body:      "First comment",
					Side:      SideRight,
					CreatedAt: time.Date(2025, 1, 15, 10, 0, 0, 0, time.UTC),
				},
				{
					ID:        "comment-2",
					Path:      "file2.go",
					Line:      NewLineRange(20, 25),
					Body:      "Second comment",
					Side:      SideLeft,
					CreatedAt: time.Date(2025, 1, 15, 11, 0, 0, 0, time.UTC),
				},
			},
			wantErr: false,
		},
		{
			name:     "empty comments",
			comments: []Comment{},
			wantErr:  false,
		},
		{
			name: "single comment",
			comments: []Comment{
				{
					ID:   "comment-3",
					Path: "test.go",
					Line: NewSingleLine(5),
					Body: "Test comment",
				},
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output, err := formatter.FormatComments(tt.comments)
			if (err != nil) != tt.wantErr {
				t.Errorf("FormatComments() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			// Verify it's valid JSON
			var parsed []Comment
			if err := json.Unmarshal([]byte(output), &parsed); err != nil {
				t.Errorf("Result is not valid JSON: %v", err)
				return
			}

			// Verify count matches
			if len(parsed) != len(tt.comments) {
				t.Errorf("Got %d comments, want %d", len(parsed), len(tt.comments))
			}

			// Verify comment IDs are preserved
			for i, comment := range tt.comments {
				if i < len(parsed) && parsed[i].ID != comment.ID {
					t.Errorf("Comment[%d].ID = %s, want %s", i, parsed[i].ID, comment.ID)
				}
			}
		})
	}
}

// TestJSONFormatter_FormatSingleComment tests JSON formatting of a single comment.
func TestJSONFormatter_FormatSingleComment(t *testing.T) {
	formatter := NewJSONFormatter()

	tests := []struct {
		name    string
		comment Comment
		wantErr bool
	}{
		{
			name: "comment with all fields",
			comment: Comment{
				ID:        "abc123",
				Path:      "src/main.go",
				Line:      NewSingleLine(42),
				Body:      "This needs refactoring",
				Side:      SideRight,
				CreatedAt: time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
				Status:    StatusUnresolved,
			},
			wantErr: false,
		},
		{
			name: "comment with line range",
			comment: Comment{
				ID:   "def456",
				Path: "test.go",
				Line: NewLineRange(10, 15),
				Body: "Multi-line comment",
				Side: SideLeft,
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output, err := formatter.FormatSingleComment(tt.comment)
			if (err != nil) != tt.wantErr {
				t.Errorf("FormatSingleComment() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			// Verify it's valid JSON
			var parsed Comment
			if err := json.Unmarshal([]byte(output), &parsed); err != nil {
				t.Errorf("Result is not valid JSON: %v", err)
				return
			}

			// Verify key fields match
			if parsed.ID != tt.comment.ID {
				t.Errorf("ID = %s, want %s", parsed.ID, tt.comment.ID)
			}
			if parsed.Path != tt.comment.Path {
				t.Errorf("Path = %s, want %s", parsed.Path, tt.comment.Path)
			}
			if parsed.Body != tt.comment.Body {
				t.Errorf("Body = %s, want %s", parsed.Body, tt.comment.Body)
			}
		})
	}
}

// TestTextFormatter_FormatSubmitResult tests text formatting of submit results.
func TestTextFormatter_FormatSubmitResult(t *testing.T) {
	formatter := NewTextFormatter()

	tests := []struct {
		name         string
		result       SubmitResult
		wantContains []string
		wantErr      bool
	}{
		{
			name: "submission with file",
			result: SubmitResult{
				Status:           "success",
				PRNumber:         123,
				Repository:       Repository{Owner: "owner", Name: "repo"},
				Comments:         5,
				SubmittedAt:      time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
				PostSubmitAction: "clear",
				File:             "src/main.go",
			},
			wantContains: []string{
				"owner/repo",
				"#123",
				"5 comments",
				"src/main.go",
			},
			wantErr: false,
		},
		{
			name: "submission without file",
			result: SubmitResult{
				Status:           "success",
				PRNumber:         456,
				Repository:       Repository{Owner: "org", Name: "project"},
				Comments:         10,
				SubmittedAt:      time.Date(2025, 1, 16, 14, 0, 0, 0, time.UTC),
				PostSubmitAction: "archive",
			},
			wantContains: []string{
				"org/project",
				"#456",
				"10 comments",
			},
			wantErr: false,
		},
		{
			name: "submission with one comment",
			result: SubmitResult{
				Status:           "success",
				PRNumber:         789,
				Repository:       Repository{Owner: "user", Name: "test"},
				Comments:         1,
				SubmittedAt:      time.Date(2025, 1, 17, 9, 0, 0, 0, time.UTC),
				PostSubmitAction: "none",
			},
			wantContains: []string{
				"user/test",
				"#789",
				"1 comments", // Note: the implementation doesn't pluralize
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output, err := formatter.FormatSubmitResult(tt.result)
			if (err != nil) != tt.wantErr {
				t.Errorf("FormatSubmitResult() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			for _, want := range tt.wantContains {
				if !strings.Contains(output, want) {
					t.Errorf("Output missing expected string %q\nGot: %s", want, output)
				}
			}
		})
	}
}

// TestTextFormatter_FormatComments tests text table formatting of comments.
func TestTextFormatter_FormatComments(t *testing.T) {
	formatter := NewTextFormatter()

	tests := []struct {
		name         string
		comments     []Comment
		wantContains []string
		wantErr      bool
	}{
		{
			name: "single comment",
			comments: []Comment{
				{
					ID:        "abc123",
					Path:      "src/main.go",
					Line:      NewSingleLine(42),
					Body:      "Fix this",
					Side:      SideRight,
					CreatedAt: time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
				},
			},
			wantContains: []string{
				"ID",
				"File",
				"Line",
				"Side",
				"Comment",
				"abc123",
				"src/main.go",
				"42",
				"RIGHT",
				"Fix this",
				"Total: 1 items",
			},
			wantErr: false,
		},
		{
			name:     "empty comments",
			comments: []Comment{},
			wantContains: []string{
				"No comments found",
				"Total: 0 items",
			},
			wantErr: false,
		},
		{
			name: "multiple comments",
			comments: []Comment{
				{
					ID:   "comment1",
					Path: "file1.go",
					Line: NewSingleLine(10),
					Body: "First",
					Side: SideLeft,
				},
				{
					ID:   "comment2",
					Path: "file2.go",
					Line: NewLineRange(20, 25),
					Body: "Second",
					Side: SideRight,
				},
			},
			wantContains: []string{
				"comment1",
				"comment2",
				"file1.go",
				"file2.go",
				"Total: 2 items",
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output, err := formatter.FormatComments(tt.comments)
			if (err != nil) != tt.wantErr {
				t.Errorf("FormatComments() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			for _, want := range tt.wantContains {
				if !strings.Contains(output, want) {
					t.Errorf("Output missing expected string %q\nGot:\n%s", want, output)
				}
			}
		})
	}
}

// TestTextFormatter_FormatSingleComment tests text formatting of a single comment.
func TestTextFormatter_FormatSingleComment(t *testing.T) {
	formatter := NewTextFormatter()

	tests := []struct {
		name         string
		comment      Comment
		wantContains []string
		wantErr      bool
	}{
		{
			name: "comment with all fields",
			comment: Comment{
				ID:        "abc123def456",
				Path:      "src/main.go",
				Line:      NewSingleLine(42),
				Body:      "This needs refactoring\nAcross multiple lines",
				Side:      SideRight,
				CreatedAt: time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
			},
			wantContains: []string{
				"File: src/main.go",
				"Line: 42",
				"Side: RIGHT",
				"ID: abc123de", // FormatIDShort truncates to 8 chars
				"2025-01-15",
				"This needs refactoring",
			},
			wantErr: false,
		},
		{
			name: "comment with line range",
			comment: Comment{
				ID:   "xyz789",
				Path: "test.go",
				Line: NewLineRange(10, 15),
				Body: "Multi-line range",
				Side: SideLeft,
			},
			wantContains: []string{
				"File: test.go",
				"Line: 10-15",
				"Side: LEFT",
				"xyz789",
				"Multi-line range",
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output, err := formatter.FormatSingleComment(tt.comment)
			if (err != nil) != tt.wantErr {
				t.Errorf("FormatSingleComment() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			for _, want := range tt.wantContains {
				if !strings.Contains(output, want) {
					t.Errorf("Output missing expected string %q\nGot:\n%s", want, output)
				}
			}
		})
	}
}

// TestWrapText tests the text wrapping behavior.
func TestWrapText(t *testing.T) {
	tests := []struct {
		name      string
		text      string
		width     int
		wantLines int
		wantFirst string
	}{
		{
			name:      "text fits in width",
			text:      "short text",
			width:     20,
			wantLines: 1,
			wantFirst: "short text",
		},
		{
			name:      "text needs wrapping",
			text:      "this is a longer text that needs wrapping",
			width:     10,
			wantLines: 5, // Should wrap at word boundaries
			wantFirst: "this is a",
		},
		{
			name:      "single long word",
			text:      "verylongwordthatcannotbewrapped",
			width:     10,
			wantLines: 4, // Should break the word
			wantFirst: "verylongwo",
		},
		{
			name:      "empty text",
			text:      "",
			width:     10,
			wantLines: 1,
			wantFirst: "",
		},
		{
			name:      "zero width",
			text:      "some text",
			width:     0,
			wantLines: 1,
			wantFirst: "some text",
		},
		{
			name:      "multiple spaces",
			text:      "word1    word2    word3",
			width:     15,
			wantLines: 2,
			wantFirst: "word1 word2",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lines := wrapText(tt.text, tt.width)

			if len(lines) != tt.wantLines {
				t.Errorf("wrapText() got %d lines, want %d\nLines: %v", len(lines), tt.wantLines, lines)
			}

			if len(lines) > 0 && lines[0] != tt.wantFirst {
				t.Errorf("First line = %q, want %q", lines[0], tt.wantFirst)
			}
		})
	}
}

// TestTruncateString tests string truncation behavior.
func TestTruncateString(t *testing.T) {
	tests := []struct {
		name   string
		input  string
		maxLen int
		want   string
	}{
		{
			name:   "no truncation needed",
			input:  "short",
			maxLen: 10,
			want:   "short",
		},
		{
			name:   "truncation with ellipsis",
			input:  "this is a very long string",
			maxLen: 10,
			want:   "this is...",
		},
		{
			name:   "exact length",
			input:  "exactly10c",
			maxLen: 10,
			want:   "exactly10c",
		},
		{
			name:   "maxLen less than 4",
			input:  "test",
			maxLen: 2,
			want:   "te",
		},
		{
			name:   "empty string",
			input:  "",
			maxLen: 10,
			want:   "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := TruncateString(tt.input, tt.maxLen)
			if got != tt.want {
				t.Errorf("TruncateString(%q, %d) = %q, want %q", tt.input, tt.maxLen, got, tt.want)
			}
		})
	}
}
