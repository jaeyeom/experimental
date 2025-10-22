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
						Side:      "RIGHT",
						CreatedAt: time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
					},
					Context: &LineContext{
						StartLine: 8,
						EndLine:   12,
						Lines:     []string{"func main() {", "	result := process()", "	save(result)", "}", ""},
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
						Side:      "LEFT",
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
						StartLine: 3,
						EndLine:   7,
						Lines:     []string{"line3", "line4", "line5", "line6", "line7"},
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
					} else if parsed[i].Context.StartLine != comment.Context.StartLine {
						t.Errorf("Comment[%d].Context.StartLine = %d, want %d", i, parsed[i].Context.StartLine, comment.Context.StartLine)
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
						Side:      "RIGHT",
						CreatedAt: time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC),
					},
					Context: &LineContext{
						StartLine: 8,
						EndLine:   12,
						Lines:     []string{"func main() {", "	result := process()", "	save(result)", "}", ""},
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
						Side:      "LEFT",
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
						Side:      "RIGHT",
						CreatedAt: time.Date(2025, 1, 15, 12, 0, 0, 0, time.UTC),
					},
					Context: &LineContext{
						StartLine: 21,
						EndLine:   27,
						Lines:     []string{"line21", "line22", "line23", "line24", "line25", "line26", "line27"},
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
				Side:      "RIGHT",
				CreatedAt: time.Date(2025, 1, 15, 10, 0, 0, 0, time.UTC),
			},
			Context: &LineContext{
				StartLine: 8,
				EndLine:   12,
				Lines:     []string{"line8", "line9", "line10", "line11", "line12"},
			},
		},
		{
			Comment: Comment{
				ID:        "comment-2",
				Path:      "file2.go",
				Line:      NewSingleLine(20),
				Body:      "Second comment",
				Side:      "LEFT",
				CreatedAt: time.Date(2025, 1, 15, 11, 0, 0, 0, time.UTC),
			},
			Context: &LineContext{
				StartLine: 18,
				EndLine:   22,
				Lines:     []string{"line18", "line19", "line20", "line21", "line22"},
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
		StartLine: 10,
		EndLine:   14,
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
