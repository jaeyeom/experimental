package models

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestGetLineContext(t *testing.T) {
	// Create a temporary test file
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.go")

	content := `package main

import "fmt"

func main() {
	fmt.Println("Hello, World!")
	x := 42
	y := x * 2
	fmt.Println(y)
}
`
	if err := os.WriteFile(testFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	tests := []struct {
		name         string
		lineNumber   int
		contextLines int
		wantStart    int
		wantEnd      int
		wantLines    int
		wantErr      bool
	}{
		{
			name:         "middle of file with 2 context lines",
			lineNumber:   6,
			contextLines: 2,
			wantStart:    4,
			wantEnd:      8,
			wantLines:    5,
			wantErr:      false,
		},
		{
			name:         "beginning of file with 3 context lines",
			lineNumber:   2,
			contextLines: 3,
			wantStart:    1,
			wantEnd:      5,
			wantLines:    5,
			wantErr:      false,
		},
		{
			name:         "end of file with 3 context lines",
			lineNumber:   9,
			contextLines: 3,
			wantStart:    6,
			wantEnd:      10,
			wantLines:    5,
			wantErr:      false,
		},
		{
			name:         "first line with 1 context line",
			lineNumber:   1,
			contextLines: 1,
			wantStart:    1,
			wantEnd:      2,
			wantLines:    2,
			wantErr:      false,
		},
		{
			name:         "last line with 1 context line",
			lineNumber:   10,
			contextLines: 1,
			wantStart:    9,
			wantEnd:      10,
			wantLines:    2,
			wantErr:      false,
		},
		{
			name:         "line number out of range - too large",
			lineNumber:   100,
			contextLines: 3,
			wantErr:      true,
		},
		{
			name:         "line number out of range - zero",
			lineNumber:   0,
			contextLines: 3,
			wantErr:      true,
		},
		{
			name:         "line number out of range - negative",
			lineNumber:   -1,
			contextLines: 3,
			wantErr:      true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx, err := GetLineContext(testFile, tt.lineNumber, tt.contextLines)

			if tt.wantErr {
				if err == nil {
					t.Errorf("GetLineContext() expected error but got nil")
				}
				return
			}

			if err != nil {
				t.Errorf("GetLineContext() unexpected error: %v", err)
				return
			}

			wantRange := NewLineRange(tt.wantStart, tt.wantEnd)
			if ctx.Range != wantRange {
				t.Errorf("Range = %v, want %v", ctx.Range, wantRange)
			}

			if len(ctx.Lines) != tt.wantLines {
				t.Errorf("Lines count = %d, want %d", len(ctx.Lines), tt.wantLines)
			}
		})
	}
}

func TestGetLineContext_FileNotFound(t *testing.T) {
	_, err := GetLineContext("/nonexistent/file.go", 10, 3)
	if err == nil {
		t.Error("GetLineContext() expected error for nonexistent file")
	}
	if !strings.Contains(err.Error(), "failed to open file") {
		t.Errorf("Expected 'failed to open file' error, got: %v", err)
	}
}

func TestGetLineContextForComment(t *testing.T) {
	// Create a temporary test file
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.go")

	content := `package main

func add(a, b int) int {
	return a + b
}

func multiply(a, b int) int {
	result := a * b
	return result
}
`
	if err := os.WriteFile(testFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	tests := []struct {
		name         string
		comment      Comment
		contextLines int
		wantStart    int
		wantEnd      int
		wantErr      bool
	}{
		{
			name: "single line comment",
			comment: Comment{
				Path: testFile,
				Line: NewSingleLine(4),
			},
			contextLines: 2,
			wantStart:    2,
			wantEnd:      6,
			wantErr:      false,
		},
		{
			name: "multi-line comment",
			comment: Comment{
				Path: testFile,
				Line: NewLineRange(8, 9),
			},
			contextLines: 1,
			wantStart:    7,
			wantEnd:      9, // targetLine is (8+9)/2=8, so context is 8-1 to 8+1 = 7 to 9
			wantErr:      false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx, err := GetLineContextForComment(tt.comment.Path, tt.comment, tt.contextLines)

			if tt.wantErr {
				if err == nil {
					t.Errorf("GetLineContextForComment() expected error but got nil")
				}
				return
			}

			if err != nil {
				t.Errorf("GetLineContextForComment() unexpected error: %v", err)
				return
			}

			wantRange := NewLineRange(tt.wantStart, tt.wantEnd)
			if ctx.Range != wantRange {
				t.Errorf("Range = %v, want %v", ctx.Range, wantRange)
			}
		})
	}
}

func TestFormatLineContext(t *testing.T) {
	tests := []struct {
		name           string
		context        *LineContext
		highlightLine  int
		wantContains   []string
		wantNotContain []string
	}{
		{
			name: "normal context with highlight",
			context: &LineContext{
				Range: NewLineRange(10, 12),
				Lines: []string{"func main() {", "	fmt.Println(\"Hello\")", "}"},
			},
			highlightLine: 11,
			wantContains:  []string{">   11 |", "    10 |", "    12 |", "fmt.Println"},
		},
		{
			name: "context without highlight",
			context: &LineContext{
				Range: NewLineRange(1, 3),
				Lines: []string{"package main", "", "import \"fmt\""},
			},
			highlightLine:  5, // Line not in context
			wantContains:   []string{"     1 |", "     2 |", "     3 |"},
			wantNotContain: []string{">"},
		},
		{
			name:          "nil context",
			context:       nil,
			highlightLine: 10,
			wantContains:  []string{},
		},
		{
			name: "empty lines",
			context: &LineContext{
				Range: NewLineRange(5, 7),
				Lines: []string{"", "", ""},
			},
			highlightLine: 6,
			wantContains:  []string{">    6 |", "     5 |", "     7 |"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := FormatLineContext(tt.context, tt.highlightLine)

			if tt.context == nil && result != "" {
				t.Errorf("Expected empty string for nil context, got: %q", result)
				return
			}

			for _, want := range tt.wantContains {
				if !strings.Contains(result, want) {
					t.Errorf("FormatLineContext() result missing %q\nGot:\n%s", want, result)
				}
			}

			for _, notWant := range tt.wantNotContain {
				if strings.Contains(result, notWant) {
					t.Errorf("FormatLineContext() result should not contain %q\nGot:\n%s", notWant, result)
				}
			}
		})
	}
}

func TestCommentWithLineContext_Integration(t *testing.T) {
	// Create a temporary test file
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "example.go")

	content := `package example

// Add returns the sum of two integers.
func Add(a, b int) int {
	return a + b
}

// Multiply returns the product of two integers.
func Multiply(a, b int) int {
	return a * b
}
`
	if err := os.WriteFile(testFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	comment := Comment{
		ID:        "test-comment-1",
		Path:      testFile,
		Line:      NewSingleLine(5),
		Body:      "Consider adding error handling",
		Side:      SideRight,
		CreatedAt: time.Now(),
	}

	// Get context for the comment
	ctx, err := GetLineContextForComment(testFile, comment, 2)
	if err != nil {
		t.Fatalf("Failed to get line context: %v", err)
	}

	// Create CommentWithLineContext
	cwc := CommentWithLineContext{
		Comment: comment,
		Context: ctx,
	}

	// Verify the structure
	if cwc.Line != NewSingleLine(5) {
		t.Errorf("Comment line = %d, want 5", cwc.Line)
	}

	wantRange := NewLineRange(3, 7)
	if cwc.Context.Range != wantRange {
		t.Errorf("Context range = %v, want %v", cwc.Context.Range, wantRange)
	}

	if len(cwc.Context.Lines) != 5 {
		t.Errorf("Context lines count = %d, want 5", len(cwc.Context.Lines))
	}

	// Verify the actual lines contain expected content
	expectedContents := []string{"Add returns", "func Add", "return a + b"}
	contextStr := strings.Join(cwc.Context.Lines, "\n")
	for _, expected := range expectedContents {
		if !strings.Contains(contextStr, expected) {
			t.Errorf("Context lines missing expected content: %q", expected)
		}
	}
}
