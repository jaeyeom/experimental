package models

import (
	"strings"
	"testing"
	"time"
)

func TestDiffSpec(t *testing.T) {
	tests := []struct {
		name string
		adj  LineAdjustment
		want string
	}{
		// Delete operations
		{
			name: "delete single line",
			adj: LineAdjustment{
				Operation: OperationDelete,
				OldStart:  12,
				OldEnd:    12,
				NewStart:  11,
				NewEnd:    11,
			},
			want: "12d11",
		},
		{
			name: "delete multiple lines",
			adj: LineAdjustment{
				Operation: OperationDelete,
				OldStart:  15,
				OldEnd:    17,
				NewStart:  14,
				NewEnd:    14,
			},
			want: "15,17d14",
		},
		// Insert operations
		{
			name: "insert single line",
			adj: LineAdjustment{
				Operation: OperationInsert,
				OldStart:  10,
				OldEnd:    10,
				NewStart:  11,
				NewEnd:    11,
			},
			want: "10a11",
		},
		{
			name: "insert multiple lines",
			adj: LineAdjustment{
				Operation: OperationInsert,
				OldStart:  30,
				OldEnd:    30,
				NewStart:  31,
				NewEnd:    33,
			},
			want: "30a31,33",
		},
		// Change operations
		{
			name: "change single line",
			adj: LineAdjustment{
				Operation: OperationChange,
				OldStart:  101,
				OldEnd:    101,
				NewStart:  101,
				NewEnd:    101,
			},
			want: "101c101",
		},
		{
			name: "change multiple lines to single line",
			adj: LineAdjustment{
				Operation: OperationChange,
				OldStart:  5,
				OldEnd:    7,
				NewStart:  6,
				NewEnd:    6,
			},
			want: "5,7c6",
		},
		{
			name: "change single line to multiple lines",
			adj: LineAdjustment{
				Operation: OperationChange,
				OldStart:  8,
				OldEnd:    8,
				NewStart:  9,
				NewEnd:    12,
			},
			want: "8c9,12",
		},
		{
			name: "change multiple lines to multiple lines",
			adj: LineAdjustment{
				Operation: OperationChange,
				OldStart:  45,
				OldEnd:    47,
				NewStart:  45,
				NewEnd:    46,
			},
			want: "45,47c45,46",
		},
		// Edge cases
		{
			name: "invalid operation type",
			adj: LineAdjustment{
				Operation: "x", // Invalid operation
				OldStart:  1,
				OldEnd:    1,
				NewStart:  1,
				NewEnd:    1,
			},
			want: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.adj.DiffSpec()
			if got != tt.want {
				t.Errorf("DiffSpec() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestDiffSpecRoundTrip(t *testing.T) {
	// Test that ParseDiffSpec and DiffSpec are inverse operations
	specs := []string{
		"12d11",
		"15,17d14",
		"10a11",
		"30a31,33",
		"101c101",
		"5,7c6",
		"8c9,12",
		"45,47c45,46",
	}

	for _, spec := range specs {
		t.Run(spec, func(t *testing.T) {
			// Parse the spec
			adjustments, err := ParseDiffSpec(spec)
			if err != nil {
				t.Fatalf("ParseDiffSpec(%q) failed: %v", spec, err)
			}
			if len(adjustments) != 1 {
				t.Fatalf("Expected 1 adjustment, got %d", len(adjustments))
			}

			// Convert back to spec
			got := adjustments[0].DiffSpec()
			if got != spec {
				t.Errorf("Round trip failed: ParseDiffSpec(%q).DiffSpec() = %q", spec, got)
			}
		})
	}
}

func TestDiffSpecWithTimestamp(t *testing.T) {
	// Test that DiffSpec doesn't depend on other fields like AppliedAt
	adj1 := LineAdjustment{
		Operation:   OperationDelete,
		OldStart:    10,
		OldEnd:      12,
		NewStart:    10,
		NewEnd:      10,
		AppliedAt:   time.Now(),
		Description: "Test deletion",
	}

	adj2 := LineAdjustment{
		Operation:   OperationDelete,
		OldStart:    10,
		OldEnd:      12,
		NewStart:    10,
		NewEnd:      10,
		AppliedAt:   time.Now().Add(time.Hour),
		Description: "Different description",
	}

	if adj1.DiffSpec() != adj2.DiffSpec() {
		t.Errorf("DiffSpec should only depend on operation and line numbers")
	}

	expected := "10,12d10"
	if adj1.DiffSpec() != expected {
		t.Errorf("DiffSpec() = %q, want %q", adj1.DiffSpec(), expected)
	}
}

func TestParseDiffSpec(t *testing.T) {
	tests := []struct {
		name    string
		spec    string
		want    []LineAdjustment
		wantErr bool
	}{
		{
			name: "simple deletion",
			spec: "15,17d14",
			want: []LineAdjustment{
				{
					Operation: OperationDelete,
					OldStart:  15,
					OldEnd:    17,
					NewStart:  14,
					NewEnd:    14,
				},
			},
		},
		{
			name: "single line deletion",
			spec: "12d11",
			want: []LineAdjustment{
				{
					Operation: OperationDelete,
					OldStart:  12,
					OldEnd:    12,
					NewStart:  11,
					NewEnd:    11,
				},
			},
		},
		{
			name: "simple insertion",
			spec: "30a31,33",
			want: []LineAdjustment{
				{
					Operation: OperationInsert,
					OldStart:  30,
					OldEnd:    30,
					NewStart:  31,
					NewEnd:    33,
				},
			},
		},
		{
			name: "single line insertion",
			spec: "10a11",
			want: []LineAdjustment{
				{
					Operation: OperationInsert,
					OldStart:  10,
					OldEnd:    10,
					NewStart:  11,
					NewEnd:    11,
				},
			},
		},
		{
			name: "simple change",
			spec: "5,7c6,8",
			want: []LineAdjustment{
				{
					Operation: OperationChange,
					OldStart:  5,
					OldEnd:    7,
					NewStart:  6,
					NewEnd:    8,
				},
			},
		},
		{
			name: "single line change",
			spec: "101c101",
			want: []LineAdjustment{
				{
					Operation: OperationChange,
					OldStart:  101,
					OldEnd:    101,
					NewStart:  101,
					NewEnd:    101,
				},
			},
		},
		{
			name: "multiple operations",
			spec: "15,17d14;30a31,33;45,47c45,46",
			want: []LineAdjustment{
				{
					Operation: OperationDelete,
					OldStart:  15,
					OldEnd:    17,
					NewStart:  14,
					NewEnd:    14,
				},
				{
					Operation: OperationInsert,
					OldStart:  30,
					OldEnd:    30,
					NewStart:  31,
					NewEnd:    33,
				},
				{
					Operation: OperationChange,
					OldStart:  45,
					OldEnd:    47,
					NewStart:  45,
					NewEnd:    46,
				},
			},
		},
		{
			name:    "invalid operation",
			spec:    "15,17x14",
			wantErr: true,
		},
		{
			name:    "invalid format - no operation",
			spec:    "15,17",
			wantErr: true,
		},
		{
			name:    "invalid format - malformed",
			spec:    "abc",
			wantErr: true,
		},
		{
			name:    "empty spec",
			spec:    "",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseDiffSpec(tt.spec)
			if (err != nil) != tt.wantErr {
				t.Errorf("ParseDiffSpec() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr {
				if len(got) != len(tt.want) {
					t.Errorf("ParseDiffSpec() returned %d adjustments, want %d", len(got), len(tt.want))
					return
				}
				for i, adj := range got {
					want := tt.want[i]
					if adj.Operation != want.Operation ||
						adj.OldStart != want.OldStart ||
						adj.OldEnd != want.OldEnd ||
						adj.NewStart != want.NewStart ||
						adj.NewEnd != want.NewEnd ||
						adj.DiffSpec() != want.DiffSpec() {
						t.Errorf("ParseDiffSpec() adjustment[%d] = %+v, want %+v", i, adj, want)
					}
				}
			}
		})
	}
}

func TestAdjustLineNumber(t *testing.T) {
	tests := []struct {
		name        string
		line        int
		adjustments []LineAdjustment
		want        int
		isValid     bool
	}{
		{
			name: "line unaffected by deletions",
			line: 10,
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			want:    10,
			isValid: true,
		},
		{
			name: "line after deletion",
			line: 20,
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			want:    17, // 20 - 3 deleted lines
			isValid: true,
		},
		{
			name: "line on deleted range",
			line: 16,
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			want:    -1,
			isValid: false,
		},
		{
			name: "line before insertion",
			line: 10,
			adjustments: []LineAdjustment{
				{Operation: OperationInsert, OldStart: 30, OldEnd: 30, NewStart: 31, NewEnd: 33},
			},
			want:    10,
			isValid: true,
		},
		{
			name: "line after insertion",
			line: 35,
			adjustments: []LineAdjustment{
				{Operation: OperationInsert, OldStart: 30, OldEnd: 30, NewStart: 31, NewEnd: 33},
			},
			want:    38, // 35 + 3 inserted lines
			isValid: true,
		},
		{
			name: "line within change range",
			line: 6,
			adjustments: []LineAdjustment{
				{Operation: OperationChange, OldStart: 5, OldEnd: 7, NewStart: 5, NewEnd: 6},
			},
			want:    6, // Maps to new line 6 (second line of the old range maps to second position)
			isValid: true,
		},
		{
			name: "multiple adjustments",
			line: 50,
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14}, // -3
				{Operation: OperationInsert, OldStart: 30, OldEnd: 30, NewStart: 31, NewEnd: 33}, // +3
				{Operation: OperationChange, OldStart: 45, OldEnd: 47, NewStart: 45, NewEnd: 46}, // -1
			},
			want:    49, // 50 - 3 + 3 - 1
			isValid: true,
		},
		{
			name: "line deleted by first adjustment",
			line: 16,
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
				{Operation: OperationInsert, OldStart: 30, OldEnd: 30, NewStart: 31, NewEnd: 33},
			},
			want:    -1,
			isValid: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, isValid := AdjustLineNumber(tt.line, tt.adjustments)
			if got != tt.want {
				t.Errorf("AdjustLineNumber() = %v, want %v", got, tt.want)
			}
			if isValid != tt.isValid {
				t.Errorf("AdjustLineNumber() isValid = %v, want %v", isValid, tt.isValid)
			}
		})
	}
}

func TestAdjustComment(t *testing.T) {
	tests := []struct {
		name        string
		comment     Comment
		adjustments []LineAdjustment
		wantLine    LineRange
		wantStart   *int
		wantValid   bool
	}{
		{
			name: "single line comment unaffected",
			comment: Comment{
				ID:   "test1",
				Path: "file.go",
				Line: NewSingleLine(10),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  NewSingleLine(10),
			wantStart: nil,
			wantValid: true,
		},
		{
			name: "single line comment after deletion",
			comment: Comment{
				ID:   "test2",
				Path: "file.go",
				Line: NewSingleLine(20),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  NewSingleLine(17),
			wantStart: nil,
			wantValid: true,
		},
		{
			name: "multi-line comment adjusted",
			comment: Comment{
				ID:   "test3",
				Path: "file.go",
				Line: NewLineRange(20, 25),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  NewLineRange(17, 22),
			wantStart: intPtr(17),
			wantValid: true,
		},
		{
			name: "multi-line comment start deleted",
			comment: Comment{
				ID:   "test4",
				Path: "file.go",
				Line: NewLineRange(16, 20),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  NewSingleLine(-1),
			wantStart: nil,
			wantValid: false,
		},
		{
			name: "multi-line comment end deleted",
			comment: Comment{
				ID:   "test5",
				Path: "file.go",
				Line: NewLineRange(10, 16),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  NewSingleLine(-1),
			wantStart: nil,
			wantValid: false,
		},
		{
			name: "comment preserves original line numbers",
			comment: Comment{
				ID:   "test6",
				Path: "file.go",
				Line: NewSingleLine(20),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationInsert, OldStart: 10, OldEnd: 10, NewStart: 11, NewEnd: 13},
			},
			wantLine:  NewSingleLine(23),
			wantStart: nil,
			wantValid: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Clone comment to avoid modifying the test data
			comment := tt.comment
			comment.CreatedAt = time.Now()

			// Apply adjustments
			isValid := AdjustComment(&comment, tt.adjustments)

			if isValid != tt.wantValid {
				t.Errorf("AdjustComment() isValid = %v, want %v", isValid, tt.wantValid)
			}

			if isValid {
				if comment.Line != tt.wantLine {
					t.Errorf("AdjustComment() Range.EndLine = %v, want %v", comment.Line, tt.wantLine)
				}

				switch {
				case tt.wantStart == nil && comment.IsMultiLine():
					t.Errorf("AdjustComment() is multi-line (StartLine = %v), want single-line", comment.Line.StartLine)
				case tt.wantStart != nil && !comment.IsMultiLine():
					t.Errorf("AdjustComment() is single-line, want multi-line with StartLine = %v", *tt.wantStart)
				case tt.wantStart != nil && comment.Line.StartLine != *tt.wantStart:
					t.Errorf("AdjustComment() Range.StartLine = %v, want %v", comment.Line.StartLine, *tt.wantStart)
				}

				// Check that original line numbers are preserved
				if comment.OriginalRange == nil {
					t.Error("AdjustComment() should set OriginalRange")
				}

				// Check adjustment history
				if len(comment.AdjustmentHistory) != len(tt.adjustments) {
					t.Errorf("AdjustComment() AdjustmentHistory length = %v, want %v",
						len(comment.AdjustmentHistory), len(tt.adjustments))
				}
			}
		})
	}
}

func TestValidateAdjustmentAgainstDiff(t *testing.T) {
	diffHunks := []DiffHunk{
		{
			File:  "file.go",
			Side:  "RIGHT",
			Range: NewLineRange(10, 20),
		},
		{
			File:  "file.go",
			Side:  "RIGHT",
			Range: NewLineRange(30, 40),
		},
	}

	tests := []struct {
		name        string
		comment     Comment
		adjustments []LineAdjustment
		wantErr     bool
		errMsg      string
	}{
		{
			name: "adjusted comment within diff hunk",
			comment: Comment{
				Path: "file.go",
				Line: NewSingleLine(35),
				Side: "RIGHT",
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 22, OldEnd: 24, NewStart: 22, NewEnd: 22},
			},
			wantErr: false,
		},
		{
			name: "adjusted comment outside diff hunks",
			comment: Comment{
				Path: "file.go",
				Line: NewSingleLine(50),
				Side: "RIGHT",
			},
			adjustments: []LineAdjustment{
				{Operation: OperationInsert, OldStart: 45, OldEnd: 45, NewStart: 46, NewEnd: 48},
			},
			wantErr: true,
			errMsg:  "outside diff hunks",
		},
		{
			name: "comment on deleted line",
			comment: Comment{
				Path: "file.go",
				Line: NewSingleLine(15),
				Side: "RIGHT",
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 14, OldEnd: 16, NewStart: 14, NewEnd: 14},
			},
			wantErr: true,
			errMsg:  "comment on deleted line",
		},
		{
			name: "wrong file path",
			comment: Comment{
				Path: "other.go",
				Line: NewSingleLine(15),
				Side: "RIGHT",
			},
			adjustments: []LineAdjustment{},
			wantErr:     true,
			errMsg:      "no diff hunks for file",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := ValidateAdjustmentAgainstDiff(tt.comment, tt.adjustments, diffHunks)
			if (err != nil) != tt.wantErr {
				t.Errorf("ValidateAdjustmentAgainstDiff() error = %v, wantErr %v", err, tt.wantErr)
			}
			if err != nil && tt.errMsg != "" && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("ValidateAdjustmentAgainstDiff() error = %v, want error containing %v", err, tt.errMsg)
			}
		})
	}
}

// TestConvertUnifiedDiffToClassicDiff tests the unified diff conversion functionality.
func TestConvertUnifiedDiffToClassicDiff(t *testing.T) {
	tests := []struct {
		name        string
		unifiedDiff string
		want        string
		wantErr     bool
	}{
		{
			name: "simple deletion",
			unifiedDiff: `diff --git a/old.txt b/old.txt
--- a/old.txt
+++ b/old.txt
@@ -1,7 +1,6 @@
 1
 2
-3
-4
 5
 6
 7`,
			want:    "3,4d2",
			wantErr: false,
		},
		{
			name: "simple insertion",
			unifiedDiff: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,5 @@
 line1
 line2
+new_line3
+new_line4
 line3`,
			want:    "2a3,4",
			wantErr: false,
		},
		{
			name: "simple change",
			unifiedDiff: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,3 @@
 line1
-old_line2
+new_line2
 line3`,
			want:    "2c2",
			wantErr: false,
		},
		{
			name: "mixed operations",
			unifiedDiff: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,7 +1,6 @@
 1
 2
-3
-4
+NEW_LINE
 5
 6
 7`,
			want:    "3,4c3",
			wantErr: false,
		},
		{
			name: "multiple hunks",
			unifiedDiff: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,2 @@
 line1
-line2
 line3
@@ -10,3 +9,4 @@
 line10
 line11
+new_line
 line12`,
			want:    "2d1;11a11",
			wantErr: false,
		},
		{
			name: "no changes",
			unifiedDiff: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,3 @@
 line1
 line2
 line3`,
			wantErr: true,
		},
		{
			name:        "empty diff",
			unifiedDiff: "",
			wantErr:     true,
		},
		{
			name: "malformed hunk header",
			unifiedDiff: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ invalid @@
 line1`,
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ConvertUnifiedDiffToClassicDiff(tt.unifiedDiff)
			if (err != nil) != tt.wantErr {
				t.Errorf("ConvertUnifiedDiffToClassicDiff() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && got != tt.want {
				t.Errorf("ConvertUnifiedDiffToClassicDiff() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestConvertUnifiedDiffToMultiFileClassicDiff tests multi-file unified diff conversion.
func TestConvertUnifiedDiffToMultiFileClassicDiff(t *testing.T) {
	tests := []struct {
		name        string
		unifiedDiff string
		want        []FileAdjustmentSpec
		wantErr     bool
	}{
		{
			name: "single file",
			unifiedDiff: `diff --git a/file1.txt b/file1.txt
--- a/file1.txt
+++ b/file1.txt
@@ -1,3 +1,2 @@
 line1
-line2
 line3`,
			want: []FileAdjustmentSpec{
				{
					FilePath:    "file1.txt",
					ClassicDiff: "2d1",
				},
			},
			wantErr: false,
		},
		{
			name: "multiple files",
			unifiedDiff: `diff --git a/file1.txt b/file1.txt
--- a/file1.txt
+++ b/file1.txt
@@ -1,3 +1,2 @@
 line1
-line2
 line3
diff --git a/file2.txt b/file2.txt
--- a/file2.txt
+++ b/file2.txt
@@ -5,3 +5,4 @@
 line5
 line6
+new_line
 line7`,
			want: []FileAdjustmentSpec{
				{
					FilePath:    "file1.txt",
					ClassicDiff: "2d1",
				},
				{
					FilePath:    "file2.txt",
					ClassicDiff: "6a7",
				},
			},
			wantErr: false,
		},
		{
			name:        "empty diff",
			unifiedDiff: "",
			wantErr:     true,
		},
		{
			name: "file without changes",
			unifiedDiff: `diff --git a/file1.txt b/file1.txt
--- a/file1.txt
+++ b/file1.txt
@@ -1,3 +1,3 @@
 line1
 line2
 line3`,
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ConvertUnifiedDiffToMultiFileClassicDiff(tt.unifiedDiff)
			if (err != nil) != tt.wantErr {
				t.Errorf("ConvertUnifiedDiffToMultiFileClassicDiff() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr {
				if len(got) != len(tt.want) {
					t.Errorf("ConvertUnifiedDiffToMultiFileClassicDiff() returned %d files, want %d", len(got), len(tt.want))
					return
				}
				for i, spec := range got {
					wantSpec := tt.want[i]
					if spec.FilePath != wantSpec.FilePath {
						t.Errorf("ConvertUnifiedDiffToMultiFileClassicDiff()[%d].FilePath = %v, want %v", i, spec.FilePath, wantSpec.FilePath)
					}
					if spec.ClassicDiff != wantSpec.ClassicDiff {
						t.Errorf("ConvertUnifiedDiffToMultiFileClassicDiff()[%d].ClassicDiff = %v, want %v", i, spec.ClassicDiff, wantSpec.ClassicDiff)
					}
				}
			}
		})
	}
}

// TestFilterUnifiedDiffForFile tests filtering unified diff for a specific file.
func TestFilterUnifiedDiffForFile(t *testing.T) {
	multiFileDiff := `diff --git a/file1.txt b/file1.txt
--- a/file1.txt
+++ b/file1.txt
@@ -1,3 +1,2 @@
 line1
-line2
 line3
diff --git a/file2.txt b/file2.txt
--- a/file2.txt
+++ b/file2.txt
@@ -5,3 +5,4 @@
 line5
 line6
+new_line
 line7
diff --git a/file3.txt b/file3.txt
--- a/file3.txt
+++ b/file3.txt
@@ -10,2 +10,3 @@
 line10
+inserted_line
 line11`

	tests := []struct {
		name       string
		targetFile string
		want       string
		wantErr    bool
	}{
		{
			name:       "existing file1",
			targetFile: "file1.txt",
			want:       "2d1",
			wantErr:    false,
		},
		{
			name:       "existing file2",
			targetFile: "file2.txt",
			want:       "6a7",
			wantErr:    false,
		},
		{
			name:       "existing file3",
			targetFile: "file3.txt",
			want:       "10a11",
			wantErr:    false,
		},
		{
			name:       "non-existing file",
			targetFile: "nonexistent.txt",
			want:       "",
			wantErr:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := FilterUnifiedDiffForFile(multiFileDiff, tt.targetFile)
			if (err != nil) != tt.wantErr {
				t.Errorf("FilterUnifiedDiffForFile() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && got != tt.want {
				t.Errorf("FilterUnifiedDiffForFile() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestDetectUnifiedDiffFormat tests unified diff format detection.
func TestDetectUnifiedDiffFormat(t *testing.T) {
	tests := []struct {
		name     string
		diffSpec string
		want     bool
	}{
		{
			name: "valid unified diff with file headers",
			diffSpec: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,2 @@
 line1
-line2
 line3`,
			want: true,
		},
		{
			name: "minimal unified diff (hunk header only)",
			diffSpec: `@@ -1,3 +1,2 @@
 line1
-line2
 line3`,
			want: true,
		},
		{
			name:     "classic diff format",
			diffSpec: "15,17d14",
			want:     false,
		},
		{
			name:     "simple mapping format",
			diffSpec: "15:-2",
			want:     false,
		},
		{
			name:     "empty string",
			diffSpec: "",
			want:     false,
		},
		{
			name: "just file headers without hunks",
			diffSpec: `diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt`,
			want: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := detectUnifiedDiffFormat(tt.diffSpec)
			if got != tt.want {
				t.Errorf("detectUnifiedDiffFormat() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestDetectFormat tests format detection including unified diff.
func TestDetectFormat(t *testing.T) {
	tests := []struct {
		name     string
		diffSpec string
		want     FormatType
	}{
		{
			name:     "classic diff format",
			diffSpec: "15,17d14",
			want:     FormatClassicDiff,
		},
		{
			name:     "simple mapping format",
			diffSpec: "15:-2;30:+3",
			want:     FormatSimpleMapping,
		},
		{
			name: "unified diff format",
			diffSpec: `@@ -1,3 +1,2 @@
 line1
-line2
 line3`,
			want: FormatUnifiedDiff,
		},
		{
			name:     "unknown format",
			diffSpec: "invalid format",
			want:     FormatUnknown,
		},
		{
			name:     "empty string",
			diffSpec: "",
			want:     FormatUnknown,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := DetectFormat(tt.diffSpec)
			if got != tt.want {
				t.Errorf("DetectFormat() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestParseUnifiedDiffSpec tests parsing unified diff specs.
func TestParseUnifiedDiffSpec(t *testing.T) {
	tests := []struct {
		name        string
		spec        string
		wantErr     bool
		wantOpCount int
	}{
		{
			name: "valid unified diff",
			spec: `@@ -1,3 +1,2 @@
 line1
-line2
 line3`,
			wantErr:     false,
			wantOpCount: 1,
		},
		{
			name:        "empty spec",
			spec:        "",
			wantErr:     true,
			wantOpCount: 0,
		},
		{
			name: "malformed unified diff",
			spec: `@@ invalid @@
 line1`,
			wantErr:     true,
			wantOpCount: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseUnifiedDiffSpec(tt.spec)
			if (err != nil) != tt.wantErr {
				t.Errorf("ParseUnifiedDiffSpec() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && len(got) != tt.wantOpCount {
				t.Errorf("ParseUnifiedDiffSpec() returned %d operations, want %d", len(got), tt.wantOpCount)
			}
		})
	}
}

// Helper functions.
func intPtr(i int) *int {
	return &i
}
