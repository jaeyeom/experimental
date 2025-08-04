package models

import (
	"strings"
	"testing"
	"time"
)

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
		wantLine    int
		wantStart   *int
		wantValid   bool
	}{
		{
			name: "single line comment unaffected",
			comment: Comment{
				ID:   "test1",
				Path: "file.go",
				Line: 10,
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  10,
			wantStart: nil,
			wantValid: true,
		},
		{
			name: "single line comment after deletion",
			comment: Comment{
				ID:   "test2",
				Path: "file.go",
				Line: 20,
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  17,
			wantStart: nil,
			wantValid: true,
		},
		{
			name: "multi-line comment adjusted",
			comment: Comment{
				ID:        "test3",
				Path:      "file.go",
				Line:      25,
				StartLine: intPtr(20),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  22,
			wantStart: intPtr(17),
			wantValid: true,
		},
		{
			name: "multi-line comment start deleted",
			comment: Comment{
				ID:        "test4",
				Path:      "file.go",
				Line:      20,
				StartLine: intPtr(16),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  -1,
			wantStart: nil,
			wantValid: false,
		},
		{
			name: "multi-line comment end deleted",
			comment: Comment{
				ID:        "test5",
				Path:      "file.go",
				Line:      16,
				StartLine: intPtr(10),
			},
			adjustments: []LineAdjustment{
				{Operation: OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
			},
			wantLine:  -1,
			wantStart: nil,
			wantValid: false,
		},
		{
			name: "comment preserves original line numbers",
			comment: Comment{
				ID:   "test6",
				Path: "file.go",
				Line: 20,
			},
			adjustments: []LineAdjustment{
				{Operation: OperationInsert, OldStart: 10, OldEnd: 10, NewStart: 11, NewEnd: 13},
			},
			wantLine:  23,
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
					t.Errorf("AdjustComment() Line = %v, want %v", comment.Line, tt.wantLine)
				}

				switch {
				case tt.wantStart == nil && comment.StartLine != nil:
					t.Errorf("AdjustComment() StartLine = %v, want nil", *comment.StartLine)
				case tt.wantStart != nil && comment.StartLine == nil:
					t.Errorf("AdjustComment() StartLine = nil, want %v", *tt.wantStart)
				case tt.wantStart != nil && comment.StartLine != nil && *comment.StartLine != *tt.wantStart:
					t.Errorf("AdjustComment() StartLine = %v, want %v", *comment.StartLine, *tt.wantStart)
				}

				// Check that original line numbers are preserved
				if comment.OriginalLine == 0 {
					t.Error("AdjustComment() should set OriginalLine")
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
			File:      "file.go",
			Side:      "RIGHT",
			StartLine: 10,
			EndLine:   20,
		},
		{
			File:      "file.go",
			Side:      "RIGHT",
			StartLine: 30,
			EndLine:   40,
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
				Line: 35,
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
				Line: 50,
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
				Line: 15,
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
				Line: 15,
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

// Helper functions.
func intPtr(i int) *int {
	return &i
}
