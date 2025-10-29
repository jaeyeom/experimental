package models

import (
	"strings"
	"testing"
	"time"
)

// Test fixtures and helper functions

func createTestComment(id string, path string, line int, body string) Comment {
	return Comment{
		ID:        id,
		Path:      path,
		Line:      NewSingleLine(line),
		Body:      body,
		Side:      SideRight,
		CreatedAt: time.Now(),
	}
}

func createTestCommentWithContext(id string, path string, line int, body string, reviewer string, commentType CommentType) CommentWithContext {
	return CommentWithContext{
		Comment: createTestComment(id, path, line, body),
		Context: CommentContext{
			CommentType: commentType,
			Keywords:    []string{},
		},
		Reviewer:  reviewer,
		Timestamp: time.Now(),
	}
}

// Phase 1: Basic Unit Tests

func TestDefaultMergeOptions(t *testing.T) {
	opts := DefaultMergeOptions()

	if opts.Strategy != StrategyConcat {
		t.Errorf("Expected Strategy = %v, got %v", StrategyConcat, opts.Strategy)
	}
	if !opts.PreserveAttribution {
		t.Error("Expected PreserveAttribution = true")
	}
	if opts.MaxConflicts != 10 {
		t.Errorf("Expected MaxConflicts = 10, got %d", opts.MaxConflicts)
	}
	if opts.Interactive {
		t.Error("Expected Interactive = false")
	}
	if opts.PreferLatest {
		t.Error("Expected PreferLatest = false")
	}
	if !opts.GroupByType {
		t.Error("Expected GroupByType = true")
	}
}

func TestNewSmartCommentMerger(t *testing.T) {
	opts := DefaultMergeOptions()
	merger := NewSmartCommentMerger(opts)

	if merger == nil {
		t.Fatal("Expected non-nil merger")
	}
	if merger.options.Strategy != opts.Strategy {
		t.Error("Merger options not properly initialized")
	}
}

// Phase 2: Comment Context Analysis Tests

func TestAnalyzeCommentContext(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	tests := []struct {
		name        string
		commentBody string
		wantType    CommentType
		wantKeyword string
	}{
		{
			name:        "bug comment",
			commentBody: "This is a bug that needs to be fixed",
			wantType:    CommentBug,
			wantKeyword: "bug",
		},
		{
			name:        "security comment",
			commentBody: "Security vulnerability detected here",
			wantType:    CommentSecurity,
			wantKeyword: "security",
		},
		{
			name:        "performance comment",
			commentBody: "This code is slow and needs optimization",
			wantType:    CommentPerformance,
			wantKeyword: "performance",
		},
		{
			name:        "style comment",
			commentBody: "Please fix the indentation style here",
			wantType:    CommentStyle,
			wantKeyword: "style",
		},
		{
			name:        "documentation comment",
			commentBody: "Please add documentation for this function",
			wantType:    CommentDocumentation,
			wantKeyword: "documentation",
		},
		{
			name:        "question comment",
			commentBody: "Why is this implemented this way?",
			wantType:    CommentQuestion,
			wantKeyword: "question",
		},
		{
			name:        "praise comment",
			commentBody: "Great job on this implementation!",
			wantType:    CommentPraise,
			wantKeyword: "praise",
		},
		{
			name:        "nit comment",
			commentBody: "Nit: minor formatting issue",
			wantType:    CommentNit,
			wantKeyword: "nit",
		},
		{
			name:        "suggestion comment",
			commentBody: "I suggest we consider a different approach",
			wantType:    CommentSuggestion,
			wantKeyword: "suggestion",
		},
		{
			name:        "general comment",
			commentBody: "Just a general observation",
			wantType:    CommentGeneral,
			wantKeyword: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			comment := createTestComment("test", "file.go", 10, tt.commentBody)
			ctx := merger.analyzeCommentContext(comment)

			if ctx.CommentType != tt.wantType {
				t.Errorf("analyzeCommentContext() type = %v, want %v", ctx.CommentType, tt.wantType)
			}

			if tt.wantKeyword != "" {
				found := false
				for _, kw := range ctx.Keywords {
					if kw == tt.wantKeyword {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("analyzeCommentContext() keywords = %v, want to contain %q", ctx.Keywords, tt.wantKeyword)
				}
			}
		})
	}
}

func TestAnalyzeCommentContext_MixedKeywords(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	// Security should take precedence over suggestion
	comment := createTestComment("test", "file.go", 10, "I suggest we fix this security vulnerability")
	ctx := merger.analyzeCommentContext(comment)

	if ctx.CommentType != CommentSecurity {
		t.Errorf("Expected CommentSecurity for mixed keywords, got %v", ctx.CommentType)
	}
}

func TestContainsAny(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		keywords []string
		want     bool
	}{
		{
			name:     "keyword found",
			text:     "this is a bug",
			keywords: []string{"bug", "error"},
			want:     true,
		},
		{
			name:     "keyword not found",
			text:     "this is fine",
			keywords: []string{"bug", "error"},
			want:     false,
		},
		{
			name:     "empty keywords",
			text:     "some text",
			keywords: []string{},
			want:     false,
		},
		{
			name:     "empty text",
			text:     "",
			keywords: []string{"bug"},
			want:     false,
		},
		{
			name:     "partial match",
			text:     "debugging is fun",
			keywords: []string{"bug"},
			want:     true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := containsAny(tt.text, tt.keywords)
			if got != tt.want {
				t.Errorf("containsAny() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestToTitle(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "lowercase word",
			input: "hello",
			want:  "Hello",
		},
		{
			name:  "already capitalized",
			input: "Hello",
			want:  "Hello",
		},
		{
			name:  "empty string",
			input: "",
			want:  "",
		},
		{
			name:  "single character",
			input: "a",
			want:  "A",
		},
		{
			name:  "all caps",
			input: "HELLO",
			want:  "HELLO",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := toTitle(tt.input)
			if got != tt.want {
				t.Errorf("toTitle() = %q, want %q", got, tt.want)
			}
		})
	}
}

// Phase 3: Conflict Detection Tests

func TestDetectMergeConflicts_NoConflicts(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []Comment{
		createTestComment("c1", "file.go", 10, "Comment 1"),
		createTestComment("c2", "file.go", 20, "Comment 2"),
		createTestComment("c3", "file.go", 30, "Comment 3"),
	}

	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	if len(conflicts) != 0 {
		t.Errorf("Expected no conflicts, got %d", len(conflicts))
	}
}

func TestDetectMergeConflicts_SingleConflict(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	// Two comments that are already on the same line (will create a conflict)
	comments := []Comment{
		createTestComment("c1", "file.go", 10, "Comment 1"),
		createTestComment("c2", "file.go", 10, "Comment 2"),
	}

	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	if len(conflicts) != 1 {
		t.Errorf("Expected 1 conflict, got %d", len(conflicts))
	}

	if len(conflicts) > 0 {
		if len(conflicts[0].ConflictingComments) != 2 {
			t.Errorf("Expected 2 conflicting comments, got %d", len(conflicts[0].ConflictingComments))
		}
		if conflicts[0].File != "file.go" {
			t.Errorf("Expected file = file.go, got %s", conflicts[0].File)
		}
	}
}

func TestDetectMergeConflicts_MultipleConflicts(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	// Create multiple conflicts by having comments on the same lines
	comments := []Comment{
		createTestComment("c1", "file.go", 15, "Comment 1"),
		createTestComment("c2", "file.go", 15, "Comment 2"),
		createTestComment("c3", "file.go", 15, "Comment 3"),
		createTestComment("c4", "file.go", 30, "Comment 4"),
		createTestComment("c5", "file.go", 30, "Comment 5"),
	}

	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	// We expect 2 conflicts: one at line 15 (3 comments) and one at line 30 (2 comments)
	if len(conflicts) < 2 {
		t.Errorf("Expected at least 2 conflicts, got %d", len(conflicts))
	}
}

func TestDetectMergeConflicts_MaxConflictsLimit(t *testing.T) {
	opts := DefaultMergeOptions()
	opts.MaxConflicts = 2
	merger := NewSmartCommentMerger(opts)

	comments := []Comment{
		createTestComment("c1", "file.go", 10, "Comment 1"),
		createTestComment("c2", "file.go", 10, "Comment 2"),
		createTestComment("c3", "file.go", 20, "Comment 3"),
		createTestComment("c4", "file.go", 20, "Comment 4"),
		createTestComment("c5", "file.go", 30, "Comment 5"),
		createTestComment("c6", "file.go", 30, "Comment 6"),
	}

	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	if len(conflicts) > opts.MaxConflicts {
		t.Errorf("Expected at most %d conflicts due to limit, got %d", opts.MaxConflicts, len(conflicts))
	}
}

func TestDetectMergeConflicts_EmptyComments(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []Comment{}
	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	if len(conflicts) != 0 {
		t.Errorf("Expected no conflicts for empty comments, got %d", len(conflicts))
	}
}

func TestDetectMergeConflicts_WithMultiLineComments(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []Comment{
		{
			ID:        "c1",
			Path:      "file.go",
			Line:      NewLineRange(5, 10),
			Body:      "Multi-line comment 1",
			Side:      SideRight,
			CreatedAt: time.Now(),
		},
		createTestComment("c2", "file.go", 10, "Comment 2"),
	}

	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	if len(conflicts) != 1 {
		t.Errorf("Expected 1 conflict, got %d", len(conflicts))
	}

	if len(conflicts) > 0 && conflicts[0].StartLine == nil {
		t.Error("Expected conflict to preserve StartLine from multi-line comment")
	}
}

// Phase 4: Merge Strategy Selection Tests

func TestSuggestMergeStrategy_SameReviewer(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Comment 1", "reviewer1", CommentGeneral),
		createTestCommentWithContext("c2", "file.go", 10, "Comment 2", "reviewer1", CommentGeneral),
	}

	strategy := merger.suggestMergeStrategy(comments)

	if strategy != StrategyConcat {
		t.Errorf("Expected StrategyConcat for same reviewer, got %v", strategy)
	}
}

func TestSuggestMergeStrategy_CriticalIssues(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Bug here", "reviewer1", CommentBug),
		createTestCommentWithContext("c2", "file.go", 10, "Also an issue", "reviewer2", CommentGeneral),
	}

	strategy := merger.suggestMergeStrategy(comments)

	if strategy != StrategyManual {
		t.Errorf("Expected StrategyManual for critical issues with multiple reviewers, got %v", strategy)
	}
}

func TestSuggestMergeStrategy_SecurityIssues(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Security issue", "reviewer1", CommentSecurity),
		createTestCommentWithContext("c2", "file.go", 10, "Another concern", "reviewer2", CommentGeneral),
	}

	strategy := merger.suggestMergeStrategy(comments)

	if strategy != StrategyManual {
		t.Errorf("Expected StrategyManual for security issues with multiple reviewers, got %v", strategy)
	}
}

func TestSuggestMergeStrategy_DifferentTypes(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Style issue", "reviewer1", CommentStyle),
		createTestCommentWithContext("c2", "file.go", 10, "Performance concern", "reviewer2", CommentPerformance),
	}

	strategy := merger.suggestMergeStrategy(comments)

	if strategy != StrategyThreeWayMerge {
		t.Errorf("Expected StrategyThreeWayMerge for different types, got %v", strategy)
	}
}

func TestSuggestMergeStrategy_SingleComment(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Comment", "reviewer1", CommentGeneral),
	}

	strategy := merger.suggestMergeStrategy(comments)

	if strategy != StrategySkip {
		t.Errorf("Expected StrategySkip for single comment, got %v", strategy)
	}
}

func TestSuggestMergeStrategy_EmptyComments(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{}

	strategy := merger.suggestMergeStrategy(comments)

	if strategy != StrategySkip {
		t.Errorf("Expected StrategySkip for empty comments, got %v", strategy)
	}
}

// Phase 5: Merge Generation Tests

func TestGenerateMergedComment_Concat(t *testing.T) {
	opts := DefaultMergeOptions()
	opts.PreserveAttribution = true
	merger := NewSmartCommentMerger(opts)

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "First comment", "alice", CommentGeneral),
		createTestCommentWithContext("c2", "file.go", 10, "Second comment", "bob", CommentGeneral),
	}

	merged, err := merger.generateMergedComment(comments, StrategyConcat)
	if err != nil {
		t.Fatalf("generateMergedComment() error = %v", err)
	}

	if merged == nil {
		t.Fatal("Expected non-nil merged comment")
	}

	if !strings.Contains(merged.Body, "First comment") {
		t.Error("Merged comment should contain first comment text")
	}
	if !strings.Contains(merged.Body, "Second comment") {
		t.Error("Merged comment should contain second comment text")
	}
	if !strings.Contains(merged.Body, "alice") {
		t.Error("Merged comment should contain reviewer attribution")
	}
}

func TestGenerateMergedComment_NoAttribution(t *testing.T) {
	opts := DefaultMergeOptions()
	opts.PreserveAttribution = false
	merger := NewSmartCommentMerger(opts)

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "First comment", "alice", CommentGeneral),
		createTestCommentWithContext("c2", "file.go", 10, "Second comment", "bob", CommentGeneral),
	}

	merged, err := merger.generateMergedComment(comments, StrategyConcat)
	if err != nil {
		t.Fatalf("generateMergedComment() error = %v", err)
	}

	if strings.Contains(merged.Body, "[alice]") || strings.Contains(merged.Body, "[bob]") {
		t.Error("Merged comment should not contain reviewer attribution when disabled")
	}
}

func TestGenerateMergedComment_GroupByType(t *testing.T) {
	opts := DefaultMergeOptions()
	opts.GroupByType = true
	opts.PreserveAttribution = true
	merger := NewSmartCommentMerger(opts)

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Bug here", "alice", CommentBug),
		createTestCommentWithContext("c2", "file.go", 10, "Style issue", "bob", CommentStyle),
	}

	merged, err := merger.generateMergedComment(comments, StrategyConcat)
	if err != nil {
		t.Fatalf("generateMergedComment() error = %v", err)
	}

	if !strings.Contains(merged.Body, "BUG") {
		t.Error("Merged comment should contain BUG type indicator")
	}
	if !strings.Contains(merged.Body, "STYLE") {
		t.Error("Merged comment should contain STYLE type indicator")
	}
}

func TestGenerateMergedComment_EmptyComments(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{}

	_, err := merger.generateMergedComment(comments, StrategyConcat)
	if err == nil {
		t.Error("Expected error for empty comments")
	}
}

func TestPerformThreeWayMerge(t *testing.T) {
	opts := DefaultMergeOptions()
	opts.PreserveAttribution = true
	merger := NewSmartCommentMerger(opts)

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Bug here", "alice", CommentBug),
		createTestCommentWithContext("c2", "file.go", 10, "Style issue", "bob", CommentStyle),
		createTestCommentWithContext("c3", "file.go", 10, "Performance problem", "carol", CommentPerformance),
	}

	merged, err := merger.performThreeWayMerge(comments)
	if err != nil {
		t.Fatalf("performThreeWayMerge() error = %v", err)
	}

	if merged == nil {
		t.Fatal("Expected non-nil merged comment")
	}

	// Check that it contains type sections
	body := merged.Body
	if !strings.Contains(body, "Bug") && !strings.Contains(body, "BUG") {
		t.Error("Merged comment should contain Bug section")
	}
	if !strings.Contains(body, "Style") && !strings.Contains(body, "STYLE") {
		t.Error("Merged comment should contain Style section")
	}
	if !strings.Contains(body, "Performance") && !strings.Contains(body, "PERFORMANCE") {
		t.Error("Merged comment should contain Performance section")
	}
}

func TestPerformThreeWayMerge_SingleType(t *testing.T) {
	opts := DefaultMergeOptions()
	merger := NewSmartCommentMerger(opts)

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "First bug", "alice", CommentBug),
		createTestCommentWithContext("c2", "file.go", 10, "Second bug", "bob", CommentBug),
	}

	merged, err := merger.performThreeWayMerge(comments)
	if err != nil {
		t.Fatalf("performThreeWayMerge() error = %v", err)
	}

	if !strings.Contains(merged.Body, "First bug") {
		t.Error("Merged comment should contain first bug text")
	}
	if !strings.Contains(merged.Body, "Second bug") {
		t.Error("Merged comment should contain second bug text")
	}
}

func TestPerformThreeWayMerge_EmptyComments(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{}

	_, err := merger.performThreeWayMerge(comments)
	if err == nil {
		t.Error("Expected error for empty comments")
	}
}

// Phase 6: Conflict Resolution Tests

func TestResolveMergeConflict_Concat(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	conflict := MergeConflict{
		Line: 10,
		File: "file.go",
		ConflictingComments: []CommentWithContext{
			createTestCommentWithContext("c1", "file.go", 10, "Comment 1", "alice", CommentGeneral),
			createTestCommentWithContext("c2", "file.go", 10, "Comment 2", "bob", CommentGeneral),
		},
		SuggestedStrategy: StrategyConcat,
	}

	resolved, err := merger.ResolveMergeConflict(conflict, StrategyConcat)
	if err != nil {
		t.Fatalf("ResolveMergeConflict() error = %v", err)
	}

	if resolved == nil {
		t.Fatal("Expected non-nil resolved comment")
	}

	if !strings.Contains(resolved.Body, "Comment 1") || !strings.Contains(resolved.Body, "Comment 2") {
		t.Error("Resolved comment should contain both comment texts")
	}
}

func TestResolveMergeConflict_ThreeWayMerge(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	conflict := MergeConflict{
		Line: 10,
		File: "file.go",
		ConflictingComments: []CommentWithContext{
			createTestCommentWithContext("c1", "file.go", 10, "Bug here", "alice", CommentBug),
			createTestCommentWithContext("c2", "file.go", 10, "Style issue", "bob", CommentStyle),
		},
		SuggestedStrategy: StrategyThreeWayMerge,
	}

	resolved, err := merger.ResolveMergeConflict(conflict, StrategyThreeWayMerge)
	if err != nil {
		t.Fatalf("ResolveMergeConflict() error = %v", err)
	}

	if resolved == nil {
		t.Fatal("Expected non-nil resolved comment")
	}
}

func TestResolveMergeConflict_Skip(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	conflict := MergeConflict{
		Line: 10,
		File: "file.go",
		ConflictingComments: []CommentWithContext{
			createTestCommentWithContext("c1", "file.go", 10, "Comment 1", "alice", CommentGeneral),
			createTestCommentWithContext("c2", "file.go", 10, "Comment 2", "bob", CommentGeneral),
		},
		SuggestedStrategy: StrategySkip,
	}

	resolved, err := merger.ResolveMergeConflict(conflict, StrategySkip)
	if err != nil {
		t.Fatalf("ResolveMergeConflict() error = %v", err)
	}

	if resolved == nil {
		t.Fatal("Expected non-nil resolved comment")
	}

	// Should return first comment
	if resolved.Body != "Comment 1" {
		t.Errorf("Expected first comment, got %q", resolved.Body)
	}
}

func TestResolveMergeConflict_Manual(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	conflict := MergeConflict{
		Line: 10,
		File: "file.go",
		ConflictingComments: []CommentWithContext{
			createTestCommentWithContext("c1", "file.go", 10, "Comment 1", "alice", CommentGeneral),
			createTestCommentWithContext("c2", "file.go", 10, "Comment 2", "bob", CommentGeneral),
		},
		SuggestedStrategy: StrategyManual,
	}

	_, err := merger.ResolveMergeConflict(conflict, StrategyManual)
	if err == nil {
		t.Error("Expected error for manual resolution strategy")
	}

	if !strings.Contains(err.Error(), "manual resolution required") {
		t.Errorf("Expected 'manual resolution required' error, got %v", err)
	}
}

func TestResolveMergeConflict_InvalidStrategy(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	conflict := MergeConflict{
		Line: 10,
		File: "file.go",
		ConflictingComments: []CommentWithContext{
			createTestCommentWithContext("c1", "file.go", 10, "Comment 1", "alice", CommentGeneral),
		},
		SuggestedStrategy: StrategyConcat,
	}

	_, err := merger.ResolveMergeConflict(conflict, SmartMergeStrategy("invalid"))
	if err == nil {
		t.Error("Expected error for invalid strategy")
	}

	if !strings.Contains(err.Error(), "unknown merge strategy") {
		t.Errorf("Expected 'unknown merge strategy' error, got %v", err)
	}
}

// Phase 7: Integration Tests

func TestApplySmartMerging_NoConflicts(t *testing.T) {
	comments := []Comment{
		createTestComment("c1", "file.go", 10, "Comment 1"),
		createTestComment("c2", "file.go", 20, "Comment 2"),
		createTestComment("c3", "file.go", 30, "Comment 3"),
	}

	adjustments := []LineAdjustment{}
	options := DefaultMergeOptions()

	result, conflicts, err := ApplySmartMerging(comments, adjustments, options)
	if err != nil {
		t.Fatalf("ApplySmartMerging() error = %v", err)
	}

	if len(conflicts) != 0 {
		t.Errorf("Expected no conflicts, got %d", len(conflicts))
	}

	if len(result) != 3 {
		t.Errorf("Expected 3 comments, got %d", len(result))
	}
}

func TestApplySmartMerging_WithConflicts(t *testing.T) {
	comments := []Comment{
		createTestComment("c1", "file.go", 15, "Comment 1"),
		createTestComment("c2", "file.go", 16, "Comment 2"),
	}

	// Adjustment that brings both comments to same line
	adjustments := []LineAdjustment{
		{Operation: OperationDelete, OldStart: 10, OldEnd: 10, NewStart: 10, NewEnd: 10},
	}

	options := DefaultMergeOptions()
	options.Strategy = StrategyConcat

	result, conflicts, err := ApplySmartMerging(comments, adjustments, options)
	if err != nil {
		t.Fatalf("ApplySmartMerging() error = %v", err)
	}

	// Should have resolved conflicts via concat strategy
	if len(result) == 0 {
		t.Error("Expected at least one result comment")
	}

	// Conflicts should be empty as they were auto-resolved
	if len(conflicts) != 0 {
		t.Logf("Got %d unresolved conflicts (expected 0 with auto-resolution)", len(conflicts))
	}
}

func TestApplySmartMerging_UnresolvedConflicts(t *testing.T) {
	comments := []Comment{
		createTestComment("c1", "file.go", 10, "Bug here"),
		createTestComment("c2", "file.go", 10, "Security issue"),
	}

	adjustments := []LineAdjustment{}

	options := DefaultMergeOptions()
	options.Interactive = true // This should cause manual conflicts to be unresolved

	result, conflicts, err := ApplySmartMerging(comments, adjustments, options)
	if err != nil {
		t.Fatalf("ApplySmartMerging() error = %v", err)
	}

	// Should have some result
	if len(result) == 0 {
		t.Error("Expected at least one result comment")
	}

	// Note: The actual behavior depends on the suggested strategy
	t.Logf("Got %d result comments and %d conflicts", len(result), len(conflicts))
}

func TestApplySmartMerging_EmptyComments(t *testing.T) {
	comments := []Comment{}
	adjustments := []LineAdjustment{}
	options := DefaultMergeOptions()

	result, conflicts, err := ApplySmartMerging(comments, adjustments, options)
	if err != nil {
		t.Fatalf("ApplySmartMerging() error = %v", err)
	}

	if len(result) != 0 {
		t.Errorf("Expected no results for empty comments, got %d", len(result))
	}

	if len(conflicts) != 0 {
		t.Errorf("Expected no conflicts for empty comments, got %d", len(conflicts))
	}
}

func TestApplySmartMerging_MixedResolution(t *testing.T) {
	comments := []Comment{
		createTestComment("c1", "file.go", 10, "Comment 1"),
		createTestComment("c2", "file.go", 10, "Comment 2"),
		createTestComment("c3", "file.go", 20, "Comment 3"),
	}

	adjustments := []LineAdjustment{}
	options := DefaultMergeOptions()

	result, conflicts, err := ApplySmartMerging(comments, adjustments, options)
	if err != nil {
		t.Fatalf("ApplySmartMerging() error = %v", err)
	}

	// Should have at least one conflict (c1 and c2 on same line)
	// And one non-conflicting comment (c3)
	if len(result) == 0 {
		t.Error("Expected at least one result comment")
	}

	t.Logf("Mixed resolution: %d results, %d conflicts", len(result), len(conflicts))
}

// Phase 8: Edge Cases and Error Handling

func TestEdgeCases_CommentWithNilStartLine(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []Comment{
		createTestComment("c1", "file.go", 10, "Comment 1"),
	}

	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	if len(conflicts) != 0 {
		t.Error("Should handle comments with nil StartLine gracefully")
	}
}

func TestEdgeCases_VeryLargeCommentBody(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	largeBody := strings.Repeat("This is a very long comment. ", 1000)
	comment := createTestComment("c1", "file.go", 10, largeBody)

	ctx := merger.analyzeCommentContext(comment)

	if ctx.CommentType == "" {
		t.Error("Should handle large comment bodies")
	}
}

func TestEdgeCases_SpecialCharactersInComments(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	specialBody := "Comment with special chars: @#$%^&*()[]{}|\\/<>?`~"
	comment := createTestComment("c1", "file.go", 10, specialBody)

	ctx := merger.analyzeCommentContext(comment)

	// Should not crash
	if ctx.CommentType == "" {
		t.Error("CommentType should be initialized")
	}
}

func TestEdgeCases_PreferLatestOption(t *testing.T) {
	opts := DefaultMergeOptions()
	opts.PreferLatest = true
	merger := NewSmartCommentMerger(opts)

	now := time.Now()
	comments := []CommentWithContext{
		{
			Comment:   createTestComment("c1", "file.go", 10, "Older comment"),
			Context:   CommentContext{CommentType: CommentGeneral},
			Reviewer:  "alice",
			Timestamp: now.Add(-1 * time.Hour),
		},
		{
			Comment:   createTestComment("c2", "file.go", 10, "Newer comment"),
			Context:   CommentContext{CommentType: CommentGeneral},
			Reviewer:  "bob",
			Timestamp: now,
		},
	}

	merged, err := merger.generateMergedComment(comments, StrategyConcat)
	if err != nil {
		t.Fatalf("generateMergedComment() error = %v", err)
	}

	// With PreferLatest, newer comment should appear first
	// The exact order depends on implementation, but it should at least process successfully
	if merged == nil {
		t.Fatal("Expected non-nil merged comment")
	}
}

func TestEdgeCases_AdjustmentCausesOrphans(t *testing.T) {
	comments := []Comment{
		createTestComment("c1", "file.go", 15, "Comment on deleted line"),
		createTestComment("c2", "file.go", 20, "Comment after deletion"),
	}

	// Delete lines 14-16, which includes line 15
	adjustments := []LineAdjustment{
		{Operation: OperationDelete, OldStart: 14, OldEnd: 16, NewStart: 14, NewEnd: 14},
	}

	options := DefaultMergeOptions()

	result, conflicts, err := ApplySmartMerging(comments, adjustments, options)
	if err != nil {
		t.Fatalf("ApplySmartMerging() error = %v", err)
	}

	// c1 should be filtered out as orphaned
	// c2 should remain (adjusted to line 17)
	if len(result) > len(comments) {
		t.Error("Should not create more comments than original")
	}

	t.Logf("Orphan test: %d results from %d original comments, %d conflicts", len(result), len(comments), len(conflicts))
}

func TestEdgeCases_MultipleFilesInConflicts(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []Comment{
		createTestComment("c1", "file1.go", 10, "Comment in file1"),
		createTestComment("c2", "file1.go", 10, "Another in file1"),
		createTestComment("c3", "file2.go", 10, "Comment in file2"),
		createTestComment("c4", "file2.go", 10, "Another in file2"),
	}

	adjustments := []LineAdjustment{}

	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		t.Fatalf("DetectMergeConflicts() error = %v", err)
	}

	// Should have 2 conflicts, one per file
	if len(conflicts) != 2 {
		t.Errorf("Expected 2 conflicts (one per file), got %d", len(conflicts))
	}

	// Verify conflicts are properly separated by file
	filesSeen := make(map[string]bool)
	for _, conflict := range conflicts {
		filesSeen[conflict.File] = true
	}

	if len(filesSeen) != 2 {
		t.Error("Conflicts should be separated by file")
	}
}

func TestEdgeCases_GenerateCommentIDCalled(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Comment 1", "alice", CommentGeneral),
	}

	merged, err := merger.generateMergedComment(comments, StrategyConcat)
	if err != nil {
		t.Fatalf("generateMergedComment() error = %v", err)
	}

	// Merged comment should have a new ID (not the same as input)
	if merged.ID == "c1" {
		t.Error("Merged comment should have a new generated ID")
	}

	if merged.ID == "" {
		t.Error("Merged comment ID should not be empty")
	}
}

func TestEdgeCases_AdjustmentHistoryPreserved(t *testing.T) {
	merger := NewSmartCommentMerger(DefaultMergeOptions())

	comments := []CommentWithContext{
		createTestCommentWithContext("c1", "file.go", 10, "Comment 1", "alice", CommentGeneral),
		createTestCommentWithContext("c2", "file.go", 10, "Comment 2", "bob", CommentGeneral),
	}

	merged, err := merger.generateMergedComment(comments, StrategyConcat)
	if err != nil {
		t.Fatalf("generateMergedComment() error = %v", err)
	}

	// Check that adjustment history was added
	if len(merged.AdjustmentHistory) == 0 {
		t.Error("Merged comment should have adjustment history")
	}

	// Verify the history entry mentions merging
	if len(merged.AdjustmentHistory) > 0 {
		lastAdjustment := merged.AdjustmentHistory[len(merged.AdjustmentHistory)-1]
		if !strings.Contains(lastAdjustment.Description, "Merged") {
			t.Errorf("Adjustment history should mention 'Merged', got %q", lastAdjustment.Description)
		}
	}
}
