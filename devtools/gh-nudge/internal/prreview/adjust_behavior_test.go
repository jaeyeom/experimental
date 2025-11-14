package prreview

import (
	"fmt"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

// TestAdjustCommandUnifiedDiff_SingleFile tests unified diff processing for a single file.
func TestAdjustCommandUnifiedDiff_SingleFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-unified-single-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)
	file := "src/main.go"

	// Setup diff hunks and comments
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add test comments
	comments := []models.Comment{
		{
			ID:        "comment1",
			Path:      file,
			Line:      models.NewSingleLine(20),
			Body:      "Comment at line 20",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "comment2",
			Path:      file,
			Line:      models.NewSingleLine(30),
			Body:      "Comment at line 30",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	// Create unified diff that deletes lines 15-17 (3 lines)
	unifiedDiff := `--- a/src/main.go
+++ b/src/main.go
@@ -15,6 +12,3 @@
 line 14
-line 15
-line 16
-line 17
 line 18
`

	t.Run("processes unified diff for single file with dryRun", func(t *testing.T) {
		err := handler.AdjustCommand(repository, fmt.Sprintf("%d", prNumber),
			file, unifiedDiff, true, false, "table")
		if err != nil {
			t.Errorf("AdjustCommand failed: %v", err)
		}

		// Comments should not change in dry run
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}
		if stored.Comments[0].Line != models.NewSingleLine(20) {
			t.Errorf("Comment should not change in dry run")
		}
	})

	t.Run("processes unified diff for single file without dryRun", func(t *testing.T) {
		err := handler.AdjustCommand(repository, fmt.Sprintf("%d", prNumber),
			file, unifiedDiff, false, false, "table")
		if err != nil {
			t.Errorf("AdjustCommand failed: %v", err)
		}

		// Comments should be adjusted (line 20 -> 17, line 30 -> 27)
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}

		// Line 20 should move to 17 (20 - 3 deleted lines)
		if stored.Comments[0].Line != models.NewSingleLine(17) {
			t.Errorf("Comment at line 20 should move to 17, got %v", stored.Comments[0].Line)
		}
		// Line 30 should move to 27
		if stored.Comments[1].Line != models.NewSingleLine(27) {
			t.Errorf("Comment at line 30 should move to 27, got %v", stored.Comments[1].Line)
		}
	})
}

// TestAdjustCommandUnifiedDiff_MultiFile tests unified diff processing for multiple files.
func TestAdjustCommandUnifiedDiff_MultiFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-unified-multi-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)

	// Setup diff hunks for multiple files
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation("file1.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
			{
				Location: models.NewFileLocation("file2.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments in both files
	comments := []models.Comment{
		{
			ID:        "file1_comment",
			Path:      "file1.go",
			Line:      models.NewSingleLine(20),
			Body:      "File1 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "file2_comment",
			Path:      "file2.go",
			Line:      models.NewSingleLine(25),
			Body:      "File2 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	// Multi-file unified diff
	multiFileUnifiedDiff := `diff --git a/file1.go b/file1.go
--- a/file1.go
+++ b/file1.go
@@ -15,6 +15,3 @@
-line 15
-line 16
-line 17
 line 18
diff --git a/file2.go b/file2.go
--- a/file2.go
+++ b/file2.go
@@ -20,4 +20,2 @@
-line 20
-line 21
 line 22
`

	t.Run("processes multiple files from unified diff", func(t *testing.T) {
		// Use empty file parameter to process all files
		err := handler.AdjustCommand(repository, fmt.Sprintf("%d", prNumber),
			"", multiFileUnifiedDiff, false, false, "table")
		if err != nil {
			t.Errorf("AdjustCommand failed: %v", err)
		}

		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}

		// file1.go: line 20 -> 17 (3 lines deleted)
		// file2.go: line 25 -> 23 (2 lines deleted)
		for _, comment := range stored.Comments {
			if comment.Path == "file1.go" && comment.Line != models.NewSingleLine(17) {
				t.Errorf("file1.go comment should move to line 17, got %v", comment.Line)
			}
			if comment.Path == "file2.go" && comment.Line != models.NewSingleLine(23) {
				t.Errorf("file2.go comment should move to line 23, got %v", comment.Line)
			}
		}
	})
}

// TestApplyPRAdjustmentsWithCounts tests the behavior of counting adjusted/orphaned/warnings.
func TestApplyPRAdjustmentsWithCounts(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-adjust-counts-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)
	file := "test.go"

	// Setup diff hunks
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(10, 50)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments - some will be adjusted, some orphaned
	comments := []models.Comment{
		{
			ID:        "adjusted1",
			Path:      file,
			Line:      models.NewSingleLine(25),
			Body:      "Will be adjusted",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "adjusted2",
			Path:      file,
			Line:      models.NewSingleLine(35),
			Body:      "Will also be adjusted",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "orphaned",
			Path:      file,
			Line:      models.NewSingleLine(20),
			Body:      "Will be orphaned (on deleted line)",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	// Parse adjustments that delete line 20
	adjustments, err := models.ParseDiffSpecWithAutoDetection("20d19")
	if err != nil {
		t.Fatal(err)
	}

	t.Run("counts adjusted comments correctly", func(t *testing.T) {
		target := models.NewPRTarget(prNumber)
		adjusted, orphaned, warnings, err := handler.applyAdjustmentsWithCounts(
			repository, target, file, adjustments, false)
		if err != nil {
			t.Errorf("applyAdjustmentsWithCountsUnified failed: %v", err)
		}

		// Should adjust 2 comments (line 25 and 35 move down)
		if adjusted != 2 {
			t.Errorf("Expected 2 adjusted comments, got %d", adjusted)
		}

		// Should orphan 1 comment (line 20 is deleted)
		if orphaned != 1 {
			t.Errorf("Expected 1 orphaned comment, got %d", orphaned)
		}

		// No warnings with these simple adjustments
		if warnings != 0 {
			t.Errorf("Expected 0 warnings, got %d", warnings)
		}
	})

	t.Run("force flag preserves orphaned comments with marker", func(t *testing.T) {
		// Re-add comments
		if err := store.ClearComments(repository, target); err != nil {
			t.Fatal(err)
		}
		for _, comment := range comments {
			if err := store.AddComment(repository, target, comment); err != nil {
				t.Fatal(err)
			}
		}

		target := models.NewPRTarget(prNumber)
		adjusted, orphaned, _, err := handler.applyAdjustmentsWithCounts(
			repository, target, file, adjustments, true)
		if err != nil {
			t.Errorf("applyAdjustmentsWithCountsUnified with force failed: %v", err)
		}

		if adjusted != 2 || orphaned != 1 {
			t.Errorf("Expected adjusted=2, orphaned=1, got adjusted=%d, orphaned=%d",
				adjusted, orphaned)
		}

		// Verify orphaned comment has marker
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}

		foundOrphanedMarker := false
		for _, comment := range stored.Comments {
			if strings.Contains(comment.Body, "[ORPHANED") {
				foundOrphanedMarker = true
				break
			}
		}
		if !foundOrphanedMarker {
			t.Error("Expected orphaned comment to have [ORPHANED] marker with force flag")
		}
	})
}

// TestApplyBranchAdjustmentsWithCounts tests branch-specific counting behavior.
func TestApplyBranchAdjustmentsWithCounts(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-branch-counts-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	branchName := "feature/test"
	branchTarget := models.NewBranchTarget(branchName)
	file := "test.go"

	// Setup branch diff hunks
	branchDiffHunks := models.ReviewDiffHunks{
		Target:     branchTarget.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(10, 50)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, branchTarget, branchDiffHunks); err != nil {
		t.Fatal(err)
	}

	// Add branch comments
	comments := []models.Comment{
		{
			ID:        "branch1",
			Path:      file,
			Line:      models.NewSingleLine(25),
			Body:      "Branch comment 1",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "branch2",
			Path:      file,
			Line:      models.NewSingleLine(30),
			Body:      "Branch comment 2",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, branchTarget, comment); err != nil {
			t.Fatal(err)
		}
	}

	// Simple adjustment: delete lines 20-22
	adjustments, err := models.ParseDiffSpecWithAutoDetection("20,22d19")
	if err != nil {
		t.Fatal(err)
	}

	t.Run("counts branch adjustments correctly", func(t *testing.T) {
		target := models.NewBranchTarget(branchName)
		adjusted, orphaned, warnings, err := handler.applyAdjustmentsWithCounts(
			repository, target, file, adjustments, false)
		if err != nil {
			t.Errorf("applyAdjustmentsWithCountsUnified failed: %v", err)
		}

		// Both comments should be adjusted (moved down by 3 lines)
		if adjusted != 2 {
			t.Errorf("Expected 2 adjusted comments, got %d", adjusted)
		}

		if orphaned != 0 {
			t.Errorf("Expected 0 orphaned comments, got %d", orphaned)
		}

		if warnings != 0 {
			t.Errorf("Expected 0 warnings, got %d", warnings)
		}
	})
}

// TestFormatTablePreview tests table formatting behavior.
func TestFormatTablePreview(t *testing.T) {
	tests := []struct {
		name         string
		preview      AdjustmentPreview
		wantContains []string
		wantErr      bool
	}{
		{
			name: "formats basic preview with adjustments",
			preview: AdjustmentPreview{
				Repository: models.NewRepository("owner", "repo"),
				Identifier: "123",
				File:       "test.go",
				DiffSpec:   "15,17d14",
				Adjustments: []models.LineAdjustment{
					{Operation: models.OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 14, NewEnd: 14},
				},
				CommentChanges: []CommentChange{
					{
						CommentID:      "abc123",
						CommentIDShort: "abc123",
						OriginalLine:   models.NewSingleLine(20),
						NewLine:        models.NewSingleLine(17),
						Body:           "Test comment",
						Status:         "adjusted",
					},
				},
			},
			wantContains: []string{"owner/repo", "123", "test.go", "15,17d14", "abc123", "adjusted"},
			wantErr:      false,
		},
		{
			name: "includes warnings in output",
			preview: AdjustmentPreview{
				Repository: models.NewRepository("owner", "repo"),
				Identifier: "123",
				File:       "test.go",
				DiffSpec:   "10d9",
				Adjustments: []models.LineAdjustment{
					{Operation: models.OperationDelete, OldStart: 10, OldEnd: 10, NewStart: 9, NewEnd: 9},
				},
				CommentChanges: []CommentChange{
					{
						CommentID:      "warn1",
						CommentIDShort: "warn1",
						OriginalLine:   models.NewSingleLine(10),
						NewLine:        models.LineRange{StartLine: -1, EndLine: -1},
						Body:           "Deleted comment",
						Status:         "deleted",
						Warning:        "Comment on deleted line",
					},
				},
				Warnings: []string{"Comment warn1 will be marked as orphaned"},
			},
			wantContains: []string{"Warnings:", "⚠", "orphaned"},
			wantErr:      false,
		},
		{
			name: "handles empty comments list",
			preview: AdjustmentPreview{
				Repository: models.NewRepository("owner", "repo"),
				Identifier: "123",
				File:       "empty.go",
				DiffSpec:   "10d9",
				Adjustments: []models.LineAdjustment{
					{Operation: models.OperationDelete, OldStart: 10, OldEnd: 10, NewStart: 9, NewEnd: 9},
				},
				CommentChanges: []CommentChange{},
			},
			wantContains: []string{"No comments in this file"},
			wantErr:      false,
		},
		{
			name: "formats multi-line comments",
			preview: AdjustmentPreview{
				Repository: models.NewRepository("owner", "repo"),
				Identifier: "123",
				File:       "test.go",
				DiffSpec:   "10d9",
				Adjustments: []models.LineAdjustment{
					{Operation: models.OperationDelete, OldStart: 10, OldEnd: 10, NewStart: 9, NewEnd: 9},
				},
				CommentChanges: []CommentChange{
					{
						CommentID:      "multi",
						CommentIDShort: "multi",
						OriginalLine:   models.NewLineRange(20, 25),
						NewLine:        models.NewLineRange(19, 24),
						Body:           "Multi-line comment",
						Status:         "adjusted",
					},
				},
			},
			wantContains: []string{"20-25", "19-24"},
			wantErr:      false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := formatTablePreview(tt.preview)
			if (err != nil) != tt.wantErr {
				t.Errorf("formatTablePreview() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			for _, want := range tt.wantContains {
				if !strings.Contains(result, want) {
					t.Errorf("formatTablePreview() result should contain %q, got:\n%s", want, result)
				}
			}
		})
	}
}

// TestFormatJSONPreview tests JSON formatting behavior.
func TestFormatJSONPreview(t *testing.T) {
	preview := AdjustmentPreview{
		Repository: models.NewRepository("owner", "repo"),
		Identifier: "123",
		File:       "test.go",
		DiffSpec:   "15,17d14",
		Adjustments: []models.LineAdjustment{
			{Operation: models.OperationDelete, OldStart: 15, OldEnd: 17, NewStart: 9, NewEnd: 9},
		},
		CommentChanges: []CommentChange{
			{
				CommentID:      "abc123",
				CommentIDShort: "abc123",
				OriginalLine:   models.NewSingleLine(20),
				NewLine:        models.NewSingleLine(17),
				Body:           "Test comment",
				Status:         "adjusted",
			},
		},
		Warnings: []string{"Warning 1"},
	}

	result, err := formatJSONPreview(preview)
	if err != nil {
		t.Errorf("formatJSONPreview() error = %v", err)
		return
	}

	// Verify JSON contains expected fields
	expectedFields := []string{
		`"repository"`,
		`"identifier"`,
		`"file"`,
		`"diffSpec"`,
		`"adjustments"`,
		`"commentChanges"`,
		`"warnings"`,
		`"abc123"`,
	}

	for _, field := range expectedFields {
		if !strings.Contains(result, field) {
			t.Errorf("JSON should contain %s, got:\n%s", field, result)
		}
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
			name:   "truncates long string",
			input:  "This is a very long string that needs to be truncated",
			maxLen: 20,
			want:   "This is a very lo...",
		},
		{
			name:   "preserves short string",
			input:  "Short",
			maxLen: 20,
			want:   "Short",
		},
		{
			name:   "replaces newlines with spaces",
			input:  "Line 1\nLine 2\nLine 3",
			maxLen: 30,
			want:   "Line 1 Line 2 Line 3",
		},
		{
			name:   "truncates string with newlines",
			input:  "Line 1\nLine 2\nLine 3 is very long",
			maxLen: 15,
			want:   "Line 1 Line ...",
		},
		{
			name:   "handles exact length",
			input:  "Exactly20Characters!",
			maxLen: 20,
			want:   "Exactly20Characters!",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := truncateString(tt.input, tt.maxLen)
			if got != tt.want {
				t.Errorf("truncateString() = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestExtractUniqueFilePaths tests unique file path extraction.
func TestExtractUniqueFilePaths(t *testing.T) {
	comments := []models.Comment{
		{Path: "file1.go"},
		{Path: "file2.go"},
		{Path: "file1.go"}, // Duplicate
		{Path: "file3.go"},
		{Path: "file2.go"}, // Duplicate
	}

	paths := extractUniqueFilePaths(comments)

	// Should have 3 unique paths
	if len(paths) != 3 {
		t.Errorf("Expected 3 unique paths, got %d", len(paths))
	}

	// Verify all unique paths are present
	pathMap := make(map[string]bool)
	for _, p := range paths {
		pathMap[p] = true
	}

	expectedPaths := []string{"file1.go", "file2.go", "file3.go"}
	for _, expected := range expectedPaths {
		if !pathMap[expected] {
			t.Errorf("Expected path %s not found in result", expected)
		}
	}
}

// TestAdjustCommandExtended_MappingFile tests mapping file processing behavior.
func TestAdjustCommandExtended_MappingFile(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-mapping-file-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)

	// Setup diff hunks for multiple files
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation("file1.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
			{
				Location: models.NewFileLocation("file2.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments
	comments := []models.Comment{
		{
			ID:        "file1_c1",
			Path:      "file1.go",
			Line:      models.NewSingleLine(20),
			Body:      "File1 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "file2_c1",
			Path:      "file2.go",
			Line:      models.NewSingleLine(30),
			Body:      "File2 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	// Create mapping file
	mappingContent := `file1.go:15:-2
file1.go:25:+3
file2.go:20:-1
# Comment line
file2.go:35:+2
`
	mappingFile := tmpDir + "/mapping.txt"
	if err := os.WriteFile(mappingFile, []byte(mappingContent), 0o600); err != nil {
		t.Fatal(err)
	}

	t.Run("processes mapping file for multiple files", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			DryRun:      false,
			Force:       false,
			Format:      "table",
			MappingFile: mappingFile,
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			"", "", opts)
		if err != nil {
			t.Errorf("AdjustCommandExtended with mapping file failed: %v", err)
		}

		// Verify comments were adjusted according to mapping
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}

		for _, comment := range stored.Comments {
			if comment.Path == "file1.go" {
				// Line 20 should be adjusted based on mappings at 15:-2 and 25:+3
				// Since 20 is after 15 but before 25, it gets -2 offset -> 18
				if comment.Line != models.NewSingleLine(18) {
					t.Errorf("file1.go comment should be at line 18, got %v", comment.Line)
				}
			}
			if comment.Path == "file2.go" {
				// Line 30 should be adjusted based on mapping at 20:-1
				// Since 30 is after 20, it gets -1 offset -> 29
				if comment.Line != models.NewSingleLine(29) {
					t.Errorf("file2.go comment should be at line 29, got %v", comment.Line)
				}
			}
		}
	})

	t.Run("handles invalid mapping file", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			MappingFile: "/nonexistent/mapping.txt",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			"", "", opts)

		if err == nil {
			t.Error("Expected error for nonexistent mapping file")
		}
	})
}

// TestAdjustCommandExtended_AllFiles tests all-files batch processing.
func TestAdjustCommandExtended_AllFiles(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-all-files-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)

	// Setup diff hunks for multiple files
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation("file1.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
			{
				Location: models.NewFileLocation("file2.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
			{
				Location: models.NewFileLocation("file3.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments in multiple files
	comments := []models.Comment{
		{
			ID:        "f1c1",
			Path:      "file1.go",
			Line:      models.NewSingleLine(20),
			Body:      "File1 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "f2c1",
			Path:      "file2.go",
			Line:      models.NewSingleLine(25),
			Body:      "File2 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "f3c1",
			Path:      "file3.go",
			Line:      models.NewSingleLine(30),
			Body:      "File3 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	t.Run("processes all files with comments", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			DryRun:   false,
			Force:    false,
			Format:   "table",
			AllFiles: true,
		}

		// Simple adjustment: delete line 15 in all files
		diffSpec := "15d14"

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			"", diffSpec, opts)
		if err != nil {
			t.Errorf("AdjustCommandExtended with AllFiles failed: %v", err)
		}

		// Verify all comments in all files were adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}

		if len(stored.Comments) != 3 {
			t.Errorf("Expected 3 comments after adjustment, got %d", len(stored.Comments))
		}

		// All comments should have been adjusted down by 1 line
		for _, comment := range stored.Comments {
			var expectedLine int
			switch comment.Path {
			case "file1.go":
				expectedLine = 19 // 20 - 1
			case "file2.go":
				expectedLine = 24 // 25 - 1
			case "file3.go":
				expectedLine = 29 // 30 - 1
			}
			if comment.Line != models.NewSingleLine(expectedLine) {
				t.Errorf("%s comment should be at line %d, got %v",
					comment.Path, expectedLine, comment.Line)
			}
		}
	})

	t.Run("handles empty comment list gracefully", func(t *testing.T) {
		// Clear all comments
		if err := store.ClearComments(repository, target); err != nil {
			t.Fatal(err)
		}

		opts := AdjustOptionsExtended{
			DryRun:   false,
			AllFiles: true,
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			"", "15d14", opts)
		// Should not error, just report no files with comments
		if err != nil {
			t.Errorf("Should handle empty comments gracefully: %v", err)
		}
	})
}

// TestPrintBatchSummary tests batch summary output behavior.
func TestPrintBatchSummary(t *testing.T) {
	handler := &CommandHandler{}

	tests := []struct {
		name            string
		dryRun          bool
		totalAdjusted   int
		totalOrphaned   int
		totalWarnings   int
		fileCount       int
		expectDryRunMsg bool
	}{
		{
			name:            "dry run shows dry run message",
			dryRun:          true,
			totalAdjusted:   5,
			totalOrphaned:   1,
			totalWarnings:   2,
			fileCount:       3,
			expectDryRunMsg: true,
		},
		{
			name:            "actual run shows summary",
			dryRun:          false,
			totalAdjusted:   10,
			totalOrphaned:   2,
			totalWarnings:   3,
			fileCount:       5,
			expectDryRunMsg: false,
		},
		{
			name:            "zero counts handled correctly",
			dryRun:          false,
			totalAdjusted:   0,
			totalOrphaned:   0,
			totalWarnings:   0,
			fileCount:       0,
			expectDryRunMsg: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(_ *testing.T) {
			// printBatchSummary just prints to stdout, so we verify it doesn't panic
			handler.printBatchSummary(tt.dryRun, tt.totalAdjusted, tt.totalOrphaned,
				tt.totalWarnings, tt.fileCount)
		})
	}
}

// TestGetAllFilesWithComments tests file path collection behavior.
func TestGetAllFilesWithComments(t *testing.T) {
	t.Run("gets files from PR comments", func(t *testing.T) {
		tmpDir, err := os.MkdirTemp("", "test-get-files-pr-*")
		if err != nil {
			t.Fatal(err)
		}
		defer os.RemoveAll(tmpDir)

		store, err := storage.NewGitHubStorage(tmpDir)
		if err != nil {
			t.Fatal(err)
		}

		handler := &CommandHandler{
			storage:     store,
			storageHome: tmpDir,
		}

		repository := models.NewRepository("testowner", "testrepo")
		prNumber := 123
		target := models.NewPRTarget(prNumber)

		// Add comments in various files
		comments := []models.Comment{
			{Path: "file1.go", Line: models.NewSingleLine(10), Side: models.SideRight},
			{Path: "file2.go", Line: models.NewSingleLine(20), Side: models.SideRight},
			{Path: "file1.go", Line: models.NewSingleLine(30), Side: models.SideRight},
			{Path: "file3.go", Line: models.NewSingleLine(40), Side: models.SideRight},
		}

		for _, comment := range comments {
			if err := store.AddComment(repository, target, comment); err != nil {
				t.Fatal(err)
			}
		}

		parsed := models.ParsedIdentifier{Type: models.IdentifierPR, PRNumber: prNumber}
		files, err := handler.getAllFilesWithComments(repository, parsed)
		if err != nil {
			t.Errorf("getAllFilesWithComments failed: %v", err)
		}

		if len(files) != 3 {
			t.Errorf("Expected 3 unique files, got %d", len(files))
		}
	})

	t.Run("gets files from branch comments", func(t *testing.T) {
		tmpDir, err := os.MkdirTemp("", "test-get-files-branch-*")
		if err != nil {
			t.Fatal(err)
		}
		defer os.RemoveAll(tmpDir)

		store, err := storage.NewGitHubStorage(tmpDir)
		if err != nil {
			t.Fatal(err)
		}

		handler := &CommandHandler{
			storage:     store,
			storageHome: tmpDir,
		}

		repository := models.NewRepository("testowner", "testrepo")
		branchName := "feature/test-branch"
		branchTarget := models.NewBranchTarget(branchName)

		// First create branch diff hunks (required for branch storage)
		branchDiffHunks := models.ReviewDiffHunks{
			Target:     branchTarget.String(),
			Repository: repository,
			CapturedAt: time.Now(),
			DiffHunks: []models.DiffHunk{
				{Location: models.NewFileLocation("branch1.go", models.NewLineRange(1, 100)), Side: models.SideRight},
				{Location: models.NewFileLocation("branch2.go", models.NewLineRange(1, 100)), Side: models.SideRight},
			},
		}
		if err := store.CaptureDiffHunks(repository, branchTarget, branchDiffHunks); err != nil {
			t.Fatal(err)
		}

		// Add branch comments
		comments := []models.Comment{
			{Path: "branch1.go", Line: models.NewSingleLine(10), Side: models.SideRight},
			{Path: "branch2.go", Line: models.NewSingleLine(20), Side: models.SideRight},
		}

		for _, comment := range comments {
			if err := store.AddComment(repository, branchTarget, comment); err != nil {
				t.Fatal(err)
			}
		}

		// Verify comments were stored
		storedComments, err := store.GetComments(repository, branchTarget)
		if err != nil {
			t.Fatalf("Failed to get branch comments: %v", err)
		}
		t.Logf("Stored %d branch comments", len(storedComments.Comments))
		for _, c := range storedComments.Comments {
			t.Logf("  Comment: %s", c.Path)
		}

		parsed := models.ParsedIdentifier{
			Type:       models.IdentifierBranch,
			BranchName: branchName,
		}
		t.Logf("Parsed identifier: PR=%d, Branch=%s, IsPR=%v", parsed.PRNumber, parsed.BranchName, parsed.IsPR())

		files, err := handler.getAllFilesWithComments(repository, parsed)
		if err != nil {
			t.Fatalf("getAllFilesWithComments for branch failed: %v", err)
		}

		t.Logf("Files returned: %v", files)

		if len(files) != 2 {
			t.Errorf("Expected 2 unique files, got %d: %v", len(files), files)
		}
	})
}

// TestAdjustCommandSingleFileExtended tests single file extended options.
func TestAdjustCommandSingleFileExtended(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-single-ext-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)
	file := "test.go"

	// Setup
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	comment := models.Comment{
		ID:        "test1",
		Path:      file,
		Line:      models.NewSingleLine(20),
		Body:      "Test comment",
		Side:      models.SideRight,
		CreatedAt: time.Now(),
	}
	if err := store.AddComment(repository, target, comment); err != nil {
		t.Fatal(err)
	}

	t.Run("dry run mode", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			DryRun: true,
			Format: "table",
		}

		parsed := models.ParsedIdentifier{Type: models.IdentifierPR, PRNumber: prNumber}
		err := handler.adjustCommandSingleFileExtended(repository, parsed,
			file, "15d14", opts)
		if err != nil {
			t.Errorf("Dry run failed: %v", err)
		}

		// Comment should not be modified
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}
		if stored.Comments[0].Line != models.NewSingleLine(20) {
			t.Error("Comment should not change in dry run")
		}
	})

	t.Run("json format output", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			DryRun: true,
			Format: "json",
		}

		parsed := models.ParsedIdentifier{Type: models.IdentifierPR, PRNumber: prNumber}
		err := handler.adjustCommandSingleFileExtended(repository, parsed,
			file, "15d14", opts)
		if err != nil {
			t.Errorf("JSON format failed: %v", err)
		}
	})

	t.Run("force flag allows validation warnings", func(t *testing.T) {
		// Add a comment that will be outside diff hunks after adjustment
		diffHunks2 := models.ReviewDiffHunks{
			Target:     target.String(),
			Repository: repository,
			CapturedAt: time.Now(),
			DiffHunks: []models.DiffHunk{
				{
					Location: models.NewFileLocation(file, models.NewLineRange(1, 15)),
					Side:     models.SideRight,
				},
			},
		}
		if err := store.CaptureDiffHunks(repository, target, diffHunks2); err != nil {
			t.Fatal(err)
		}

		opts := AdjustOptionsExtended{
			DryRun: false,
			Force:  true,
			Format: "table",
		}

		parsed := models.ParsedIdentifier{Type: models.IdentifierPR, PRNumber: prNumber}
		// This should succeed with force even though comment moves outside hunks
		err := handler.adjustCommandSingleFileExtended(repository, parsed,
			file, "5d4", opts)
		if err != nil {
			t.Errorf("Force flag should allow warnings: %v", err)
		}
	})
}

// TestAdjustCommandExtended_AutoDetectHighConfidence tests auto-detection with high confidence.
func TestAdjustCommandExtended_AutoDetectHighConfidence(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-autodetect-high-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	mockGitClient := &MockGitClient{
		AutoDetectChangesFunc: func(file string, _ []models.DiffHunk) (*models.AutoDetectResult, error) {
			return &models.AutoDetectResult{
				File: file,
				Changes: []models.LineChange{
					{Type: models.LineDeleted, OriginalLine: 15, NewLine: 0},
					{Type: models.LineDeleted, OriginalLine: 16, NewLine: 0},
				},
				Suggestions: []models.MappingSuggestion{
					{OriginalLine: 20, Offset: -2, Confidence: models.ConfidenceHigh, Reason: "Lines deleted above"},
					{OriginalLine: 30, Offset: -2, Confidence: models.ConfidenceHigh, Reason: "Lines deleted above"},
				},
				Confidence: models.ConfidenceHigh,
			}, nil
		},
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
		gitClient:   mockGitClient,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 123
	target := models.NewPRTarget(prNumber)
	file := "test.go"

	// Setup diff hunks
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments
	comments := []models.Comment{
		{
			ID:        "c1",
			Path:      file,
			Line:      models.NewSingleLine(20),
			Body:      "Comment at line 20",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "c2",
			Path:      file,
			Line:      models.NewSingleLine(30),
			Body:      "Comment at line 30",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	t.Run("applies high confidence auto-detection", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			AutoDetect: true,
			DryRun:     false,
			Format:     "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			file, "", opts)
		if err != nil {
			t.Errorf("Auto-detection with high confidence should succeed: %v", err)
		}

		// Verify comments were adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}

		for _, comment := range stored.Comments {
			if comment.ID == "c1" && comment.Line != models.NewSingleLine(18) {
				t.Errorf("Comment c1 should be at line 18, got %v", comment.Line)
			}
			if comment.ID == "c2" && comment.Line != models.NewSingleLine(28) {
				t.Errorf("Comment c2 should be at line 28, got %v", comment.Line)
			}
		}
	})
}

// TestAdjustCommandExtended_AutoDetectMediumConfidence tests auto-detection with medium confidence.
func TestAdjustCommandExtended_AutoDetectMediumConfidence(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-autodetect-medium-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	mockGitClient := &MockGitClient{
		AutoDetectChangesFunc: func(file string, _ []models.DiffHunk) (*models.AutoDetectResult, error) {
			return &models.AutoDetectResult{
				File: file,
				Changes: []models.LineChange{
					{Type: models.LineAdded, OriginalLine: 0, NewLine: 15},
				},
				Suggestions: []models.MappingSuggestion{
					{OriginalLine: 20, Offset: 1, Confidence: models.ConfidenceMedium, Reason: "Line added above"},
				},
				Confidence: models.ConfidenceMedium,
			}, nil
		},
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
		gitClient:   mockGitClient,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 124
	target := models.NewPRTarget(prNumber)
	file := "test.go"

	// Setup diff hunks and comments
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	comment := models.Comment{
		ID:        "c1",
		Path:      file,
		Line:      models.NewSingleLine(20),
		Body:      "Comment at line 20",
		Side:      models.SideRight,
		CreatedAt: time.Now(),
	}
	if err := store.AddComment(repository, target, comment); err != nil {
		t.Fatal(err)
	}

	t.Run("applies medium confidence auto-detection", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			AutoDetect: true,
			DryRun:     false,
			Format:     "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			file, "", opts)
		if err != nil {
			t.Errorf("Auto-detection with medium confidence should succeed: %v", err)
		}

		// Verify comment was adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}

		if stored.Comments[0].Line != models.NewSingleLine(21) {
			t.Errorf("Comment should be at line 21, got %v", stored.Comments[0].Line)
		}
	})
}

// TestAdjustCommandExtended_AutoDetectLowConfidence tests auto-detection with low confidence.
func TestAdjustCommandExtended_AutoDetectLowConfidence(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-autodetect-low-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	mockGitClient := &MockGitClient{
		AutoDetectChangesFunc: func(file string, _ []models.DiffHunk) (*models.AutoDetectResult, error) {
			return &models.AutoDetectResult{
				File: file,
				Changes: []models.LineChange{
					{Type: models.LineChanged, OriginalLine: 15, NewLine: 15},
				},
				Suggestions: []models.MappingSuggestion{
					{OriginalLine: 20, Offset: 1, Confidence: models.ConfidenceLow, Reason: "Uncertain mapping"},
				},
				Confidence: models.ConfidenceLow,
			}, nil
		},
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
		gitClient:   mockGitClient,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 125
	target := models.NewPRTarget(prNumber)
	file := "test.go"

	// Setup diff hunks and comments
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	comment := models.Comment{
		ID:        "c1",
		Path:      file,
		Line:      models.NewSingleLine(20),
		Body:      "Comment at line 20",
		Side:      models.SideRight,
		CreatedAt: time.Now(),
	}
	if err := store.AddComment(repository, target, comment); err != nil {
		t.Fatal(err)
	}

	t.Run("rejects low confidence without force flag", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			AutoDetect: true,
			DryRun:     false,
			Force:      false,
			Format:     "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			file, "", opts)
		if err == nil {
			t.Error("Auto-detection with low confidence should fail without force flag")
		}
		if !strings.Contains(err.Error(), "low confidence") {
			t.Errorf("Error should mention low confidence, got: %v", err)
		}

		// Verify comment was NOT adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}
		if stored.Comments[0].Line != models.NewSingleLine(20) {
			t.Error("Comment should not be adjusted when low confidence is rejected")
		}
	})

	t.Run("applies low confidence with force flag", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			AutoDetect: true,
			DryRun:     false,
			Force:      true,
			Format:     "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			file, "", opts)
		if err != nil {
			t.Errorf("Auto-detection with low confidence and force flag should succeed: %v", err)
		}

		// Verify comment was adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}
		if stored.Comments[0].Line != models.NewSingleLine(21) {
			t.Errorf("Comment should be at line 21, got %v", stored.Comments[0].Line)
		}
	})
}

// TestAdjustCommandExtended_AutoDetectNoStoredHunks tests auto-detection without stored diff hunks.
func TestAdjustCommandExtended_AutoDetectNoStoredHunks(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-autodetect-nohunks-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	mockGitClient := &MockGitClient{}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
		gitClient:   mockGitClient,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 126
	file := "test.go"

	t.Run("fails when no stored diff hunks exist", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			AutoDetect: true,
			Format:     "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			file, "", opts)
		if err == nil {
			t.Error("Auto-detection should fail when no stored diff hunks exist")
		}
		// The error should be about failing to get stored diff hunks
		if !strings.Contains(err.Error(), "failed to get stored diff hunks") &&
			!strings.Contains(err.Error(), "failed to get diff hunks") {
			t.Errorf("Error should mention failure to get stored diff hunks, got: %v", err)
		}
	})
}

// TestAdjustCommandExtended_AutoDetectNoChanges tests auto-detection with no changes.
func TestAdjustCommandExtended_AutoDetectNoChanges(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-autodetect-nochanges-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	mockGitClient := &MockGitClient{
		AutoDetectChangesFunc: func(file string, _ []models.DiffHunk) (*models.AutoDetectResult, error) {
			return &models.AutoDetectResult{
				File:        file,
				Changes:     []models.LineChange{},
				Suggestions: []models.MappingSuggestion{},
				Confidence:  models.ConfidenceHigh,
			}, nil
		},
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
		gitClient:   mockGitClient,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 127
	target := models.NewPRTarget(prNumber)
	file := "test.go"

	// Setup diff hunks
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	t.Run("handles no changes detected gracefully", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			AutoDetect: true,
			Format:     "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			file, "", opts)
		if err != nil {
			t.Errorf("Auto-detection with no changes should succeed: %v", err)
		}
	})
}

// TestAdjustCommandExtended_UnifiedDiffWithExtendedOptions tests unified diff with extended options.
func TestAdjustCommandExtended_UnifiedDiffWithExtendedOptions(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-unified-ext-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 128
	target := models.NewPRTarget(prNumber)

	// Setup diff hunks for multiple files
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation("file1.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
			{
				Location: models.NewFileLocation("file2.go", models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	// Add comments
	comments := []models.Comment{
		{
			ID:        "f1c1",
			Path:      "file1.go",
			Line:      models.NewSingleLine(20),
			Body:      "File1 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
		{
			ID:        "f2c1",
			Path:      "file2.go",
			Line:      models.NewSingleLine(25),
			Body:      "File2 comment",
			Side:      models.SideRight,
			CreatedAt: time.Now(),
		},
	}

	for _, comment := range comments {
		if err := store.AddComment(repository, target, comment); err != nil {
			t.Fatal(err)
		}
	}

	multiFileUnifiedDiff := `diff --git a/file1.go b/file1.go
--- a/file1.go
+++ b/file1.go
@@ -15,6 +15,3 @@
-line 15
-line 16
-line 17
 line 18
diff --git a/file2.go b/file2.go
--- a/file2.go
+++ b/file2.go
@@ -20,4 +20,2 @@
-line 20
-line 21
 line 22
`

	t.Run("processes unified diff with dry run", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			DryRun: true,
			Format: "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			"", multiFileUnifiedDiff, opts)
		if err != nil {
			t.Errorf("Unified diff with dry run should succeed: %v", err)
		}

		// Verify comments were NOT adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}
		for _, comment := range stored.Comments {
			if comment.Path == "file1.go" && comment.Line != models.NewSingleLine(20) {
				t.Error("file1.go comment should not change in dry run")
			}
			if comment.Path == "file2.go" && comment.Line != models.NewSingleLine(25) {
				t.Error("file2.go comment should not change in dry run")
			}
		}
	})

	t.Run("processes unified diff with json format", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			DryRun: true,
			Format: "json",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			"", multiFileUnifiedDiff, opts)
		if err != nil {
			t.Errorf("Unified diff with json format should succeed: %v", err)
		}
	})

	t.Run("processes unified diff without dry run", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			DryRun: false,
			Format: "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			"", multiFileUnifiedDiff, opts)
		if err != nil {
			t.Errorf("Unified diff should succeed: %v", err)
		}

		// Verify comments were adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}
		for _, comment := range stored.Comments {
			if comment.Path == "file1.go" && comment.Line != models.NewSingleLine(17) {
				t.Errorf("file1.go comment should be at line 17, got %v", comment.Line)
			}
			if comment.Path == "file2.go" && comment.Line != models.NewSingleLine(23) {
				t.Errorf("file2.go comment should be at line 23, got %v", comment.Line)
			}
		}
	})
}

// TestAdjustCommandExtended_AutoDetectDryRun tests auto-detection in dry run mode.
func TestAdjustCommandExtended_AutoDetectDryRun(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "test-autodetect-dryrun-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		t.Fatal(err)
	}

	mockGitClient := &MockGitClient{
		AutoDetectChangesFunc: func(file string, _ []models.DiffHunk) (*models.AutoDetectResult, error) {
			return &models.AutoDetectResult{
				File: file,
				Changes: []models.LineChange{
					{Type: models.LineDeleted, OriginalLine: 15, NewLine: 0},
				},
				Suggestions: []models.MappingSuggestion{
					{OriginalLine: 20, Offset: -1, Confidence: models.ConfidenceHigh, Reason: "Line deleted above"},
				},
				Confidence: models.ConfidenceHigh,
			}, nil
		},
	}

	handler := &CommandHandler{
		storage:     store,
		storageHome: tmpDir,
		gitClient:   mockGitClient,
	}

	repository := models.NewRepository("testowner", "testrepo")
	prNumber := 129
	target := models.NewPRTarget(prNumber)
	file := "test.go"

	// Setup diff hunks and comment
	diffHunks := models.ReviewDiffHunks{
		Target:     target.String(),
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks: []models.DiffHunk{
			{
				Location: models.NewFileLocation(file, models.NewLineRange(1, 100)),
				Side:     models.SideRight,
			},
		},
	}
	if err := store.CaptureDiffHunks(repository, target, diffHunks); err != nil {
		t.Fatal(err)
	}

	comment := models.Comment{
		ID:        "c1",
		Path:      file,
		Line:      models.NewSingleLine(20),
		Body:      "Comment at line 20",
		Side:      models.SideRight,
		CreatedAt: time.Now(),
	}
	if err := store.AddComment(repository, target, comment); err != nil {
		t.Fatal(err)
	}

	t.Run("dry run does not modify comments", func(t *testing.T) {
		opts := AdjustOptionsExtended{
			AutoDetect: true,
			DryRun:     true,
			Format:     "table",
		}

		err := handler.AdjustCommandExtended(repository, fmt.Sprintf("%d", prNumber),
			file, "", opts)
		if err != nil {
			t.Errorf("Auto-detection dry run should succeed: %v", err)
		}

		// Verify comment was NOT adjusted
		stored, err := store.GetComments(repository, target)
		if err != nil {
			t.Fatal(err)
		}
		if stored.Comments[0].Line != models.NewSingleLine(20) {
			t.Error("Comment should not change in dry run mode")
		}
	})
}
