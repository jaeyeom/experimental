package git

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestParseCurrentDiff(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name     string
		input    string
		expected []models.LineChange
	}{
		{
			name:     "empty diff",
			input:    "",
			expected: []models.LineChange{},
		},
		{
			name: "simple addition",
			input: `@@ -10,3 +10,4 @@
 context line 1
 context line 2
+added line
 context line 3`,
			expected: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 12, NewLine: 12},
			},
		},
		{
			name: "simple deletion",
			input: `@@ -10,4 +10,3 @@
 context line 1
 context line 2
-deleted line
 context line 3`,
			expected: []models.LineChange{
				{Type: models.LineDeleted, OriginalLine: 12, NewLine: -1},
			},
		},
		{
			name: "mixed changes",
			input: `@@ -10,5 +10,5 @@
 context line 1
-deleted line
+added line
 context line 2
 context line 3`,
			expected: []models.LineChange{
				{Type: models.LineDeleted, OriginalLine: 11, NewLine: -1},
				{Type: models.LineAdded, OriginalLine: 12, NewLine: 11},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := gc.parseCurrentDiff(tt.input)
			if err != nil {
				t.Fatalf("parseCurrentDiff() error = %v", err)
			}

			if len(result) != len(tt.expected) {
				t.Fatalf("parseCurrentDiff() returned %d changes, want %d", len(result), len(tt.expected))
			}

			for i, change := range result {
				expected := tt.expected[i]
				if change.Type != expected.Type {
					t.Errorf("Change %d: Type = %v, want %v", i, change.Type, expected.Type)
				}
				if change.OriginalLine != expected.OriginalLine {
					t.Errorf("Change %d: OriginalLine = %d, want %d", i, change.OriginalLine, expected.OriginalLine)
				}
				if change.NewLine != expected.NewLine {
					t.Errorf("Change %d: NewLine = %d, want %d", i, change.NewLine, expected.NewLine)
				}
			}
		})
	}
}

func TestGroupConsecutiveChanges(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name     string
		changes  []models.LineChange
		expected int // number of groups expected
	}{
		{
			name:     "empty changes",
			changes:  []models.LineChange{},
			expected: 0,
		},
		{
			name: "single change",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
			},
			expected: 1,
		},
		{
			name: "consecutive changes",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
				{Type: models.LineDeleted, OriginalLine: 11},
				{Type: models.LineAdded, OriginalLine: 12},
			},
			expected: 1,
		},
		{
			name: "non-consecutive changes",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
				{Type: models.LineDeleted, OriginalLine: 20},
				{Type: models.LineAdded, OriginalLine: 30},
			},
			expected: 3,
		},
		{
			name: "mixed consecutive and non-consecutive",
			changes: []models.LineChange{
				{Type: models.LineAdded, OriginalLine: 10},
				{Type: models.LineDeleted, OriginalLine: 11},
				{Type: models.LineAdded, OriginalLine: 25}, // gap here
				{Type: models.LineDeleted, OriginalLine: 26},
			},
			expected: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			groups := gc.groupConsecutiveChanges(tt.changes)
			if len(groups) != tt.expected {
				t.Errorf("groupConsecutiveChanges() returned %d groups, want %d", len(groups), tt.expected)
			}
		})
	}
}

func TestCalculateNetOffset(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name     string
		changes  []models.LineChange
		expected int
	}{
		{
			name:     "empty changes",
			changes:  []models.LineChange{},
			expected: 0,
		},
		{
			name: "only additions",
			changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineAdded},
				{Type: models.LineAdded},
			},
			expected: 3,
		},
		{
			name: "only deletions",
			changes: []models.LineChange{
				{Type: models.LineDeleted},
				{Type: models.LineDeleted},
			},
			expected: -2,
		},
		{
			name: "mixed additions and deletions",
			changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
				{Type: models.LineAdded},
			},
			expected: 1,
		},
		{
			name: "equal additions and deletions",
			changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
			},
			expected: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := gc.calculateNetOffset(tt.changes)
			if result != tt.expected {
				t.Errorf("calculateNetOffset() = %d, want %d", result, tt.expected)
			}
		})
	}
}

func TestCalculateOverallConfidence(t *testing.T) {
	gc := &Client{}

	tests := []struct {
		name        string
		suggestions []models.MappingSuggestion
		expected    models.ConfidenceLevel
	}{
		{
			name:        "empty suggestions",
			suggestions: []models.MappingSuggestion{},
			expected:    models.ConfidenceLow,
		},
		{
			name: "all high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
			},
			expected: models.ConfidenceHigh,
		},
		{
			name: "majority high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceMedium},
			},
			expected: models.ConfidenceHigh,
		},
		{
			name: "minority high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceHigh},
				{Confidence: models.ConfidenceMedium},
				{Confidence: models.ConfidenceLow},
			},
			expected: models.ConfidenceMedium,
		},
		{
			name: "no high confidence",
			suggestions: []models.MappingSuggestion{
				{Confidence: models.ConfidenceMedium},
				{Confidence: models.ConfidenceLow},
			},
			expected: models.ConfidenceMedium,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := gc.calculateOverallConfidence(tt.suggestions)
			if result != tt.expected {
				t.Errorf("calculateOverallConfidence() = %v, want %v", result, tt.expected)
			}
		})
	}
}

// setupTestRepo creates a temporary git repository for testing.
func setupTestRepo(t *testing.T) (string, func()) {
	t.Helper()

	// Create temporary directory
	tempDir, err := os.MkdirTemp("", "git-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}

	cleanup := func() {
		os.RemoveAll(tempDir)
	}

	// Initialize git repo
	cmd := exec.Command("git", "init")
	cmd.Dir = tempDir
	if err := cmd.Run(); err != nil {
		cleanup()
		t.Fatalf("Failed to initialize git repo: %v", err)
	}

	// Configure git user for commits
	cmd = exec.Command("git", "config", "user.email", "test@example.com")
	cmd.Dir = tempDir
	if err := cmd.Run(); err != nil {
		cleanup()
		t.Fatalf("Failed to configure git email: %v", err)
	}

	cmd = exec.Command("git", "config", "user.name", "Test User")
	cmd.Dir = tempDir
	if err := cmd.Run(); err != nil {
		cleanup()
		t.Fatalf("Failed to configure git name: %v", err)
	}

	return tempDir, cleanup
}

// commitFile creates a file with content and commits it.
func commitFile(t *testing.T, repoPath, filename, content string) string {
	t.Helper()

	// Write file
	filePath := filepath.Join(repoPath, filename)
	if err := os.WriteFile(filePath, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write file: %v", err)
	}

	// Add file
	cmd := exec.Command("git", "add", filename)
	cmd.Dir = repoPath
	if err := cmd.Run(); err != nil {
		t.Fatalf("Failed to add file: %v", err)
	}

	// Commit
	cmd = exec.Command("git", "commit", "-m", "test commit")
	cmd.Dir = repoPath
	if err := cmd.Run(); err != nil {
		t.Fatalf("Failed to commit: %v", err)
	}

	// Get commit SHA
	cmd = exec.Command("git", "rev-parse", "HEAD")
	cmd.Dir = repoPath
	output, err := cmd.Output()
	if err != nil {
		t.Fatalf("Failed to get commit SHA: %v", err)
	}

	return string(output[:40]) // Return SHA
}

func TestAutoDetectChanges(t *testing.T) {
	tests := []struct {
		name            string
		initialContent  string
		modifiedContent string
		wantChanges     int
		wantSuggestions int
		wantConfidence  models.ConfidenceLevel
		wantError       bool
	}{
		{
			name: "no changes",
			initialContent: `line 1
line 2
line 3`,
			modifiedContent: `line 1
line 2
line 3`,
			wantChanges:     0,
			wantSuggestions: 0,
			wantConfidence:  models.ConfidenceLow, // No suggestions = low confidence
			wantError:       false,
		},
		{
			name: "simple line addition",
			initialContent: `line 1
line 2
line 3`,
			modifiedContent: `line 1
line 2
new line
line 3`,
			wantChanges:     1,
			wantSuggestions: 1,
			wantConfidence:  models.ConfidenceHigh,
			wantError:       false,
		},
		{
			name: "simple line deletion",
			initialContent: `line 1
line 2
line 3
line 4`,
			modifiedContent: `line 1
line 2
line 4`,
			wantChanges:     1,
			wantSuggestions: 1,
			wantConfidence:  models.ConfidenceHigh,
			wantError:       false,
		},
		{
			name: "multiple additions",
			initialContent: `line 1
line 2
line 3`,
			modifiedContent: `line 1
new line 1
new line 2
line 2
line 3`,
			wantChanges:     2,
			wantSuggestions: 1, // Grouped together
			wantConfidence:  models.ConfidenceHigh,
			wantError:       false,
		},
		{
			name: "mixed additions and deletions",
			initialContent: `line 1
line 2
line 3
line 4
line 5`,
			modifiedContent: `line 1
new line
line 3
line 5`,
			wantChanges:     3, // 1 deletion + 1 addition + 1 deletion
			wantSuggestions: 1, // Grouped together as consecutive changes
			wantConfidence:  models.ConfidenceHigh,
			wantError:       false,
		},
		{
			name: "non-consecutive changes",
			initialContent: `line 1
line 2
line 3
line 4
line 5
line 6
line 7
line 8
line 9
line 10`,
			modifiedContent: `line 1
new line
line 2
line 3
line 4
line 5
line 6
line 7
another new line
line 8
line 9
line 10`,
			wantChanges:     2,
			wantSuggestions: 2, // Two separate groups
			wantConfidence:  models.ConfidenceHigh,
			wantError:       false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Setup test repository
			repoPath, cleanup := setupTestRepo(t)
			defer cleanup()

			// Create initial file and commit
			filename := "test.txt"
			sha := commitFile(t, repoPath, filename, tt.initialContent)

			// Create stored diff hunks (simulating captured state)
			// Use a range that won't overlap with test changes (far away)
			storedDiffHunks := []models.DiffHunk{
				{
					Location: models.NewFileLocation(filename, models.LineRange{StartLine: 100, EndLine: 110}),
					Side:     models.SideRight,
					SHA:      sha,
				},
			}

			// Modify the file
			filePath := filepath.Join(repoPath, filename)
			if err := os.WriteFile(filePath, []byte(tt.modifiedContent), 0o600); err != nil {
				t.Fatalf("Failed to modify file: %v", err)
			}

			// Run AutoDetectChanges
			gc := &Client{repoPath: repoPath}
			result, err := gc.AutoDetectChanges(filename, storedDiffHunks)

			// Check error expectation
			if tt.wantError {
				if err == nil {
					t.Errorf("AutoDetectChanges() expected error, got nil")
				}
				return
			}

			if err != nil {
				t.Fatalf("AutoDetectChanges() unexpected error: %v", err)
			}

			// Verify result
			if result.File != filename {
				t.Errorf("AutoDetectChanges() File = %q, want %q", result.File, filename)
			}

			if len(result.Changes) != tt.wantChanges {
				t.Errorf("AutoDetectChanges() Changes count = %d, want %d", len(result.Changes), tt.wantChanges)
			}

			if len(result.Suggestions) != tt.wantSuggestions {
				t.Errorf("AutoDetectChanges() Suggestions count = %d, want %d", len(result.Suggestions), tt.wantSuggestions)
			}

			if result.Confidence != tt.wantConfidence {
				t.Errorf("AutoDetectChanges() Confidence = %v, want %v", result.Confidence, tt.wantConfidence)
			}
		})
	}
}

func TestAutoDetectChanges_ErrorCases(t *testing.T) {
	tests := []struct {
		name            string
		setupRepo       func(t *testing.T, repoPath string) (string, []models.DiffHunk)
		wantErrorSubstr string
	}{
		{
			name: "no stored diff hunks",
			setupRepo: func(_ *testing.T, _ string) (string, []models.DiffHunk) {
				return "test.txt", []models.DiffHunk{}
			},
			wantErrorSubstr: "no stored diff hunks",
		},
		{
			name: "invalid commit SHA",
			setupRepo: func(t *testing.T, repoPath string) (string, []models.DiffHunk) {
				filename := "test.txt"
				commitFile(t, repoPath, filename, "initial content")
				return filename, []models.DiffHunk{
					{
						Location: models.NewFileLocation(filename, models.LineRange{StartLine: 1, EndLine: 3}),
						Side:     models.SideRight,
						SHA:      "0000000000000000000000000000000000000000", // Invalid SHA
					},
				}
			},
			wantErrorSubstr: "no longer exists",
		},
		{
			name: "file not tracked by git",
			setupRepo: func(t *testing.T, repoPath string) (string, []models.DiffHunk) {
				// Commit an initial file
				filename := "test.txt"
				sha := commitFile(t, repoPath, filename, "initial content")

				// Return a different filename that doesn't exist
				return "nonexistent.txt", []models.DiffHunk{
					{
						Location: models.NewFileLocation("nonexistent.txt", models.LineRange{StartLine: 1, EndLine: 3}),
						Side:     models.SideRight,
						SHA:      sha,
					},
				}
			},
			wantErrorSubstr: "not tracked by git",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Setup test repository
			repoPath, cleanup := setupTestRepo(t)
			defer cleanup()

			// Setup specific test scenario
			filename, storedDiffHunks := tt.setupRepo(t, repoPath)

			// Run AutoDetectChanges
			gc := &Client{repoPath: repoPath}
			result, err := gc.AutoDetectChanges(filename, storedDiffHunks)

			// Should return error
			if err == nil {
				t.Errorf("AutoDetectChanges() expected error containing %q, got nil", tt.wantErrorSubstr)
				return
			}

			// Verify error message
			if !contains(err.Error(), tt.wantErrorSubstr) {
				t.Errorf("AutoDetectChanges() error = %q, want substring %q", err.Error(), tt.wantErrorSubstr)
			}

			// Result should be nil on error
			if result != nil {
				t.Errorf("AutoDetectChanges() result should be nil on error, got %v", result)
			}
		})
	}
}

func TestAutoDetectChanges_ConfidenceScoring(t *testing.T) {
	tests := []struct {
		name            string
		initialContent  string
		modifiedContent string
		wantConfidence  models.ConfidenceLevel
		description     string
	}{
		{
			name: "high confidence - simple changes",
			initialContent: `line 1
line 2
line 3`,
			modifiedContent: `line 1
new line
line 2
line 3`,
			wantConfidence: models.ConfidenceHigh,
			description:    "Simple addition should result in high confidence",
		},
		{
			name: "medium confidence - complex changes",
			initialContent: `line 1
line 2
line 3
line 4
line 5
line 6
line 7`,
			modifiedContent: `line 1
new 1
new 2
new 3
new 4
new 5
new 6
line 2
line 3
line 4
line 5
line 6
line 7`,
			wantConfidence: models.ConfidenceMedium,
			description:    "Many consecutive changes (>5) should lower confidence",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Setup test repository
			repoPath, cleanup := setupTestRepo(t)
			defer cleanup()

			// Create initial file and commit
			filename := "test.txt"
			sha := commitFile(t, repoPath, filename, tt.initialContent)

			// Create stored diff hunks
			// Use a range that won't overlap with test changes (far away)
			storedDiffHunks := []models.DiffHunk{
				{
					Location: models.NewFileLocation(filename, models.LineRange{StartLine: 100, EndLine: 110}),
					Side:     models.SideRight,
					SHA:      sha,
				},
			}

			// Modify the file
			filePath := filepath.Join(repoPath, filename)
			if err := os.WriteFile(filePath, []byte(tt.modifiedContent), 0o600); err != nil {
				t.Fatalf("Failed to modify file: %v", err)
			}

			// Run AutoDetectChanges
			gc := &Client{repoPath: repoPath}
			result, err := gc.AutoDetectChanges(filename, storedDiffHunks)
			if err != nil {
				t.Fatalf("AutoDetectChanges() unexpected error: %v", err)
			}

			if result.Confidence != tt.wantConfidence {
				t.Errorf("AutoDetectChanges() Confidence = %v, want %v (%s)",
					result.Confidence, tt.wantConfidence, tt.description)
			}
		})
	}
}

// contains is a helper function to check if a string contains a substring.
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(substr) == 0 ||
		(len(s) > 0 && len(substr) > 0 && containsHelper(s, substr)))
}

func containsHelper(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
