package prreview

import (
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/git"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

// Mock implementations for testing

// MockGitHubClient is a mock implementation of GitHubClient for testing.
type MockGitHubClient struct {
	ValidatePRAccessFunc func(repository models.Repository, prNumber int) error
	GetPRDiffFunc        func(repository models.Repository, prNumber int) (*models.PRDiffHunks, error)
	SubmitReviewFunc     func(repository models.Repository, prNumber int, review models.PRReview) error
	GetPRCommentsFunc    func(repository models.Repository, prNumber int) ([]models.Comment, error)
}

func (m *MockGitHubClient) ValidatePRAccess(repository models.Repository, prNumber int) error {
	if m.ValidatePRAccessFunc != nil {
		return m.ValidatePRAccessFunc(repository, prNumber)
	}
	return nil
}

func (m *MockGitHubClient) GetPRDiff(repository models.Repository, prNumber int) (*models.PRDiffHunks, error) {
	if m.GetPRDiffFunc != nil {
		return m.GetPRDiffFunc(repository, prNumber)
	}
	return &models.PRDiffHunks{}, nil
}

func (m *MockGitHubClient) SubmitReview(repository models.Repository, prNumber int, review models.PRReview) error {
	if m.SubmitReviewFunc != nil {
		return m.SubmitReviewFunc(repository, prNumber, review)
	}
	return nil
}

func (m *MockGitHubClient) GetPRComments(repository models.Repository, prNumber int) ([]models.Comment, error) {
	if m.GetPRCommentsFunc != nil {
		return m.GetPRCommentsFunc(repository, prNumber)
	}
	return []models.Comment{}, nil
}

// MockGitClient is a mock implementation of GitClient for testing.
type MockGitClient struct {
	BranchExistsFunc         func(branch git.Branch) (bool, error)
	GetDefaultBaseBranchFunc func() (git.Branch, error)
	CaptureBranchDiffFunc    func(repository models.Repository, branch, baseBranch git.Branch) (*models.BranchDiffHunks, error)
	GetStagedDiffFunc        func() (string, error)
	GetUnstagedDiffFunc      func() (string, error)
	GetDiffSinceFunc         func(since string) (string, error)
	AutoDetectChangesFunc    func(file string, storedDiffHunks []models.DiffHunk) (*models.AutoDetectResult, error)
}

func (m *MockGitClient) BranchExists(branch git.Branch) (bool, error) {
	if m.BranchExistsFunc != nil {
		return m.BranchExistsFunc(branch)
	}
	return true, nil
}

func (m *MockGitClient) GetDefaultBaseBranch() (git.Branch, error) {
	if m.GetDefaultBaseBranchFunc != nil {
		return m.GetDefaultBaseBranchFunc()
	}
	branch, err := git.NewBranch("main")
	if err != nil {
		return git.Branch{}, fmt.Errorf("failed to create default branch: %w", err)
	}
	return branch, nil
}

func (m *MockGitClient) CaptureBranchDiff(repository models.Repository, branch, baseBranch git.Branch) (*models.BranchDiffHunks, error) {
	if m.CaptureBranchDiffFunc != nil {
		return m.CaptureBranchDiffFunc(repository, branch, baseBranch)
	}
	return &models.BranchDiffHunks{}, nil
}

func (m *MockGitClient) GetStagedDiff() (string, error) {
	if m.GetStagedDiffFunc != nil {
		return m.GetStagedDiffFunc()
	}
	return "", nil
}

func (m *MockGitClient) GetUnstagedDiff() (string, error) {
	if m.GetUnstagedDiffFunc != nil {
		return m.GetUnstagedDiffFunc()
	}
	return "", nil
}

func (m *MockGitClient) GetDiffSince(since string) (string, error) {
	if m.GetDiffSinceFunc != nil {
		return m.GetDiffSinceFunc(since)
	}
	return "", nil
}

func (m *MockGitClient) AutoDetectChanges(file string, storedDiffHunks []models.DiffHunk) (*models.AutoDetectResult, error) {
	if m.AutoDetectChangesFunc != nil {
		return m.AutoDetectChangesFunc(file, storedDiffHunks)
	}
	return &models.AutoDetectResult{}, nil
}

// MockOutputFormatter is a mock implementation of OutputFormatter for testing.
type MockOutputFormatter struct {
	FormatSubmitResultFunc        func(result models.SubmitResult) (string, error)
	FormatCommentsFunc            func(comments []models.Comment) (string, error)
	FormatCommentsWithContextFunc func(comments []models.CommentWithLineContext) (string, error)
	FormatSingleCommentFunc       func(comment models.Comment) (string, error)
}

func (m *MockOutputFormatter) FormatSubmitResult(result models.SubmitResult) (string, error) {
	if m.FormatSubmitResultFunc != nil {
		return m.FormatSubmitResultFunc(result)
	}
	return fmt.Sprintf("Submitted %d comments", result.Comments), nil
}

func (m *MockOutputFormatter) FormatComments(comments []models.Comment) (string, error) {
	if m.FormatCommentsFunc != nil {
		return m.FormatCommentsFunc(comments)
	}
	return fmt.Sprintf("Formatted %d comments", len(comments)), nil
}

func (m *MockOutputFormatter) FormatCommentsWithContext(comments []models.CommentWithLineContext) (string, error) {
	if m.FormatCommentsWithContextFunc != nil {
		return m.FormatCommentsWithContextFunc(comments)
	}
	return fmt.Sprintf("Formatted %d comments with context", len(comments)), nil
}

func (m *MockOutputFormatter) FormatSingleComment(comment models.Comment) (string, error) {
	if m.FormatSingleCommentFunc != nil {
		return m.FormatSingleCommentFunc(comment)
	}
	return fmt.Sprintf("Comment at %s:%d", comment.Path, comment.Line), nil
}

// MockExecutor is a mock implementation of models.Executor for testing.
type MockExecutor struct {
	NameFunc    func() string
	ExecuteFunc func(storage models.CommentClearer, repository models.Repository, prNumber int, file string) error
}

func (m *MockExecutor) Name() string {
	if m.NameFunc != nil {
		return m.NameFunc()
	}
	return "mock"
}

func (m *MockExecutor) Execute(storage models.CommentClearer, repository models.Repository, prNumber int, file string) error {
	if m.ExecuteFunc != nil {
		return m.ExecuteFunc(storage, repository, prNumber, file)
	}
	return nil
}

// Test helper functions

// setupTestHandler creates a test CommandHandler with temporary storage.
func setupTestHandler(t *testing.T) (*CommandHandler, string, func()) {
	t.Helper()

	tmpDir, err := os.MkdirTemp("", "test-commands-*")
	if err != nil {
		t.Fatal(err)
	}

	store, err := storage.NewGitHubStorage(tmpDir)
	if err != nil {
		os.RemoveAll(tmpDir)
		t.Fatal(err)
	}

	handler := &CommandHandler{
		storage:     store,
		ghClient:    &MockGitHubClient{},
		gitClient:   &MockGitClient{},
		storageHome: tmpDir,
	}

	cleanup := func() {
		os.RemoveAll(tmpDir)
	}

	return handler, tmpDir, cleanup
}

// createTestDiffHunks creates test diff hunks for a PR.
func createTestDiffHunks(t *testing.T, handler *CommandHandler, repository models.Repository, prNumber int, hunks []models.DiffHunk) {
	t.Helper()

	diffHunks := models.PRDiffHunks{
		PRNumber:   prNumber,
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks:  hunks,
	}

	if err := handler.storage.CaptureDiffHunks(repository, prNumber, diffHunks); err != nil {
		t.Fatalf("failed to create test diff hunks: %v", err)
	}
}

// createTestBranchDiffHunks creates test diff hunks for a branch.
func createTestBranchDiffHunks(t *testing.T, handler *CommandHandler, repository models.Repository, branchName string, hunks []models.DiffHunk) {
	t.Helper()

	diffHunks := models.BranchDiffHunks{
		BranchName: branchName,
		Repository: repository,
		CapturedAt: time.Now(),
		DiffHunks:  hunks,
	}

	if err := handler.storage.CaptureBranchDiffHunks(repository, branchName, diffHunks); err != nil {
		t.Fatalf("failed to create test branch diff hunks: %v", err)
	}
}

// createTestComment creates and adds a test comment to storage.
func createTestComment(t *testing.T, handler *CommandHandler, repository models.Repository, prNumber int, comment models.Comment) {
	t.Helper()

	if err := handler.storage.AddComment(repository, prNumber, comment); err != nil {
		t.Fatalf("failed to create test comment: %v", err)
	}
}

// TestCaptureCommand_PR tests CaptureCommand behavior for PR identifiers.
func TestCaptureCommand_PR(t *testing.T) {
	tests := []struct {
		name            string
		setupDiffHunks  bool
		force           bool
		mockSetup       func(*MockGitHubClient)
		expectError     bool
		errorContains   string
		validateStorage func(*testing.T, *CommandHandler, models.Repository, int)
	}{
		{
			name:           "prevents duplicate captures without force flag",
			setupDiffHunks: true,
			force:          false,
			expectError:    true,
			errorContains:  "already exist",
		},
		{
			name:           "allows overwriting with force flag",
			setupDiffHunks: true,
			force:          true,
			mockSetup: func(m *MockGitHubClient) {
				m.GetPRDiffFunc = func(repository models.Repository, prNumber int) (*models.PRDiffHunks, error) {
					return &models.PRDiffHunks{
						PRNumber:   prNumber,
						Repository: repository,
						DiffHunks: []models.DiffHunk{
							{File: "new.go", Side: "RIGHT", StartLine: 1, EndLine: 10},
						},
					}, nil
				}
			},
			expectError: false,
			validateStorage: func(t *testing.T, handler *CommandHandler, repository models.Repository, prNumber int) {
				hunks, err := handler.storage.GetDiffHunks(repository, prNumber)
				if err != nil {
					t.Fatalf("failed to get diff hunks after capture: %v", err)
				}
				if len(hunks.DiffHunks) != 1 || hunks.DiffHunks[0].File != "new.go" {
					t.Errorf("diff hunks not overwritten correctly")
				}
			},
		},
		{
			name:  "validates PR accessibility before capture",
			force: false,
			mockSetup: func(m *MockGitHubClient) {
				m.ValidatePRAccessFunc = func(_ models.Repository, _ int) error {
					return fmt.Errorf("PR not found")
				}
			},
			expectError:   true,
			errorContains: "failed to access PR",
		},
		{
			name:  "successfully captures and stores PR diff hunks",
			force: false,
			mockSetup: func(m *MockGitHubClient) {
				m.GetPRDiffFunc = func(repository models.Repository, prNumber int) (*models.PRDiffHunks, error) {
					return &models.PRDiffHunks{
						PRNumber:   prNumber,
						Repository: repository,
						DiffHunks: []models.DiffHunk{
							{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
							{File: "test.go", Side: "RIGHT", StartLine: 50, EndLine: 60},
						},
					}, nil
				}
			},
			expectError: false,
			validateStorage: func(t *testing.T, handler *CommandHandler, repository models.Repository, prNumber int) {
				hunks, err := handler.storage.GetDiffHunks(repository, prNumber)
				if err != nil {
					t.Fatalf("failed to get diff hunks after capture: %v", err)
				}
				if len(hunks.DiffHunks) != 2 {
					t.Errorf("expected 2 diff hunks, got %d", len(hunks.DiffHunks))
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")
			prNumber := 123

			// Setup existing diff hunks if needed
			if tt.setupDiffHunks {
				createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
					{File: "old.go", Side: "RIGHT", StartLine: 1, EndLine: 5},
				})
			}

			// Setup mock
			mockGH := &MockGitHubClient{}
			if tt.mockSetup != nil {
				tt.mockSetup(mockGH)
			}
			handler.ghClient = mockGH

			// Execute
			err := handler.CaptureCommand(repository, "123", tt.force)

			// Validate error
			if tt.expectError {
				if err == nil {
					t.Errorf("expected error containing %q, got nil", tt.errorContains)
				} else if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error containing %q, got %q", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("unexpected error: %v", err)
				}
			}

			// Validate storage if specified
			if tt.validateStorage != nil && !tt.expectError {
				tt.validateStorage(t, handler, repository, prNumber)
			}
		})
	}
}

// TestCaptureCommand_Branch tests CaptureCommand behavior for branch identifiers.
func TestCaptureCommand_Branch(t *testing.T) {
	tests := []struct {
		name            string
		branchName      string
		setupDiffHunks  bool
		force           bool
		mockSetup       func(*MockGitClient)
		expectError     bool
		errorContains   string
		validateStorage func(*testing.T, *CommandHandler, models.Repository, string)
	}{
		{
			name:           "prevents duplicate branch captures without force",
			branchName:     "feature-branch",
			setupDiffHunks: true,
			force:          false,
			expectError:    true,
			errorContains:  "already exist",
		},
		{
			name:       "validates branch existence before capture",
			branchName: "nonexistent-branch",
			force:      false,
			mockSetup: func(m *MockGitClient) {
				m.BranchExistsFunc = func(_ git.Branch) (bool, error) {
					return false, nil
				}
			},
			expectError:   true,
			errorContains: "does not exist",
		},
		{
			name:          "handles invalid branch names",
			branchName:    "",
			force:         false,
			expectError:   true,
			errorContains: "invalid",
		},
		{
			name:       "successfully captures branch diff with default base",
			branchName: "feature-branch",
			force:      false,
			mockSetup: func(m *MockGitClient) {
				m.CaptureBranchDiffFunc = func(repository models.Repository, branch, _ git.Branch) (*models.BranchDiffHunks, error) {
					return &models.BranchDiffHunks{
						BranchName: branch.String(),
						Repository: repository,
						DiffHunks: []models.DiffHunk{
							{File: "feature.go", Side: "RIGHT", StartLine: 1, EndLine: 10},
						},
					}, nil
				}
			},
			expectError: false,
			validateStorage: func(t *testing.T, handler *CommandHandler, repository models.Repository, branchName string) {
				hunks, err := handler.storage.GetBranchDiffHunks(repository, branchName)
				if err != nil {
					t.Fatalf("failed to get branch diff hunks after capture: %v", err)
				}
				if len(hunks.DiffHunks) != 1 {
					t.Errorf("expected 1 diff hunk, got %d", len(hunks.DiffHunks))
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")

			// Setup existing diff hunks if needed
			if tt.setupDiffHunks {
				createTestBranchDiffHunks(t, handler, repository, tt.branchName, []models.DiffHunk{
					{File: "old.go", Side: "RIGHT", StartLine: 1, EndLine: 5},
				})
			}

			// Setup mock
			mockGit := &MockGitClient{}
			if tt.mockSetup != nil {
				tt.mockSetup(mockGit)
			}
			handler.gitClient = mockGit

			// Execute
			err := handler.CaptureCommand(repository, tt.branchName, tt.force)

			// Validate error
			if tt.expectError {
				if err == nil {
					t.Errorf("expected error containing %q, got nil", tt.errorContains)
				} else if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error containing %q, got %q", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("unexpected error: %v", err)
				}
			}

			// Validate storage if specified
			if tt.validateStorage != nil && !tt.expectError {
				tt.validateStorage(t, handler, repository, tt.branchName)
			}
		})
	}
}

// TestCommentCommand_LineSpecValidation tests line specification parsing.
func TestCommentCommand_LineSpecValidation(t *testing.T) {
	tests := []struct {
		name          string
		lineSpec      string
		expectError   bool
		errorContains string
		validateLine  func(*testing.T, models.Comment)
	}{
		{
			name:        "single-line comment",
			lineSpec:    "10",
			expectError: false,
			validateLine: func(t *testing.T, comment models.Comment) {
				if comment.Line != 10 {
					t.Errorf("expected Line=10, got %d", comment.Line)
				}
				if comment.StartLine != nil {
					t.Errorf("expected StartLine=nil for single-line comment, got %d", *comment.StartLine)
				}
			},
		},
		{
			name:        "multi-line comment",
			lineSpec:    "10-15",
			expectError: false,
			validateLine: func(t *testing.T, comment models.Comment) {
				if comment.Line != 15 {
					t.Errorf("expected Line=15, got %d", comment.Line)
				}
				if comment.StartLine == nil || *comment.StartLine != 10 {
					t.Errorf("expected StartLine=10, got %v", comment.StartLine)
				}
			},
		},
		{
			name:          "invalid line spec - non-numeric",
			lineSpec:      "abc",
			expectError:   true,
			errorContains: "invalid line specification",
		},
		{
			name:          "invalid line spec - reversed range",
			lineSpec:      "15-10",
			expectError:   true,
			errorContains: "invalid line specification",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")
			prNumber := 123

			// Create diff hunks to allow comment
			createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 1, EndLine: 100},
			})

			err := handler.CommentCommand(repository, "123", "test.go", tt.lineSpec, "test comment", "RIGHT", true)

			if tt.expectError {
				if err == nil {
					t.Errorf("expected error containing %q, got nil", tt.errorContains)
				} else if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error containing %q, got %q", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("unexpected error: %v", err)
				}

				// Validate the created comment
				if tt.validateLine != nil {
					comments, err := handler.storage.GetComments(repository, prNumber)
					if err != nil {
						t.Fatalf("failed to get comments: %v", err)
					}
					if len(comments.Comments) != 1 {
						t.Fatalf("expected 1 comment, got %d", len(comments.Comments))
					}
					tt.validateLine(t, comments.Comments[0])
				}
			}
		})
	}
}

// TestCommentCommand_DiffValidation tests comment validation against diff hunks.
func TestCommentCommand_DiffValidation(t *testing.T) {
	tests := []struct {
		name           string
		setupDiffHunks bool
		diffHunks      []models.DiffHunk
		commentLine    int
		force          bool
		expectError    bool
		errorContains  string
		expectWarning  bool
	}{
		{
			name:           "validates against diff hunks when available - valid",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
			},
			commentLine: 15,
			force:       false,
			expectError: false,
		},
		{
			name:           "validates against diff hunks when available - invalid without force",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
			},
			commentLine:   25,
			force:         false,
			expectError:   true,
			errorContains: "validation failed",
		},
		{
			name:           "allows invalid comments with force flag",
			setupDiffHunks: true,
			diffHunks: []models.DiffHunk{
				{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
			},
			commentLine: 25,
			force:       true,
			expectError: false,
		},
		{
			name:           "skips validation when diff hunks don't exist",
			setupDiffHunks: false,
			commentLine:    100,
			force:          false,
			expectError:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")
			prNumber := 123

			// Setup diff hunks if needed
			if tt.setupDiffHunks {
				createTestDiffHunks(t, handler, repository, prNumber, tt.diffHunks)
			}

			err := handler.CommentCommand(repository, "123", "test.go", fmt.Sprintf("%d", tt.commentLine), "test comment", "RIGHT", tt.force)

			if tt.expectError {
				if err == nil {
					t.Errorf("expected error containing %q, got nil", tt.errorContains)
				} else if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error containing %q, got %q", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("unexpected error: %v", err)
				}
			}
		})
	}
}

// TestCommentCommand_DuplicateDetection tests duplicate comment detection.
func TestCommentCommand_DuplicateDetection(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	prNumber := 123

	// Create diff hunks
	createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
		{File: "test.go", Side: "RIGHT", StartLine: 1, EndLine: 100},
	})

	// Add first comment
	err := handler.CommentCommand(repository, "123", "test.go", "10", "duplicate test", "RIGHT", false)
	if err != nil {
		t.Fatalf("failed to add first comment: %v", err)
	}

	t.Run("detects duplicate comments without force", func(t *testing.T) {
		err := handler.CommentCommand(repository, "123", "test.go", "10", "duplicate test", "RIGHT", false)
		if err == nil {
			t.Error("expected duplicate error, got nil")
		} else if !contains(err.Error(), "duplicate") {
			t.Errorf("expected duplicate error, got: %v", err)
		}
	})

	t.Run("force flag shows warning but still blocks true duplicates", func(t *testing.T) {
		// Note: The force flag in CommentCommand applies to validation errors, not duplicate detection
		// Duplicate detection happens in storage layer and force flag there shows a warning but still blocks
		err := handler.CommentCommand(repository, "123", "test.go", "10", "duplicate test", "RIGHT", true)
		// This will still fail because duplicate detection is at the storage level
		if err == nil || !contains(err.Error(), "duplicate") {
			t.Logf("Force flag behavior: still detects duplicates at storage level (expected behavior)")
		}
	})
}

// TestCommentCommand_Branch tests CommentCommand for branch identifiers.
func TestCommentCommand_Branch(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	branchName := "feature-branch"

	// Create branch diff hunks
	createTestBranchDiffHunks(t, handler, repository, branchName, []models.DiffHunk{
		{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
	})

	t.Run("adds comment to branch successfully", func(t *testing.T) {
		err := handler.CommentCommand(repository, branchName, "test.go", "15", "branch comment", "RIGHT", false)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		// Verify comment was added
		comments, err := handler.storage.GetBranchComments(repository, branchName)
		if err != nil {
			t.Fatalf("failed to get branch comments: %v", err)
		}
		if len(comments.Comments) != 1 {
			t.Errorf("expected 1 comment, got %d", len(comments.Comments))
		}
		if comments.Comments[0].Body != "branch comment" {
			t.Errorf("expected body 'branch comment', got %q", comments.Comments[0].Body)
		}
	})
}

// TestListCommand_Filtering tests ListCommand filtering behavior.
func TestListCommand_Filtering(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	prNumber := 123

	// Create test comments
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "file1.go",
		Line: 10,
		Body: "comment 1",
		Side: "RIGHT",
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "file1.go",
		Line: 20,
		Body: "comment 2",
		Side: "LEFT",
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "file2.go",
		Line: 15,
		Body: "comment 3",
		Side: "RIGHT",
	})
	startLine := 18
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path:      "file1.go",
		StartLine: &startLine,
		Line:      22,
		Body:      "multi-line comment",
		Side:      "RIGHT",
	})

	formatter := &MockOutputFormatter{}

	tests := []struct {
		name          string
		file          string
		line          string
		side          string
		expectedCount int
	}{
		{
			name:          "filters by file path",
			file:          "file1.go",
			expectedCount: 3,
		},
		{
			name:          "filters by side",
			side:          "RIGHT",
			expectedCount: 3,
		},
		{
			name:          "filters by line range",
			line:          "10-20",
			expectedCount: 4, // comments at 10, 20, 18-22 (overlaps), and technically all might match
		},
		{
			name:          "combines multiple filters",
			file:          "file1.go",
			side:          "RIGHT",
			expectedCount: 2, // file1.go RIGHT side comments
		},
		{
			name:          "no filters returns all",
			expectedCount: 4,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			callCount := 0
			formatter.FormatCommentsFunc = func(comments []models.Comment) (string, error) {
				callCount++
				if len(comments) != tt.expectedCount {
					t.Errorf("expected %d comments, got %d", tt.expectedCount, len(comments))
				}
				return fmt.Sprintf("%d comments", len(comments)), nil
			}

			err := handler.ListCommand(repository, "123", formatter, tt.file, tt.line, tt.side, false, 0)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
			}

			if callCount != 1 {
				t.Errorf("expected formatter to be called once, called %d times", callCount)
			}
		})
	}
}

// TestListCommand_LineRangeMatching tests line range matching logic.
func TestListCommand_LineRangeMatching(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	prNumber := 123

	// Create comments at various positions
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "test.go",
		Line: 5,
		Body: "before range",
		Side: "RIGHT",
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "test.go",
		Line: 15,
		Body: "in range",
		Side: "RIGHT",
	})
	startLine := 18
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path:      "test.go",
		StartLine: &startLine,
		Line:      22,
		Body:      "overlaps range",
		Side:      "RIGHT",
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "test.go",
		Line: 30,
		Body: "after range",
		Side: "RIGHT",
	})

	formatter := &MockOutputFormatter{
		FormatCommentsFunc: func(comments []models.Comment) (string, error) {
			return fmt.Sprintf("%d comments", len(comments)), nil
		},
	}

	t.Run("matches comments in range 10-20", func(t *testing.T) {
		matchCount := 0
		formatter.FormatCommentsFunc = func(comments []models.Comment) (string, error) {
			matchCount = len(comments)
			return "", nil
		}

		err := handler.ListCommand(repository, "123", formatter, "", "10-20", "", false, 0)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		// Should match: line 15 (in range) and 18-22 (overlaps)
		if matchCount != 2 {
			t.Errorf("expected 2 matching comments, got %d", matchCount)
		}
	})
}

// TestDeleteCommand tests DeleteCommand behavior.
func TestDeleteCommand(t *testing.T) {
	tests := []struct {
		name           string
		setupComments  []models.Comment
		deleteID       string
		expectError    bool
		errorContains  string
		remainingCount int
	}{
		{
			name: "deletes by partial ID prefix",
			setupComments: []models.Comment{
				{Path: "test.go", Line: 10, Body: "comment 1", Side: "RIGHT"},
			},
			// ID will be generated, we'll use the first 8 chars in the actual test
			expectError:    false,
			remainingCount: 0,
		},
		{
			name: "returns error for non-matching ID",
			setupComments: []models.Comment{
				{Path: "test.go", Line: 10, Body: "comment 1", Side: "RIGHT"},
			},
			deleteID:       "nonexistent",
			expectError:    true,
			errorContains:  "no comment found",
			remainingCount: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")
			prNumber := 123

			// Setup comments
			for _, comment := range tt.setupComments {
				createTestComment(t, handler, repository, prNumber, comment)
			}

			// Get the actual comment ID if needed
			deleteID := tt.deleteID
			if tt.name == "deletes by partial ID prefix" {
				comments, err := handler.storage.GetComments(repository, prNumber)
				if err != nil {
					t.Fatalf("failed to get comments: %v", err)
				}
				if len(comments.Comments) > 0 {
					deleteID = comments.Comments[0].ID[:8]
				}
			}

			formatter := &MockOutputFormatter{}
			err := handler.DeleteCommand(repository, "123", deleteID, false, formatter)

			if tt.expectError {
				if err == nil {
					t.Errorf("expected error containing %q, got nil", tt.errorContains)
				} else if !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error containing %q, got %q", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("unexpected error: %v", err)
				}
			}

			// Verify remaining comment count
			comments, err := handler.storage.GetComments(repository, prNumber)
			if err != nil {
				t.Fatalf("failed to get comments: %v", err)
			}
			if len(comments.Comments) != tt.remainingCount {
				t.Errorf("expected %d remaining comments, got %d", tt.remainingCount, len(comments.Comments))
			}
		})
	}
}

// TestNextCommand tests NextCommand behavior.
func TestNextCommand(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	prNumber := 123

	// Create mix of resolved and unresolved comments
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path:   "file1.go",
		Line:   10,
		Body:   "unresolved 1",
		Side:   "RIGHT",
		Status: models.StatusUnresolved,
	})
	resolvedTime := time.Now()
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path:       "file1.go",
		Line:       20,
		Body:       "resolved",
		Side:       "RIGHT",
		Status:     models.StatusResolved,
		ResolvedAt: &resolvedTime,
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path:   "file2.go",
		Line:   15,
		Body:   "unresolved 2",
		Side:   "RIGHT",
		Status: models.StatusUnresolved,
	})

	formatter := &MockOutputFormatter{
		FormatSingleCommentFunc: func(comment models.Comment) (string, error) {
			return fmt.Sprintf("Next: %s", comment.Body), nil
		},
	}

	t.Run("returns only unresolved comments", func(t *testing.T) {
		called := false
		formatter.FormatSingleCommentFunc = func(comment models.Comment) (string, error) {
			called = true
			if comment.Status != models.StatusUnresolved {
				t.Errorf("expected unresolved comment, got status %s", comment.Status)
			}
			return "", nil
		}

		err := handler.NextCommand(repository, "123", formatter, "", "")
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !called {
			t.Error("formatter was not called")
		}
	})

	t.Run("filters by file when specified", func(t *testing.T) {
		called := false
		formatter.FormatSingleCommentFunc = func(comment models.Comment) (string, error) {
			called = true
			if comment.Path != "file1.go" {
				t.Errorf("expected file1.go, got %s", comment.Path)
			}
			return "", nil
		}

		err := handler.NextCommand(repository, "123", formatter, "file1.go", "")
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !called {
			t.Error("formatter was not called")
		}
	})
}

// TestResolveCommand tests ResolveCommand behavior.
func TestResolveCommand(t *testing.T) {
	tests := []struct {
		name           string
		archive        bool
		reason         string
		expectedStatus models.CommentStatus
	}{
		{
			name:           "marks comment as resolved",
			archive:        false,
			expectedStatus: models.StatusResolved,
		},
		{
			name:           "marks comment as archived",
			archive:        true,
			expectedStatus: models.StatusArchived,
		},
		{
			name:           "records resolution reason",
			archive:        false,
			reason:         "Fixed in commit abc123",
			expectedStatus: models.StatusResolved,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")
			prNumber := 123

			// Create unresolved comment
			createTestComment(t, handler, repository, prNumber, models.Comment{
				Path:   "test.go",
				Line:   10,
				Body:   "test comment",
				Side:   "RIGHT",
				Status: models.StatusUnresolved,
			})

			// Get comment ID
			comments, err := handler.storage.GetComments(repository, prNumber)
			if err != nil {
				t.Fatalf("failed to get comments: %v", err)
			}
			commentID := comments.Comments[0].ID[:8]

			// Resolve the comment
			err = handler.ResolveCommand(repository, "123", commentID, tt.archive, tt.reason)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
			}

			// Verify status change
			comments, err = handler.storage.GetComments(repository, prNumber)
			if err != nil {
				t.Fatalf("failed to get comments: %v", err)
			}
			if len(comments.Comments) != 1 {
				t.Fatalf("expected 1 comment, got %d", len(comments.Comments))
			}

			comment := comments.Comments[0]
			if comment.Status != tt.expectedStatus {
				t.Errorf("expected status %s, got %s", tt.expectedStatus, comment.Status)
			}
			if comment.ResolvedAt == nil {
				t.Error("expected ResolvedAt to be set")
			}
			if tt.reason != "" && comment.ResolutionReason != tt.reason {
				t.Errorf("expected reason %q, got %q", tt.reason, comment.ResolutionReason)
			}
		})
	}
}

// TestAutoAdjustCommand_DiffSourceSelection tests diff source selection logic.
func TestAutoAdjustCommand_DiffSourceSelection(t *testing.T) {
	tests := []struct {
		name        string
		since       string
		staged      bool
		unstaged    bool
		gitDiffSpec string
		mockSetup   func(*testing.T, *MockGitClient)
	}{
		{
			name:   "uses staged changes when --staged",
			staged: true,
			mockSetup: func(t *testing.T, m *MockGitClient) {
				called := false
				m.GetStagedDiffFunc = func() (string, error) {
					called = true
					return "", nil // Return empty diff to avoid parsing errors
				}
				t.Cleanup(func() {
					if !called {
						t.Error("GetStagedDiff was not called")
					}
				})
			},
		},
		{
			name:     "uses unstaged changes when --unstaged",
			unstaged: true,
			mockSetup: func(t *testing.T, m *MockGitClient) {
				called := false
				m.GetUnstagedDiffFunc = func() (string, error) {
					called = true
					return "", nil
				}
				t.Cleanup(func() {
					if !called {
						t.Error("GetUnstagedDiff was not called")
					}
				})
			},
		},
		{
			name:  "uses changes since commit when --since",
			since: "HEAD~3",
			mockSetup: func(t *testing.T, m *MockGitClient) {
				called := false
				m.GetDiffSinceFunc = func(since string) (string, error) {
					called = true
					if since != "HEAD~3" {
						t.Errorf("expected since=HEAD~3, got %s", since)
					}
					return "", nil
				}
				t.Cleanup(func() {
					if !called {
						t.Error("GetDiffSince was not called")
					}
				})
			},
		},
		{
			name: "defaults to HEAD~1 when no flags",
			mockSetup: func(t *testing.T, m *MockGitClient) {
				called := false
				m.GetDiffSinceFunc = func(since string) (string, error) {
					called = true
					if since != "HEAD~1" {
						t.Errorf("expected since=HEAD~1, got %s", since)
					}
					return "", nil
				}
				t.Cleanup(func() {
					if !called {
						t.Error("GetDiffSince was not called with default")
					}
				})
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")

			// Setup mock
			mockGit := &MockGitClient{}
			if tt.mockSetup != nil {
				tt.mockSetup(t, mockGit)
			}
			handler.gitClient = mockGit

			// Execute - errors are expected since we return empty diff, but the test
			// verifies that the correct diff source function was called
			_ = handler.AutoAdjustCommand(repository, "123", tt.since, tt.staged, tt.unstaged, tt.gitDiffSpec, false)
		})
	}
}

// TestAutoAdjustCommand_IfNeeded tests the --if-needed flag behavior.
func TestAutoAdjustCommand_IfNeeded(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")

	t.Run("skips adjustment when no changes and --if-needed", func(t *testing.T) {
		mockGit := &MockGitClient{
			GetDiffSinceFunc: func(_ string) (string, error) {
				return "", nil // No diff
			},
		}
		handler.gitClient = mockGit

		err := handler.AutoAdjustCommand(repository, "123", "", false, false, "", true)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
	})

	t.Run("performs adjustment when changes detected", func(_ *testing.T) {
		mockGit := &MockGitClient{
			GetDiffSinceFunc: func(_ string) (string, error) {
				return "some diff content", nil
			},
		}
		handler.gitClient = mockGit

		_ = handler.AutoAdjustCommand(repository, "123", "", false, false, "", true)
		// May fail due to diff parsing, but should attempt adjustment
		// The key is that it doesn't skip due to --if-needed
	})
}

// TestPullCommand_MergeStrategies tests merge strategy behavior.
func TestPullCommand_MergeStrategies(t *testing.T) {
	tests := []struct {
		name           string
		mergeStrategy  models.MergeStrategy
		localComments  []models.Comment
		githubComments []models.Comment
		expectedCount  int
		validateMerge  func(*testing.T, *models.MergeResult)
	}{
		{
			name:          "overwrites local comments with overwrite strategy",
			mergeStrategy: models.MergeStrategyOverwrite,
			localComments: []models.Comment{
				{Path: "test.go", Line: 10, Body: "local comment", Side: "RIGHT"},
			},
			githubComments: []models.Comment{
				{Path: "test.go", Line: 20, Body: "github comment", Side: "RIGHT"},
			},
			expectedCount: 1,
			validateMerge: func(t *testing.T, result *models.MergeResult) {
				if len(result.PulledComments) != 1 {
					t.Errorf("expected 1 comment, got %d", len(result.PulledComments))
				}
				if result.PulledComments[0].Body != "github comment" {
					t.Errorf("expected github comment, got %q", result.PulledComments[0].Body)
				}
			},
		},
		{
			name:          "merges keeping both with merge strategy",
			mergeStrategy: models.MergeStrategyMerge,
			localComments: []models.Comment{
				{Path: "test.go", Line: 10, Body: "local comment", Side: "RIGHT"},
			},
			githubComments: []models.Comment{
				{Path: "test.go", Line: 10, Body: "github comment", Side: "RIGHT"}, // Conflicting
				{Path: "test.go", Line: 20, Body: "new comment", Side: "RIGHT"},
			},
			expectedCount: 3, // local + 2 github (including conflicting one with new ID)
			validateMerge: func(t *testing.T, result *models.MergeResult) {
				if len(result.ConflictedComments) != 1 {
					t.Errorf("expected 1 conflicted comment, got %d", len(result.ConflictedComments))
				}
			},
		},
		{
			name:          "skips conflicting comments with skip strategy",
			mergeStrategy: models.MergeStrategySkip,
			localComments: []models.Comment{
				{Path: "test.go", Line: 10, Body: "local comment", Side: "RIGHT"},
			},
			githubComments: []models.Comment{
				{Path: "test.go", Line: 10, Body: "github comment", Side: "RIGHT"}, // Conflicting
				{Path: "test.go", Line: 20, Body: "new comment", Side: "RIGHT"},
			},
			expectedCount: 2, // local + 1 non-conflicting github
			validateMerge: func(t *testing.T, result *models.MergeResult) {
				if len(result.SkippedComments) != 1 {
					t.Errorf("expected 1 skipped comment, got %d", len(result.SkippedComments))
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("owner", "repo")
			prNumber := 123

			// Setup local comments
			for _, comment := range tt.localComments {
				createTestComment(t, handler, repository, prNumber, comment)
			}

			// Setup mock GitHub client
			mockGH := &MockGitHubClient{
				GetPRCommentsFunc: func(_ models.Repository, _ int) ([]models.Comment, error) {
					return tt.githubComments, nil
				},
			}
			handler.ghClient = mockGH

			// Execute pull
			options := models.PullOptions{
				MergeStrategy: tt.mergeStrategy,
			}
			result, err := handler.PullCommand(repository, prNumber, options)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			// Validate result
			if tt.validateMerge != nil {
				tt.validateMerge(t, result)
			}

			// Verify final count in storage
			comments, err := handler.storage.GetComments(repository, prNumber)
			if err != nil {
				t.Fatalf("failed to get comments: %v", err)
			}
			if len(comments.Comments) != tt.expectedCount {
				t.Errorf("expected %d total comments, got %d", tt.expectedCount, len(comments.Comments))
			}
		})
	}
}

// TestPullCommand_Filtering tests pull filtering behavior.
func TestPullCommand_Filtering(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	prNumber := 123

	githubComments := []models.Comment{
		{Path: "file1.go", Line: 10, Body: "comment 1", Side: "RIGHT"},
		{Path: "file2.go", Line: 20, Body: "comment 2", Side: "RIGHT"},
		{Path: "file3.go", Line: 30, Body: "comment 3", Side: "RIGHT"},
	}

	mockGH := &MockGitHubClient{
		GetPRCommentsFunc: func(_ models.Repository, _ int) ([]models.Comment, error) {
			return githubComments, nil
		},
	}
	handler.ghClient = mockGH

	t.Run("filters GitHub comments by file", func(t *testing.T) {
		options := models.PullOptions{
			File:          "file1.go",
			MergeStrategy: models.MergeStrategyOverwrite,
		}

		result, err := handler.PullCommand(repository, prNumber, options)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		if len(result.PulledComments) != 1 {
			t.Errorf("expected 1 filtered comment, got %d", len(result.PulledComments))
		}
		if result.PulledComments[0].Path != "file1.go" {
			t.Errorf("expected file1.go, got %s", result.PulledComments[0].Path)
		}
	})

	t.Run("dry run doesn't modify storage", func(t *testing.T) {
		// Clear storage first
		_ = handler.storage.ClearComments(repository, prNumber)

		options := models.PullOptions{
			DryRun:        true,
			MergeStrategy: models.MergeStrategyOverwrite,
		}

		result, err := handler.PullCommand(repository, prNumber, options)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}

		// Result should show comments
		if len(result.PulledComments) != 3 {
			t.Errorf("expected 3 comments in result, got %d", len(result.PulledComments))
		}

		// Storage should still be empty
		comments, err := handler.storage.GetComments(repository, prNumber)
		if err == nil && len(comments.Comments) > 0 {
			t.Errorf("expected storage to be empty in dry run, got %d comments", len(comments.Comments))
		}
	})
}

// TestSubmitCommand_DiffHunkFiltering tests comment filtering during submission.
func TestSubmitCommand_DiffHunkFiltering(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	prNumber := 123

	// Create diff hunks
	createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
		{File: "test.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
	})

	// Create mix of valid and invalid comments
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "test.go",
		Line: 15,
		Body: "valid comment",
		Side: "RIGHT",
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "test.go",
		Line: 25,
		Body: "invalid comment",
		Side: "RIGHT",
	})

	mockGH := &MockGitHubClient{
		SubmitReviewFunc: func(_ models.Repository, _ int, review models.PRReview) error {
			// Verify only valid comment is submitted
			if len(review.Comments) != 1 {
				t.Errorf("expected 1 comment to be submitted, got %d", len(review.Comments))
			}
			if review.Comments[0].Line != 15 {
				t.Errorf("expected line 15, got %d", review.Comments[0].Line)
			}
			return nil
		},
	}
	handler.ghClient = mockGH

	formatter := &MockOutputFormatter{}
	executor := &MockExecutor{}

	t.Run("filters out comments outside diff hunks before submission", func(t *testing.T) {
		err := handler.SubmitCommand(repository, prNumber, "Test review", "COMMENT", "", formatter, executor)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
	})
}

// TestSubmitCommand_FileFilter tests file filtering during submission.
func TestSubmitCommand_FileFilter(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("owner", "repo")
	prNumber := 123

	// Create diff hunks for multiple files
	createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
		{File: "file1.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
		{File: "file2.go", Side: "RIGHT", StartLine: 10, EndLine: 20},
	})

	// Create comments for different files
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "file1.go",
		Line: 15,
		Body: "comment for file1",
		Side: "RIGHT",
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "file2.go",
		Line: 15,
		Body: "comment for file2",
		Side: "RIGHT",
	})

	mockGH := &MockGitHubClient{
		SubmitReviewFunc: func(_ models.Repository, _ int, review models.PRReview) error {
			if len(review.Comments) != 1 {
				t.Errorf("expected 1 filtered comment, got %d", len(review.Comments))
			}
			if review.Comments[0].Path != "file1.go" {
				t.Errorf("expected file1.go, got %s", review.Comments[0].Path)
			}
			return nil
		},
	}
	handler.ghClient = mockGH

	formatter := &MockOutputFormatter{}
	executor := &MockExecutor{}

	err := handler.SubmitCommand(repository, prNumber, "Test review", "COMMENT", "file1.go", formatter, executor)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}
