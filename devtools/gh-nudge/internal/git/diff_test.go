package git

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/internal/executor"
)

// Test data constants.
const (
	testRepoPath  = "/test/repo"
	testCommitSHA = "abc123def456"
	testBaseSHA   = "base789xyz012"
	testBranch    = "feature-branch"
	testBase      = "main"
)

// Helper to create test repository.
func testRepository() models.Repository {
	return models.NewRepository("owner", "repo")
}

// Helper to create mock executor with common setup.
func newMockExecutor() *executor.MockExecutor {
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand("git", true)
	return mock
}

// TestNewClient tests the NewClient constructor which creates a git client with default settings.
// It verifies:
//   - Client is created with non-nil values
//   - Repository path is correctly set
//   - Default executor is initialized
//   - Context is initialized
func TestNewClient(t *testing.T) {
	t.Run("creates client with default executor", func(t *testing.T) {
		client := NewClient(testRepoPath)
		if client == nil {
			t.Fatal("expected non-nil client")
			return
		}
		if client.repoPath != testRepoPath {
			t.Errorf("expected repoPath %s, got %s", testRepoPath, client.repoPath)
		}
		if client.exec == nil {
			t.Error("expected non-nil executor")
		}
		if client.ctx == nil {
			t.Error("expected non-nil context")
		}
	})
}

// TestNewClientWithExecutor tests the NewClientWithExecutor constructor which creates
// a git client with a custom executor and context (useful for testing).
// It verifies:
//   - Client is created with provided custom executor
//   - Client uses the provided context
//   - Repository path is correctly set
func TestNewClientWithExecutor(t *testing.T) {
	t.Run("creates client with custom executor", func(t *testing.T) {
		ctx := context.Background()
		mockExec := newMockExecutor()
		client := NewClientWithExecutor(ctx, testRepoPath, mockExec)

		if client == nil {
			t.Fatal("expected non-nil client")
		}
		if client.repoPath != testRepoPath {
			t.Errorf("expected repoPath %s, got %s", testRepoPath, client.repoPath)
		}
		if client.exec != mockExec {
			t.Error("expected provided executor")
		}
		if client.ctx != ctx {
			t.Error("expected provided context")
		}
	})
}

// TestCaptureBranchDiff tests the CaptureBranchDiff method which captures diff hunks
// between two branches. It verifies:
//   - Successful diff capture with correct metadata (branch names, commit SHAs, timestamps)
//   - Proper parsing of diff output into DiffHunk structures with LEFT/RIGHT sides
//   - Error handling when git rev-parse fails for branch or base branch
//   - Error handling when git diff command fails
//   - Correct parsing of multiple diff hunks across multiple files
func TestCaptureBranchDiff(t *testing.T) {
	branch := mustNewBranch(t, testBranch)
	baseBranch := mustNewBranch(t, testBase)
	repo := testRepository()

	t.Run("successfully captures diff between branches", func(t *testing.T) {
		mockExec := newMockExecutor()

		// Mock git rev-parse for feature branch
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBranch).
			WillSucceed(testCommitSHA+"\n", 0).Build()

		// Mock git rev-parse for base branch
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBase).
			WillSucceed(testBaseSHA+"\n", 0).Build()

		// Mock git diff
		diffOutput := `diff --git a/file.go b/file.go
index abc123..def456 100644
--- a/file.go
+++ b/file.go
@@ -10,5 +10,6 @@ func example() {
 	line1
 	line2
+	line3
 	line4
 	line5
`
		mockExec.ExpectCommandWithArgs("git", "diff", "--unified=3", fmt.Sprintf("%s...%s", testBase, testBranch)).
			WillSucceed(diffOutput, 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.CaptureBranchDiff(repo, branch, baseBranch)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result == nil {
			t.Fatal("expected non-nil result")
		}
		if result.BranchName != testBranch {
			t.Errorf("expected branch %s, got %s", testBranch, result.BranchName)
		}
		if result.CommitSHA != testCommitSHA {
			t.Errorf("expected commit SHA %s, got %s", testCommitSHA, result.CommitSHA)
		}
		if result.BaseSHA != testBaseSHA {
			t.Errorf("expected base SHA %s, got %s", testBaseSHA, result.BaseSHA)
		}
		if result.BaseBranch != testBase {
			t.Errorf("expected base branch %s, got %s", testBase, result.BaseBranch)
		}
		if len(result.DiffHunks) == 0 {
			t.Error("expected at least one diff hunk")
		}
		if result.CapturedAt.IsZero() {
			t.Error("expected non-zero captured time")
		}
	})

	t.Run("fails when getting commit SHA fails", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBranch).
			WillFail("fatal: bad revision", 128).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.CaptureBranchDiff(repo, branch, baseBranch)

		if err == nil {
			t.Fatal("expected error when git rev-parse fails")
		}
	})

	t.Run("fails when getting base SHA fails", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBranch).
			WillSucceed(testCommitSHA+"\n", 0).Build()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBase).
			WillFail("fatal: bad revision", 128).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.CaptureBranchDiff(repo, branch, baseBranch)

		if err == nil {
			t.Fatal("expected error when getting base SHA fails")
		}
	})

	t.Run("fails when git diff fails", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBranch).
			WillSucceed(testCommitSHA+"\n", 0).Build()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBase).
			WillSucceed(testBaseSHA+"\n", 0).Build()
		mockExec.ExpectCommandWithArgs("git", "diff", "--unified=3", fmt.Sprintf("%s...%s", testBase, testBranch)).
			WillFail("fatal: ambiguous argument", 128).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.CaptureBranchDiff(repo, branch, baseBranch)

		if err == nil {
			t.Fatal("expected error when git diff fails")
		}
	})

	t.Run("parses multiple hunks correctly", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBranch).
			WillSucceed(testCommitSHA+"\n", 0).Build()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", testBase).
			WillSucceed(testBaseSHA+"\n", 0).Build()

		diffOutput := `diff --git a/file1.go b/file1.go
@@ -10,3 +10,4 @@ func test() {
 line1
 line2
+line3
diff --git a/file2.go b/file2.go
@@ -20,2 +20,3 @@ func test2() {
 line1
+line2
`
		mockExec.ExpectCommandWithArgs("git", "diff", "--unified=3", fmt.Sprintf("%s...%s", testBase, testBranch)).
			WillSucceed(diffOutput, 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.CaptureBranchDiff(repo, branch, baseBranch)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(result.DiffHunks) < 2 {
			t.Errorf("expected at least 2 hunks, got %d", len(result.DiffHunks))
		}
	})
}

// TestGetStagedDiff tests the GetStagedDiff method which retrieves the diff of staged changes.
// It verifies:
//   - Successfully retrieves staged diff output
//   - Returns empty string when no staged changes exist
//   - Error handling when git diff --cached fails
func TestGetStagedDiff(t *testing.T) {
	t.Run("successfully gets staged diff", func(t *testing.T) {
		mockExec := newMockExecutor()
		expectedDiff := "some staged changes"
		mockExec.ExpectCommandWithArgs("git", "diff", "--cached", "--unified=3").
			WillSucceed(expectedDiff, 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetStagedDiff()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result != expectedDiff {
			t.Errorf("expected diff %q, got %q", expectedDiff, result)
		}
	})

	t.Run("fails when git diff fails", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "diff", "--cached", "--unified=3").
			WillFail("fatal: not a git repository", 128).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.GetStagedDiff()

		if err == nil {
			t.Fatal("expected error when git diff fails")
		}
	})

	t.Run("returns empty string when no staged changes", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "diff", "--cached", "--unified=3").
			WillSucceed("", 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetStagedDiff()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result != "" {
			t.Errorf("expected empty diff, got %q", result)
		}
	})
}

// TestGetUnstagedDiff tests the GetUnstagedDiff method which retrieves the diff of
// unstaged working directory changes.
// It verifies:
//   - Successfully retrieves unstaged diff output
//   - Error handling when git diff fails
func TestGetUnstagedDiff(t *testing.T) {
	t.Run("successfully gets unstaged diff", func(t *testing.T) {
		mockExec := newMockExecutor()
		expectedDiff := "some unstaged changes"
		mockExec.ExpectCommandWithArgs("git", "diff", "--unified=3").
			WillSucceed(expectedDiff, 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetUnstagedDiff()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result != expectedDiff {
			t.Errorf("expected diff %q, got %q", expectedDiff, result)
		}
	})

	t.Run("fails when git diff fails", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "diff", "--unified=3").
			WillFail("fatal: not a git repository", 128).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.GetUnstagedDiff()

		if err == nil {
			t.Fatal("expected error when git diff fails")
		}
	})
}

// TestGetDiffSince tests the GetDiffSince method which retrieves the diff since a specific commit.
// It verifies:
//   - Successfully retrieves diff since the specified commit SHA
//   - Error handling when git diff fails with invalid commit SHA
func TestGetDiffSince(t *testing.T) {
	sinceSHA := "abc123"

	t.Run("successfully gets diff since commit", func(t *testing.T) {
		mockExec := newMockExecutor()
		expectedDiff := "changes since commit"
		mockExec.ExpectCommandWithArgs("git", "diff", "--unified=3", sinceSHA).
			WillSucceed(expectedDiff, 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetDiffSince(sinceSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result != expectedDiff {
			t.Errorf("expected diff %q, got %q", expectedDiff, result)
		}
	})

	t.Run("fails when git diff fails", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "diff", "--unified=3", sinceSHA).
			WillFail("fatal: bad revision", 128).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.GetDiffSince(sinceSHA)

		if err == nil {
			t.Fatal("expected error when git diff fails")
		}
	})
}

// TestGetDefaultBaseBranch tests the GetDefaultBaseBranch method which determines
// the default base branch (main or master).
// It verifies:
//   - Returns "main" when git symbolic-ref succeeds and points to main
//   - Returns "master" when git symbolic-ref succeeds and points to master
//   - Falls back to checking for "main" branch when symbolic-ref fails
//   - Falls back to checking for "master" branch when main doesn't exist
//   - Returns error when neither main nor master branches exist
func TestGetDefaultBaseBranch(t *testing.T) {
	t.Run("returns main when symbolic-ref succeeds", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "symbolic-ref", "refs/remotes/origin/HEAD").
			WillSucceed("refs/remotes/origin/main\n", 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetDefaultBaseBranch()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result.String() != "main" {
			t.Errorf("expected main, got %s", result.String())
		}
	})

	t.Run("returns master when symbolic-ref succeeds with master", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "symbolic-ref", "refs/remotes/origin/HEAD").
			WillSucceed("refs/remotes/origin/master\n", 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetDefaultBaseBranch()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result.String() != "master" {
			t.Errorf("expected master, got %s", result.String())
		}
	})

	t.Run("falls back to checking main when symbolic-ref fails", func(t *testing.T) {
		mockExec := newMockExecutor()
		// symbolic-ref fails
		mockExec.ExpectCommandWithArgs("git", "symbolic-ref", "refs/remotes/origin/HEAD").
			WillFail("fatal: ref refs/remotes/origin/HEAD is not a symbolic ref", 1).Build()
		// Fallback: check if main exists
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "origin/main").
			WillSucceed(testCommitSHA+"\n", 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetDefaultBaseBranch()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result.String() != "main" {
			t.Errorf("expected main, got %s", result.String())
		}
	})

	t.Run("falls back to checking master when main does not exist", func(t *testing.T) {
		mockExec := newMockExecutor()
		// symbolic-ref fails
		mockExec.ExpectCommandWithArgs("git", "symbolic-ref", "refs/remotes/origin/HEAD").
			WillFail("fatal: ref refs/remotes/origin/HEAD is not a symbolic ref", 1).Build()
		// main doesn't exist
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "origin/main").
			WillFail("fatal: Needed a single revision", 128).Build()
		// master exists
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "origin/master").
			WillSucceed(testCommitSHA+"\n", 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		result, err := client.GetDefaultBaseBranch()
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if result.String() != "master" {
			t.Errorf("expected master, got %s", result.String())
		}
	})

	t.Run("fails when no default branch found", func(t *testing.T) {
		mockExec := newMockExecutor()
		// symbolic-ref fails
		mockExec.ExpectCommandWithArgs("git", "symbolic-ref", "refs/remotes/origin/HEAD").
			WillFail("fatal: ref refs/remotes/origin/HEAD is not a symbolic ref", 1).Build()
		// main doesn't exist
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "origin/main").
			WillFail("fatal: Needed a single revision", 128).Build()
		// master doesn't exist
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "origin/master").
			WillFail("fatal: Needed a single revision", 128).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.GetDefaultBaseBranch()

		if err == nil {
			t.Fatal("expected error when no default branch found")
		}
	})
}

// TestBranchExists tests the BranchExists method which checks if a branch exists
// in the repository.
// It verifies:
//   - Returns true when the branch exists (git rev-parse succeeds)
//   - Returns false when the branch doesn't exist (exit code 1)
//   - Returns error for other git failures (e.g., not a git repository)
func TestBranchExists(t *testing.T) {
	branch := mustNewBranch(t, "test-branch")

	t.Run("returns true when branch exists", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "test-branch").
			WillSucceed(testCommitSHA+"\n", 0).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		exists, err := client.BranchExists(branch)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !exists {
			t.Error("expected branch to exist")
		}
	})

	t.Run("returns false when branch does not exist", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "test-branch").
			WillReturn(&executor.ExecutionResult{
				Command:  "git",
				Args:     []string{"rev-parse", "--verify", "test-branch"},
				ExitCode: 1,
				Stderr:   "fatal: Needed a single revision",
			}, nil).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		exists, err := client.BranchExists(branch)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if exists {
			t.Error("expected branch to not exist")
		}
	})

	t.Run("returns error for other git failures", func(t *testing.T) {
		mockExec := newMockExecutor()
		mockExec.ExpectCommandWithArgs("git", "rev-parse", "--verify", "test-branch").
			WillReturn(&executor.ExecutionResult{
				Command:  "git",
				Args:     []string{"rev-parse", "--verify", "test-branch"},
				ExitCode: 128,
				Stderr:   "fatal: not a git repository",
			}, nil).Build()

		client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)
		_, err := client.BranchExists(branch)

		if err == nil {
			t.Fatal("expected error for non-branch-not-found failures")
		}
	})
}

// TestParseRange tests the parseRange method which parses git diff range specifications
// like "15,10" or "15" into start line and count values.
// It verifies:
//   - Parses single line numbers correctly (e.g., "15" -> start=15, count=1)
//   - Parses range with count correctly (e.g., "10,5" -> start=10, count=5)
//   - Handles zero count ranges (e.g., "10,0" -> start=10, count=0)
//   - Returns error for invalid formats (multiple commas, non-numeric values, empty strings)
func TestParseRange(t *testing.T) {
	client := NewClient(testRepoPath)

	tests := []struct {
		name          string
		input         string
		wantStartLine int
		wantCount     int
		wantErr       bool
	}{
		{
			name:          "parses single line",
			input:         "15",
			wantStartLine: 15,
			wantCount:     1,
			wantErr:       false,
		},
		{
			name:          "parses range with count",
			input:         "10,5",
			wantStartLine: 10,
			wantCount:     5,
			wantErr:       false,
		},
		{
			name:          "parses range with zero count",
			input:         "10,0",
			wantStartLine: 10,
			wantCount:     0,
			wantErr:       false,
		},
		{
			name:    "fails on invalid format",
			input:   "10,5,3",
			wantErr: true,
		},
		{
			name:    "fails on non-numeric start",
			input:   "abc,5",
			wantErr: true,
		},
		{
			name:    "fails on non-numeric count",
			input:   "10,xyz",
			wantErr: true,
		},
		{
			name:    "fails on empty string",
			input:   "",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			startLine, count, err := client.parseRange(tt.input)

			if tt.wantErr {
				if err == nil {
					t.Fatal("expected error but got none")
				}
				return
			}

			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if startLine != tt.wantStartLine {
				t.Errorf("expected start line %d, got %d", tt.wantStartLine, startLine)
			}
			if count != tt.wantCount {
				t.Errorf("expected count %d, got %d", tt.wantCount, count)
			}
		})
	}
}

// TestParseHunkHeader tests the parseHunkHeader method which parses git diff hunk headers
// (e.g., "@@ -10,3 +10,4 @@ func test()") into LEFT and RIGHT DiffHunk structures.
// It verifies:
//   - Parses headers with additions/deletions into separate LEFT and RIGHT hunks
//   - Parses single line changes correctly
//   - Handles zero old count (pure additions) by returning only RIGHT hunk
//   - Gracefully handles malformed headers by returning no hunks
//   - Returns error for missing range information
func TestParseHunkHeader(t *testing.T) {
	client := NewClient(testRepoPath)
	testFile := "test.go"

	tests := []struct {
		name          string
		header        string
		wantLeftSide  bool
		wantLeftLine  int
		wantLeftEnd   int
		wantRightSide bool
		wantRightLine int
		wantRightEnd  int
		wantErr       bool
	}{
		{
			name:          "parses header with additions",
			header:        "@@ -10,3 +10,4 @@ func test() {",
			wantLeftSide:  true,
			wantLeftLine:  10,
			wantLeftEnd:   12,
			wantRightSide: true,
			wantRightLine: 10,
			wantRightEnd:  13,
			wantErr:       false,
		},
		{
			name:          "parses header with single line",
			header:        "@@ -15 +15 @@ context",
			wantLeftSide:  true,
			wantLeftLine:  15,
			wantLeftEnd:   15,
			wantRightSide: true,
			wantRightLine: 15,
			wantRightEnd:  15,
			wantErr:       false,
		},
		{
			name:          "parses header with zero old count",
			header:        "@@ -10,0 +10,3 @@ added lines",
			wantLeftSide:  false,
			wantRightSide: true,
			wantRightLine: 10,
			wantRightEnd:  12,
			wantErr:       false,
		},
		{
			name:          "handles malformed header by returning no hunks",
			header:        "@@ invalid @@",
			wantLeftSide:  false,
			wantRightSide: false,
			wantErr:       false,
		},
		{
			name:    "fails on missing ranges",
			header:  "@@ @@",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			leftHunk, rightHunk, err := client.parseHunkHeader(tt.header, testFile, testCommitSHA, testBaseSHA)

			if tt.wantErr {
				if err == nil {
					t.Fatal("expected error but got none")
				}
				return
			}

			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if tt.wantLeftSide {
				if leftHunk == nil {
					t.Fatal("expected left hunk but got nil")
				}
				if leftHunk.Side != models.SideLeft {
					t.Errorf("expected LEFT side, got %s", leftHunk.Side)
				}
				if leftHunk.Location.Lines.StartLine != tt.wantLeftLine {
					t.Errorf("expected left start line %d, got %d", tt.wantLeftLine, leftHunk.Location.Lines.StartLine)
				}
				if leftHunk.Location.Lines.EndLine != tt.wantLeftEnd {
					t.Errorf("expected left end line %d, got %d", tt.wantLeftEnd, leftHunk.Location.Lines.EndLine)
				}
				if leftHunk.SHA != testBaseSHA {
					t.Errorf("expected left SHA %s, got %s", testBaseSHA, leftHunk.SHA)
				}
			} else if leftHunk != nil {
				t.Error("expected no left hunk but got one")
			}

			if tt.wantRightSide {
				if rightHunk == nil {
					t.Fatal("expected right hunk but got nil")
				}
				if rightHunk.Side != models.SideRight {
					t.Errorf("expected RIGHT side, got %s", rightHunk.Side)
				}
				if rightHunk.Location.Lines.StartLine != tt.wantRightLine {
					t.Errorf("expected right start line %d, got %d", tt.wantRightLine, rightHunk.Location.Lines.StartLine)
				}
				if rightHunk.Location.Lines.EndLine != tt.wantRightEnd {
					t.Errorf("expected right end line %d, got %d", tt.wantRightEnd, rightHunk.Location.Lines.EndLine)
				}
				if rightHunk.SHA != testCommitSHA {
					t.Errorf("expected right SHA %s, got %s", testCommitSHA, rightHunk.SHA)
				}
			} else if rightHunk != nil {
				t.Error("expected no right hunk but got one")
			}
		})
	}
}

// TestParseDiffOutput tests the parseDiffOutput method which parses complete git diff output
// into DiffHunk structures.
// It verifies:
//   - Parses simple diffs with one hunk into LEFT and RIGHT hunks
//   - Parses diffs with multiple files correctly
//   - Handles empty diff output (no changes)
//   - Skips malformed hunk headers and continues parsing valid ones
func TestParseDiffOutput(t *testing.T) {
	client := NewClient(testRepoPath)

	t.Run("parses simple diff with one hunk", func(t *testing.T) {
		diffOutput := `diff --git a/file.go b/file.go
index abc123..def456 100644
--- a/file.go
+++ b/file.go
@@ -10,3 +10,4 @@ func test() {
 line1
 line2
+line3
`
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(hunks) == 0 {
			t.Fatal("expected at least one hunk")
		}
		// Should have one LEFT hunk and one RIGHT hunk
		if len(hunks) != 2 {
			t.Errorf("expected 2 hunks (LEFT and RIGHT), got %d", len(hunks))
		}
	})

	t.Run("parses multiple files", func(t *testing.T) {
		diffOutput := `diff --git a/file1.go b/file1.go
@@ -10,2 +10,3 @@ func test() {
 line1
+line2
diff --git a/file2.go b/file2.go
@@ -20,2 +20,3 @@ func test2() {
 line1
+line2
`
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Each hunk should create LEFT and RIGHT hunks = 2 hunks per file
		if len(hunks) < 2 {
			t.Errorf("expected at least 2 hunks, got %d", len(hunks))
		}
	})

	t.Run("handles empty diff", func(t *testing.T) {
		diffOutput := ""
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(hunks) != 0 {
			t.Errorf("expected no hunks for empty diff, got %d", len(hunks))
		}
	})

	t.Run("skips malformed hunk headers", func(t *testing.T) {
		diffOutput := `diff --git a/file.go b/file.go
@@ invalid header @@
 some content
@@ -10,2 +10,3 @@ valid header
 line1
+line2
`
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Should parse the valid hunk and skip the malformed one
		if len(hunks) == 0 {
			t.Error("expected at least one hunk from valid header")
		}
	})
}

// Note: Tests for AutoDetectChanges, GroupConsecutiveChanges, CalculateNetOffset,
// CalculateOverallConfidence, and ParseCurrentDiff are in autodetect_test.go

// TestValidateSuggestionsAgainstDiffHunks tests the validateSuggestionsAgainstDiffHunks method
// which validates automatic mapping suggestions for reasonableness.
// It verifies:
//   - Filters out suggestions with invalid line numbers (<=0)
//   - Marks suggestions with unusually large offsets (>1000 or <-1000) as low confidence
//   - Handles empty suggestion lists correctly
func TestValidateSuggestionsAgainstDiffHunks(t *testing.T) {
	client := NewClient(testRepoPath)

	t.Run("filters out invalid line numbers", func(t *testing.T) {
		suggestions := []models.MappingSuggestion{
			{OriginalLine: 0, Offset: 5},  // Invalid
			{OriginalLine: -1, Offset: 3}, // Invalid
			{OriginalLine: 10, Offset: 2}, // Valid
		}

		result := client.validateSuggestionsAgainstDiffHunks(suggestions, nil)

		if len(result) != 1 {
			t.Errorf("expected 1 valid suggestion, got %d", len(result))
		}
		if result[0].OriginalLine != 10 {
			t.Errorf("expected original line 10, got %d", result[0].OriginalLine)
		}
	})

	t.Run("marks large offsets as low confidence", func(t *testing.T) {
		suggestions := []models.MappingSuggestion{
			{OriginalLine: 10, Offset: 2000, Confidence: models.ConfidenceHigh},
		}

		result := client.validateSuggestionsAgainstDiffHunks(suggestions, nil)

		if len(result) != 1 {
			t.Fatalf("expected 1 suggestion, got %d", len(result))
		}
		if result[0].Confidence != models.ConfidenceLow {
			t.Errorf("expected low confidence for large offset, got %s", result[0].Confidence)
		}
	})

	t.Run("handles empty suggestions", func(t *testing.T) {
		suggestions := []models.MappingSuggestion{}
		result := client.validateSuggestionsAgainstDiffHunks(suggestions, nil)

		if len(result) != 0 {
			t.Errorf("expected 0 suggestions, got %d", len(result))
		}
	})
}

// TestConflictsWithDiffHunks tests the conflictsWithDiffHunks method which checks if
// a change group overlaps with stored diff hunks.
// It verifies:
//   - Detects overlapping line ranges between change groups and stored hunks
//   - Returns false when ranges don't overlap
//   - Handles empty hunk lists correctly
func TestConflictsWithDiffHunks(t *testing.T) {
	client := NewClient(testRepoPath)

	t.Run("detects overlap with stored hunk", func(t *testing.T) {
		group := ChangeGroup{
			Line: models.LineRange{StartLine: 10, EndLine: 15},
		}
		hunks := []models.DiffHunk{
			{
				Location: models.NewFileLocation("test.go", models.NewLineRange(12, 17)),
			},
		}

		conflicts := client.conflictsWithDiffHunks(group, hunks)

		if !conflicts {
			t.Error("expected conflict to be detected")
		}
	})

	t.Run("no conflict when ranges don't overlap", func(t *testing.T) {
		group := ChangeGroup{
			Line: models.LineRange{StartLine: 10, EndLine: 15},
		}
		hunks := []models.DiffHunk{
			{
				Location: models.NewFileLocation("test.go", models.NewLineRange(20, 25)),
			},
		}

		conflicts := client.conflictsWithDiffHunks(group, hunks)

		if conflicts {
			t.Error("expected no conflict")
		}
	})

	t.Run("handles empty hunks", func(t *testing.T) {
		group := ChangeGroup{
			Line: models.LineRange{StartLine: 10, EndLine: 15},
		}

		conflicts := client.conflictsWithDiffHunks(group, []models.DiffHunk{})

		if conflicts {
			t.Error("expected no conflict with empty hunks")
		}
	})
}

// TestCreateSuggestionFromGroup tests the createSuggestionFromGroup method which creates
// automatic mapping suggestions from grouped line changes.
// It verifies:
//   - Creates suggestions with correct offset for groups with net changes
//   - Returns nil for groups with zero net change (no adjustment needed)
//   - Lowers confidence to medium for complex change patterns (>5 changes)
//   - Lowers confidence to low when changes conflict with stored diff hunks
func TestCreateSuggestionFromGroup(t *testing.T) {
	client := NewClient(testRepoPath)

	t.Run("creates suggestion for group with net change", func(t *testing.T) {
		group := ChangeGroup{
			Line:      models.LineRange{StartLine: 10, EndLine: 12},
			NetOffset: 2,
			Changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineAdded},
			},
		}

		suggestion := client.createSuggestionFromGroup(group, nil)

		if suggestion == nil {
			t.Fatal("expected suggestion but got nil")
		}
		if suggestion.Offset != 2 {
			t.Errorf("expected offset 2, got %d", suggestion.Offset)
		}
		if suggestion.OriginalLine != 10 {
			t.Errorf("expected original line 10, got %d", suggestion.OriginalLine)
		}
		if suggestion.Confidence != models.ConfidenceHigh {
			t.Errorf("expected high confidence, got %s", suggestion.Confidence)
		}
	})

	t.Run("returns nil for zero net change", func(t *testing.T) {
		group := ChangeGroup{
			Line:      models.LineRange{StartLine: 10, EndLine: 12},
			NetOffset: 0,
			Changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineDeleted},
			},
		}

		suggestion := client.createSuggestionFromGroup(group, nil)

		if suggestion != nil {
			t.Error("expected nil suggestion for zero net change")
		}
	})

	t.Run("lowers confidence for complex changes", func(t *testing.T) {
		changes := make([]models.LineChange, 10)
		for i := range changes {
			changes[i] = models.LineChange{Type: models.LineAdded}
		}
		group := ChangeGroup{
			Line:      models.LineRange{StartLine: 10, EndLine: 20},
			NetOffset: 10,
			Changes:   changes,
		}

		suggestion := client.createSuggestionFromGroup(group, nil)

		if suggestion == nil {
			t.Fatal("expected suggestion but got nil")
		}
		if suggestion.Confidence != models.ConfidenceMedium {
			t.Errorf("expected medium confidence for complex changes, got %s", suggestion.Confidence)
		}
	})

	t.Run("lowers confidence for conflicts", func(t *testing.T) {
		group := ChangeGroup{
			Line:      models.LineRange{StartLine: 10, EndLine: 15},
			NetOffset: 2,
			Changes: []models.LineChange{
				{Type: models.LineAdded},
				{Type: models.LineAdded},
			},
		}
		hunks := []models.DiffHunk{
			{
				Location: models.NewFileLocation("test.go", models.NewLineRange(12, 17)),
			},
		}

		suggestion := client.createSuggestionFromGroup(group, hunks)

		if suggestion == nil {
			t.Fatal("expected suggestion but got nil")
		}
		if suggestion.Confidence != models.ConfidenceLow {
			t.Errorf("expected low confidence for conflicts, got %s", suggestion.Confidence)
		}
	})
}

// Helper function to create branch or fail test.
func mustNewBranch(t *testing.T, name string) Branch {
	t.Helper()
	branch, err := NewBranch(name)
	if err != nil {
		t.Fatalf("failed to create branch %s: %v", name, err)
	}
	return branch
}

// Benchmark tests for performance-critical operations.
func BenchmarkParseDiffOutput(b *testing.B) {
	client := NewClient(testRepoPath)
	diffOutput := `diff --git a/file.go b/file.go
@@ -10,3 +10,4 @@ func test() {
 line1
 line2
+line3
 line4
`
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
	}
}

func BenchmarkParseRange(b *testing.B) {
	client := NewClient(testRepoPath)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _, _ = client.parseRange("10,5")
	}
}

// TestRealWorldDiffParsing tests parsing of realistic git diff output with real-world
// formatting and edge cases.
// It verifies:
//   - Parses actual git diff output with multiple context lines
//   - Extracts correct file paths from diff headers
//   - Handles real formatting including error message modifications
func TestRealWorldDiffParsing(t *testing.T) {
	client := NewClient(testRepoPath)

	t.Run("parses real git diff output", func(t *testing.T) {
		// This is a real example of git diff output
		diffOutput := `diff --git a/internal/git/diff.go b/internal/git/diff.go
index 1234567..abcdefg 100644
--- a/internal/git/diff.go
+++ b/internal/git/diff.go
@@ -319,10 +319,12 @@ func (gc *Client) AutoDetectChanges(file string, storedDiffHunks []models.DiffH
 	// Get current file diff compared to the original captured state
 	currentDiff, err := gc.getFileDiffFromCapture(file, storedDiffHunks)
 	if err != nil {
-		return nil, fmt.Errorf("failed to get current file diff: %w", err)
+		return nil, fmt.Errorf("failed to get file diff: %w", err)
 	}

 	// Parse the current diff to detect line changes
 	changes, err := gc.parseCurrentDiff(currentDiff)
+	if err != nil {
+		return nil, fmt.Errorf("failed to parse diff: %w", err)
+	}
 	if err != nil {
 		return nil, fmt.Errorf("failed to parse current diff: %w", err)
`
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error parsing real diff: %v", err)
		}
		if len(hunks) == 0 {
			t.Fatal("expected hunks from real diff output")
		}

		// Verify the hunks have correct file paths
		for _, hunk := range hunks {
			if hunk.Location.Path != "internal/git/diff.go" {
				t.Errorf("expected path 'internal/git/diff.go', got %s", hunk.Location.Path)
			}
		}
	})
}

// TestEdgeCases tests parsing of edge cases and unusual diff formats.
// It verifies:
//   - Handles diffs with only deletions (no additions)
//   - Handles binary file diffs (should produce no hunks)
//   - Handles very long lines (10000+ characters) in diffs
//   - Handles Unicode characters (émojis, Chinese characters) in diffs
func TestEdgeCases(t *testing.T) {
	client := NewClient(testRepoPath)

	t.Run("handles diff with only deletions", func(t *testing.T) {
		diffOutput := `diff --git a/file.go b/file.go
@@ -10,3 +10,1 @@ func test() {
-line1
-line2
 line3
`
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Should have LEFT hunk for deletions, and RIGHT hunk
		if len(hunks) == 0 {
			t.Fatal("expected hunks for deletions")
		}
	})

	t.Run("handles diff with binary files", func(t *testing.T) {
		diffOutput := `diff --git a/image.png b/image.png
index abc123..def456 100644
Binary files a/image.png and b/image.png differ
`
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		// Binary diffs should not produce hunks
		if len(hunks) != 0 {
			t.Errorf("expected no hunks for binary diff, got %d", len(hunks))
		}
	})

	t.Run("handles very long lines in diff", func(t *testing.T) {
		longLine := "+" + string(make([]byte, 10000))
		diffOutput := fmt.Sprintf(`diff --git a/file.go b/file.go
@@ -10,1 +10,2 @@ func test() {
 line1
%s
`, longLine)

		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(hunks) == 0 {
			t.Fatal("expected hunks even with long lines")
		}
	})

	t.Run("handles diff with unicode characters", func(t *testing.T) {
		diffOutput := `diff --git a/file.go b/file.go
@@ -10,2 +10,3 @@ func test() {
 line1 with émojis 🎉
+line2 with 中文字符
 line3
`
		hunks, err := client.parseDiffOutput(diffOutput, testCommitSHA, testBaseSHA)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if len(hunks) == 0 {
			t.Fatal("expected hunks with unicode characters")
		}
	})
}

// TestConcurrentAccess tests thread safety of parsing methods when accessed concurrently.
// It verifies:
//   - Multiple goroutines can safely call parseRange concurrently
//   - Multiple goroutines can safely call parseHunkHeader concurrently
//   - No race conditions or panics occur during concurrent access
//   - All concurrent operations complete within reasonable time (5 seconds)
func TestConcurrentAccess(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping concurrent test in short mode")
	}

	mockExec := newMockExecutor()
	mockExec.ExpectCommand("git").WillSucceed(testCommitSHA+"\n", 0).Build()

	client := NewClientWithExecutor(context.Background(), testRepoPath, mockExec)

	// Run multiple goroutines accessing client methods
	const numGoroutines = 10
	done := make(chan bool, numGoroutines)

	for i := 0; i < numGoroutines; i++ {
		go func() {
			defer func() { done <- true }()
			_, _, _ = client.parseRange("10,5")
			_, _, _ = client.parseHunkHeader("@@ -10,3 +10,4 @@", "test.go", testCommitSHA, testBaseSHA)
		}()
	}

	// Wait for all goroutines to complete
	timeout := time.After(5 * time.Second)
	for i := 0; i < numGoroutines; i++ {
		select {
		case <-done:
			// Success
		case <-timeout:
			t.Fatal("concurrent test timed out")
		}
	}
}
