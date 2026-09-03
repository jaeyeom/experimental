package gitchanges

import (
	"context"
	"errors"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

const testGit = "git"

func TestChangedArgvAndParse(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testGit, true)
	mock.ExpectCommandWithArgs(testGit, "diff", "--name-only", "main...HEAD").
		WillSucceed("internal/auth/token.go\nconfig/schema.go\n\n", 0).Build()
	got, err := Changed(context.Background(), mock, "main", "/repo")
	if err != nil {
		t.Fatalf("Changed() error = %v", err)
	}
	want := []string{"internal/auth/token.go", "config/schema.go"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("Changed() = %v, want %v", got, want)
	}
}

func TestChangedNotRepo(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testGit, true)
	mock.ExpectCommandWithArgs(testGit, "diff", "--name-only", "main...HEAD").
		WillFail("fatal: not a git repository (or any of the parent directories): .git", 128).Build()
	_, err := Changed(context.Background(), mock, "main", "/repo")
	if !errors.Is(err, ErrNotRepo) {
		t.Fatalf("Changed() error = %v, want ErrNotRepo", err)
	}
}

func TestChangedGitMissing(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs(testGit, "diff", "--name-only", "main...HEAD").
		WillError(&executor.ExecutableNotFoundError{Command: testGit}).Build()
	_, err := Changed(context.Background(), mock, "main", "/repo")
	var notFound *executor.ExecutableNotFoundError
	if !errors.As(err, &notFound) {
		t.Fatalf("Changed() error = %v, want ExecutableNotFoundError", err)
	}
}

func TestRepoRoot(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testGit, true)
	mock.ExpectCommandWithArgs(testGit, "rev-parse", "--show-toplevel").
		WillSucceed("/repo\n", 0).Build()
	got, err := RepoRoot(context.Background(), mock, "/repo/sub")
	if err != nil {
		t.Fatalf("RepoRoot() error = %v", err)
	}
	if got != "/repo" {
		t.Fatalf("RepoRoot() = %q, want /repo", got)
	}
}

func TestRepoRootNotRepo(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.SetAvailableCommand(testGit, true)
	mock.ExpectCommandWithArgs(testGit, "rev-parse", "--show-toplevel").
		WillFail("fatal: not a git repository", 128).Build()
	_, err := RepoRoot(context.Background(), mock, "/tmp")
	if !errors.Is(err, ErrNotRepo) {
		t.Fatalf("RepoRoot() error = %v, want ErrNotRepo", err)
	}
}
