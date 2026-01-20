package ghauth

import (
	"context"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/executor"
)

func TestGHCLITokenSource(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("gh", "auth", "token", "--hostname", "github.com").
		WillSucceed("ghp_test_token_12345\n", 0).Build()

	source := NewGHCLITokenSource(mock, "")
	token, err := source.Token(context.Background())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Token should be trimmed
	if token != "ghp_test_token_12345" { //nolint:gosec // This is a test token, not a real credential
		t.Errorf("expected 'ghp_test_token_12345', got %q", token)
	}
}

func TestGHCLITokenSourceCustomHostname(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("gh", "auth", "token", "--hostname", "github.example.com").
		WillSucceed("ghp_enterprise_token\n", 0).Build()

	source := NewGHCLITokenSource(mock, "github.example.com")
	token, err := source.Token(context.Background())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if token != "ghp_enterprise_token" {
		t.Errorf("expected 'ghp_enterprise_token', got %q", token)
	}

	if source.Hostname() != "github.example.com" {
		t.Errorf("expected 'github.example.com', got %q", source.Hostname())
	}
}

func TestGHCLITokenSourceFailure(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.ExpectCommand("gh").WillFail("not logged in", 1).Build()

	source := NewGHCLITokenSource(mock, "")
	_, err := source.Token(context.Background())
	if err == nil {
		t.Fatal("expected error when gh auth token fails")
	}
}

func TestGHCLITokenSourceEmptyToken(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("gh", "auth", "token", "--hostname", "github.com").
		WillSucceed("", 0).Build()

	source := NewGHCLITokenSource(mock, "")
	_, err := source.Token(context.Background())
	if err == nil {
		t.Fatal("expected error for empty token")
	}
}

func TestIsGHCLIAvailable(t *testing.T) {
	t.Run("available and authenticated", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetAvailableCommand("gh", true)
		mock.ExpectCommandWithArgs("gh", "auth", "token", "--hostname", "github.com").
			WillSucceed("ghp_token", 0).Build()

		if !IsGHCLIAvailable(context.Background(), mock) {
			t.Error("expected gh CLI to be available")
		}
	})

	t.Run("not installed", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetAvailableCommand("gh", false)

		if IsGHCLIAvailable(context.Background(), mock) {
			t.Error("expected gh CLI to not be available")
		}
	})

	t.Run("installed but not authenticated", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.SetAvailableCommand("gh", true)
		mock.SetDefaultBehavior(&executor.ExecutionResult{
			ExitCode:  1,
			Stderr:    "not logged in",
			StartTime: time.Now(),
			EndTime:   time.Now(),
		}, nil)

		if IsGHCLIAvailable(context.Background(), mock) {
			t.Error("expected gh CLI to not be available when not authenticated")
		}
	})
}
