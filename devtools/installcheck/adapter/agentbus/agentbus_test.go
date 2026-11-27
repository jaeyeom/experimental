package agentbus

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"slices"
	"strconv"
	"testing"

	"github.com/jaeyeom/experimental/devtools/installcheck"
)

func TestHelperProcess(t *testing.T) {
	if os.Getenv("GO_WANT_HELPER_PROCESS") != "1" {
		return
	}
	code, _ := strconv.Atoi(os.Getenv("HELPER_EXIT_CODE"))
	os.Exit(code)
}

func exitErrorWithCode(t *testing.T, code int) error {
	t.Helper()
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(exe, "-test.run=^TestHelperProcess$") //nolint:gosec // G204: self-invocation for ExitError test
	cmd.Env = append(os.Environ(),
		"GO_WANT_HELPER_PROCESS=1",
		"HELPER_EXIT_CODE="+strconv.Itoa(code),
	)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("run helper: %w", err)
	}
	return nil
}

func TestNotifier(t *testing.T) {
	t.Run("sends agentbus message", func(t *testing.T) {
		var gotName string
		var gotArgs []string
		n := Notifier{
			Recipient: "installer",
			Run: func(_ context.Context, name string, args ...string) ([]byte, error) {
				gotName = name
				gotArgs = args
				return nil, nil
			},
		}
		if err := n.Notify(context.Background(), installcheck.Notification{
			Kind: installcheck.NotifyLanded,
			Body: "hello",
		}); err != nil {
			t.Fatalf("Notify() = %v", err)
		}
		if gotName != "agentbus" {
			t.Fatalf("cmd = %q, want agentbus", gotName)
		}
		want := []string{"send", "installer", "hello"}
		if !slices.Equal(gotArgs, want) {
			t.Fatalf("args = %v, want %v", gotArgs, want)
		}
	})

	t.Run("exit code 3 treated as sent", func(t *testing.T) {
		exit3 := exitErrorWithCode(t, 3)
		n := Notifier{
			Recipient: "installer",
			Run: func(_ context.Context, _ string, _ ...string) ([]byte, error) {
				return nil, exit3
			},
		}
		if err := n.Notify(context.Background(), installcheck.Notification{Body: "held"}); err != nil {
			t.Fatalf("Notify() with exit 3 = %v, want nil", err)
		}
	})

	t.Run("non-3 exit error fails", func(t *testing.T) {
		exit1 := exitErrorWithCode(t, 1)
		n := Notifier{
			Recipient: "installer",
			Run: func(_ context.Context, _ string, _ ...string) ([]byte, error) {
				return nil, exit1
			},
		}
		err := n.Notify(context.Background(), installcheck.Notification{Body: "fail"})
		if err == nil {
			t.Fatal("expected error for exit 1")
		}
	})

	t.Run("generic error fails", func(t *testing.T) {
		n := Notifier{
			Recipient: "installer",
			Run: func(_ context.Context, _ string, _ ...string) ([]byte, error) {
				return nil, errors.New("bus down")
			},
		}
		if err := n.Notify(context.Background(), installcheck.Notification{Body: "fail"}); err == nil {
			t.Fatal("expected error")
		}
	})

	t.Run("empty recipient rejected", func(t *testing.T) {
		n := Notifier{Recipient: "  "}
		if err := n.Notify(context.Background(), installcheck.Notification{Body: "hi"}); err == nil {
			t.Fatal("expected error for empty recipient")
		}
	})
}
