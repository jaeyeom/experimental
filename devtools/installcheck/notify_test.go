package installcheck

import (
	"bytes"
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
)

func TestStdoutNotifier(t *testing.T) {
	t.Run("writes body with trailing newline", func(t *testing.T) {
		var buf bytes.Buffer
		n := StdoutNotifier{Out: &buf}
		if err := n.Notify(context.Background(), Notification{Kind: NotifyLanded, Body: "hello"}); err != nil {
			t.Fatalf("Notify() = %v", err)
		}
		if got := buf.String(); got != "hello\n" {
			t.Fatalf("got %q, want %q", got, "hello\n")
		}
	})

	t.Run("preserves existing trailing newline", func(t *testing.T) {
		var buf bytes.Buffer
		n := StdoutNotifier{Out: &buf}
		if err := n.Notify(context.Background(), Notification{Body: "hello\n"}); err != nil {
			t.Fatalf("Notify() = %v", err)
		}
		if got := buf.String(); got != "hello\n" {
			t.Fatalf("got %q, want %q", got, "hello\n")
		}
	})

	t.Run("nil writer fails", func(t *testing.T) {
		n := StdoutNotifier{}
		if err := n.Notify(context.Background(), Notification{Body: "hello"}); err == nil {
			t.Fatal("expected error for nil writer")
		}
	})
}

func TestCommandNotifier(t *testing.T) {
	t.Run("positional fallback and env vars", func(t *testing.T) {
		var gotArgv, gotEnv []string
		n := CommandNotifier{
			Template:  []string{"~/bin/notify.sh", "--flag"},
			Recipient: "ops",
			BaseEnv:   []string{"HOME=/home/u", "PATH=/usr/bin"},
			Run: func(_ context.Context, argv, env []string) error {
				gotArgv = argv
				gotEnv = env
				return nil
			},
		}
		err := n.Notify(context.Background(), Notification{Kind: NotifyLanded, Body: "landed #1"})
		if err != nil {
			t.Fatalf("Notify() = %v", err)
		}
		wantArgv := []string{"/home/u/bin/notify.sh", "--flag", "landed #1"}
		if !slices.Equal(gotArgv, wantArgv) {
			t.Fatalf("argv = %v, want %v", gotArgv, wantArgv)
		}
		for _, wantEnv := range []string{
			"HOME=/home/u",
			"INSTALL_CHECK_KIND=landed",
			"INSTALL_CHECK_RECIPIENT=ops",
			"INSTALL_CHECK_MESSAGE=landed #1",
		} {
			if !slices.Contains(gotEnv, wantEnv) {
				t.Errorf("env %v missing %q", gotEnv, wantEnv)
			}
		}
	})

	t.Run("placeholder substitution", func(t *testing.T) {
		var gotArgv []string
		n := CommandNotifier{
			Template:  []string{"send-hook", "--kind={kind}", "--to", "{recipient}", "{message}"},
			Recipient: "installer",
			BaseEnv:   []string{"PATH=/usr/bin"},
			Run: func(_ context.Context, argv, _ []string) error {
				gotArgv = argv
				return nil
			},
		}
		err := n.Notify(context.Background(), Notification{Kind: NotifyFailure, Body: "boom"})
		if err != nil {
			t.Fatalf("Notify() = %v", err)
		}
		want := []string{"send-hook", "--kind=failure", "--to", "installer", "boom"}
		if !slices.Equal(gotArgv, want) {
			t.Fatalf("argv = %v, want %v", gotArgv, want)
		}
	})

	t.Run("empty template fails", func(t *testing.T) {
		n := CommandNotifier{}
		if err := n.Notify(context.Background(), Notification{Body: "x"}); err == nil {
			t.Fatal("expected error for empty template")
		}
	})

	t.Run("runner error wrapped", func(t *testing.T) {
		n := CommandNotifier{
			Template: []string{"my-notify"},
			Run: func(_ context.Context, _, _ []string) error {
				return errors.New("exit status 1")
			},
		}
		err := n.Notify(context.Background(), Notification{Body: "x"})
		if err == nil || !strings.Contains(err.Error(), "my-notify") {
			t.Fatalf("err = %v, want wrapped command error", err)
		}
	})
}
