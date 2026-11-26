package agentbus

import (
	"context"
	"errors"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestWhoami(t *testing.T) {
	for out, want := range map[string]string{
		"pm/coord\nalias\n":    "pm/coord",
		"herdr:mac/s1/w6:p0\n": "herdr:mac/s1/w6:p0",
	} {
		mock := leaseMock(t)
		mock.ExpectCommandWithArgs("agentbus", "whoami").WillSucceed(out, 0).Once().Build()
		got, err := NewClient(mock, "agentbus").Whoami(context.Background())
		if err != nil || got != want {
			t.Fatalf("Whoami() = %q, %v; want %q", got, err, want)
		}
	}
}

func TestWhoamiRejects(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommand("agentbus").WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Once().Build()
	if _, err := NewClient(mock, "agentbus").Whoami(context.Background()); !errors.Is(err, ErrMissing) {
		t.Fatalf("missing: err = %v", err)
	}
	for _, out := range []string{"", "\n  \n", "has space\n"} {
		mock := leaseMock(t)
		mock.ExpectCommand("agentbus").WillSucceed(out, 0).Once().Build()
		if _, err := NewClient(mock, "agentbus").Whoami(context.Background()); err == nil {
			t.Fatalf("output %q: expected an error", out)
		}
	}
}
