package agentbus

import (
	"context"
	"errors"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestWake(t *testing.T) {
	tests := []struct {
		name     string
		machine  string
		wantArg  string
		output   string
		wantWoke bool
	}{
		{name: "not hibernated", machine: "", wantArg: "local", output: "", wantWoke: false},
		{name: "woke", machine: "", wantArg: "local", output: "woke w1:p1\n", wantWoke: true},
		{name: "remote machine", machine: "mac", wantArg: "mac", output: "woke w1:p1\n", wantWoke: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mock := leaseMock(t)
			mock.ExpectCommandWithArgs("agentbus", "wake", "--if-hibernated", "--wait", "--machine", tc.wantArg, "w1:p1").
				WillSucceed(tc.output, 0).Once().Build()
			woke, err := NewClient(mock, "agentbus").Wake(context.Background(), tc.machine, "w1:p1")
			if err != nil {
				t.Fatal(err)
			}
			if woke != tc.wantWoke {
				t.Fatalf("woke = %v, want %v", woke, tc.wantWoke)
			}
			if err := mock.AssertExpectationsMet(); err != nil {
				t.Fatal(err)
			}
			if got := mock.Executions()[0].Timeout; got != wakeTimeout {
				t.Fatalf("timeout = %s, want %s", got, wakeTimeout)
			}
		})
	}
}

func TestWakeFailed(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommand("agentbus").WillFail("agentbus wake: w1:p1: relaunch timed out\n", 1).Once().Build()
	_, err := NewClient(mock, "agentbus").Wake(context.Background(), "", "w1:p1")
	if err == nil || errors.Is(err, ErrMissing) {
		t.Fatalf("err = %v", err)
	}
	if !strings.Contains(err.Error(), "agentbus wake: exit 1: agentbus wake: w1:p1: relaunch timed out") {
		t.Fatalf("err = %v", err)
	}
}

func TestWakeMissingBinary(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommand("agentbus").WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Once().Build()
	_, err := NewClient(mock, "agentbus").Wake(context.Background(), "", "w1:p1")
	if !errors.Is(err, ErrMissing) {
		t.Fatalf("err = %v, want ErrMissing", err)
	}
}

func TestWakeRejectsEmptyPane(t *testing.T) {
	if _, err := NewClient(leaseMock(t), "agentbus").Wake(context.Background(), "", ""); err == nil {
		t.Fatal("empty pane: expected an error")
	}
}
