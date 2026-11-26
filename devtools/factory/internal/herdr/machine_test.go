package herdr

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const machineTabs = `{"id":"cli:tab:list","result":{"type":"tab_list","tabs":[]}}`

func TestOnMachinePrefixesEveryCall(t *testing.T) {
	t.Parallel()
	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "--machine", "mac1", "tab", "list").WillSucceed(machineTabs, 0).Once().Build()
	mock.ExpectCommandWithArgs(testHerdrBin, "--machine", "mac1", "agent", "prompt", "w1:p1", "go", "--wait", "--until", "working", "--timeout", "30000").
		WillSucceed(`{"id":"x","result":{"type":"agent_prompted","agent":{"pane_id":"w1:p1","agent_status":"working"}}}`, 0).Once().Build()
	base := NewClient(mock, testHerdrBin)
	client := base.OnMachine("mac1")
	if client.Machine() != "mac1" || base.Machine() != "" {
		t.Fatalf("machines = %q, %q", client.Machine(), base.Machine())
	}
	if _, err := client.Tabs(context.Background()); err != nil {
		t.Fatalf("Tabs() error = %v", err)
	}
	assertCallTimeout(t, mock, RemoteReadTimeout)
	if err := client.Prompt(context.Background(), "w1:p1", "go", []string{"working"}, 30*time.Second); err != nil {
		t.Fatalf("Prompt() error = %v", err)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	calls := mock.Executions()
	if calls[1].Timeout != 30*time.Second+promptTimeoutGrace {
		t.Fatalf("prompt timeout = %s", calls[1].Timeout)
	}
}

func TestOnMachineEmptyIsLocal(t *testing.T) {
	t.Parallel()
	mock := newHerdrMock()
	mock.ExpectCommandWithArgs(testHerdrBin, "tab", "list").WillSucceed(machineTabs, 0).Once().Build()
	client := NewClient(mock, testHerdrBin).OnMachine("mac1").OnMachine("")
	if _, err := client.Tabs(context.Background()); err != nil {
		t.Fatalf("Tabs() error = %v", err)
	}
	assertCallTimeout(t, mock, defaultCallTimeout)
}

func TestUnreachable(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "nil", err: nil},
		{name: "timeout", err: &RunError{Bin: "herdr", Err: &executor.TimeoutError{Command: "herdr", Timeout: time.Second}}, want: true},
		{name: "non-zero exit", err: &ProcError{ExitCode: 1, Stderr: "connect failed"}, want: true},
		{name: "wrapped exit", err: errors.Join(errors.New("lane"), &ProcError{ExitCode: 255}), want: true},
		{name: "not installed", err: ErrNotInstalled},
		{name: "decode error", err: ErrDegraded},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := Unreachable(tc.err); got != tc.want {
				t.Fatalf("Unreachable(%v) = %t, want %t", tc.err, got, tc.want)
			}
		})
	}
}

func TestWhyUnreachable(t *testing.T) {
	t.Parallel()
	callErr := &RunError{Bin: "herdr", Err: &executor.TimeoutError{Command: "herdr", Timeout: RemoteReadTimeout}}
	tests := []struct {
		name   string
		status string
		fail   bool
		want   string
	}{
		{
			name:   "host down",
			status: `[{"id":"abc","label":"mac1","status":"error","error":"ssh: connect to host mac1\r\nport 22: Operation timed out"}]`,
			want:   "host unreachable: ssh: connect to host mac1 port 22: Operation timed out",
		},
		{
			name:   "host answers, call hangs",
			status: `[{"id":"abc","label":"mac1","status":"ok"}]`,
			want:   "authentication locked (unlock your SSH/gpg agent): machine status ok; run herdr:",
		},
		{
			name: "probe fails",
			fail: true,
			want: "host unreachable: run herdr:",
		},
		{
			name:   "probe shape",
			status: `{"oops":true}`,
			want:   "herdr machine status: expected a JSON array",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := newHerdrMock()
			exp := mock.ExpectCommandWithArgs(testHerdrBin, "machine", "status", "--json", "mac1")
			if tc.fail {
				exp.WillFail("no server", 1).Once().Build()
			} else {
				exp.WillSucceed(tc.status, 0).Once().Build()
			}
			got := NewClient(mock, testHerdrBin).OnMachine("mac1").WhyUnreachable(context.Background(), callErr)
			if !strings.Contains(got, tc.want) {
				t.Fatalf("WhyUnreachable() = %q, want %q", got, tc.want)
			}
			if strings.ContainsAny(got, "\r\n") {
				t.Fatalf("WhyUnreachable() = %q, want one line", got)
			}
			calls := mock.Executions()
			if len(calls) != 1 || slices.Contains(calls[0].Args, "--machine") {
				t.Fatalf("calls = %#v, want one local machine status", calls)
			}
		})
	}
}

func TestDecodeMachineStatus(t *testing.T) {
	t.Parallel()
	got, err := decodeMachineStatus([]byte(`[{"id":"abc","label":"mac1","status":"ok"},{"id":"def","label":"linux","status":"error"}]`), "abc")
	if err != nil || got.Label != "mac1" || got.Status != "ok" {
		t.Fatalf("decode = %#v, %v", got, err)
	}
	if _, err := decodeMachineStatus([]byte(`[]`), "mac1"); err == nil || !strings.Contains(err.Error(), `expected one entry for "mac1", found 0`) {
		t.Fatalf("decode error = %v", err)
	}
	if _, err := decodeMachineStatus([]byte(`[{"label":"mac1"}]`), "mac1"); err == nil || !strings.Contains(err.Error(), "expected a status") {
		t.Fatalf("decode error = %v", err)
	}
}
