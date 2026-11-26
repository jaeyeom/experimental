package agentbus

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const (
	guardedYes = `{"guarded": true, "checkout": "local:/src/repo"}`
	guardedNo  = `{"guarded": false}`
	freeJSON   = `{"checkout": "local:/src/repo", "state": "free", "queue": [], "takeover": "log"}`
	heldJSON   = `{"checkout": "local:/src/repo", "state": "held",
 "holder": {"handle": "herdr:local/default/w1:p2", "name": "pm/foo", "since": "2026-11-11T13:00:00Z", "deadline": "2026-11-11T14:20:00Z", "why": "PRJ-1 Fix it"},
 "queue": [{"handle": "herdr:local/default/w1:p3", "name": "", "urgent": false, "for": "40m", "since": "2026-11-11T13:10:00Z"}],
 "backup_ref": ""}`
)

func leaseMock(t *testing.T) *executor.MockExecutor {
	t.Helper()
	mock := executor.NewMockExecutor()
	mock.SetDefaultBehavior(nil, errors.New("unexpected command"))
	return mock
}

func TestReadHeld(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--guarded", "--json", "--path", "/src/repo").WillSucceed(guardedYes, 0).Once().Build()
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--path", "/src/repo", "--json").WillSucceed(heldJSON, 0).Once().Build()
	got, err := NewClient(mock, "agentbus").Read(context.Background(), "/src/repo")
	if err != nil {
		t.Fatal(err)
	}
	if !got.Guarded || got.Status.State != StateHeld || got.Status.Who() != "pm/foo" || len(got.Status.Queue) != 1 {
		t.Fatalf("reading = %#v", got)
	}
	if got.Status.Holder.Deadline != "2026-11-11T14:20:00Z" {
		t.Fatalf("deadline = %q", got.Status.Holder.Deadline)
	}
	if err := mock.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	for _, call := range mock.Executions() {
		if call.Timeout != callTimeout {
			t.Fatalf("timeout = %s, want %s", call.Timeout, callTimeout)
		}
	}
}

func TestReadUnguardedSkipsStatus(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommand("agentbus").WillSucceed(guardedNo, 0).Once().Build()
	got, err := NewClient(mock, "agentbus").Read(context.Background(), "/src/other")
	if err != nil {
		t.Fatal(err)
	}
	if got.Guarded {
		t.Fatalf("reading = %#v", got)
	}
	if n := len(mock.Executions()); n != 1 {
		t.Fatalf("executions = %d, want 1", n)
	}
}

func TestReadMissingBinary(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommand("agentbus").WillError(&executor.ExecutableNotFoundError{Command: "agentbus"}).Once().Build()
	_, err := NewClient(mock, "agentbus").Read(context.Background(), "/src/repo")
	if !errors.Is(err, ErrMissing) {
		t.Fatalf("err = %v, want ErrMissing", err)
	}
}

func TestReadHubUnreachable(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommand("agentbus").WillSucceed(guardedYes, 0).Once().Build()
	mock.ExpectCommand("agentbus").WillFail("agentbus lease status: hub mac: ssh: connect timed out\n", 1).Once().Build()
	_, err := NewClient(mock, "agentbus").Read(context.Background(), "/src/repo")
	if err == nil || errors.Is(err, ErrMissing) {
		t.Fatalf("err = %v", err)
	}
	if !strings.Contains(err.Error(), "exit 1: agentbus lease status: hub mac: ssh: connect timed out") {
		t.Fatalf("err = %v", err)
	}
}

func TestReadRejectsEmptyPathAndNilExecutor(t *testing.T) {
	if _, err := NewClient(leaseMock(t), "agentbus").Read(context.Background(), ""); err == nil {
		t.Fatal("empty path: expected an error")
	}
	if _, err := NewClient(nil, "agentbus").Read(context.Background(), "/src/repo"); err == nil {
		t.Fatal("nil executor: expected an error")
	}
}

func TestDecodeStatus(t *testing.T) {
	st, err := DecodeStatus([]byte(freeJSON))
	if err != nil {
		t.Fatal(err)
	}
	if st.State != StateFree || st.Holder != nil || len(st.Queue) != 0 {
		t.Fatalf("status = %#v", st)
	}
	paused, err := DecodeStatus([]byte(`{"checkout":"local:/r","state":"paused","holder":{"handle":"user","name":"","since":"2026-11-11T13:00:00Z","deadline":"","why":""},"queue":null}`))
	if err != nil {
		t.Fatal(err)
	}
	if paused.Who() != "user" {
		t.Fatalf("who = %q", paused.Who())
	}
	adopt, err := DecodeStatus([]byte(`{"checkout":"local:/r","state":"needs-adopt","queue":[],"backup_ref":"refs/lease-backup/x"}`))
	if err != nil {
		t.Fatal(err)
	}
	if adopt.BackupRef != "refs/lease-backup/x" {
		t.Fatalf("backup ref = %q", adopt.BackupRef)
	}
}

func TestDecodeStatusRejects(t *testing.T) {
	tests := []struct {
		name, in, want string
	}{
		{"empty", "", "expected a JSON object, found empty output"},
		{"array", `[]`, "expected a JSON object"},
		{"unknown state", `{"checkout":"local:/r","state":"maybe","queue":[]}`, `expected state free, held, paused, releasing, or needs-adopt, found "maybe"`},
		{"missing state", `{"checkout":"local:/r","queue":[]}`, `expected a "state" field, found none`},
		{"missing queue", `{"checkout":"local:/r","state":"free"}`, `expected a "queue" field, found none`},
		{"empty checkout", `{"checkout":"","state":"free","queue":[]}`, "expected a non-empty checkout, found empty"},
		{"state not a string", `{"checkout":"local:/r","state":1,"queue":[]}`, "expected the lease status shape"},
		{"queue not a list", `{"checkout":"local:/r","state":"free","queue":"none"}`, "expected the lease status shape"},
		{"free with holder", `{"checkout":"local:/r","state":"free","holder":{"handle":"h"},"queue":[]}`, `expected no holder for state free, found "h"`},
		{"held without holder", `{"checkout":"local:/r","state":"held","queue":[]}`, "expected a holder for state held, found none"},
		{"holder without handle", `{"checkout":"local:/r","state":"held","holder":{"name":"x"},"queue":[]}`, "holder: expected a non-empty handle, found empty"},
		{"bad deadline", `{"checkout":"local:/r","state":"held","holder":{"handle":"h","deadline":"14:20"},"queue":[]}`, `holder deadline: expected an RFC 3339 time, found "14:20"`},
		{"waiter without handle", `{"checkout":"local:/r","state":"free","queue":[{"name":"x"}]}`, "queue 1: expected a non-empty handle, found empty"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := DecodeStatus([]byte(tt.in))
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want it to contain %q", err, tt.want)
			}
		})
	}
}

func TestDecodeGuarded(t *testing.T) {
	for _, in := range []string{`{}`, `{"guarded":"yes"}`, `null`, `nope`} {
		if _, err := DecodeGuarded([]byte(in)); err == nil {
			t.Fatalf("%s: expected an error", in)
		}
	}
}

func TestSummary(t *testing.T) {
	held, err := DecodeStatus([]byte(heldJSON))
	if err != nil {
		t.Fatal(err)
	}
	if got := held.Summary(time.UTC); got != "held by pm/foo until 14:20 (1 queued)" {
		t.Fatalf("summary = %q", got)
	}
	free := Status{Checkout: "local:/r", State: StateFree}
	if got := free.Summary(time.UTC); got != "free" {
		t.Fatalf("summary = %q", got)
	}
	paused := Status{Checkout: "local:/r", State: StatePaused, Holder: &Holder{Handle: "user"}}
	if got := paused.Summary(time.UTC); got != "paused by user (0 queued)" {
		t.Fatalf("summary = %q", got)
	}
	adopt := Status{Checkout: "local:/r", State: StateNeedsAdopt}
	if got := adopt.Summary(time.UTC); got != "needs-adopt (no holder, 0 queued)" {
		t.Fatalf("summary = %q", got)
	}
}

func TestHeldBy(t *testing.T) {
	held, err := DecodeStatus([]byte(heldJSON))
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		owner Owner
		want  bool
	}{
		{Owner{}, false},
		{Owner{ItemID: "PRJ-1"}, true},
		{Owner{ItemID: "PRJ-10"}, false},
		{Owner{Agent: "pm/foo"}, true},
		{Owner{ItemID: "PRJ-2", Agent: "prj-2"}, false},
	}
	for _, c := range cases {
		if got := held.HeldBy(c.owner); got != c.want {
			t.Fatalf("HeldBy(%#v) = %t, want %t", c.owner, got, c.want)
		}
	}
	paused := held
	paused.State = StatePaused
	if paused.HeldBy(Owner{ItemID: "PRJ-1"}) {
		t.Fatal("a paused lease is never held by the owner")
	}
	if !slices.Equal([]string{held.Queue[0].For}, []string{"40m"}) {
		t.Fatalf("queue = %#v", held.Queue)
	}
}
