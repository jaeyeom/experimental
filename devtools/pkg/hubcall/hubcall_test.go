package hubcall_test

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall/sshtest"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

func TestQuote(t *testing.T) {
	for in, want := range map[string]string{
		"send":           "send",
		"ops/x":          "ops/x",
		"--file=-":       "--file=-",
		"a b; c":         "'a b; c'",
		"":               "''",
		"it's":           `'it'\''s'`,
		"$HOME":          "'$HOME'",
		"herdr:m/s/w1:2": "herdr:m/s/w1:2",
	} {
		if got := hubcall.Quote(in); got != want {
			t.Errorf("Quote(%q) = %q, want %q", in, got, want)
		}
	}
}

type out struct {
	stdout, stderr strings.Builder
}

func run(t *testing.T, hub hubcall.Hub, c hubcall.Call) (int, *out, error) {
	t.Helper()
	var o out
	c.Stdout = &o.stdout
	c.Stderr = &o.stderr
	code, err := hub.Run(context.Background(), c)
	return code, &o, err //nolint:wrapcheck // tests inspect Run's own error
}

func TestRunForwardsArgsStdinAndResult(t *testing.T) {
	fake := sshtest.Install(t)
	fake.InstallTool("tool")
	fake.Write("tool.stdout", "remote out\n")
	fake.Write("tool.stderr", "remote err\n")
	fake.Write("tool.code", "3")
	hub := hubcall.Hub{Target: "me@hub", Tool: "tool", ConnectTimeout: 7 * time.Second}
	code, o, err := run(t, hub, hubcall.Call{
		Args:  []string{"send", "ops/x", "a b; c"},
		Env:   []string{"AGENTBUS_AS_HANDLE=herdr:linux2/s/w1:p1"},
		Stdin: strings.NewReader("body\n"),
	})
	if err != nil || code != 3 {
		t.Fatalf("Run = %d, %v; stderr %q", code, err, o.stderr.String())
	}
	if o.stdout.String() != "remote out\n" || o.stderr.String() != "remote err\n" {
		t.Fatalf("stdout %q, stderr %q", o.stdout.String(), o.stderr.String())
	}
	if got := fake.Lines("tool.args"); !slices.Equal(got, []string{"send", "ops/x", "a b; c"}) {
		t.Fatalf("remote args = %q", got)
	}
	if got := fake.Read("tool.stdin"); got != "body\n" {
		t.Fatalf("remote stdin = %q", got)
	}
	if got := fake.Read("tool.handle"); got != "herdr:linux2/s/w1:p1" {
		t.Fatalf("remote handle = %q", got)
	}
	argv := fake.Lines("argv")
	for _, want := range []string{"BatchMode=yes", "ConnectTimeout=7", "me@hub"} {
		if !slices.Contains(argv, want) {
			t.Fatalf("ssh argv %q lacks %q", argv, want)
		}
	}
	if slices.ContainsFunc(argv, func(arg string) bool { return strings.Contains(arg, "IdentityAgent") }) {
		t.Fatalf("ssh argv %q sets IdentityAgent", argv)
	}
	if last := argv[len(argv)-1]; !strings.HasSuffix(last, "tool send ops/x 'a b; c'") {
		t.Fatalf("remote command = %q", last)
	}
}

func TestRunSetsIdentityAgent(t *testing.T) {
	fake := sshtest.Install(t)
	fake.InstallTool("tool")
	hub := hubcall.Hub{Target: "me@hub", Tool: "tool", ConnectTimeout: 5 * time.Second, IdentityAgent: "/tmp/agent.sock"}
	code, _, err := run(t, hub, hubcall.Call{Args: []string{"list"}})
	if err != nil || code != 0 {
		t.Fatalf("Run = %d, %v", code, err)
	}
	argv := fake.Lines("argv")
	const opt = "IdentityAgent=/tmp/agent.sock"
	if slices.Index(argv, opt) < 0 || slices.Index(argv, opt) > slices.Index(argv, "--") {
		t.Fatalf("ssh argv = %q", argv)
	}
}

func TestRunFailsClearlyWithoutSSH(t *testing.T) {
	t.Setenv("PATH", t.TempDir())
	_, _, err := run(t, hubcall.Hub{Target: "hub", Tool: "tool"}, hubcall.Call{})
	if !errors.Is(err, hubcall.ErrNoSSH) {
		t.Fatalf("err = %v", err)
	}
}

func wantUnreachable(t *testing.T, err error, cause string) {
	t.Helper()
	var ue *hubcall.UnreachableError
	if !errors.As(err, &ue) || ue.Cause != cause {
		t.Fatalf("err = %v, want cause %q", err, cause)
	}
	if want := "hub hub unreachable: " + cause; err.Error() != want {
		t.Fatalf("err = %q, want %q", err.Error(), want)
	}
}

func TestRunHangIsKilledAtTheDeadline(t *testing.T) {
	for _, tt := range []struct {
		name      string
		reachable bool
		cause     string
	}{
		{"probe succeeds", true, remotecall.CauseAuthLocked},
		{"probe fails", false, remotecall.CauseHostUnreachable},
	} {
		t.Run(tt.name, func(t *testing.T) {
			fake := sshtest.Install(t)
			fake.Write("hang", "")
			if tt.reachable {
				fake.Reachable()
			} else {
				fake.Unreachable()
			}
			start := time.Now()
			_, _, err := run(t, hubcall.Hub{Target: "hub", Tool: "tool", ConnectTimeout: time.Second, Deadline: 300 * time.Millisecond}, hubcall.Call{})
			if took := time.Since(start); took > 5*time.Second {
				t.Fatalf("Run took %s", took)
			}
			wantUnreachable(t, err, tt.cause)
		})
	}
}

func TestRunClassifiesSSHFailures(t *testing.T) {
	for _, tt := range []struct {
		file, cause string
	}{
		{"locked", remotecall.CauseAuthLocked},
		{"down", remotecall.CauseHostUnreachable},
	} {
		t.Run(tt.file, func(t *testing.T) {
			fake := sshtest.Install(t)
			fake.Write(tt.file, "")
			fake.Unreachable()
			_, _, err := run(t, hubcall.Hub{Target: "hub", Tool: "tool", ConnectTimeout: time.Second}, hubcall.Call{})
			wantUnreachable(t, err, tt.cause)
		})
	}
}

func TestRunRemoteExit255IsNotUnreachable(t *testing.T) {
	fake := sshtest.Install(t)
	fake.InstallTool("tool")
	fake.Write("tool.code", "255")
	code, _, err := run(t, hubcall.Hub{Target: "hub", Tool: "tool"}, hubcall.Call{})
	if err != nil || code != 255 {
		t.Fatalf("Run = %d, %v", code, err)
	}
}
