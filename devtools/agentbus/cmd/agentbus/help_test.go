package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

var helpEnv = []string{"XDG_RUNTIME_DIR=/nonexistent", "HOME=/nonexistent"}

// noDaemon fails the test if anything dials or starts the daemon.
func noDaemon(t *testing.T) {
	t.Helper()
	oldDial, oldEnsure := dialDaemon, ensureDaemon
	t.Cleanup(func() { dialDaemon, ensureDaemon = oldDial, oldEnsure })
	dialDaemon = func(context.Context, string, daemon.Request) (daemon.Response, error) {
		t.Fatal("help dialed the daemon")
		return daemon.Response{}, nil
	}
	ensureDaemon = func([]string, io.Writer) int {
		t.Fatal("help started the daemon")
		return 1
	}
}

func runHelp(args ...string) (int, string, string) {
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), args, helpEnv, strings.NewReader(""), &stdout, &stderr)
	return code, stdout.String(), stderr.String()
}

func TestTopLevelHelpDocumentsRemoteMachines(t *testing.T) {
	noDaemon(t)
	_, stdout, _ := runHelp("--help")
	for _, want := range []string{"herdr:<machine>/<session>/<pane>", "machines = [", "remote_timeout", "authentication locked (unlock your SSH/gpg agent)"} {
		if !strings.Contains(stdout, want) {
			t.Errorf("help is missing %q", want)
		}
	}
}

func TestTopLevelHelpListsEveryCommand(t *testing.T) {
	noDaemon(t)
	for _, arg := range []string{"--help", "-h", "help"} {
		code, stdout, stderr := runHelp(arg)
		if code != 0 || stderr != "" {
			t.Fatalf("%s: code %d, stderr %q", arg, code, stderr)
		}
		if !strings.Contains(stdout, "Usage:") || !strings.Contains(stdout, "--help") {
			t.Fatalf("%s: stdout = %q", arg, stdout)
		}
		for _, cmd := range commands() {
			if !strings.Contains(stdout, "  "+cmd.name+" ") {
				t.Errorf("%s: help is missing %q", arg, cmd.name)
			}
		}
	}
}

func TestCommandHelpDoesNotContactTheDaemon(t *testing.T) {
	noDaemon(t)
	for _, cmd := range commands() {
		for _, args := range [][]string{{cmd.name, "--help"}, {cmd.name, "-h"}, {"help", cmd.name}} {
			code, stdout, stderr := runHelp(args...)
			if code != 0 || stderr != "" {
				t.Fatalf("%v: code %d, stderr %q", args, code, stderr)
			}
			for _, want := range []string{"Usage: agentbus " + cmd.name, "Example"} {
				if !strings.Contains(stdout, want) {
					t.Errorf("%v: stdout is missing %q: %q", args, want, stdout)
				}
			}
		}
	}
}

func TestCommandHelpListsFlagsWithDefaults(t *testing.T) {
	noDaemon(t)
	for _, tt := range []struct {
		cmd   string
		flags []string
	}{
		{"register", []string{"--when"}},
		{"describe", []string{"--when"}},
		{"release", []string{"--to"}},
		{"list", []string{"--json", "--fresh", "name", "when", "liveness", "liveness_age", "transport", "handle", "alive", "dead", "unknown"}},
		{"send", []string{"--file", "--ttl", "--wait", "--client-id", "--json"}},
		{"reply", []string{"--file", "stdin", "--client-id", "--json", "--"}},
		{"log", []string{"--since", "--name", "--msg", "--event", "--follow", "--json"}},
	} {
		_, stdout, _ := runHelp(tt.cmd, "--help")
		if !strings.Contains(stdout, "Flags:") || !strings.Contains(stdout, "default") {
			t.Errorf("%s: no flag defaults: %q", tt.cmd, stdout)
		}
		for _, flag := range tt.flags {
			if !strings.Contains(stdout, flag) {
				t.Errorf("%s: help is missing %s", tt.cmd, flag)
			}
		}
	}
}

func TestHelpLaterInTheBodyIsSent(t *testing.T) {
	old := dialDaemon
	t.Cleanup(func() { dialDaemon = old })
	var got daemon.Request
	dialDaemon = func(_ context.Context, _ string, req daemon.Request) (daemon.Response, error) {
		got = req
		return daemon.Response{OK: true, Name: "m-2", State: "queued"}, nil
	}
	for _, tt := range []struct {
		args     []string
		op, body string
	}{
		{[]string{"send", "ops/inbox", "see", "--help"}, "send", "see --help"},
		{[]string{"reply", "m-1", "see", "-h"}, "reply", "see -h"},
	} {
		got = daemon.Request{}
		code, stdout, stderr := runHelp(tt.args...)
		if code != 0 || strings.Contains(stdout, "Usage:") {
			t.Fatalf("%v: code %d, stdout %q, stderr %q", tt.args, code, stdout, stderr)
		}
		if got.Op != tt.op || got.Body != tt.body {
			t.Fatalf("%v: daemon got %+v", tt.args, got)
		}
	}
}

func TestEveryCommandHasHelpText(t *testing.T) {
	seen := map[string]bool{}
	for _, cmd := range commands() {
		if seen[cmd.name] {
			t.Errorf("%s registered twice", cmd.name)
		}
		seen[cmd.name] = true
		if cmd.summary == "" || cmd.usage == "" || cmd.example == "" || cmd.run == nil {
			t.Errorf("%s: incomplete entry: %+v", cmd.name, cmd)
		}
		if !strings.HasPrefix(cmd.usage, "agentbus "+cmd.name) {
			t.Errorf("%s: usage %q does not start with the command", cmd.name, cmd.usage)
		}
	}
}

func TestNoArgumentsPrintsUsageAndDaemonStatus(t *testing.T) {
	old := ensureDaemon
	t.Cleanup(func() { ensureDaemon = old })
	ensureDaemon = func(_ []string, stderr io.Writer) int {
		fmt.Fprintln(stderr, "agentbusd ready at /fake.sock")
		return 0
	}
	code, stdout, stderr := runHelp()
	if code != 0 {
		t.Fatalf("code %d, stderr %q", code, stderr)
	}
	if !strings.Contains(stdout, "Usage:") || !strings.Contains(stdout, "  send ") {
		t.Fatalf("stdout = %q", stdout)
	}
	if !strings.Contains(stderr, "agentbusd ready at /fake.sock") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestUnknownCommandHints(t *testing.T) {
	noDaemon(t)
	for _, args := range [][]string{{"bogus"}, {"help", "bogus"}} {
		code, _, stderr := runHelp(args...)
		if code != 2 {
			t.Fatalf("%v: code %d", args, code)
		}
		if !strings.Contains(stderr, `unknown command "bogus"`) || !strings.Contains(stderr, "run 'agentbus --help'") {
			t.Fatalf("%v: stderr = %q", args, stderr)
		}
	}
}
