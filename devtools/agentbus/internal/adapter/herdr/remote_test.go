package herdr

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// installFakeHerdr puts a fake herdr first on PATH. local and m2 both
// list one idle claude agent in pane w2:p3 with an empty composer.
func installFakeHerdr(t *testing.T) *herdrtest.Fake {
	t.Helper()
	f := herdrtest.Install(t)
	for _, m := range []string{"local", "m2"} {
		f.Write(m+".agents", agentList)
		f.Write(m+".screen", claudeScreen("❯ "))
	}
	return f
}

func TestRemoteCallsUseMachine(t *testing.T) {
	f := installFakeHerdr(t)
	ad := &Adapter{Timeout: 10 * time.Second}
	ctx := context.Background()

	if _, err := ad.Deliver(ctx, "herdr:m2/default/w2:p3", "hello"); err != nil {
		t.Fatal(err)
	}
	if got := f.Argv(); len(got) != 1 || got[0] != "--machine m2 agent prompt w2:p3 hello" {
		t.Fatalf("remote deliver argv = %q", got)
	}
	if _, err := ad.Deliver(ctx, "herdr:local/default/w2:p3", "hello"); err != nil {
		t.Fatal(err)
	}
	if got := f.Argv(); len(got) != 1 || got[0] != "agent prompt w2:p3 hello" {
		t.Fatalf("local deliver argv = %q", got)
	}

	state, err := ad.State(ctx, "herdr:m2/default/w2:p3")
	if err != nil || state != adapter.StateIdle {
		t.Fatalf("remote state = %q, %v", state, err)
	}
	if got := f.Argv(); len(got) != 1 || got[0] != "--machine m2 agent list" {
		t.Fatalf("remote state argv = %q", got)
	}
	gone, err := ad.State(ctx, "herdr:m2/default/w9:p9")
	if err != nil || gone != adapter.StateGone {
		t.Fatalf("missing remote pane = %q, %v", gone, err)
	}
	f.Argv()

	empty, _, err := ad.ComposerEmpty(ctx, "herdr:m2/default/w2:p3")
	if err != nil || !empty {
		t.Fatalf("remote composer = %v, %v", empty, err)
	}
	want := []string{"--machine m2 agent list", "--machine m2 agent read w2:p3 --source visible --format ansi"}
	if got := f.Argv(); strings.Join(got, "|") != strings.Join(want, "|") {
		t.Fatalf("remote composer argv = %q, want %q", got, want)
	}
}

func TestUnreachableMachineCauses(t *testing.T) {
	tests := []struct {
		name  string
		files []string
		cause string
	}{
		{"connection refused", []string{"m2.down"}, remotecall.CauseHostUnreachable},
		{"publickey", []string{"m2.locked"}, remotecall.CauseAuthLocked},
		{"hang, host answers", []string{"m2.hang"}, remotecall.CauseAuthLocked},
		{"hang, host down", []string{"m2.hang", "m2.status-down"}, remotecall.CauseHostUnreachable},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			f := installFakeHerdr(t)
			for _, name := range tt.files {
				f.Write(name, "")
			}
			ad := &Adapter{
				Timeout:      300 * time.Millisecond,
				AgentSockets: func(context.Context) []string { return nil },
			}
			start := time.Now()
			_, err := ad.State(context.Background(), "herdr:m2/default/w2:p3")
			if took := time.Since(start); took > 5*time.Second {
				t.Fatalf("State took %s", took)
			}
			ue := remotecall.AsUnreachable(err)
			if ue == nil || ue.Cause != tt.cause || ue.Machine != "m2" {
				t.Fatalf("State error = %v, want cause %q", err, tt.cause)
			}
			if _, err := ad.Deliver(context.Background(), "herdr:m2/default/w2:p3", "hi"); remotecall.AsUnreachable(err) == nil {
				t.Fatalf("Deliver error = %v, want unreachable", err)
			}
		})
	}
}

func TestReachableMachineErrorIsHandledAsToday(t *testing.T) {
	f := installFakeHerdr(t)
	f.Write("m2.prompt-error", "")
	ad := &Adapter{Timeout: 10 * time.Second}
	result, err := ad.Deliver(context.Background(), "herdr:m2/default/w2:p3", "hi")
	if err != nil || result != adapter.ResultFailed {
		t.Fatalf("Deliver = %q, %v; want failed, nil", result, err)
	}
}

func TestInventoryAddsConfiguredMachines(t *testing.T) {
	f := installFakeHerdr(t)
	ad := &Adapter{Machines: []string{"m2"}, Timeout: 10 * time.Second}
	agents, err := ad.Inventory(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(agents) != 2 || agents[0].Handle != "herdr:local/default/w2:p3" || agents[1].Handle != "herdr:m2/default/w2:p3" || agents[1].TabLabel != "m2-tab" {
		t.Fatalf("inventory = %+v", agents)
	}

	f.Write("m2.down", "")
	agents, err = ad.Inventory(context.Background())
	var warn *adapter.InventoryWarning
	if !errors.As(err, &warn) || len(warn.Lines) != 1 || warn.Lines[0] != "machine m2 unreachable: host unreachable" {
		t.Fatalf("inventory error = %v", err)
	}
	if len(agents) != 1 || agents[0].Handle != "herdr:local/default/w2:p3" {
		t.Fatalf("inventory with m2 down = %+v", agents)
	}
}

func listenFakeAgent(t *testing.T, path string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = ln.Close() })
}

// installFakeSSHHerdr puts a fake herdr on PATH that records each
// invocation's SSH_AUTH_SOCK and fails with Permission denied unless
// SSH_AUTH_SOCK equals goodSock.
func installFakeSSHHerdr(t *testing.T, goodSock string) string {
	t.Helper()
	dir := t.TempDir()
	logFile := filepath.Join(dir, "socks")
	script := fmt.Sprintf(`#!/bin/sh
printf '%%s\n' "${SSH_AUTH_SOCK:-}" >> %q
if [ "$1 $2" = "--machine m2" ] && [ "$3 $4" = "machine status" ]; then
  echo '[{"id":"id-m2","label":"m2","status":"reachable","error":null}]'
  exit 0
fi
if [ "$1 $2" = "machine status" ]; then
  echo '[{"id":"id-m2","label":"m2","status":"reachable","error":null}]'
  exit 0
fi
if [ "${SSH_AUTH_SOCK:-}" != %q ]; then
  echo "user@m2: Permission denied (publickey)." >&2
  exit 255
fi
case "$3 $4" in
"agent list") echo %q ;;
*) echo '{"result":{}}' ;;
esac
`, logFile, goodSock, agentList)
	bin := filepath.Join(dir, "herdr")
	if err := os.WriteFile(bin, []byte(script), 0o755); err != nil { //nolint:gosec // test executable
		t.Fatal(err)
	}
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	return logFile
}

func TestSSHAgentConfiguredWinsOverInherited(t *testing.T) {
	dir, err := os.MkdirTemp("", "ab-ssh-*")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })

	configured := filepath.Join(dir, "cfg.sock")
	inherited := filepath.Join(dir, "inh.sock")
	listenFakeAgent(t, configured)
	listenFakeAgent(t, inherited)

	logFile := installFakeSSHHerdr(t, configured)
	ad := &Adapter{
		Timeout:        2 * time.Second,
		SSHAgentSocket: configured,
		Env:            []string{"PATH=" + os.Getenv("PATH"), "HOME=" + filepath.Join(dir, "empty-home"), "SSH_AUTH_SOCK=" + inherited},
	}
	state, err := ad.State(context.Background(), "herdr:m2/default/w2:p3")
	if err != nil || state != adapter.StateIdle {
		t.Fatalf("State = %q, %v; want idle, nil", state, err)
	}
	raw, err := os.ReadFile(logFile)
	if err != nil {
		t.Fatal(err)
	}
	if got := strings.TrimSpace(string(raw)); got != configured {
		t.Fatalf("herdr saw SSH_AUTH_SOCK=%q, want %q", got, configured)
	}
}

func TestSSHAgentFallbackAndRetryOnceOnAuthLocked(t *testing.T) {
	dir, err := os.MkdirTemp("", "ab-ssh-*")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })

	// First socket (~/.ssh/agent.sock) is live but locked; second (inherited) works.
	home := filepath.Join(dir, "home")
	lockedSock := filepath.Join(home, ".ssh", "agent.sock")
	workingSock := filepath.Join(dir, "work.sock")
	listenFakeAgent(t, lockedSock)
	listenFakeAgent(t, workingSock)

	logFile := installFakeSSHHerdr(t, workingSock)
	var logs []string
	ad := &Adapter{
		Timeout: 2 * time.Second,
		Env:     []string{"PATH=" + os.Getenv("PATH"), "HOME=" + home, "SSH_AUTH_SOCK=" + workingSock},
		Log: func(format string, args ...any) {
			logs = append(logs, fmt.Sprintf(format, args...))
		},
	}
	state, err := ad.State(context.Background(), "herdr:m2/default/w2:p3")
	if err != nil || state != adapter.StateIdle {
		t.Fatalf("State = %q, %v; want idle, nil", state, err)
	}
	raw, err := os.ReadFile(logFile)
	if err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimSpace(string(raw)), "\n")
	if len(lines) != 2 || lines[0] != lockedSock || lines[1] != workingSock {
		t.Fatalf("herdr SSH_AUTH_SOCK sequence = %v, want [%s %s]", lines, lockedSock, workingSock)
	}
	if len(logs) != 1 || !strings.Contains(logs[0], workingSock) {
		t.Fatalf("logs = %v, want mention of %s", logs, workingSock)
	}
}

func TestSSHAgentAuthLockedNamesSocketPath(t *testing.T) {
	dir, err := os.MkdirTemp("", "ab-ssh-*")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })

	lockedSock := filepath.Join(dir, "gcr.sock")
	listenFakeAgent(t, lockedSock)
	installFakeSSHHerdr(t, filepath.Join(dir, "other.sock"))

	ad := &Adapter{
		Timeout:      2 * time.Second,
		AgentSockets: func(context.Context) []string { return []string{lockedSock} },
	}
	_, err = ad.State(context.Background(), "herdr:m2/default/w2:p3")
	ue := remotecall.AsUnreachable(err)
	want := remotecall.AuthLockedCause(lockedSock)
	if ue == nil || ue.Cause != want {
		t.Fatalf("err = %v, want cause %q", err, want)
	}
}

func TestSSHAgentHostUnreachableDoesNotRetry(t *testing.T) {
	f := installFakeHerdr(t)
	f.Write("m2.down", "")
	ad := &Adapter{
		Timeout:      2 * time.Second,
		AgentSockets: func(context.Context) []string { return []string{"/sock/a", "/sock/b"} },
	}
	_, err := ad.State(context.Background(), "herdr:m2/default/w2:p3")
	ue := remotecall.AsUnreachable(err)
	if ue == nil || ue.Cause != remotecall.CauseHostUnreachable {
		t.Fatalf("err = %v, want CauseHostUnreachable", err)
	}
	// Only 1 agent list call, no retry on /sock/b.
	if got := f.Argv(); len(got) != 1 {
		t.Fatalf("argv count = %d (%v), want 1 (no retry on host unreachable)", len(got), got)
	}
}
