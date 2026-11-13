package main

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"
	"time"

	"golang.org/x/sys/unix"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

const herdrSock = "/run/herdr/herdr.sock"

// sandboxHub is startHub with a herdr pane, this machine's label, and a
// fake herdr whose local server knows that pane.
func sandboxHub(t *testing.T) (*hubSetup, *herdrtest.Fake) {
	t.Helper()
	h := startHub(t, `hub = "me@hub"`, `machine = "linux2"`)
	fake := herdrtest.Install(t)
	fake.Write("local.agents", agentsJSON("w1:p1=idle"))
	h.env = append(h.env, "HERDR_PANE_ID=w1:p1", "HERDR_SOCKET_PATH="+herdrSock)
	return h, fake
}

func TestBlockedHerdrSocketNamesTheSocketAndTheHint(t *testing.T) {
	h, fake := sandboxHub(t)
	fake.Write("local.eperm", "")
	code, _, stderr := h.run(t, "", "send", "ops/x", "hi")
	if code != 1 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	want := "identify this pane: connect " + herdrSock + ": operation not permitted (sandbox? allow this unix socket)"
	if !strings.Contains(stderr, want) {
		t.Fatalf("stderr = %q, want %q", stderr, want)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("a failed identify still ran ssh")
	}
}

func TestRegisterCachesThePaneHandleForSandboxedCalls(t *testing.T) {
	h, fake := sandboxHub(t)
	if code, _, stderr := h.run(t, "", "register", "eng/x", "--when", "testing"); code != 0 {
		t.Fatalf("register = %d, stderr %q", code, stderr)
	}
	info, err := os.Stat(filepath.Join(h.runtime, "agentbus", "panes", "w1:p1"))
	if err != nil {
		t.Fatalf("no cached handle: %v", err)
	}
	if mode := info.Mode().Perm(); mode != 0o600 {
		t.Fatalf("cache mode = %o, want 600", mode)
	}

	fake.Write("local.eperm", "")
	fake.Argv()
	if code, _, stderr := h.run(t, "", "send", "ops/x", "hi"); code != 0 {
		t.Fatalf("sandboxed send = %d, stderr %q", code, stderr)
	}
	if got := h.ssh.Read("agentbus.handle"); got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("caller = %q", got)
	}
	if argv := fake.Argv(); len(argv) != 0 {
		t.Fatalf("a cached handle still asked herdr: %q", argv)
	}
}

func TestRegisterRefreshesAStaleCache(t *testing.T) {
	h, fake := sandboxHub(t)
	dir := filepath.Join(h.runtime, "agentbus", "panes")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "w1:p1"), []byte("herdr:local/old/w1:p1\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if code, _, stderr := h.run(t, "", "register", "eng/x", "--when", "testing"); code != 0 {
		t.Fatalf("register = %d, stderr %q", code, stderr)
	}
	if got := h.ssh.Read("agentbus.handle"); got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("register caller = %q", got)
	}
	// Sandboxed, register keeps the cached handle.
	fake.Write("local.eperm", "")
	if code, _, stderr := h.run(t, "", "register", "eng/x", "--when", "testing"); code != 0 {
		t.Fatalf("sandboxed register = %d, stderr %q", code, stderr)
	}
}

func TestSandboxedTunnelFailureDoesNotFallBackToSSH(t *testing.T) {
	s := newTunnelSetup(t)
	if err := os.MkdirAll(filepath.Dir(s.tunnel), 0o755); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", s.tunnel)
	if err != nil {
		t.Fatal(err)
	}
	ln.(*net.UnixListener).SetUnlinkOnClose(false)
	ln.Close()
	env := append(slices.Clone(s.env), "SANDBOX_RUNTIME=1")
	code, _, stderr := s.run(t, env, "", "send", "ops/x", "hi")
	if code != 1 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	if s.usedSSH() {
		t.Fatalf("a sandboxed call fell back to ssh: %q", s.ssh.Lines("argv"))
	}
	for _, want := range []string{"reach the hub over the tunnel", s.tunnel, "not falling back to ssh"} {
		if !strings.Contains(stderr, want) {
			t.Errorf("stderr = %q, want %q", stderr, want)
		}
	}
	// A hook still exits 0.
	if code, _, _ := s.run(t, env, `{}`, "hook"); code != 0 || s.usedSSH() {
		t.Fatalf("hook = %d, ssh %v", code, s.usedSSH())
	}
}

func TestSandboxedSSHFailureSaysWhatToAllow(t *testing.T) {
	h, _ := sandboxHub(t)
	h.ssh.Write("down", "")
	h.ssh.Unreachable()
	env := append(slices.Clone(h.env), "SANDBOX_RUNTIME=1")
	code, _, stderr := h.runEnv(t, env, "", "list")
	if code != 1 {
		t.Fatalf("list = %d, stderr %q", code, stderr)
	}
	for _, want := range []string{"hub me@hub unreachable", "sandbox?", "~/.ssh", "tunnel"} {
		if !strings.Contains(stderr, want) {
			t.Errorf("stderr = %q, want %q", stderr, want)
		}
	}
}

func TestExplainBlocked(t *testing.T) {
	eperm := fmt.Errorf("connect to /s: %w", &net.OpError{Op: "dial", Net: "unix", Err: os.NewSyscallError("connect", unix.EPERM)})
	if got := explainBlocked(eperm, "/s").Error(); got != "connect /s: operation not permitted (sandbox? allow this unix socket)" {
		t.Fatalf("explainBlocked(EPERM) = %q", got)
	}
	text := errors.New("exit 1: Error: Operation not permitted (os error 1)")
	if got := explainBlocked(text, "/h"); !strings.Contains(got.Error(), "connect /h: operation not permitted") {
		t.Fatalf("explainBlocked(text) = %q", got)
	}
	other := errors.New("exit 1: no such pane")
	if got := explainBlocked(other, "/h"); got != other { //nolint:errorlint // the same error comes back
		t.Fatalf("explainBlocked(other) = %v", got)
	}
}

func TestDoctorSandboxListsTheSocketsAndChecksThem(t *testing.T) {
	h, _ := sandboxHub(t)
	runtime := shortRuntime(t)
	env := slices.DeleteFunc(slices.Clone(h.env), func(kv string) bool {
		return strings.HasPrefix(kv, "XDG_RUNTIME_DIR=")
	})
	env = append(env, "XDG_RUNTIME_DIR="+runtime)
	daemonSock := filepath.Join(runtime, "agentbus", "daemon.sock")
	tunnelSock := filepath.Join(runtime, "agentbus", "tunnel.sock")
	if err := os.MkdirAll(filepath.Dir(daemonSock), 0o700); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", daemonSock)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	orig := doctorProbe
	doctorProbe = func(string) remotecall.Probe { return func(context.Context) bool { return false } }
	t.Cleanup(func() { doctorProbe = orig })

	code, stdout, stderr := h.runEnv(t, env, "", "doctor", "--sandbox")
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("doctor ran ssh")
	}
	for _, want := range []string{
		`"allowUnixSockets"`,
		fmt.Sprintf("%q", herdrSock),
		fmt.Sprintf("%q", tunnelSock),
		`"allowedDomains"`,
		`"hub"`,
		"herdr socket " + herdrSock + ": ",
		"tunnel socket " + tunnelSock + ": missing",
		"ssh me@hub: blocked",
		"outbox flusher: missing",
		"outbox: no pending entries",
	} {
		if !strings.Contains(stdout, want) {
			t.Errorf("doctor output lacks %q:\n%s", want, stdout)
		}
	}
	if strings.Contains(stdout, daemonSock) || strings.Contains(stdout, "daemon socket") {
		t.Fatalf("hub mode checked the local daemon socket:\n%s", stdout)
	}
}

func TestDoctorHubModeNeedsNoLocalDaemon(t *testing.T) {
	h, _ := sandboxHub(t)
	runtimeDir := shortRuntime(t)
	env := slices.DeleteFunc(slices.Clone(h.env), func(kv string) bool {
		return strings.HasPrefix(kv, "XDG_RUNTIME_DIR=")
	})
	env = append(env, "XDG_RUNTIME_DIR="+runtimeDir)
	tunnelSock := filepath.Join(runtimeDir, "agentbus", "tunnel.sock")
	if err := os.MkdirAll(filepath.Dir(tunnelSock), 0o700); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", tunnelSock)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	home := envValue(t, env, "HOME")
	unit, err := unitPath(runtime.GOOS, home, env, outboxLaunchdLabel, outboxUnitName)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Dir(unit), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(unit, []byte("outbox\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	orig := doctorProbe
	doctorProbe = func(string) remotecall.Probe { return func(context.Context) bool { return true } }
	t.Cleanup(func() { doctorProbe = orig })

	code, stdout, stderr := h.runEnv(t, env, "", "doctor")
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("doctor ran ssh")
	}
	for _, want := range []string{
		"tunnel socket " + tunnelSock + ": ok",
		"ssh me@hub: ok (its ssh port answers)",
		"outbox flusher: present (" + unit + ")",
		"outbox: no pending entries",
	} {
		if !strings.Contains(stdout, want) {
			t.Errorf("doctor output lacks %q:\n%s", want, stdout)
		}
	}
	if strings.Contains(stdout, "daemon socket") {
		t.Fatalf("hub mode checked the local daemon socket:\n%s", stdout)
	}
}

func TestDoctorSandboxPrintsTheDarwinRuntime(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("darwin default runtime directory")
	}
	h, _ := sandboxHub(t)
	home := shortRuntime(t)
	env := slices.DeleteFunc(slices.Clone(h.env), func(kv string) bool {
		return strings.HasPrefix(kv, "XDG_RUNTIME_DIR=") || strings.HasPrefix(kv, "HOME=")
	})
	env = append(env, "HOME="+home)
	code, stdout, stderr := h.runEnv(t, env, "", "doctor", "--sandbox")
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	tunnel := filepath.Join(home, "Library", "Caches", "agentbus", "run", "agentbus", "tunnel.sock")
	if !strings.Contains(stdout, tunnel) || strings.Contains(stdout, "$XDG_RUNTIME_DIR") {
		t.Fatalf("stdout = %s", stdout)
	}
}

func envValue(t *testing.T, env []string, key string) string {
	t.Helper()
	prefix := key + "="
	for _, kv := range env {
		if value, ok := strings.CutPrefix(kv, prefix); ok {
			return value
		}
	}
	t.Fatalf("%s is unset", key)
	return ""
}

func TestDoctorListsAnExtraDaemon(t *testing.T) {
	h, _ := sandboxHub(t)
	start := time.Date(2026, 11, 12, 21, 4, 5, 0, time.UTC)
	holder := 10
	origList, origLock := listDoctorProcs, daemonLockHolder
	t.Cleanup(func() { listDoctorProcs, daemonLockHolder = origList, origLock })
	listDoctorProcs = func(context.Context) ([]doctorProc, error) {
		return []doctorProc{
			{PID: holder, UID: os.Getuid(), Start: start, Name: "agentbusd"},
			{PID: 4242, UID: os.Getuid(), Start: start.Add(time.Minute), Name: "agentbusd"},
			{PID: 7, UID: os.Getuid(), Start: start, Name: "launchd"},
			{PID: 8, UID: os.Getuid() + 1, Start: start, Name: "agentbusd"},
		}, nil
	}
	daemonLockHolder = func(string) (int, bool, error) { return holder, true, nil }

	code, stdout, stderr := h.run(t, "", "doctor")
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	for _, want := range []string{
		"other than the lock holder (pid 10)",
		"pid 4242 started " + start.Add(time.Minute).Format(time.RFC3339),
		"Stop them with: kill 4242",
	} {
		if !strings.Contains(stdout, want) {
			t.Errorf("doctor output lacks %q:\n%s", want, stdout)
		}
	}
	if strings.Contains(stdout, "pid 10 started") || strings.Contains(stdout, "pid 7") || strings.Contains(stdout, "pid 8") {
		t.Fatalf("listed the holder, another program, or another user:\n%s", stdout)
	}
}

func TestDoctorReportsAnUnverifiedHold(t *testing.T) {
	h, _ := sandboxHub(t)
	var stateHome string
	for _, kv := range h.env {
		if value, ok := strings.CutPrefix(kv, "XDG_STATE_HOME="); ok {
			stateHome = value
		}
	}
	dir := filepath.Join(stateHome, "agentbus")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	body := `{"messages":[{"id":"m-9","from":"a","to":"ops/x","sentAt":"2026-01-01T00:00:00Z","body":"hi","state":"held: ambiguous-unverified","handle":"herdr:local/default/w1:p1"},{"id":"m-8","from":"a","to":"ops/y","sentAt":"2026-01-01T00:00:00Z","body":"ok","state":"delivered","handle":"h"}]}`
	if err := os.WriteFile(filepath.Join(dir, "messages.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	code, stdout, stderr := h.run(t, "", "doctor")
	if code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	if !strings.Contains(stdout, "ambiguous-unverified: m-9 to ops/x\n") || strings.Contains(stdout, "m-8") {
		t.Fatalf("stdout = %s", stdout)
	}
}

func TestDoctorRejectsArguments(t *testing.T) {
	h, _ := sandboxHub(t)
	if code, _, stderr := h.run(t, "", "doctor", "extra"); code != 2 || !strings.Contains(stderr, "--sandbox") {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
}

func TestParseDoctorPS(t *testing.T) {
	now := time.Date(2026, 11, 12, 0, 2, 0, 0, time.UTC)
	got := parseDoctorPS("  5 501 01:00 agentbusd\n  x\n", now)
	if len(got) != 1 || got[0].PID != 5 || got[0].UID != 501 || got[0].Name != "agentbusd" {
		t.Fatalf("procs = %+v", got)
	}
	if !got[0].Start.Equal(now.Add(-time.Minute)) {
		t.Fatalf("start = %s", got[0].Start)
	}
}
