package main

import (
	"encoding/json"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall/sshtest"
)

// tunnelSetup is a second machine, linux2, in hub mode, with a fake ssh
// and a fake herdr. Its pane is w1:p1. The tunnel socket is where a
// forwarded hub socket would appear on linux2.
type tunnelSetup struct {
	ssh    *sshtest.Fake
	herdr  *herdrtest.Fake
	env    []string
	tunnel string
}

func newTunnelSetup(t *testing.T) *tunnelSetup {
	t.Helper()
	sshFake := sshtest.Install(t)
	sshFake.InstallTool("agentbus")
	herdrFake := herdrtest.Install(t)
	herdrFake.Write("local.agents", agentsJSON("w1:p1=idle", "w1:p2=idle"))
	herdrFake.Write("linux2.agents", agentsJSON("w1:p1=idle"))
	xdg := t.TempDir()
	dir := filepath.Join(xdg, "agentbus")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "config.toml"), []byte("hub = \"me@hub\"\nmachine = \"linux2\"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	runtime := shortRuntime(t)
	return &tunnelSetup{
		ssh:    sshFake,
		herdr:  herdrFake,
		tunnel: filepath.Join(runtime, "agentbus", "tunnel.sock"),
		env: []string{
			"HOME=" + t.TempDir(),
			"XDG_CONFIG_HOME=" + xdg,
			"XDG_RUNTIME_DIR=" + runtime,
			"XDG_STATE_HOME=" + t.TempDir(),
			"HERDR_PANE_ID=w1:p1",
		},
	}
}

// startHubDaemon runs the hub's daemon with its forwarded socket at the
// tunnel path, as an ssh -L forward would expose it. It returns the
// hub's own environment.
func (s *tunnelSetup) startHubDaemon(t *testing.T) ([]string, paths.Dirs) {
	t.Helper()
	base := []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	dirs.HubSocketPath = s.tunnel
	ad := &herdr.Adapter{Machines: []string{"linux2"}, Timeout: 5 * time.Second}
	stop := startDaemon(t, dirs, daemon.Config{
		Adapters:     []adapter.Adapter{ad},
		PollInterval: time.Hour,
		HubCaller:    herdr.RemoteCaller,
	})
	t.Cleanup(stop)
	return base, dirs
}

func (s *tunnelSetup) run(t *testing.T, env []string, stdin string, args ...string) (int, string, string) {
	t.Helper()
	return runCmdIn(args, env, strings.NewReader(stdin))
}

func (s *tunnelSetup) usedSSH() bool {
	return s.ssh.Read("argv") != ""
}

func TestTunnelSendCarriesTheCallerAndRepliesGoBackOverHerdr(t *testing.T) {
	s := newTunnelSetup(t)
	hubEnv, _ := s.startHubDaemon(t)

	if code, _, stderr := s.run(t, s.env, "", "register", "eng/remote", "--when", "runs on linux2"); code != 0 {
		t.Fatalf("register = %d, stderr %q", code, stderr)
	}
	out, _ := mustCmd(t, []string{"list", "--json"}, hubEnv)
	if !strings.Contains(out, `"handle":"herdr:linux2/default/w1:p1"`) {
		t.Fatalf("hub list = %s", out)
	}

	code, out, stderr := s.run(t, s.env, "question\n", "send", "herdr:local/default/w1:p2")
	id, state, _ := strings.Cut(strings.TrimSpace(out), " ")
	if code != 0 || state != "delivered" {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, out, stderr)
	}
	if s.usedSSH() {
		t.Fatalf("tunnelled send ran ssh: %q", s.ssh.Lines("argv"))
	}
	shown, _ := mustCmd(t, []string{"show", id}, hubEnv)
	if !strings.Contains(shown, "from: eng/remote\n") {
		t.Fatalf("show = %q", shown)
	}

	s.herdr.Argv()
	hubPane := append(append([]string{}, hubEnv...), "HERDR_PANE_ID=w1:p2")
	mustCmd(t, []string{"reply", id, "answer"}, hubPane)
	if !textsContain(s.herdr.Argv(), "--machine linux2 agent prompt w1:p1 ") {
		t.Fatal("the reply did not go back through herdr --machine linux2")
	}
}

func TestTunnelHookRecordsOnTheHub(t *testing.T) {
	s := newTunnelSetup(t)
	_, dirs := s.startHubDaemon(t)
	code, _, _ := s.run(t, s.env, `{"session_id":"s1","tool_input":{"to":"x","message":"hi"}}`, "hook")
	if code != 0 || s.usedSSH() {
		t.Fatalf("hook = %d, ssh %q", code, s.ssh.Lines("argv"))
	}
	if !logHas(t, dirs, eventlog.EventBypass, `"actor":"s1"`) {
		t.Fatal("the hub did not record the bypass")
	}
}

func TestTunnelMissingOrDeadUsesSSH(t *testing.T) {
	s := newTunnelSetup(t)
	if code, _, stderr := s.run(t, s.env, "", "list"); code != 0 {
		t.Fatalf("list = %d, stderr %q", code, stderr)
	}
	if got := s.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"list"}) {
		t.Fatalf("without a tunnel, hub args = %q", got)
	}

	// A tunnel that dropped leaves a socket file nobody listens on.
	if err := os.MkdirAll(filepath.Dir(s.tunnel), 0o755); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", s.tunnel)
	if err != nil {
		t.Fatal(err)
	}
	ln.(*net.UnixListener).SetUnlinkOnClose(false)
	ln.Close()
	start := time.Now()
	if code, _, stderr := s.run(t, s.env, "", "send", "ops/x", "hi"); code != 0 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	if took := time.Since(start); took > 2*time.Second {
		t.Fatalf("a dead tunnel took %s to fall back", took)
	}
	if got := s.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"send", "ops/x", "hi"}) {
		t.Fatalf("with a dead tunnel, hub args = %q", got)
	}
	if got := s.ssh.Read("agentbus.handle"); got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("ssh caller = %q", got)
	}
}

func TestTunnelProtocolMismatchFallsBackToSSH(t *testing.T) {
	s := newTunnelSetup(t)
	if err := os.MkdirAll(filepath.Dir(s.tunnel), 0o755); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", s.tunnel)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	got := make(chan daemon.Request, 4)
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			var req daemon.Request
			_ = json.NewDecoder(conn).Decode(&req)
			got <- req
			_ = json.NewEncoder(conn).Encode(daemon.Response{Error: "hub daemon speaks protocol 9, this client speaks 1", Protocol: 9})
			conn.Close()
		}
	}()
	code, _, stderr := s.run(t, s.env, "", "list")
	if code != 0 {
		t.Fatalf("list = %d, stderr %q", code, stderr)
	}
	if want := "agentbus: hub daemon speaks protocol 9, this client speaks 1\n"; stderr != want {
		t.Fatalf("stderr = %q, want %q", stderr, want)
	}
	if args := s.ssh.Lines("agentbus.args"); !slices.Equal(args, []string{"list"}) {
		t.Fatalf("hub args = %q", args)
	}
	req := <-got
	if req.Protocol != daemon.ProtocolVersion || req.Handle != "herdr:linux2/default/w1:p1" || len(req.Env) != 0 {
		t.Fatalf("tunnel request = %+v", req)
	}
}

func TestTunnelOnlyCarriesPaneCallsAndDaemonCommands(t *testing.T) {
	s := newTunnelSetup(t)
	s.startHubDaemon(t)
	// Outside a herdr pane there is no caller handle for the hub socket.
	noPane := slices.DeleteFunc(slices.Clone(s.env), func(kv string) bool { return strings.HasPrefix(kv, "HERDR_PANE_ID=") })
	if code, _, stderr := s.run(t, noPane, "", "list"); code != 0 {
		t.Fatalf("list = %d, stderr %q", code, stderr)
	}
	if got := s.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"list"}) {
		t.Fatalf("list outside a pane: hub args = %q", got)
	}
	// log reads the hub's event log file, which the tunnel does not carry.
	if code, _, stderr := s.run(t, s.env, "", "log"); code != 0 {
		t.Fatalf("log = %d, stderr %q", code, stderr)
	}
	if got := s.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"log"}) {
		t.Fatalf("log: hub args = %q", got)
	}
}
