package main

import (
	"context"
	"encoding/json"
	"errors"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func resendEnv(t *testing.T) []string {
	t.Helper()
	return []string{
		"XDG_RUNTIME_DIR=" + shortRuntime(t),
		"XDG_STATE_HOME=" + t.TempDir(),
		"XDG_CONFIG_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
}

// dropDaemon accepts the real socket, answers ping, and stores every
// other request without replying. A retry shows up as a second request.
type dropDaemon struct {
	ln   net.Listener
	mu   sync.Mutex
	got  []daemon.Request
	ping int
}

func startDropDaemon(t *testing.T, env []string) *dropDaemon {
	t.Helper()
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Dir(dirs.SocketPath), 0o755); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", dirs.SocketPath)
	if err != nil {
		t.Fatal(err)
	}
	d := &dropDaemon{ln: ln}
	go d.loop()
	t.Cleanup(func() { _ = ln.Close() })
	return d
}

func (d *dropDaemon) loop() {
	for {
		conn, err := d.ln.Accept()
		if err != nil {
			return
		}
		d.one(conn)
	}
}

func (d *dropDaemon) one(conn net.Conn) {
	defer conn.Close()
	var req daemon.Request
	if err := json.NewDecoder(conn).Decode(&req); err != nil {
		return
	}
	if req.Op == "ping" {
		d.mu.Lock()
		d.ping++
		d.mu.Unlock()
		_ = json.NewEncoder(conn).Encode(daemon.Response{OK: true})
		return
	}
	d.mu.Lock()
	d.got = append(d.got, req)
	d.mu.Unlock()
}

func (d *dropDaemon) requests() []daemon.Request {
	d.mu.Lock()
	defer d.mu.Unlock()
	out := make([]daemon.Request, len(d.got))
	copy(out, d.got)
	return out
}

func (d *dropDaemon) pings() int {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.ping
}

// allowRetry makes the down-daemon path succeed so a blind resend is visible.
func allowRetry(t *testing.T) *int {
	t.Helper()
	orig := ensureDaemonUp
	n := 0
	ensureDaemonUp = func(context.Context, paths.Dirs, []string) error {
		n++
		return nil
	}
	t.Cleanup(func() { ensureDaemonUp = orig })
	return &n
}

func TestSendDoesNotStoreTwiceWhenTheReplyIsDropped(t *testing.T) {
	env := resendEnv(t)
	d := startDropDaemon(t, env)
	retries := allowRetry(t)
	code, stdout, stderr := runCmd([]string{"send", "ops/inbox", "hello"}, env)
	got := d.requests()
	if len(got) != 1 || *retries != 0 || d.pings() != 0 {
		t.Fatalf("stored %d, retries %d, pings %d; stdout %q stderr %q", len(got), *retries, d.pings(), stdout, stderr)
	}
	want := "agentbus: unknown whether the message was stored; check: agentbus status " + got[0].ClientID + "\n" +
		"agentbus: check: agentbus log --since 1m --name ops/inbox\n"
	if code == 0 || stderr != want || stdout != "" {
		t.Fatalf("code %d stdout %q stderr %q", code, stdout, stderr)
	}
	if got[0].Op != "send" || got[0].Body != "hello" || got[0].Name != "ops/inbox" {
		t.Fatalf("request = %+v", got[0])
	}
	if !mintedClientID(got[0].ClientID, "client") {
		t.Fatalf("client id = %q", got[0].ClientID)
	}
}

func TestReplyDoesNotStoreTwiceWhenTheReplyIsDropped(t *testing.T) {
	env := resendEnv(t)
	d := startDropDaemon(t, env)
	retries := allowRetry(t)
	code, stdout, stderr := runCmd([]string{"reply", "m-7", "yes"}, env)
	got := d.requests()
	if *retries != 0 || stdout != "" {
		t.Fatalf("retries %d stdout %q stderr %q", *retries, stdout, stderr)
	}
	if len(got) != 2 || got[0].Op != "show" || got[1].Op != "reply" || got[1].Body != "yes" || !mintedClientID(got[1].ClientID, "client") {
		t.Fatalf("request = %+v", got)
	}
	want := "agentbus: unknown whether the message was stored; check: agentbus status " + got[1].ClientID + "\n"
	if code == 0 || stderr != want {
		t.Fatalf("code %d stderr %q", code, stderr)
	}
}

func TestWriteErrorDoesNotResend(t *testing.T) {
	env := resendEnv(t)
	retries := allowRetry(t)
	orig := dialDaemon
	dials := 0
	var clientID string
	dialDaemon = func(_ context.Context, _ string, req daemon.Request) (daemon.Response, error) {
		dials++
		clientID = req.ClientID
		return daemon.Response{}, client.MarkSent(errors.New("write request: broken pipe"))
	}
	t.Cleanup(func() { dialDaemon = orig })
	code, _, stderr := runCmd([]string{"send", "ops/inbox", "hello"}, env)
	if dials != 1 || *retries != 0 || code == 0 {
		t.Fatalf("dials %d retries %d code %d stderr %q", dials, *retries, code, stderr)
	}
	if !strings.Contains(stderr, "unknown whether the message was stored; check: agentbus status "+clientID) ||
		!strings.Contains(stderr, "agentbus log --since 1m --name ops/inbox") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestRegisterDoesNotResendAfterTheRequestWasWritten(t *testing.T) {
	env := resendEnv(t)
	d := startDropDaemon(t, env)
	retries := allowRetry(t)
	code, stdout, stderr := runCmd([]string{"register", "ops/inbox", "--when", "reviews"}, env)
	got := d.requests()
	if len(got) != 1 || *retries != 0 || d.pings() != 0 {
		t.Fatalf("stored %d, retries %d, pings %d; stdout %q stderr %q", len(got), *retries, d.pings(), stdout, stderr)
	}
	if code == 0 || !strings.Contains(stderr, "read response") || strings.Contains(stderr, "unknown whether the message was stored") {
		t.Fatalf("code %d stderr %q", code, stderr)
	}
	if got[0].Op != "register" || got[0].Name != "ops/inbox" {
		t.Fatalf("request = %+v", got[0])
	}
}

func TestSendRedialsWhenTheDaemonIsNotListening(t *testing.T) {
	env := resendEnv(t)
	cfg := filepath.Join(envGet(env, "XDG_CONFIG_HOME"), "agentbus")
	if err := os.MkdirAll(cfg, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(cfg, "config.toml"), []byte("machine = \"linux2\"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	var mu sync.Mutex
	var ids []string
	n := 0
	orig := dialDaemon
	dialDaemon = func(_ context.Context, socket string, req daemon.Request) (daemon.Response, error) {
		mu.Lock()
		n++
		call := n
		ids = append(ids, req.ClientID)
		mu.Unlock()
		if call == 1 {
			return daemon.Response{}, errors.New("connect to " + socket + ": connection refused")
		}
		return daemon.Response{OK: true, Name: "m-9", State: "queued"}, nil
	}
	t.Cleanup(func() { dialDaemon = orig })
	retries := allowRetry(t)
	code, stdout, stderr := runCmd([]string{"send", "ops/inbox", "hello"}, env)
	if code != 0 || stdout != "m-9 queued\n" || stderr != "" {
		t.Fatalf("code %d stdout %q stderr %q", code, stdout, stderr)
	}
	if *retries != 1 || len(ids) != 2 || ids[0] == "" || ids[0] != ids[1] || !mintedClientID(ids[0], "linux2") {
		t.Fatalf("retries %d ids %q", *retries, ids)
	}
}

func TestSendKeepsAnExplicitClientIDOnRedial(t *testing.T) {
	env := resendEnv(t)
	const id = "mac:01ARZ3NDEKTSV4RRFFQ69G5FAV"
	var ids []string
	n := 0
	orig := dialDaemon
	dialDaemon = func(_ context.Context, _ string, req daemon.Request) (daemon.Response, error) {
		n++
		ids = append(ids, req.ClientID)
		if n == 1 {
			return daemon.Response{}, errors.New("connect to /sock: connection refused")
		}
		return daemon.Response{OK: true, Name: "m-2", State: "delivered"}, nil
	}
	t.Cleanup(func() { dialDaemon = orig })
	retries := allowRetry(t)
	code, stdout, stderr := runCmd([]string{"send", "--client-id", id, "ops/inbox", "hello"}, env)
	if code != 0 || stdout != "m-2 delivered\n" || *retries != 1 {
		t.Fatalf("code %d stdout %q stderr %q retries %d", code, stdout, stderr, *retries)
	}
	if len(ids) != 2 || ids[0] != id || ids[1] != id {
		t.Fatalf("ids = %q", ids)
	}
}

func mintedClientID(id, machine string) bool {
	if mailbox.ValidClientID(id) != nil || !strings.HasPrefix(id, machine+":") {
		return false
	}
	rest := strings.TrimPrefix(id, machine+":")
	return rest != "" && !strings.Contains(rest, ":")
}
