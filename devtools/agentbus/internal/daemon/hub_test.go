package daemon

import (
	"context"
	"encoding/json"
	"net"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func call(t *testing.T, socketPath string, req Request) Response {
	t.Helper()
	conn, err := net.Dial("unix", socketPath)
	if err != nil {
		t.Fatalf("dial %s: %v", socketPath, err)
	}
	defer conn.Close()
	if err := json.NewEncoder(conn).Encode(req); err != nil {
		t.Fatal(err)
	}
	var resp Response
	if err := json.NewDecoder(conn).Decode(&resp); err != nil {
		t.Fatal(err)
	}
	return resp
}

func hubDirs(t *testing.T) paths.Dirs {
	t.Helper()
	dir := shortDir(t)
	return paths.Dirs{
		SocketPath:    filepath.Join(dir, "daemon.sock"),
		HubSocketPath: filepath.Join(dir, "hub.sock"),
		StateDir:      filepath.Join(t.TempDir(), "state"),
	}
}

func TestHubSocketNeedsProtocolAndARemoteCallerHandle(t *testing.T) {
	dirs := hubDirs(t)
	stop := startWith(t, dirs, Config{HubCaller: herdr.RemoteCaller})
	defer stop()

	const remote = "herdr:linux2/default/w1:p1"
	for _, tt := range []struct {
		name string
		req  Request
		want string
	}{
		{"old client", Request{Op: "ping", Handle: remote}, "hub daemon speaks protocol 1, this client speaks 0"},
		{"newer client", Request{Op: "ping", Protocol: 2, Handle: remote}, "hub daemon speaks protocol 1, this client speaks 2"},
		{"no handle", Request{Op: "ping", Protocol: ProtocolVersion}, "must carry the caller handle"},
		{"local handle", Request{Op: "ping", Protocol: ProtocolVersion, Handle: "herdr:local/default/w1:p1"}, "must name a remote machine"},
		{"not herdr", Request{Op: "ping", Protocol: ProtocolVersion, Handle: "fake:x"}, "not a herdr handle"},
	} {
		resp := call(t, dirs.HubSocketPath, tt.req)
		if resp.OK || !strings.Contains(resp.Error, tt.want) {
			t.Errorf("%s: response %+v, want error %q", tt.name, resp, tt.want)
		}
		if resp.Protocol != ProtocolVersion {
			t.Errorf("%s: protocol = %d", tt.name, resp.Protocol)
		}
	}
	if resp := call(t, dirs.HubSocketPath, Request{Op: "ping", Protocol: ProtocolVersion, Handle: remote}); !resp.OK {
		t.Fatalf("forwarded ping = %+v", resp)
	}
	// The local socket keeps today's rules: no protocol or handle needed.
	if resp := call(t, dirs.SocketPath, Request{Op: "ping"}); !resp.OK {
		t.Fatalf("local ping = %+v", resp)
	}
}

func TestNoHubSocketWithoutHubCaller(t *testing.T) {
	dirs := hubDirs(t)
	stop := start(t, dirs)
	defer stop()
	var d net.Dialer
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	if conn, err := d.DialContext(ctx, "unix", dirs.HubSocketPath); err == nil {
		conn.Close()
		t.Fatal("a daemon without HubCaller listens on the hub socket")
	}
}
