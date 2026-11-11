package daemon

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
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
		{"old client", Request{Op: "ping", Handle: remote}, fmt.Sprintf("hub daemon speaks protocol %d, this client speaks 0", ProtocolVersion)},
		{"newer client", Request{Op: "ping", Protocol: ProtocolVersion + 1, Handle: remote}, fmt.Sprintf("hub daemon speaks protocol %d, this client speaks %d", ProtocolVersion, ProtocolVersion+1)},
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

// A caller on another machine may change leases only on its own
// machine's checkouts. Read-only ops still see every machine, and local
// callers keep today's rules.
func TestHubRefusesARemoteCallersLeaseOnAnotherMachine(t *testing.T) {
	herdrtest.Install(t).Write("local.agents", `{"result":{"agents":[]}}`)
	dirs := hubDirs(t)
	ad := &herdr.Adapter{Machines: []string{"linux2", "laptop"}, Timeout: time.Second}
	stop := startWith(t, dirs, Config{Adapters: []adapter.Adapter{ad}, HubCaller: herdr.RemoteCaller})
	defer stop()

	const (
		linux2 = "herdr:linux2/default/w1:p1"
		laptop = "herdr:laptop/default/w1:p1"
	)
	hub := func(handle, op string, args lease.Args) Response {
		t.Helper()
		return call(t, dirs.HubSocketPath, Request{Op: op, Protocol: ProtocolVersion, Handle: handle, Lease: &args})
	}
	if resp := hub(laptop, "lease-claim", lease.Args{Checkout: "laptop:/x", For: "40m"}); !resp.OK || resp.Lease.Result != lease.ResultGranted {
		t.Fatalf("laptop's claim of its own checkout = %+v", resp)
	}
	for _, op := range []string{
		"lease-claim", "lease-extend", "lease-leave",
		"lease-release-begin", "lease-release-abort", "lease-release",
		"lease-adopt-begin", "lease-adopt",
		"lease-pause", "lease-unshare", "lease-resume-begin", "lease-resume-abort", "lease-resume",
	} {
		resp := hub(linux2, op, lease.Args{Checkout: "laptop:/x", For: "40m"})
		if resp.OK || !strings.Contains(resp.Error, "laptop:/x") || !strings.Contains(resp.Error, "machine linux2") {
			t.Errorf("%s of laptop:/x by linux2 = %+v, want a machine refusal", op, resp)
		}
	}
	if resp := hub(linux2, "lease-claim", lease.Args{Checkout: "linux2:/x", For: "40m"}); !resp.OK || resp.Lease.Result != lease.ResultGranted {
		t.Fatalf("linux2's claim of its own checkout = %+v", resp)
	}
	// check only answers for the caller, so it is not refused.
	if resp := hub(linux2, "lease-check", lease.Args{Checkout: "laptop:/x"}); !resp.OK || resp.Lease.Result != lease.ResultDenied {
		t.Fatalf("check of laptop:/x by linux2 = %+v", resp)
	}
	if resp := hub(linux2, "lease-status", lease.Args{Checkout: "laptop:/x"}); !resp.OK || resp.Lease.Leases[0].Holder.Handle != laptop {
		t.Fatalf("status of laptop:/x by linux2 = %+v", resp)
	}
	resp := hub(linux2, "lease-status", lease.Args{All: true})
	if !resp.OK || len(resp.Lease.Leases) != 2 || resp.Lease.Leases[0].Checkout != "laptop:/x" || resp.Lease.Leases[1].Checkout != "linux2:/x" {
		t.Fatalf("status --all by linux2 = %+v", resp)
	}
	// The user on the hub keeps today's rules.
	if resp := call(t, dirs.SocketPath, Request{Op: "lease-pause", Lease: &lease.Args{Checkout: "laptop:/x"}}); !resp.OK {
		t.Fatalf("the hub user's pause of laptop:/x = %+v", resp)
	}
}
