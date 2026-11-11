package daemon

import (
	"bufio"
	"encoding/json"
	"net"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// openStream sends req and returns a reader of the daemon's responses.
func openStream(t *testing.T, socketPath string, req Request) (net.Conn, *json.Decoder) {
	t.Helper()
	conn, err := net.Dial("unix", socketPath)
	if err != nil {
		t.Fatalf("dial %s: %v", socketPath, err)
	}
	t.Cleanup(func() { conn.Close() })
	if err := json.NewEncoder(conn).Encode(req); err != nil {
		t.Fatal(err)
	}
	return conn, json.NewDecoder(bufio.NewReader(conn))
}

func next(t *testing.T, conn net.Conn, dec *json.Decoder) Response {
	t.Helper()
	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	var resp Response
	if err := dec.Decode(&resp); err != nil {
		t.Fatalf("read stream: %v", err)
	}
	return resp
}

func TestLogOpReadsTheEventLogAndEnds(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	stop := start(t, dirs)
	defer stop()
	conn, dec := openStream(t, dirs.SocketPath, Request{Op: "log", Event: "start"})
	first := next(t, conn, dec)
	if !first.OK || !strings.Contains(first.Text, `"event":"start"`) {
		t.Fatalf("first = %+v", first)
	}
	if end := next(t, conn, dec); !end.OK || !end.End {
		t.Fatalf("end = %+v", end)
	}
}

func TestLogOpRejectsABadSince(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	stop := start(t, dirs)
	defer stop()
	resp := call(t, dirs.SocketPath, Request{Op: "log", Since: "yesterday"})
	if resp.OK || !strings.Contains(resp.Error, "since") {
		t.Fatalf("resp = %+v", resp)
	}
}

// A follow stream holds its connection open. Other clients are still
// served, and their events reach the stream.
func TestLogFollowStreamsWhileOtherClientsAreServed(t *testing.T) {
	dirs := hubDirs(t)
	stop := startWith(t, dirs, Config{HubCaller: herdr.RemoteCaller})
	defer stop()
	const remote = "herdr:linux2/default/w1:p1"
	conn, dec := openStream(t, dirs.HubSocketPath, Request{
		Op: "log", Follow: true, Event: "bypass", Protocol: ProtocolVersion, Handle: remote,
	})

	var wg sync.WaitGroup
	for i := range 8 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if err := ping(dirs.SocketPath); err != nil {
				t.Errorf("ping %d during a follow: %v", i, err)
			}
			call(t, dirs.HubSocketPath, Request{Op: "bypass", Session: "s", Body: "hi", Protocol: ProtocolVersion, Handle: remote})
		}()
	}
	wg.Wait()
	for range 8 {
		resp := next(t, conn, dec)
		if !resp.OK || !strings.Contains(resp.Text, `"event":"bypass"`) || resp.Protocol != ProtocolVersion {
			t.Fatalf("streamed = %+v", resp)
		}
	}
}

// A client that leaves a follow stream does not keep the daemon from
// stopping.
func TestStopEndsAFollowStream(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	stop := start(t, dirs)
	conn, dec := openStream(t, dirs.SocketPath, Request{Op: "log", Follow: true})
	if resp := next(t, conn, dec); !resp.OK {
		t.Fatalf("first = %+v", resp)
	}
	stop()
	_ = conn.SetReadDeadline(time.Now().Add(2 * time.Second))
	// Drain the stream; it must end, by EOF or a closed connection,
	// before the read deadline.
	var resp Response
	for {
		if err := dec.Decode(&resp); err != nil {
			return
		}
	}
}
