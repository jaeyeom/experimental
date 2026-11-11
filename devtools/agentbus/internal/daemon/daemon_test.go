package daemon

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestServePingAndStateSurvivesRestart(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	stop := start(t, dirs)
	if err := ping(dirs.SocketPath); err != nil {
		t.Fatalf("ping: %v", err)
	}
	stop()
	if _, err := os.Stat(dirs.StateDir); err != nil {
		t.Fatalf("state dir after shutdown: %v", err)
	}

	stop = start(t, dirs)
	defer stop()
	if err := ping(dirs.SocketPath); err != nil {
		t.Fatalf("ping after restart: %v", err)
	}
}

func TestServeLogsStartAndStopForALaterReader(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	stop := start(t, dirs)
	stop()
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 2 {
		t.Fatalf("log lines = %q", lines)
	}
	if !strings.Contains(lines[0], `"event":"start"`) || !strings.Contains(lines[1], `"event":"stop"`) {
		t.Fatalf("log lines = %q", lines)
	}
}

func start(t *testing.T, dirs paths.Dirs) func() {
	t.Helper()
	return startWith(t, dirs, Config{})
}

func startWith(t *testing.T, dirs paths.Dirs, cfg Config) func() {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	errc := make(chan error, 1)
	go func() { errc <- Serve(ctx, dirs, cfg) }()
	deadline := time.Now().Add(2 * time.Second)
	for ping(dirs.SocketPath) != nil {
		if time.Now().After(deadline) {
			cancel()
			t.Fatalf("daemon did not listen: %v", <-errc)
		}
		time.Sleep(10 * time.Millisecond)
	}
	return func() {
		cancel()
		select {
		case err := <-errc:
			if err != nil {
				t.Errorf("Serve: %v", err)
			}
		case <-time.After(2 * time.Second):
			t.Error("Serve did not return")
		}
	}
}

func shortDir(t *testing.T) string {
	t.Helper()
	// macOS rejects unix socket paths longer than about 104 bytes, and
	// t.TempDir is longer than that.
	dir, err := os.MkdirTemp("/tmp", "ab")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}

func ping(socketPath string) error {
	conn, err := net.Dial("unix", socketPath)
	if err != nil {
		return fmt.Errorf("dial: %w", err)
	}
	defer conn.Close()
	if err := json.NewEncoder(conn).Encode(Request{Op: "ping"}); err != nil {
		return fmt.Errorf("encode ping: %w", err)
	}
	var resp Response
	if err := json.NewDecoder(conn).Decode(&resp); err != nil {
		return fmt.Errorf("decode ping: %w", err)
	}
	if !resp.OK {
		return fmt.Errorf("ping not ok: %s", resp.Error)
	}
	return nil
}
