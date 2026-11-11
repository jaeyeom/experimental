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

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
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

// slowInventory is an adapter whose inventory is still running when the
// daemon is told to stop, like a herdr call that has not answered yet.
type slowInventory struct {
	fake.Adapter
	cancelled chan struct{}
	release   chan struct{}
}

func (a *slowInventory) Inventory(ctx context.Context) ([]adapter.Agent, error) {
	<-ctx.Done()
	a.cancelled <- struct{}{}
	<-a.release
	return nil, fmt.Errorf("inventory: %w", ctx.Err())
}

func TestServeWaitsForTheInventoryPollBeforeReturning(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	ad := &slowInventory{
		Adapter:   fake.Adapter{AdapterName: "herdr"},
		cancelled: make(chan struct{}, 1),
		release:   make(chan struct{}),
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errc := make(chan error, 1)
	go func() {
		errc <- Serve(ctx, dirs, Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	}()
	for ping(dirs.SocketPath) != nil {
		select {
		case err := <-errc:
			t.Fatalf("Serve: %v", err)
		case <-time.After(10 * time.Millisecond):
		}
	}
	cancel()
	<-ad.cancelled
	select {
	case err := <-errc:
		close(ad.release)
		t.Fatalf("Serve returned (%v) while the inventory poll was still running", err)
	case <-time.After(200 * time.Millisecond):
	}
	close(ad.release)
	if err := <-errc; err != nil {
		t.Fatalf("Serve: %v", err)
	}
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) == 0 || !strings.Contains(lines[len(lines)-1], `"event":"stop"`) {
		t.Fatalf("stop is not the last event: %q", lines)
	}
}

// slowDelivery is an adapter whose paste is still running when the
// daemon is told to stop, like a remote prompt that has not answered yet.
type slowDelivery struct {
	fake.Adapter
	cancelled chan struct{}
	release   chan struct{}
}

func (a *slowDelivery) Deliver(ctx context.Context, _, _ string) (adapter.Result, error) {
	<-ctx.Done()
	a.cancelled <- struct{}{}
	<-a.release
	return adapter.ResultFailed, fmt.Errorf("deliver: %w", ctx.Err())
}

func TestServeWaitsForAQueueFlushBeforeReturning(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	const handle = "herdr:m2/default/w2:p3"
	if err := os.MkdirAll(dirs.StateDir, 0o700); err != nil {
		t.Fatal(err)
	}
	queued := `{"messages":[{"id":"m1","from":"alice","to":"bob","sentAt":"2026-01-01T00:00:00Z","body":"hi","handle":"` + handle + `"}]}`
	if err := os.WriteFile(filepath.Join(dirs.StateDir, "messages.json"), []byte(queued), 0o600); err != nil {
		t.Fatal(err)
	}
	ad := &slowDelivery{
		Adapter:   fake.Adapter{AdapterName: "herdr", Reach: map[string]adapter.State{handle: adapter.StateIdle}},
		cancelled: make(chan struct{}, 1),
		release:   make(chan struct{}),
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errc := make(chan error, 1)
	go func() {
		errc <- Serve(ctx, dirs, Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	}()
	for ping(dirs.SocketPath) != nil {
		select {
		case err := <-errc:
			t.Fatalf("Serve: %v", err)
		case <-time.After(10 * time.Millisecond):
		}
	}
	cancel()
	<-ad.cancelled
	select {
	case err := <-errc:
		close(ad.release)
		t.Fatalf("Serve returned (%v) while the queue flush was still running", err)
	case <-time.After(200 * time.Millisecond):
	}
	close(ad.release)
	if err := <-errc; err != nil {
		t.Fatalf("Serve: %v", err)
	}
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) == 0 || !strings.Contains(lines[len(lines)-1], `"event":"stop"`) {
		t.Fatalf("stop is not the last event: %q", lines)
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
