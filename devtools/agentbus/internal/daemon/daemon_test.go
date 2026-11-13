package daemon

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
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
// Shutdown must not cancel that paste; the test releases it.
type slowDelivery struct {
	fake.Adapter
	started   chan struct{}
	cancelled chan struct{}
	release   chan struct{}
}

func (a *slowDelivery) Deliver(ctx context.Context, _, _ string) (adapter.Result, error) {
	select {
	case a.started <- struct{}{}:
	default:
	}
	select {
	case <-ctx.Done():
		a.cancelled <- struct{}{}
		return adapter.ResultFailed, fmt.Errorf("deliver: %w", ctx.Err())
	case <-a.release:
		return adapter.ResultAccepted, nil
	}
}

func TestServeWaitsForAQueueFlushBeforeReturning(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	const handle = "herdr:m2/default/w2:p3"
	writeQueued(t, dirs.StateDir, queuedMessage(handle, "m1", "hi", "2026-01-01T00:00:00Z"))
	ad := &slowDelivery{
		Adapter:   fake.Adapter{AdapterName: "herdr", Reach: map[string]adapter.State{handle: adapter.StateIdle}},
		started:   make(chan struct{}, 1),
		cancelled: make(chan struct{}, 1),
		release:   make(chan struct{}),
	}
	cancel, errc := startDaemon(t, dirs, Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer cancel()
	waitPaste(t, ad.started)
	cancel()
	select {
	case <-ad.cancelled:
		t.Fatal("shutdown canceled the in-flight paste")
	case <-time.After(200 * time.Millisecond):
	}
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

// pasteGate counts pastes. It accepts after wait, after release, or
// fails when its own context ends, depending on which is set.
type pasteGate struct {
	fake.Adapter
	calls     atomic.Int32
	started   chan struct{}
	release   chan struct{}
	wait      time.Duration
	untilDone bool
}

func (a *pasteGate) Deliver(ctx context.Context, _, text string) (adapter.Result, error) {
	a.calls.Add(1)
	a.Screen = text
	select {
	case a.started <- struct{}{}:
	default:
	}
	if a.untilDone {
		<-ctx.Done()
		return adapter.ResultFailed, fmt.Errorf("deliver: %w", ctx.Err())
	}
	if a.wait > 0 {
		timer := time.NewTimer(a.wait)
		defer timer.Stop()
		select {
		case <-ctx.Done():
			return adapter.ResultFailed, fmt.Errorf("deliver: %w", ctx.Err())
		case <-timer.C:
			return adapter.ResultAccepted, nil
		}
	}
	select {
	case <-ctx.Done():
		return adapter.ResultFailed, fmt.Errorf("deliver: %w", ctx.Err())
	case <-a.release:
		return adapter.ResultAccepted, nil
	}
}

func TestShutdownLetsAnInFlightPasteFinish(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	const handle = "herdr:m2/default/w2:p3"
	writeQueued(t, dirs.StateDir, queuedMessage(handle, "m1", "hi", "2026-01-01T00:00:00Z"))
	ad := &pasteGate{
		Adapter: fake.Adapter{AdapterName: "herdr", Reach: map[string]adapter.State{handle: adapter.StateIdle}},
		started: make(chan struct{}, 1),
		wait:    time.Second,
	}
	cancel, errc := startDaemon(t, dirs, Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer cancel()
	waitPaste(t, ad.started)
	cancel()
	select {
	case err := <-errc:
		if err != nil {
			t.Fatalf("Serve: %v", err)
		}
	case <-time.After(3 * time.Second):
		t.Fatal("Serve did not return after the in-flight paste")
	}
	if got := ad.calls.Load(); got != 1 {
		t.Fatalf("pastes = %d, want 1", got)
	}
	if got := mustState(t, dirs.StateDir, "m1"); got != "delivered" {
		t.Fatalf("state = %q, want delivered", got)
	}
}

func TestPasteBoundEndsAsAmbiguousAndTheNextDaemonConfirms(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	const handle = "herdr:m2/default/w2:p3"
	writeQueued(t, dirs.StateDir, queuedMessage(handle, "m1", "hi", "2026-01-01T00:00:00Z"))
	ad := &pasteGate{
		Adapter:   fake.Adapter{AdapterName: "herdr", Reach: map[string]adapter.State{handle: adapter.StateIdle}},
		started:   make(chan struct{}, 1),
		untilDone: true,
	}
	cancel, errc := startDaemon(t, dirs, Config{
		Adapters:     []adapter.Adapter{ad},
		PollInterval: time.Hour,
		PasteBound:   80 * time.Millisecond,
	})
	waitPaste(t, ad.started)
	waitForState(t, dirs.StateDir, "m1", "held: ambiguous")
	cancel()
	if err := <-errc; err != nil {
		t.Fatalf("Serve: %v", err)
	}
	if got := ad.calls.Load(); got != 1 {
		t.Fatalf("pastes after the bound = %d, want 1", got)
	}

	cancel, errc = startDaemon(t, dirs, Config{
		Adapters:     []adapter.Adapter{ad},
		PollInterval: time.Hour,
		PasteBound:   80 * time.Millisecond,
	})
	defer cancel()
	waitForState(t, dirs.StateDir, "m1", "delivered")
	cancel()
	if err := <-errc; err != nil {
		t.Fatalf("next Serve: %v", err)
	}
	if got := ad.calls.Load(); got != 1 {
		t.Fatalf("pastes after confirm = %d, want 1", got)
	}
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(strings.Join(lines, "\n"), "confirmed-after-ambiguous") {
		t.Fatalf("log = %q", lines)
	}
}

func TestShutdownDoesNotStartAnotherPaste(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "daemon.sock"),
		StateDir:   filepath.Join(t.TempDir(), "state"),
	}
	const handle = "herdr:m2/default/w2:p3"
	writeQueued(t, dirs.StateDir, `{"messages":[`+
		`{"id":"m1","from":"alice","to":"bob","sentAt":"2026-01-01T00:00:00Z","body":"one","handle":"`+handle+`"},`+
		`{"id":"m2","from":"alice","to":"bob","sentAt":"2026-01-01T00:00:01Z","body":"two","handle":"`+handle+`"}]}`)
	ad := &pasteGate{
		Adapter: fake.Adapter{AdapterName: "herdr", Reach: map[string]adapter.State{handle: adapter.StateIdle}},
		started: make(chan struct{}, 1),
		release: make(chan struct{}),
	}
	cancel, errc := startDaemon(t, dirs, Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer cancel()
	waitPaste(t, ad.started)
	cancel()
	select {
	case err := <-errc:
		t.Fatalf("Serve returned (%v) before the in-flight paste finished", err)
	case <-time.After(200 * time.Millisecond):
	}
	close(ad.release)
	select {
	case err := <-errc:
		if err != nil {
			t.Fatalf("Serve: %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Serve did not return")
	}
	if got := ad.calls.Load(); got != 1 {
		t.Fatalf("pastes = %d, want 1", got)
	}
	if got := mustState(t, dirs.StateDir, "m1"); got != "delivered" {
		t.Fatalf("first state = %q, want delivered", got)
	}
	if got := mustState(t, dirs.StateDir, "m2"); got == "delivered" || strings.HasPrefix(got, "held:") {
		t.Fatalf("second state = %q, want still queued", got)
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

func writeQueued(t *testing.T, dir, body string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "messages.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}

func queuedMessage(handle, id, body, sentAt string) string {
	return `{"messages":[{"id":"` + id + `","from":"alice","to":"bob","sentAt":"` + sentAt + `","body":"` + body + `","handle":"` + handle + `"}]}`
}

func startDaemon(t *testing.T, dirs paths.Dirs, cfg Config) (context.CancelFunc, <-chan error) {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	errc := make(chan error, 1)
	go func() { errc <- Serve(ctx, dirs, cfg) }()
	t.Cleanup(cancel)
	deadline := time.Now().Add(2 * time.Second)
	for ping(dirs.SocketPath) != nil {
		select {
		case err := <-errc:
			cancel()
			t.Fatalf("Serve: %v", err)
		default:
		}
		if time.Now().After(deadline) {
			cancel()
			t.Fatal("daemon did not listen")
		}
		time.Sleep(10 * time.Millisecond)
	}
	return cancel, errc
}

func waitPaste(t *testing.T, started <-chan struct{}) {
	t.Helper()
	select {
	case <-started:
	case <-time.After(2 * time.Second):
		t.Fatal("paste did not start")
	}
}

func mustState(t *testing.T, dir, id string) string {
	t.Helper()
	state, ok := messageState(dir, id)
	if !ok {
		t.Fatalf("message %s is missing", id)
	}
	return state
}

func waitForState(t *testing.T, dir, id, want string) {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	var got string
	var ok bool
	for time.Now().Before(deadline) {
		got, ok = messageState(dir, id)
		if ok && got == want {
			return
		}
		time.Sleep(10 * time.Millisecond)
	}
	t.Fatalf("message %s state = %q, want %q", id, got, want)
}

func messageState(dir, id string) (string, bool) {
	raw, err := os.ReadFile(filepath.Join(dir, "messages.json"))
	if err != nil {
		return "", false
	}
	var doc struct {
		Messages []struct {
			ID    string `json:"id"`
			State string `json:"state"`
		} `json:"messages"`
	}
	if err := json.Unmarshal(raw, &doc); err != nil {
		return "", false
	}
	for _, msg := range doc.Messages {
		if msg.ID == id {
			return msg.State, true
		}
	}
	return "", false
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
