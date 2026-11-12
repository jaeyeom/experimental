package daemon

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"golang.org/x/sys/unix"
)

func TestMain(m *testing.M) {
	if os.Getenv("AGENTBUS_TEST_LOCK") == "1" {
		holdTestLock()
		return
	}
	os.Exit(m.Run())
}

// holdTestLock takes the daemon lock and waits to be killed. TestMain
// runs it in a child so LockHolder sees another process.
func holdTestLock() {
	f, err := os.OpenFile(os.Getenv("AGENTBUS_TEST_LOCK_PATH"), os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX); err != nil { //nolint:gosec // G115: a file descriptor fits in int
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if _, err := f.WriteString(strconv.Itoa(os.Getpid()) + "\n"); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	fmt.Println("locked")
	_ = os.Stdout.Sync()
	for {
		time.Sleep(time.Hour)
	}
}

func testDirs(t *testing.T) paths.Dirs {
	t.Helper()
	base := shortDir(t)
	return paths.Dirs{
		SocketPath:    filepath.Join(base, "daemon.sock"),
		HubSocketPath: filepath.Join(base, "hub.sock"),
		StateDir:      filepath.Join(t.TempDir(), "state"),
	}
}

func TestServeRejectsASecondDaemon(t *testing.T) {
	dirs := testDirs(t)
	stop := start(t, dirs)
	defer stop()
	before, err := socketIdentity(dirs.SocketPath)
	if err != nil {
		t.Fatal(err)
	}
	pidBefore, err := os.ReadFile(filepath.Join(dirs.StateDir, "daemon.pid"))
	if err != nil {
		t.Fatal(err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	err = Serve(ctx, dirs, Config{})
	if !errors.Is(err, ErrAlreadyRunning) {
		t.Fatalf("second Serve = %v, want already running", err)
	}
	after, err := socketIdentity(dirs.SocketPath)
	if err != nil {
		t.Fatal(err)
	}
	if before != after {
		t.Fatalf("socket identity %v changed to %v", before, after)
	}
	pidAfter, err := os.ReadFile(filepath.Join(dirs.StateDir, "daemon.pid"))
	if err != nil {
		t.Fatal(err)
	}
	if string(pidAfter) != string(pidBefore) {
		t.Fatalf("pid file changed from %q to %q", pidBefore, pidAfter)
	}
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{})
	if err != nil {
		t.Fatal(err)
	}
	starts := 0
	for _, line := range lines {
		if strings.Contains(line, `"event":"start"`) {
			starts++
		}
	}
	if starts != 1 {
		t.Fatalf("start events = %d, want only the first daemon's: %q", starts, lines)
	}
}

func TestServeFailsLoudlyWhenTheLockIsHeldAndNothingAnswers(t *testing.T) {
	dirs := testDirs(t)
	if err := os.MkdirAll(dirs.StateDir, 0o755); err != nil {
		t.Fatal(err)
	}
	// flock does not conflict with another lock in this process, so the
	// holder has to be a child. It takes the lock and does not listen.
	_, stop := lockChild(t, filepath.Join(dirs.StateDir, "daemon.lock"))
	defer stop()
	if err := os.MkdirAll(filepath.Dir(dirs.SocketPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(dirs.SocketPath, []byte("stale"), 0o600); err != nil {
		t.Fatal(err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	err := Serve(ctx, dirs, Config{})
	if err == nil || errors.Is(err, ErrAlreadyRunning) || !strings.Contains(err.Error(), "lock") {
		t.Fatalf("Serve = %v, want a loud lock failure", err)
	}
	data, err := os.ReadFile(dirs.SocketPath)
	if err != nil || string(data) != "stale" {
		t.Fatalf("socket = %q, %v; a locked start must not replace it", data, err)
	}
	if _, err := os.Stat(filepath.Join(dirs.StateDir, "registry.json")); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("registry.json err = %v, want no state written", err)
	}
}

func TestListenReplacesAStaleSocket(t *testing.T) {
	path := filepath.Join(shortDir(t), "daemon.sock")
	if err := os.WriteFile(path, []byte("stale"), 0o600); err != nil {
		t.Fatal(err)
	}
	sock, err := listen(path)
	if err != nil {
		t.Fatal(err)
	}
	defer sock.close()
	conn, err := net.Dial("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	conn.Close()
}

func TestListenDoesNotRemoveALiveSocket(t *testing.T) {
	path := filepath.Join(shortDir(t), "daemon.sock")
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	if _, err := listen(path); err == nil {
		t.Fatal("listen took a path that already has a listener")
	}
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("socket path: %v", err)
	}
	client, err := net.Dial("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	defer client.Close()
	accepted, err := ln.Accept()
	if err != nil {
		t.Fatal(err)
	}
	accepted.Close()
}

func TestListenDoesNotRemoveALiveHubSocket(t *testing.T) {
	dirs := testDirs(t)
	ln, err := net.Listen("unix", dirs.HubSocketPath)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	err = Serve(ctx, dirs, Config{HubCaller: func(string) (string, error) { return "me", nil }})
	if err == nil {
		t.Fatal("Serve took a hub socket that already has a listener")
	}
	if _, err := os.Stat(dirs.HubSocketPath); err != nil {
		t.Fatalf("hub socket path: %v", err)
	}
	client, err := net.Dial("unix", dirs.HubSocketPath)
	if err != nil {
		t.Fatal(err)
	}
	client.Close()
}

func TestExitingDaemonLeavesAReboundSocketAndPid(t *testing.T) {
	dirs := testDirs(t)
	ctx, cancel := context.WithCancel(context.Background())
	errc := make(chan error, 1)
	go func() { errc <- Serve(ctx, dirs, Config{}) }()
	deadline := time.Now().Add(2 * time.Second)
	for ping(dirs.SocketPath) != nil {
		if time.Now().After(deadline) {
			cancel()
			t.Fatalf("daemon did not listen: %v", <-errc)
		}
		time.Sleep(10 * time.Millisecond)
	}
	if err := os.Remove(dirs.SocketPath); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", dirs.SocketPath)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	// The daemon runs in this process, so its pid is ours. B's pid has
	// to be a different number or the exit cleanup would remove it.
	other := os.Getpid() + 1
	pidPath := filepath.Join(dirs.StateDir, "daemon.pid")
	if err := os.WriteFile(pidPath, []byte(strconv.Itoa(other)+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cancel()
	select {
	case err := <-errc:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Serve did not return")
	}
	if _, err := os.Stat(dirs.SocketPath); err != nil {
		t.Fatalf("rebound socket: %v", err)
	}
	client, err := net.Dial("unix", dirs.SocketPath)
	if err != nil {
		t.Fatal(err)
	}
	defer client.Close()
	accepted, err := ln.Accept()
	if err != nil {
		t.Fatal(err)
	}
	accepted.Close()
	data, err := os.ReadFile(pidPath)
	if err != nil {
		t.Fatal(err)
	}
	if strings.TrimSpace(string(data)) != strconv.Itoa(other) {
		t.Fatalf("pid file = %q, want the rebound daemon's pid", data)
	}
}

func TestServeRemovesAnOwnedSocketOnExit(t *testing.T) {
	dirs := testDirs(t)
	stop := startWith(t, dirs, Config{HubCaller: func(string) (string, error) { return "me", nil }})
	stop()
	for _, path := range []string{dirs.SocketPath, dirs.HubSocketPath, filepath.Join(dirs.StateDir, "daemon.pid")} {
		if _, err := os.Stat(path); !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("%s: %v, want removed", path, err)
		}
	}
}

func TestServeExitsWhenTheSocketInodeChanges(t *testing.T) {
	dirs := testDirs(t)
	ticks := make(chan time.Time)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	errc := make(chan error, 1)
	go func() {
		errc <- Serve(ctx, dirs, Config{SocketTicks: ticks})
	}()
	deadline := time.Now().Add(2 * time.Second)
	for ping(dirs.SocketPath) != nil {
		if time.Now().After(deadline) {
			t.Fatalf("daemon did not listen: %v", <-errc)
		}
		time.Sleep(10 * time.Millisecond)
	}
	if err := os.Remove(dirs.SocketPath); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", dirs.SocketPath)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	ticks <- time.Now()
	select {
	case err := <-errc:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("Serve did not exit after the socket inode changed")
	}
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventStop})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 1 || !strings.Contains(lines[0], "socket rebound") {
		t.Fatalf("stop events = %q, want the rebound reason", lines)
	}
	if _, err := os.Stat(dirs.SocketPath); err != nil {
		t.Fatalf("rebound socket: %v", err)
	}
}

func TestLockHolderReportsTheOtherProcess(t *testing.T) {
	dir := t.TempDir()
	want, stop := lockChild(t, filepath.Join(dir, "daemon.lock"))
	defer stop()
	pid, held, err := LockHolder(dir)
	if err != nil || !held || pid != want {
		t.Fatalf("LockHolder = %d, %v, %v; child pid %d", pid, held, err, want)
	}
}

// lockChild holds an exclusive flock on path until the returned stop
// runs. The child prints "locked" once it has the lock and has written
// its pid into the file.
func lockChild(t *testing.T, path string) (int, func()) {
	t.Helper()
	// Re-exec this test binary so the lock is held by another process.
	cmd := exec.Command(os.Args[0], "-test.run=^$") //nolint:gosec // G204: fixed test binary, no shell
	cmd.Env = append(os.Environ(), "AGENTBUS_TEST_LOCK=1", "AGENTBUS_TEST_LOCK_PATH="+path)
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	buf := make([]byte, 16)
	if _, err := out.Read(buf); err != nil {
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
		t.Fatal(err)
	}
	return cmd.Process.Pid, func() {
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
	}
}
