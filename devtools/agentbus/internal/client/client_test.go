package client

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"golang.org/x/sys/unix"
)

func TestMain(m *testing.M) {
	if os.Getenv("AGENTBUS_TEST_SERVE") == "1" {
		os.Exit(serveForTest())
	}
	os.Exit(m.Run())
}

func serveForTest() int {
	dirs, err := paths.FromEnv(os.Environ())
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	if err := os.MkdirAll(dirs.StateDir, 0o755); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	sid, err := unix.Getsid(0)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	if err := os.WriteFile(filepath.Join(dirs.StateDir, "sid"), []byte(strconv.Itoa(sid)+"\n"), 0o600); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	if err := daemon.Serve(ctx, dirs, daemon.Config{}); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 1
	}
	return 0
}

func TestEnsureStartsDetachedDaemon(t *testing.T) {
	runtime := shortDir(t)
	state := t.TempDir()
	env := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + state,
		"AGENTBUS_TEST_SERVE=1",
		"PATH=" + os.Getenv("PATH"),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := Ensure(ctx, dirs, os.Args[0], env); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { stopDaemon(t, dirs.StateDir) })

	info, err := os.Stat(dirs.StateDir)
	if err != nil || !info.IsDir() {
		t.Fatalf("state dir: %v", err)
	}
	childSID := readSID(t, dirs.StateDir)
	parentSID, err := unix.Getsid(0)
	if err != nil {
		t.Fatal(err)
	}
	if childSID == parentSID {
		t.Fatalf("daemon sid %d is the caller's sid", childSID)
	}

	if err := Ensure(ctx, dirs, "", env); err != nil {
		t.Fatalf("second ensure with no binary: %v", err)
	}
}

func TestEnsureReportsMissingDaemon(t *testing.T) {
	dirs := paths.Dirs{
		SocketPath: filepath.Join(shortDir(t), "missing.sock"),
		StateDir:   t.TempDir(),
	}
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	err := Ensure(ctx, dirs, "", nil)
	if err == nil || !strings.Contains(err.Error(), "agentbusd") {
		t.Fatalf("Ensure error = %v", err)
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

func stopDaemon(t *testing.T, stateDir string) {
	t.Helper()
	pid := daemonPID(t, stateDir)
	if err := unix.Kill(pid, unix.SIGTERM); err != nil {
		t.Errorf("stop daemon: %v", err)
		return
	}
	deadline := time.Now().Add(2 * time.Second)
	for time.Now().Before(deadline) {
		if err := unix.Kill(pid, 0); err != nil {
			return
		}
		time.Sleep(10 * time.Millisecond)
	}
	t.Errorf("daemon %d did not exit", pid)
}

func daemonPID(t *testing.T, stateDir string) int {
	t.Helper()
	data, err := os.ReadFile(stateDir + "/daemon.pid")
	if err != nil {
		t.Fatal(err)
	}
	pid, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil {
		t.Fatal(err)
	}
	return pid
}

func readSID(t *testing.T, stateDir string) int {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(stateDir, "sid"))
	if err != nil {
		t.Fatal(err)
	}
	sid, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil {
		t.Fatal(err)
	}
	return sid
}
