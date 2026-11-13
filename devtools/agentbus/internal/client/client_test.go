package client

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"net"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
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
	orig := supervisorRun
	supervisorRun = func(context.Context, []string) error {
		t.Error("supervisor start ran without a unit file")
		return fmt.Errorf("supervisor")
	}
	t.Cleanup(func() { supervisorRun = orig })
	runtimeDir := shortDir(t)
	state := t.TempDir()
	env := []string{
		"XDG_RUNTIME_DIR=" + runtimeDir,
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

func TestEnsureAsksTheSupervisorInsteadOfSpawning(t *testing.T) {
	if runtime.GOOS != "darwin" && runtime.GOOS != "linux" {
		t.Skip("supervisor units are launchd or systemd")
	}
	home := t.TempDir()
	var unit string
	var want []string
	switch runtime.GOOS {
	case "darwin":
		unit = filepath.Join(home, "Library", "LaunchAgents", supervisorLaunchdLabel+".plist")
		want = []string{"launchctl", "kickstart", fmt.Sprintf("gui/%d/%s", os.Getuid(), supervisorLaunchdLabel)}
	default:
		unit = filepath.Join(home, ".config", "systemd", "user", supervisorSystemdUnit)
		want = []string{"systemctl", "--user", "start", supervisorSystemdUnit}
	}
	if err := os.MkdirAll(filepath.Dir(unit), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(unit, []byte("installed\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	runtimeDir := shortDir(t)
	state := t.TempDir()
	env := []string{
		"HOME=" + home,
		"XDG_RUNTIME_DIR=" + runtimeDir,
		"XDG_STATE_HOME=" + state,
		"AGENTBUS_TEST_SERVE=1",
		"PATH=" + os.Getenv("PATH"),
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		t.Fatal(err)
	}
	origRun, origSpawn := supervisorRun, spawnDetached
	t.Cleanup(func() { supervisorRun, spawnDetached = origRun, origSpawn })
	var argv []string
	supervisorRun = func(_ context.Context, args []string) error {
		argv = append([]string{}, args...)
		return nil
	}
	spawnDetached = func(string, []string) error {
		t.Error("spawned a detached daemon while a supervisor is installed")
		return fmt.Errorf("spawned")
	}
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	err = Ensure(ctx, dirs, os.Args[0], env)
	if err == nil || !strings.Contains(err.Error(), "did not become ready") {
		t.Fatalf("Ensure = %v, want the supervisor start to be asked and no daemon spawned", err)
	}
	if strings.Join(argv, " ") != strings.Join(want, " ") {
		t.Fatalf("supervisor command = %q, want %q", argv, want)
	}
	if _, err := os.Stat(filepath.Join(dirs.StateDir, "daemon.pid")); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("detached daemon pid file: %v", err)
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

func TestCallMarksADroppedReplyAsSent(t *testing.T) {
	sock := filepath.Join(shortDir(t), "d.sock")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	go func() {
		conn, err := ln.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		var req daemon.Request
		_ = json.NewDecoder(conn).Decode(&req)
	}()
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	_, err = Call(ctx, sock, daemon.Request{Op: "send", Name: "ops/inbox", Body: "hi"})
	if !RequestSent(err) || err == nil || !strings.Contains(err.Error(), "read response") {
		t.Fatalf("Call = %v", err)
	}
}

func TestCallDoesNotMarkARefusedConnectAsSent(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	_, err := Call(ctx, filepath.Join(shortDir(t), "missing.sock"), daemon.Request{Op: "ping"})
	if err == nil || RequestSent(err) || !strings.Contains(err.Error(), "connect to") {
		t.Fatalf("Call = %v", err)
	}
}

type failWrite struct{ net.Conn }

func (failWrite) Write([]byte) (int, error) { return 0, errors.New("broken pipe") }

func TestCallWriteErrorIsSent(t *testing.T) {
	a, b := net.Pipe()
	defer b.Close()
	_, err := exchange(failWrite{a}, daemon.Request{Op: "send", Body: "hi"})
	if !RequestSent(err) || err == nil || !strings.Contains(err.Error(), "write request: broken pipe") {
		t.Fatalf("exchange = %v", err)
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
