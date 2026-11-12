package daemon

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"golang.org/x/sys/unix"
)

// ErrAlreadyRunning means another daemon holds the state directory and
// answers on the socket. The process should exit 0.
var ErrAlreadyRunning = errors.New("agentbusd is already running")

// flock is per process, so a second Serve in this process would also
// acquire it. heldDirs makes that call lose the same way a second
// process loses the flock.
var (
	heldMu   sync.Mutex
	heldDirs = map[string]struct{}{}
)

// lockDaemon takes an exclusive non-blocking flock on daemon.lock and
// holds it until the returned function runs. The lock is taken before
// any socket or state file is opened. A held lock with a daemon that
// answers is ErrAlreadyRunning; a held lock with no answer is a loud error.
func lockDaemon(dirs paths.Dirs) (func(), error) {
	if err := os.MkdirAll(dirs.StateDir, 0o755); err != nil {
		return nil, fmt.Errorf("create state dir: %w", err)
	}
	heldMu.Lock()
	if _, ok := heldDirs[dirs.StateDir]; ok {
		heldMu.Unlock()
		return nil, lockHeld(dirs)
	}
	heldDirs[dirs.StateDir] = struct{}{}
	heldMu.Unlock()

	path := filepath.Join(dirs.StateDir, "daemon.lock")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		unhold(dirs.StateDir)
		return nil, fmt.Errorf("open daemon lock: %w", err)
	}
	got, err := tryLock(f)
	if err != nil {
		_ = f.Close()
		unhold(dirs.StateDir)
		return nil, err
	}
	if !got {
		_ = f.Close()
		unhold(dirs.StateDir)
		return nil, lockHeld(dirs)
	}
	// The pid is recorded only after the lock is held, so a reader that
	// finds the lock busy can trust it. flock does not stop a later
	// write; the daemon is the only writer.
	if err := writeLockPID(f, os.Getpid()); err != nil {
		_ = f.Close()
		unhold(dirs.StateDir)
		return nil, err
	}
	return func() {
		_ = f.Close()
		unhold(dirs.StateDir)
	}, nil
}

func writeLockPID(f *os.File, pid int) error {
	if _, err := f.Seek(0, io.SeekStart); err != nil {
		return fmt.Errorf("record daemon lock pid: %w", err)
	}
	if err := f.Truncate(0); err != nil {
		return fmt.Errorf("record daemon lock pid: %w", err)
	}
	if _, err := f.WriteString(strconv.Itoa(pid) + "\n"); err != nil {
		return fmt.Errorf("record daemon lock pid: %w", err)
	}
	return nil
}

func unhold(stateDir string) {
	heldMu.Lock()
	delete(heldDirs, stateDir)
	heldMu.Unlock()
}

func lockHeld(dirs paths.Dirs) error {
	path := filepath.Join(dirs.StateDir, "daemon.lock")
	if daemonAnswers(dirs.SocketPath) {
		return fmt.Errorf("%w", ErrAlreadyRunning)
	}
	return fmt.Errorf("daemon lock %s is held, but no daemon answers on %s", path, dirs.SocketPath)
}

// tryLock takes an exclusive flock on f without blocking. The kernel
// drops it when f is closed or the process dies.
func tryLock(f *os.File) (bool, error) {
	for {
		err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB) //nolint:gosec // G115: a file descriptor fits in int
		switch {
		case err == nil:
			return true, nil
		case errors.Is(err, unix.EWOULDBLOCK) || errors.Is(err, unix.EAGAIN):
			return false, nil
		case errors.Is(err, unix.EINTR):
			continue
		default:
			return false, fmt.Errorf("flock %s: %w", f.Name(), err)
		}
	}
}

// LockHolder reports the pid recorded in the state dir's daemon lock.
// held is false when the lock file is missing or unlocked. It must not
// run in the daemon process: flock is per process, so taking it here
// and closing the file would release the daemon's lock.
func LockHolder(stateDir string) (pid int, held bool, err error) {
	f, err := os.OpenFile(filepath.Join(stateDir, "daemon.lock"), os.O_RDWR, 0)
	if errors.Is(err, os.ErrNotExist) {
		return 0, false, nil
	}
	if err != nil {
		return 0, false, fmt.Errorf("open daemon lock: %w", err)
	}
	defer f.Close()
	got, err := tryLock(f)
	if err != nil {
		return 0, false, err
	}
	if got {
		return 0, false, nil
	}
	data, err := io.ReadAll(f)
	if err != nil {
		return 0, true, fmt.Errorf("read daemon lock: %w", err)
	}
	pid, err = strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil || pid <= 0 {
		return 0, true, nil
	}
	return pid, true, nil
}

// inode is the identity of a bound socket. A later bind at the same
// path gets a new one.
type inode struct{ dev, ino uint64 }

type socketFile struct {
	ln   *net.UnixListener
	path string
	id   inode
}

func (s socketFile) close() {
	if s.ln != nil {
		_ = s.ln.Close()
	}
	if s.path == "" {
		return
	}
	// Unlink only the inode this process bound. Close must not remove a
	// path another daemon has since rebound; SetUnlinkOnClose is off.
	ok, err := sameSocket(s.path, s.id)
	if err == nil && ok {
		_ = os.Remove(s.path)
	}
}

// listen binds path. A stale socket file is removed. A live listener is
// left in place: a daemon that answers is ErrAlreadyRunning, and any
// other listener is a loud error.
func listen(path string) (socketFile, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return socketFile{}, fmt.Errorf("create socket dir: %w", err)
	}
	if err := refuseLive(path); err != nil {
		return socketFile{}, err
	}
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		return socketFile{}, fmt.Errorf("remove stale socket: %w", err)
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		return socketFile{}, fmt.Errorf("listen on %s: %w", path, err)
	}
	ul, ok := ln.(*net.UnixListener)
	if !ok {
		_ = ln.Close()
		return socketFile{}, fmt.Errorf("listen on %s: not a unix socket", path)
	}
	ul.SetUnlinkOnClose(false)
	id, err := socketIdentity(path)
	if err != nil {
		_ = ul.Close()
		_ = os.Remove(path)
		return socketFile{}, fmt.Errorf("stat socket: %w", err)
	}
	return socketFile{ln: ul, path: path, id: id}, nil
}

func refuseLive(path string) error {
	if _, err := os.Lstat(path); errors.Is(err, os.ErrNotExist) {
		return nil
	} else if err != nil {
		return fmt.Errorf("stat socket: %w", err)
	}
	live, daemon := probe(path)
	if !live {
		return nil
	}
	if daemon {
		return fmt.Errorf("%w", ErrAlreadyRunning)
	}
	return fmt.Errorf("socket %s is in use", path)
}

// probe dials path. live means a listener accepted. daemon means that
// listener answered a ping.
func probe(path string) (live, daemon bool) {
	conn, err := net.DialTimeout("unix", path, 200*time.Millisecond)
	if err != nil {
		return false, false
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(200 * time.Millisecond))
	if err := json.NewEncoder(conn).Encode(Request{Op: "ping"}); err != nil {
		return true, false
	}
	var resp Response
	if err := json.NewDecoder(conn).Decode(&resp); err != nil || !resp.OK {
		return true, false
	}
	return true, true
}

func daemonAnswers(path string) bool {
	live, daemon := probe(path)
	return live && daemon
}

func socketIdentity(path string) (inode, error) {
	var st unix.Stat_t
	if err := unix.Stat(path, &st); err != nil {
		if errors.Is(err, unix.ENOENT) {
			return inode{}, fmt.Errorf("stat %s: %w", path, os.ErrNotExist)
		}
		return inode{}, fmt.Errorf("stat %s: %w", path, err)
	}
	return statInode(st), nil
}

func sameSocket(path string, id inode) (bool, error) {
	got, err := socketIdentity(path)
	if errors.Is(err, os.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return got == id, nil
}

// removePIDFile removes path only when it still names pid. A daemon
// that lost the pid file must not delete the one a later daemon wrote.
func removePIDFile(path string, pid int) {
	data, err := os.ReadFile(path)
	if err != nil {
		return
	}
	got, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil || got != pid {
		return
	}
	_ = os.Remove(path)
}

// watchSockets exits by calling lost when a bound socket's inode is no
// longer the one this process listens on. A stat error is not that.
func watchSockets(ctx context.Context, socks []socketFile, ticks <-chan time.Time, lost func()) {
	if ticks == nil {
		<-ctx.Done()
		return
	}
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticks:
			for _, sock := range socks {
				ok, err := sameSocket(sock.path, sock.id)
				if err != nil || ok {
					continue
				}
				lost()
				return
			}
		}
	}
}
