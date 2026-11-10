// Package hubcall runs a command on a hub machine over ssh, for tools
// such as agentbus and decide whose state lives on one machine.
//
// SSH can hang instead of failing when a key is locked or the agent's
// cache has expired. Run therefore passes BatchMode and ConnectTimeout,
// and kills the whole ssh process group when the hub does not answer
// within Deadline. The remote shell writes a ready line to stderr before
// it starts the tool; that line ends the deadline, so a long remote
// command, such as `agentbus log --follow`, is not cut short.
package hubcall

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// DefaultConnectTimeout is the ssh ConnectTimeout when Hub.ConnectTimeout is zero.
const DefaultConnectTimeout = 5 * time.Second

// probeTimeout bounds the TCP connect to the hub's SSH port.
const probeTimeout = time.Second

// readyLine is what the remote shell writes to stderr once it runs.
const readyLine = "hubcall: ready"

// ErrNoSSH means ssh is not on PATH.
var ErrNoSSH = errors.New("hub mode needs ssh, which is not on PATH")

// UnreachableError means the hub could not be reached. Cause is
// remotecall.CauseHostUnreachable or remotecall.CauseAuthLocked.
type UnreachableError struct {
	Target string
	Cause  string
}

func (e *UnreachableError) Error() string {
	return "hub " + e.Target + " unreachable: " + e.Cause
}

// Hub is one ssh target that runs Tool.
type Hub struct {
	// Target is the ssh destination, such as "me@hub" or a Host alias.
	Target string
	// Tool is the command run on the hub, such as "agentbus".
	Tool string
	// ConnectTimeout is ssh's ConnectTimeout. Zero is DefaultConnectTimeout.
	ConnectTimeout time.Duration
	// Deadline bounds the time until the hub's shell runs. Zero is twice
	// ConnectTimeout.
	Deadline time.Duration
	// Probe tells a down host from a locked key. Nil is TCPProbe(Target).
	Probe remotecall.Probe
}

// Call is one remote invocation. Env holds KEY=VALUE pairs set on the
// remote command.
type Call struct {
	Args   []string
	Env    []string
	Stdin  io.Reader
	Stdout io.Writer
	Stderr io.Writer
}

// Run runs Tool with c.Args on the hub and copies stdin, stdout, stderr,
// and the exit code. The error is ErrNoSSH, an *UnreachableError, or an
// ssh start failure; the code is then 1.
func (h Hub) Run(ctx context.Context, c Call) (int, error) {
	ssh, err := exec.LookPath("ssh")
	if err != nil {
		return 1, ErrNoSSH
	}
	connect := h.ConnectTimeout
	if connect <= 0 {
		connect = DefaultConnectTimeout
	}
	deadline := h.Deadline
	if deadline <= 0 {
		deadline = 2 * connect
	}
	args := []string{
		"-o", "BatchMode=yes",
		"-o", "ConnectTimeout=" + strconv.Itoa(int((connect+time.Second-1)/time.Second)),
		"--", h.Target, RemoteCommand(h.Tool, c.Env, c.Args),
	}
	callCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	stderr := &readyWriter{out: c.Stderr}
	timer := time.AfterFunc(deadline, func() {
		if stderr.expire() {
			cancel()
		}
	})
	defer timer.Stop()
	res, err := remotecall.RunIO(callCtx, c.Stdin, writerOr(c.Stdout), stderr, ssh, args...)
	stderr.flush()
	if err != nil {
		return 1, fmt.Errorf("hub %s: %w", h.Target, err)
	}
	ready, expired, captured := stderr.state()
	if ready {
		return res.Code, nil
	}
	res.TimedOut = expired
	res.Stderr = captured
	probe := h.Probe
	if probe == nil {
		probe = TCPProbe(h.Target)
	}
	if res.TimedOut || res.Code == 255 {
		if ue := remotecall.Classify(ctx, h.Target, res, probe); ue != nil {
			return 1, &UnreachableError{Target: h.Target, Cause: ue.Cause}
		}
	}
	if res.Code < 0 {
		return 1, nil
	}
	return res.Code, nil
}

// RemoteCommand is the line the hub's login shell runs: the ready line,
// then tool with env and args, each quoted for a POSIX shell.
func RemoteCommand(tool string, env, args []string) string {
	words := []string{"exec"}
	if len(env) > 0 {
		words = append(words, "env")
		for _, kv := range env {
			words = append(words, Quote(kv))
		}
	}
	words = append(words, Quote(tool))
	for _, arg := range args {
		words = append(words, Quote(arg))
	}
	return "echo " + Quote(readyLine) + " >&2; " + strings.Join(words, " ")
}

// Quote returns s as one POSIX shell word. Words made only of safe
// characters stay as they are.
func Quote(s string) string {
	if s != "" && strings.Trim(s, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_@%+=:,./-") == "" {
		return s
	}
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// TCPProbe connects to the SSH port of target within a second. The host
// and port come from `ssh -G`, so a Host alias in ~/.ssh/config works.
func TCPProbe(target string) remotecall.Probe {
	return func(ctx context.Context) bool {
		ctx, cancel := context.WithTimeout(ctx, probeTimeout)
		defer cancel()
		host, port := sshHostPort(ctx, target)
		var d net.Dialer
		conn, err := d.DialContext(ctx, "tcp", net.JoinHostPort(host, port))
		if err != nil {
			return false
		}
		conn.Close()
		return true
	}
}

// sshHostPort resolves target with `ssh -G`, which reads the config and
// does not connect. Without it, the host is target after any user@.
func sshHostPort(ctx context.Context, target string) (string, string) {
	host, port := target, "22"
	if _, after, ok := strings.Cut(host, "@"); ok {
		host = after
	}
	res, err := remotecall.Run(ctx, "ssh", "-G", "--", target)
	if err != nil || res.Code != 0 {
		return host, port
	}
	for _, line := range strings.Split(res.Stdout, "\n") {
		key, value, ok := strings.Cut(strings.TrimSpace(line), " ")
		if !ok {
			continue
		}
		switch key {
		case "hostname":
			host = value
		case "port":
			port = value
		}
	}
	return host, port
}

func writerOr(w io.Writer) io.Writer {
	if w == nil {
		return io.Discard
	}
	return w
}

// readyWriter passes stderr through and drops the ready line. Until that
// line arrives it keeps a copy of stderr, which is ssh's own, for
// classification.
type readyWriter struct {
	out io.Writer

	mu       sync.Mutex
	ready    bool
	expired  bool
	pending  []byte
	captured bytes.Buffer
}

func (w *readyWriter) Write(p []byte) (int, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.ready {
		return len(p), w.write(p)
	}
	w.pending = append(w.pending, p...)
	for !w.ready {
		i := bytes.IndexByte(w.pending, '\n')
		if i < 0 {
			break
		}
		line := w.pending[:i+1]
		if strings.TrimRight(string(line), "\r\n") == readyLine && !w.expired {
			w.ready = true
		} else {
			w.captured.Write(line)
			if err := w.write(line); err != nil {
				return len(p), err
			}
		}
		w.pending = w.pending[i+1:]
	}
	if w.ready && len(w.pending) > 0 {
		rest := w.pending
		w.pending = nil
		return len(p), w.write(rest)
	}
	return len(p), nil
}

func (w *readyWriter) write(p []byte) error {
	if w.out == nil {
		return nil
	}
	if _, err := w.out.Write(p); err != nil {
		return fmt.Errorf("copy stderr: %w", err)
	}
	return nil
}

// expire marks the deadline passed and reports whether the hub never
// answered, so the call must be killed.
func (w *readyWriter) expire() bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.ready {
		return false
	}
	w.expired = true
	return true
}

func (w *readyWriter) flush() {
	w.mu.Lock()
	defer w.mu.Unlock()
	if len(w.pending) == 0 {
		return
	}
	if !w.ready {
		w.captured.Write(w.pending)
	}
	_ = w.write(w.pending)
	w.pending = nil
}

func (w *readyWriter) state() (ready, expired bool, captured string) {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.ready, w.expired, w.captured.String()
}
