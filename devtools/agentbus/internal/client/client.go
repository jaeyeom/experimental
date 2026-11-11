// Package client talks to agentbusd and starts it when it is down.
package client

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"golang.org/x/sys/unix"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

// Ensure reaches a daemon at dirs. If none is listening, it starts daemonBin
// in its own session and waits until a ping succeeds. A failure names what
// is missing and does not report success.
func Ensure(ctx context.Context, dirs paths.Dirs, daemonBin string, env []string) error {
	if err := ping(ctx, dirs.SocketPath); err == nil {
		return nil
	}
	if daemonBin == "" {
		return errors.New("agentbusd is not running and no daemon binary was given to start it")
	}
	if err := startDetached(daemonBin, env); err != nil {
		return fmt.Errorf("start agentbusd: %w", err)
	}
	deadline := time.Now().Add(2 * time.Second)
	var last error
	for {
		last = ping(ctx, dirs.SocketPath)
		if last == nil {
			return nil
		}
		if ctx.Err() != nil {
			return fmt.Errorf("agentbusd did not become ready: %w", ctx.Err())
		}
		if time.Now().After(deadline) {
			return fmt.Errorf("agentbusd did not become ready: %w", last)
		}
		timer := time.NewTimer(20 * time.Millisecond)
		select {
		case <-ctx.Done():
			timer.Stop()
			return fmt.Errorf("agentbusd did not become ready: %w", ctx.Err())
		case <-timer.C:
		}
	}
}

func ping(ctx context.Context, socketPath string) error {
	resp, err := Call(ctx, socketPath, daemon.Request{Op: "ping"})
	if err != nil {
		return err
	}
	if !resp.OK {
		return fmt.Errorf("ping rejected: %s", resp.Error)
	}
	return nil
}

// Call sends one request to a daemon that is already listening.
func Call(ctx context.Context, socketPath string, req daemon.Request) (daemon.Response, error) {
	dialer := net.Dialer{}
	conn, err := dialer.DialContext(ctx, "unix", socketPath)
	if err != nil {
		return daemon.Response{}, fmt.Errorf("connect to %s: %w", socketPath, err)
	}
	defer conn.Close()
	if deadline, ok := ctx.Deadline(); ok {
		_ = conn.SetDeadline(deadline)
	}
	if err := json.NewEncoder(conn).Encode(req); err != nil {
		return daemon.Response{}, fmt.Errorf("write request: %w", err)
	}
	var resp daemon.Response
	if err := json.NewDecoder(conn).Decode(&resp); err != nil {
		return daemon.Response{}, fmt.Errorf("read response: %w", err)
	}
	return resp, nil
}

// Stream sends one request and passes each reply to fn until a reply
// marks the end. It has no deadline; cancelling ctx is a clean stop. A
// reply with an error, or a connection that closes before the end, is
// an error.
func Stream(ctx context.Context, socketPath string, req daemon.Request, fn func(daemon.Response) error) error {
	dialer := net.Dialer{}
	conn, err := dialer.DialContext(ctx, "unix", socketPath)
	if err != nil {
		return fmt.Errorf("connect to %s: %w", socketPath, err)
	}
	defer conn.Close()
	stop := context.AfterFunc(ctx, func() { conn.Close() })
	defer stop()
	if err := json.NewEncoder(conn).Encode(req); err != nil {
		return fmt.Errorf("write request: %w", err)
	}
	dec := json.NewDecoder(conn)
	for {
		var resp daemon.Response
		if err := dec.Decode(&resp); err != nil {
			if ctx.Err() != nil {
				return nil
			}
			if errors.Is(err, io.EOF) {
				return errors.New("the daemon closed the stream")
			}
			return fmt.Errorf("read response: %w", err)
		}
		if resp.Error != "" {
			return errors.New(resp.Error)
		}
		if resp.End {
			return nil
		}
		if err := fn(resp); err != nil {
			return err
		}
	}
}

func startDetached(bin string, env []string) error {
	// The path is the agentbusd binary the caller chose, not a shell line.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(bin) //nolint:gosec // G702: fixed binary path, no shell
	cmd.Env = env
	cmd.Stdin = nil
	cmd.Stdout = nil
	cmd.Stderr = nil
	cmd.SysProcAttr = &unix.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		return fmt.Errorf("exec %s: %w", bin, err)
	}
	// The daemon is in its own session. Do not wait; the caller may exit.
	go func() { _ = cmd.Wait() }()
	return nil
}

// DaemonBinBeside returns the agentbusd next to this executable.
func DaemonBinBeside(exe string) (string, error) {
	candidate := filepath.Join(filepath.Dir(exe), "agentbusd")
	info, err := os.Stat(candidate)
	if err != nil {
		return "", fmt.Errorf("agentbusd is not next to %s (%s): %w", exe, candidate, err)
	}
	if info.IsDir() {
		return "", fmt.Errorf("agentbusd path %s is a directory", candidate)
	}
	return candidate, nil
}
