// Package daemon is the agentbus server. It owns the socket and the state directory.
package daemon

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

// Config chooses daemon behavior that the skeleton left open.
type Config struct {
	// RetentionDays is how many daily log files to keep. Zero uses the default.
	RetentionDays int
	// Adapters identify callers and report liveness. Empty means nobody can register.
	Adapters []adapter.Adapter
}

// Request is one client call. The daemon stamps identity from Env.
type Request struct {
	Op   string   `json:"op"`
	Name string   `json:"name,omitempty"`
	When string   `json:"when,omitempty"`
	To   string   `json:"to,omitempty"`
	Env  []string `json:"env,omitempty"`
}

// NameRecord is one registered name returned to the client.
type NameRecord struct {
	Name   string `json:"name"`
	Handle string `json:"handle"`
	When   string `json:"when,omitempty"`
}

// Response is the daemon's reply to one request.
type Response struct {
	OK     bool         `json:"ok"`
	Error  string       `json:"error,omitempty"`
	Name   string       `json:"name,omitempty"`
	Handle string       `json:"handle,omitempty"`
	Names  []NameRecord `json:"names,omitempty"`
}

// Serve listens on dirs.SocketPath until ctx is cancelled. It creates
// dirs.StateDir so registry, queue, and log state have a place that
// survives a later start. The daemon is the only writer of the event log.
func Serve(ctx context.Context, dirs paths.Dirs, cfg Config) (err error) {
	if err := os.MkdirAll(dirs.StateDir, 0o755); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	lg := eventlog.Open(dirs.StateDir)
	lg.RetentionDays = cfg.RetentionDays
	reg, err := registry.Open(dirs.StateDir, cfg.Adapters, lg)
	if err != nil {
		return fmt.Errorf("open registry: %w", err)
	}
	srv := &server{ctx: ctx, reg: reg}
	started := false
	defer func() {
		if !started {
			return
		}
		stopErr := lg.Append(eventlog.Event{
			Event:  eventlog.EventStop,
			Actor:  "daemon",
			Detail: "shutdown",
		})
		if stopErr != nil && err == nil {
			err = fmt.Errorf("log stop: %w", stopErr)
		}
	}()
	pidPath := filepath.Join(dirs.StateDir, "daemon.pid")
	if err := os.WriteFile(pidPath, []byte(strconv.Itoa(os.Getpid())+"\n"), 0o600); err != nil {
		return fmt.Errorf("record daemon pid: %w", err)
	}
	defer os.Remove(pidPath)
	if err := os.MkdirAll(filepath.Dir(dirs.SocketPath), 0o755); err != nil {
		return fmt.Errorf("create socket dir: %w", err)
	}
	if err := os.Remove(dirs.SocketPath); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("remove stale socket: %w", err)
	}
	ln, err := net.Listen("unix", dirs.SocketPath)
	if err != nil {
		return fmt.Errorf("listen on %s: %w", dirs.SocketPath, err)
	}
	defer ln.Close()
	defer os.Remove(dirs.SocketPath)
	if err := lg.Append(eventlog.Event{
		Event:  eventlog.EventStart,
		Actor:  "daemon",
		Detail: "listening",
	}); err != nil {
		return fmt.Errorf("log start: %w", err)
	}
	started = true

	go func() {
		<-ctx.Done()
		ln.Close()
	}()

	for {
		conn, err := ln.Accept()
		if err != nil {
			if ctx.Err() != nil {
				return nil
			}
			return fmt.Errorf("accept: %w", err)
		}
		srv.handle(conn)
	}
}

type server struct {
	ctx context.Context
	reg *registry.Registry
}

func (s *server) handle(conn net.Conn) {
	defer conn.Close()
	var req Request
	if err := json.NewDecoder(conn).Decode(&req); err != nil {
		write(conn, Response{Error: "bad request"})
		return
	}
	write(conn, s.dispatch(req))
}

func (s *server) dispatch(req Request) Response {
	switch req.Op {
	case "ping":
		return Response{OK: true}
	case "register":
		return s.register(req)
	case "describe":
		return s.describe(req)
	case "whoami":
		return s.whoami(req)
	case "release":
		return s.release(req)
	default:
		return Response{Error: "unknown op"}
	}
}

func (s *server) register(req Request) Response {
	handle, err := s.reg.Register(s.ctx, req.Env, req.Name, req.When)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: req.Name, Handle: handle}
}

func (s *server) describe(req Request) Response {
	names, err := s.reg.Describe(s.ctx, req.Env, req.When)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: strings.Join(names, "\n")}
}

func (s *server) whoami(req Request) Response {
	handle, names, err := s.reg.WhoAmI(s.ctx, req.Env)
	if err != nil {
		return Response{Error: err.Error()}
	}
	out := make([]NameRecord, 0, len(names))
	for _, name := range names {
		out = append(out, NameRecord{Name: name.Name, Handle: name.Handle, When: name.When})
	}
	return Response{OK: true, Handle: handle, Names: out}
}

func (s *server) release(req Request) Response {
	claimer, err := s.reg.Release(s.ctx, req.Env, req.Name, req.To)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: req.Name, Handle: claimer}
}

func write(conn net.Conn, resp Response) {
	if err := json.NewEncoder(conn).Encode(resp); err != nil {
		return
	}
}
