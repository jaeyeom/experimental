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
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

// Config chooses daemon behavior that the skeleton left open.
type Config struct {
	// RetentionDays is how many daily log files to keep. Zero uses the default.
	RetentionDays int
	// Adapters identify callers and report liveness. Empty means nobody can register.
	Adapters []adapter.Adapter
	// PollInterval is how often the daemon refreshes adapter inventories.
	// Zero uses two seconds. Names do not expire on this timer.
	PollInterval time.Duration
	// InlineBytes is the largest body pasted in full. Zero uses the default.
	// A longer or unsafe body is stored and delivered as a preview.
	InlineBytes int
}

// Request is one client call. The daemon stamps identity from Env.
type Request struct {
	Op    string   `json:"op"`
	Name  string   `json:"name,omitempty"`
	When  string   `json:"when,omitempty"`
	To    string   `json:"to,omitempty"`
	Query string   `json:"query,omitempty"`
	Body  string   `json:"body,omitempty"`
	Env   []string `json:"env,omitempty"`
}

// AgentRow is one list or search result. An empty Name means the bus
// did not assign a logical name.
type AgentRow struct {
	Name      string `json:"name,omitempty"`
	When      string `json:"when,omitempty"`
	Liveness  string `json:"liveness,omitempty"`
	Transport string `json:"transport,omitempty"`
	Handle    string `json:"handle,omitempty"`
	Label     string `json:"label,omitempty"`
	Title     string `json:"title,omitempty"`
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
	Rows   []AgentRow   `json:"rows,omitempty"`
	State  string       `json:"state,omitempty"`
	Text   string       `json:"text,omitempty"`
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
	reg, box, err := openState(dirs.StateDir, cfg, lg)
	if err != nil {
		return err
	}
	srv := &server{ctx: ctx, reg: reg, box: box}
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

	go reg.Poll(ctx, cfg.PollInterval)
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

func openState(dir string, cfg Config, lg *eventlog.Log) (*registry.Registry, *mailbox.Mailbox, error) {
	reg, err := registry.Open(dir, cfg.Adapters, lg)
	if err != nil {
		return nil, nil, fmt.Errorf("open registry: %w", err)
	}
	box, err := mailbox.Open(dir, reg, cfg.Adapters, lg, cfg.InlineBytes)
	if err != nil {
		return nil, nil, fmt.Errorf("open mailbox: %w", err)
	}
	return reg, box, nil
}

type server struct {
	ctx context.Context
	reg *registry.Registry
	box *mailbox.Mailbox
}

func (s *server) handle(conn net.Conn) {
	defer conn.Close()
	var req Request
	if err := json.NewDecoder(conn).Decode(&req); err != nil {
		write(conn, Response{Error: "bad request"})
		return
	}
	write(conn, s.dispatch(req, peerPID(conn)))
}

func (s *server) dispatch(req Request, pid int) Response {
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
	case "list":
		return s.list()
	case "search":
		return s.search(req)
	case "send":
		return s.send(req, pid)
	case "reply":
		return s.reply(req, pid)
	case "show":
		return s.show(req)
	case "thread":
		return s.thread(req)
	case "status":
		return s.status(req)
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

func (s *server) list() Response {
	rows, err := s.reg.List(s.ctx)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Rows: agentRows(rows)}
}

func (s *server) search(req Request) Response {
	rows, err := s.reg.Search(s.ctx, req.Query)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Rows: agentRows(rows)}
}

func agentRows(in []registry.Row) []AgentRow {
	if len(in) == 0 {
		return nil
	}
	out := make([]AgentRow, len(in))
	for i, row := range in {
		out[i] = AgentRow{
			Name:      row.Name,
			When:      row.When,
			Liveness:  row.Liveness,
			Transport: row.Transport,
			Handle:    row.Handle,
			Label:     row.Label,
			Title:     row.Title,
		}
	}
	return out
}

func (s *server) send(req Request, pid int) Response {
	result, err := s.box.Send(s.ctx, req.Env, pid, req.Name, req.Body)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: result.ID, State: result.State}
}

func (s *server) reply(req Request, pid int) Response {
	result, err := s.box.Reply(s.ctx, req.Env, pid, req.Name, req.Body)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: result.ID, State: result.State}
}

func (s *server) show(req Request) Response {
	text, err := s.box.Show(req.Name)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: req.Name, Text: text}
}

func (s *server) thread(req Request) Response {
	text, err := s.box.Thread(req.Name)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: req.Name, Text: text}
}

func (s *server) status(req Request) Response {
	text, err := s.box.Status(req.Name)
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: req.Name, Text: text}
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
