// Package daemon is the agentbus server. It owns the socket and the state directory.
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
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
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
	// HubCaller checks the caller handle of a request on the hub socket
	// and returns it. Nil means the daemon does not listen on the hub
	// socket.
	HubCaller func(handle string) (string, error)
	// Now is the lease clock. Nil uses time.Now.
	Now func() time.Time
}

// ProtocolVersion is the request format a hub-socket client must speak.
// A client on another machine may run a different agentbus build.
// Version 2 added the streaming log op.
const ProtocolVersion = 2

// Request is one client call. The daemon stamps identity from Env, or,
// on the hub socket, from Handle.
type Request struct {
	Op      string   `json:"op"`
	Name    string   `json:"name,omitempty"`
	When    string   `json:"when,omitempty"`
	To      string   `json:"to,omitempty"`
	Query   string   `json:"query,omitempty"`
	Body    string   `json:"body,omitempty"`
	Env     []string `json:"env,omitempty"`
	TTL     string   `json:"ttl,omitempty"`
	Wait    bool     `json:"wait,omitempty"`
	Session string   `json:"session,omitempty"`
	// Protocol is the client's ProtocolVersion. The hub socket needs it.
	Protocol int `json:"protocol,omitempty"`
	// Handle is the caller on another machine. The hub socket needs it
	// and ignores Env.
	Handle string `json:"handle,omitempty"`
	// Since, MsgID, Event, and Name filter the log op. Since is RFC3339.
	Since string `json:"since,omitempty"`
	MsgID string `json:"msg,omitempty"`
	Event string `json:"event,omitempty"`
	// Follow keeps a log stream open for new events.
	Follow bool `json:"follow,omitempty"`
	// Lease carries the arguments of the lease-* ops.
	Lease *lease.Args `json:"lease,omitempty"`
}

// bypassPreview is how many bytes of a SendMessage body a bypass event keeps.
const bypassPreview = 200

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
	// Protocol is the daemon's ProtocolVersion on hub-socket replies.
	Protocol int `json:"protocol,omitempty"`
	// End marks the last reply of a log stream that does not follow.
	End bool `json:"end,omitempty"`
	// Lease is the answer to a lease-* op.
	Lease *lease.Reply `json:"lease,omitempty"`
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
	leases, err := lease.Open(dirs.StateDir, lg, cfg.Now, func(handle string) string {
		if names := reg.NamesOf(handle); len(names) > 0 {
			return names[0]
		}
		return ""
	})
	if err != nil {
		return fmt.Errorf("open leases: %w", err)
	}
	srv := &server{ctx: ctx, reg: reg, box: box, log: lg, leases: leases, adapters: cfg.Adapters}
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
	ln, err := listen(dirs.SocketPath)
	if err != nil {
		return err
	}
	defer ln.Close()
	defer os.Remove(dirs.SocketPath)
	var hubLn net.Listener
	if cfg.HubCaller != nil && dirs.HubSocketPath != "" {
		if hubLn, err = listen(dirs.HubSocketPath); err != nil {
			return err
		}
		defer hubLn.Close()
		defer os.Remove(dirs.HubSocketPath)
		srv.hubCaller = cfg.HubCaller
	}
	if err := lg.Append(eventlog.Event{
		Event:  eventlog.EventStart,
		Actor:  "daemon",
		Detail: "listening",
	}); err != nil {
		return fmt.Errorf("log start: %w", err)
	}
	started = true

	go reg.Poll(ctx, cfg.PollInterval)
	go box.Poll(ctx, cfg.PollInterval)
	return srv.serve(ctx, ln, hubLn)
}

// serve handles connections on ln and, when it is not nil, hubLn, each
// on its own goroutine, until ctx ends. A log stream or a send --wait
// does not hold up other clients; the registry and mailbox lock their
// own state. It returns after every connection is done.
func (s *server) serve(ctx context.Context, ln, hubLn net.Listener) error {
	conns := make(chan accepted)
	errc := make(chan error, 2)
	go accept(ctx, ln, false, conns, errc)
	if hubLn != nil {
		go accept(ctx, hubLn, true, conns, errc)
	}
	var wg sync.WaitGroup
	defer wg.Wait()
	for {
		select {
		case <-ctx.Done():
			return nil
		case err := <-errc:
			if ctx.Err() != nil {
				return nil
			}
			return err
		case c := <-conns:
			wg.Add(1)
			go func() {
				defer wg.Done()
				// Shutdown closes the connection, so a client that never
				// writes a request does not keep Serve from returning.
				stop := context.AfterFunc(ctx, func() { c.conn.Close() })
				defer stop()
				s.handle(c.conn, c.hub)
			}()
		}
	}
}

// listen replaces a stale socket at path and listens on it.
func listen(path string) (net.Listener, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("create socket dir: %w", err)
	}
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("remove stale socket: %w", err)
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		return nil, fmt.Errorf("listen on %s: %w", path, err)
	}
	return ln, nil
}

type accepted struct {
	conn net.Conn
	hub  bool
}

// accept passes each connection on ln to conns until ctx ends.
func accept(ctx context.Context, ln net.Listener, hub bool, conns chan<- accepted, errc chan<- error) {
	go func() {
		<-ctx.Done()
		ln.Close()
	}()
	for {
		conn, err := ln.Accept()
		if err != nil {
			errc <- fmt.Errorf("accept: %w", err)
			return
		}
		select {
		case conns <- accepted{conn: conn, hub: hub}:
		case <-ctx.Done():
			conn.Close()
			return
		}
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
	ctx       context.Context
	reg       *registry.Registry
	box       *mailbox.Mailbox
	log       *eventlog.Log
	leases    *lease.Table
	adapters  []adapter.Adapter
	hubCaller func(string) (string, error)
}

func (s *server) handle(conn net.Conn, hub bool) {
	defer conn.Close()
	reply := func(resp Response) error {
		if hub {
			resp.Protocol = ProtocolVersion
		}
		if err := json.NewEncoder(conn).Encode(resp); err != nil {
			return fmt.Errorf("write reply: %w", err)
		}
		return nil
	}
	var req Request
	if err := json.NewDecoder(conn).Decode(&req); err != nil {
		_ = reply(Response{Error: "bad request"})
		return
	}
	scoped := s
	if hub {
		var err error
		if scoped, err = s.forwarded(req); err != nil {
			_ = reply(Response{Error: err.Error()})
			return
		}
		req.Env = nil
	}
	if req.Op == "log" {
		scoped.streamLog(conn, req, reply)
		return
	}
	_ = reply(scoped.dispatch(req, peerPID(conn)))
}

// forwarded checks a request from the hub socket and returns the server
// scoped to its caller. The caller is the request's Handle, never its Env.
func (s *server) forwarded(req Request) (*server, error) {
	if req.Protocol != ProtocolVersion {
		return nil, fmt.Errorf("hub daemon speaks protocol %d, this client speaks %d", ProtocolVersion, req.Protocol)
	}
	if req.Handle == "" {
		return nil, errors.New("a request on the hub socket must carry the caller handle")
	}
	caller, err := s.hubCaller(req.Handle)
	if err != nil {
		return nil, err
	}
	scoped := *s
	scoped.ctx = adapter.WithForwardedCaller(s.ctx, caller)
	return &scoped, nil
}

// streamLog sends each matching event log line as its own reply. Without
// Follow it ends with an End reply. With Follow it runs until the client
// leaves or the daemon stops.
func (s *server) streamLog(conn net.Conn, req Request, reply func(Response) error) {
	filter := eventlog.Filter{Name: req.Name, MsgID: req.MsgID, Event: req.Event}
	if req.Since != "" {
		ts, err := time.Parse(time.RFC3339Nano, req.Since)
		if err != nil {
			_ = reply(Response{Error: fmt.Sprintf("log since %q is not an RFC3339 time", req.Since)})
			return
		}
		filter.Since = ts
	}
	emit := func(line string) error { return reply(Response{OK: true, Text: line}) }
	if !req.Follow {
		lines, err := s.log.Read(filter)
		if err != nil {
			_ = reply(Response{Error: err.Error()})
			return
		}
		for _, line := range lines {
			if emit(line) != nil {
				return
			}
		}
		_ = reply(Response{OK: true, End: true})
		return
	}
	ctx, cancel := context.WithCancel(s.ctx)
	defer cancel()
	// The client sends nothing after its request, so a read returns only
	// when it leaves.
	go func() {
		_, _ = io.Copy(io.Discard, conn)
		cancel()
	}()
	if err := s.log.Follow(ctx, filter, emit); err != nil {
		_ = reply(Response{Error: err.Error()})
	}
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
	default:
		return s.dispatchMessage(req, pid)
	}
}

// dispatchMessage handles the message and hook ops.
func (s *server) dispatchMessage(req Request, pid int) Response {
	switch req.Op {
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
	case "bypass":
		return s.bypass(req)
	case "requeue":
		return s.requeue(req)
	default:
		if strings.HasPrefix(req.Op, "lease-") {
			return s.lease(req)
		}
		return Response{Error: "unknown op"}
	}
}

// lease handles the lease-* ops. The caller is the stamped handle, as
// whoami resolves it.
func (s *server) lease(req Request) Response {
	var args lease.Args
	if req.Lease != nil {
		args = *req.Lease
	}
	if req.Op == "lease-status" {
		return leaseResponse(s.leases.Status(args))
	}
	caller, _, err := s.reg.WhoAmI(s.ctx, req.Env)
	if err != nil && req.Op != "lease-check" {
		return Response{Error: "identify the lease caller: " + err.Error()}
	}
	var r lease.Reply
	switch req.Op {
	case "lease-claim":
		r, err = s.leases.Claim(caller, args)
	case "lease-extend":
		r, err = s.leases.Extend(caller, args)
	case "lease-leave":
		r, err = s.leases.Leave(caller, args)
	case "lease-check":
		// A caller herdr cannot name holds nothing.
		r, err = s.leases.Check(caller, args)
	case "lease-release-begin":
		r, err = s.leases.BeginRelease(caller, args)
	case "lease-release-abort":
		r, err = s.leases.AbortRelease(caller, args)
	case "lease-release":
		r, err = s.releaseLease(caller, args)
	default:
		return Response{Error: "unknown op"}
	}
	r.Caller = caller
	return leaseResponse(r, err)
}

// releaseLease ends caller's hold, grants the next waiter whose pane is
// not gone, and wakes it with an agentbus message. Pane states are read
// before the table is locked.
func (s *server) releaseLease(caller string, args lease.Args) (lease.Reply, error) {
	st, err := s.leases.Status(lease.Args{Checkout: args.Checkout})
	if err != nil {
		return lease.Reply{}, fmt.Errorf("read the lease queue: %w", err)
	}
	gone := map[string]bool{}
	for _, w := range st.Leases[0].Queue {
		gone[w.Handle] = s.paneGone(w.Handle)
	}
	r, err := s.leases.Release(caller, args, func(handle string) bool { return gone[handle] })
	if err != nil {
		return lease.Reply{}, fmt.Errorf("release: %w", err)
	}
	if r.Granted == "" {
		return r, nil
	}
	// A wake that cannot be queued does not undo the grant; the mailbox
	// logs why.
	_, _ = s.box.Post(s.ctx, "agentbus", r.Granted, lease.WakeText(args.Checkout, *r.Lease.Holder))
	return r, nil
}

// paneGone reports whether every adapter that answers says handle is
// gone. An error or an unknown state is not gone.
func (s *server) paneGone(handle string) bool {
	answered := false
	for _, ad := range s.adapters {
		state, err := ad.State(s.ctx, handle)
		if err != nil {
			continue
		}
		if state != adapter.StateGone {
			return false
		}
		answered = true
	}
	return answered
}

func leaseResponse(r lease.Reply, err error) Response {
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Lease: &r}
}

// bypass records an observe-only SendMessage. The tool call already proceeded.
// Response.Name is the bus name the target maps to, or empty.
func (s *server) bypass(req Request) Response {
	actor := strings.TrimSpace(req.Session)
	if actor == "" {
		actor = "unknown"
	}
	detail := preview(req.Body, bypassPreview)
	if detail == "" {
		detail = "bypass"
	}
	target := strings.TrimSpace(req.Name)
	resolved := s.reg.Lookup(target)
	if err := s.log.Append(eventlog.Event{
		Event:    eventlog.EventBypass,
		Actor:    actor,
		Name:     target,
		Resolved: resolved,
		Detail:   detail,
	}); err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: resolved}
}

// requeue puts back a delivered message that a UserPromptSubmit hook
// blocked because it was merged into a user draft.
func (s *server) requeue(req Request) Response {
	actor := strings.TrimSpace(req.Session)
	if actor == "" {
		actor = "unknown"
	}
	if err := s.box.Requeue(req.Name, actor); err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: req.Name}
}

// preview keeps at most limit bytes and marks a cut with an ellipsis.
func preview(body string, limit int) string {
	if len(body) <= limit {
		return body
	}
	cut := limit
	for cut > 0 && !utf8.RuneStart(body[cut]) {
		cut--
	}
	if cut == 0 {
		cut = limit
	}
	return body[:cut] + "…"
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
	text := ""
	if lines := s.box.Held(); len(lines) > 0 {
		text = strings.Join(lines, "\n") + "\n"
	}
	return Response{OK: true, Rows: agentRows(rows), Text: text}
}

func (s *server) search(req Request) Response {
	rows, err := s.reg.Search(s.ctx, req.Query)
	if err != nil {
		return Response{Error: err.Error()}
	}
	text := ""
	if lines := s.reg.Warnings(); len(lines) > 0 {
		text = strings.Join(lines, "\n") + "\n"
	}
	return Response{OK: true, Rows: agentRows(rows), Text: text}
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
	ttl, err := positiveDuration(req.TTL)
	if err != nil {
		return Response{Error: err.Error()}
	}
	result, err := s.box.Send(s.ctx, req.Env, pid, req.Name, req.Body, mailbox.Options{TTL: ttl, Wait: req.Wait})
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Name: result.ID, State: result.State}
}

func positiveDuration(raw string) (time.Duration, error) {
	if strings.TrimSpace(raw) == "" {
		return 0, nil
	}
	d, err := time.ParseDuration(strings.TrimSpace(raw))
	if err != nil || d <= 0 {
		return 0, errors.New("ttl must be a positive duration")
	}
	return d, nil
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
	text, err := s.box.Status(s.ctx, req.Name)
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
