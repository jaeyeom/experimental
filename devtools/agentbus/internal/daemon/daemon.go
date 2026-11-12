// Package daemon is the agentbus server. It owns the socket and the state directory.
package daemon

import (
	"cmp"
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
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/leasecache"
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
	// LeaseTiming is the soft-deadline timing. Zero fields use the
	// defaults. Its Takeover is lease_takeover: takeovers are carried out
	// only when it is lease.TakeoverOn, and only logged otherwise.
	LeaseTiming lease.Timing
	// LeaseTicks, when not nil, drives the soft-deadline checks instead
	// of a ticker at LeaseTiming.Interval.
	LeaseTicks <-chan time.Time
	// Notify, when not nil, tells the user that a checkout needs adopt
	// after a takeover, or that a queued pause was granted. It runs on
	// the lease watcher or a request and must not block for long.
	Notify func(text string)
	// Waker relaunches a hibernated agent when a message is delivered
	// to it or `agentbus wake` names it. Nil holds such messages as
	// hibernated.
	Waker mailbox.Waker
	// HibernatePolicy, when not nil, makes the memory-pressure
	// hibernation policy from the daemon's own hibernate ops. Serve runs
	// it on PolicyTicks, or every PolicyInterval.
	HibernatePolicy func(hibernate.Bus) *hibernate.Policy
	PolicyInterval  time.Duration
	PolicyTicks     <-chan time.Time
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
	// Hibernate carries the arguments of the hibernate-* ops.
	Hibernate *hibernate.Args `json:"hibernate,omitempty"`
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
	// Hibernate is the answer to a hibernate-* op.
	Hibernate *hibernate.Reply `json:"hibernate,omitempty"`
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
	srv, err := newServer(ctx, dirs.StateDir, cfg, lg)
	if err != nil {
		return err
	}
	srv.leaseCache = leasecache.Cache{Dir: dirs.LeaseCacheDir()}
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
		Detail: "listening; lease_takeover=" + string(srv.timing.Takeover),
	}); err != nil {
		return fmt.Errorf("log start: %w", err)
	}
	started = true

	ticks := cfg.LeaseTicks
	if ticks == nil {
		ticker := time.NewTicker(srv.timing.Interval)
		defer ticker.Stop()
		ticks = ticker.C
	}
	// The polls and lease checks end, and are waited for, before Serve
	// logs stop, so an inventory poll cut short by shutdown does not
	// write the state dir after Serve returns.
	var watching sync.WaitGroup
	defer watching.Wait()
	watchCtx, stopWatching := context.WithCancel(ctx)
	defer stopWatching()
	watching.Add(3)
	go func() {
		defer watching.Done()
		srv.reg.Poll(watchCtx, cfg.PollInterval)
	}()
	go func() {
		defer watching.Done()
		srv.box.Poll(watchCtx, cfg.PollInterval)
	}()
	go func() {
		defer watching.Done()
		srv.watchLeases(watchCtx, ticks)
	}()
	if cfg.HibernatePolicy != nil {
		pol := srv.newPolicy(cfg.HibernatePolicy)
		policyTicks := cfg.PolicyTicks
		if policyTicks == nil {
			ticker := time.NewTicker(cmp.Or(cfg.PolicyInterval, time.Minute))
			defer ticker.Stop()
			policyTicks = ticker.C
		}
		watching.Add(1)
		go func() {
			defer watching.Done()
			pol.Watch(watchCtx, policyTicks)
		}()
	}
	return srv.serve(ctx, ln, hubLn)
}

// newServer opens the registry, mailbox, and lease table in stateDir.
func newServer(ctx context.Context, stateDir string, cfg Config, lg *eventlog.Log) (*server, error) {
	reg, box, err := openState(stateDir, cfg, lg)
	if err != nil {
		return nil, err
	}
	reg.Now = cfg.Now
	if cfg.Waker != nil {
		box.SetWaker(cfg.Waker)
	}
	leases, err := lease.Open(stateDir, lg, cfg.Now, func(handle string) string {
		if names := reg.NamesOf(handle); len(names) > 0 {
			return names[0]
		}
		return ""
	})
	if err != nil {
		return nil, fmt.Errorf("open leases: %w", err)
	}
	return &server{
		ctx: ctx, reg: reg, box: box, log: lg, leases: leases,
		adapters: cfg.Adapters, timing: cfg.LeaseTiming.WithDefaults(), notify: cfg.Notify,
	}, nil
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
	timing    lease.Timing
	notify    func(text string)
	// leaseCache is the lease hook's cache of "you hold it" answers on
	// this machine. A change that can take that answer away drops it.
	leaseCache leasecache.Cache
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
	case "wake":
		return s.wakeAgent(req)
	default:
		if strings.HasPrefix(req.Op, "lease-") {
			return s.lease(req)
		}
		if strings.HasPrefix(req.Op, "hibernate-") {
			return s.hibernateOp(req)
		}
		return Response{Error: "unknown op"}
	}
}

// lease handles the lease-* ops, each only on a checkout sameMachine
// allows.
func (s *server) lease(req Request) Response {
	var args lease.Args
	if req.Lease != nil {
		args = *req.Lease
	}
	if err := s.sameMachine(req, args.Checkout); err != nil {
		return Response{Error: err.Error()}
	}
	return s.leaseOp(req, args)
}

// leaseOp runs one lease-* op. The caller is the stamped handle, as
// whoami resolves it.
func (s *server) leaseOp(req Request, args lease.Args) Response {
	switch req.Op {
	case "lease-status":
		r, err := s.leases.Status(args)
		r.Takeover = s.timing.Takeover
		return leaseResponse(r, err)
	case "lease-adopt-begin", "lease-adopt":
		return s.leaseAdopt(req, args)
	case "lease-pause", "lease-unshare", "lease-resume-begin", "lease-resume-abort", "lease-resume":
		return s.leaseUser(req, args)
	}
	caller, _, err := s.reg.WhoAmI(s.ctx, req.Env)
	if err != nil && req.Op != "lease-check" {
		return Response{Error: "identify the lease caller: " + err.Error()}
	}
	var r lease.Reply
	switch req.Op {
	case "lease-claim":
		r, err = s.leases.Claim(caller, args)
		s.tellWaiting(r)
	case "lease-extend":
		r, err = s.leases.Extend(caller, args)
	case "lease-leave":
		r, err = s.leases.Leave(caller, args)
	case "lease-check":
		r, err = s.leaseCheck(req, caller, args)
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

// leaseCheck answers whether caller may change the checkout, and logs
// a denial of the lease hook's check. A caller herdr cannot name holds
// nothing.
func (s *server) leaseCheck(req Request, caller string, args lease.Args) (lease.Reply, error) {
	r, err := s.leases.Check(caller, args)
	if err == nil && args.Hook && r.Result == lease.ResultDenied {
		s.logHookDeny(req, caller, args, r)
	}
	return r, err //nolint:wrapcheck // the table's errors are the reply
}

// logHookDeny logs the lease hook's denied tool call: the caller, the
// holder and state, the tool and its target, and, for a caller on
// another machine, that machine. A log that cannot be written does not
// change the answer.
func (s *server) logHookDeny(req Request, caller string, args lease.Args, r lease.Reply) {
	var detail []string
	// caller is the trusted form of the remote handle.
	if _, remote := s.remoteCaller(req.Env); remote && caller != "" {
		detail = append(detail, "machine "+handleMachine(caller))
	}
	holder, state := "none", lease.StateFree
	if r.Lease != nil {
		state = r.Lease.State
		if r.Lease.Holder != nil {
			holder = r.Lease.Holder.Handle
		}
	}
	detail = append(detail, "tool "+args.Tool, "target "+args.Target, "holder "+holder, "state "+string(state))
	actor := caller
	if actor == "" {
		actor = "unknown"
	}
	_ = s.log.Append(eventlog.Event{
		Event:  eventlog.EventLeaseHookDeny,
		Actor:  actor,
		Name:   args.Checkout,
		Detail: strings.Join(detail, "; "),
	})
}

// sameMachine refuses a lease change by a caller on another machine to
// a checkout keyed to a machine other than its own: such a caller runs
// the git and process steps on its own machine only. The user and
// callers on this machine keep their rules. Status and check change
// nothing, so they are not refused.
func (s *server) sameMachine(req Request, checkout string) error {
	if req.Op == "lease-status" || req.Op == "lease-check" {
		return nil
	}
	caller, remote := s.remoteCaller(req.Env)
	if !remote {
		return nil
	}
	// A checkout key is <machine>:<path>.
	machine := handleMachine(caller)
	keyMachine, _, _ := strings.Cut(checkout, ":")
	if machine == "" || keyMachine != machine {
		return fmt.Errorf("checkout %s is not on machine %s; %s may change leases only on its own machine's checkouts", checkout, machine, caller)
	}
	return nil
}

// remoteCaller returns the handle of a caller on another machine: the
// hub socket's forwarded caller, or AGENTBUS_AS_HANDLE from `agentbus
// lease forward`, which identifying the caller trusts only over ssh.
func (s *server) remoteCaller(env []string) (string, bool) {
	if caller, ok := adapter.ForwardedCaller(s.ctx); ok {
		return caller, true
	}
	caller := envValue(env, "AGENTBUS_AS_HANDLE")
	return caller, caller != ""
}

// tellWaiting sends the holder the notice of a claim that newly queued
// behind it, if the table made one. It runs after the table is
// unlocked, so a slow delivery cannot block claims.
func (s *server) tellWaiting(r lease.Reply) {
	if r.Waiting != nil {
		// A notice that cannot be queued is logged by the mailbox.
		_, _ = s.box.Post(s.ctx, "agentbus", r.Waiting.To, r.Waiting.Text)
	}
}

// releaseLease ends caller's hold, grants the next waiter whose pane is
// not gone, and wakes it with an agentbus message. Pane states are read
// before the table is locked.
func (s *server) releaseLease(caller string, args lease.Args) (lease.Reply, error) {
	gone, err := s.goneWaiters(args.Checkout)
	if err != nil {
		return lease.Reply{}, err
	}
	r, err := s.leases.Release(caller, args, func(handle string) bool { return gone[handle] })
	if err != nil {
		return lease.Reply{}, fmt.Errorf("release: %w", err)
	}
	s.dropCache(args.Checkout)
	s.wake(args.Checkout, r)
	return r, nil
}

// wake tells the waiter a release or resume granted the checkout to:
// an agent by an agentbus message, the user, who has no pane, by a
// note.
func (s *server) wake(checkout string, r lease.Reply) {
	switch r.Granted {
	case "":
	case lease.UserHolder:
		if s.notify != nil {
			s.notify(lease.PausedNote(checkout))
		}
	default:
		// A wake that cannot be queued does not undo the grant; the
		// mailbox logs why.
		_, _ = s.box.Post(s.ctx, "agentbus", r.Granted, lease.WakeText(checkout, *r.Lease.Holder))
	}
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

// pane reads handle's state for the soft deadline. The first adapter
// that knows the pane decides. An error or unknown state from any
// adapter before that is unknown, so an unreachable machine is never
// taken for gone. Gone needs every adapter to say gone.
func (s *server) pane(handle string) lease.Pane {
	gone := false
	for _, ad := range s.adapters {
		state, err := ad.State(s.ctx, handle)
		switch {
		case err != nil || state == adapter.StateUnknown:
			return lease.PaneUnknown
		case state == adapter.StateWorking || state == adapter.StateBlocked:
			return lease.PaneBusy
		case state == adapter.StateGone:
			gone = true
		default:
			// idle, done, no-agent
			return lease.PaneIdle
		}
	}
	if gone {
		return lease.PaneGone
	}
	return lease.PaneUnknown
}

// watchLeases runs the soft-deadline checks on each tick until ctx ends.
func (s *server) watchLeases(ctx context.Context, ticks <-chan time.Time) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticks:
			s.checkLeases()
		}
	}
}

// checkLeases applies the soft-deadline rules to every watched holder.
// It reads each pane state with the lease table unlocked, lets the
// table decide, then sends the check-in and asks for the takeover. A
// check that fails to save is tried again on the next tick. With
// lease_takeover "log" the table never asks for a takeover; it logs a
// lease_would_take_over event once per holder instead.
func (s *server) checkLeases() {
	for _, w := range s.leases.Watched() {
		if s.ctx.Err() != nil {
			return
		}
		d, err := s.leases.Observe(w, s.pane(w.Holder), s.timing)
		if err != nil {
			continue
		}
		if d.CheckIn != "" {
			// A check-in that cannot be queued is logged by the mailbox.
			_, _ = s.box.Post(s.ctx, "agentbus", w.Holder, d.CheckIn)
		}
		if d.TakeOver != "" {
			s.takeOver(w, d.TakeOver)
		}
	}
}

// takeOver hands w's checkout to the next live waiter as needs-adopt:
// w's holder was idle and silent for the grace after a check-in, or its
// pane, or an adopter's, is gone with a waiter queued. The daemon runs
// no git; it tells the old holder where its work will be saved, wakes
// the adopter, and notes the user. A takeover that fails to save is
// asked for again on the next check.
func (s *server) takeOver(w lease.Watch, reason string) {
	gone, err := s.goneWaiters(w.Checkout)
	if err != nil {
		return
	}
	tk, err := s.leases.TakeOver(w, reason, func(handle string) bool { return gone[handle] })
	if err != nil || !tk.Done {
		return
	}
	s.dropCache(w.Checkout)
	// Messages that cannot be queued are logged by the mailbox.
	if tk.FromHeld {
		_, _ = s.box.Post(s.ctx, "agentbus", tk.From, lease.TakenOverText(w.Checkout, tk.BackupRef))
	}
	if tk.To != "" && tk.To != lease.UserHolder {
		_, _ = s.box.Post(s.ctx, "agentbus", tk.To, lease.AdoptWakeText(w.Checkout, tk.From, tk.BackupRef))
	}
	if s.notify != nil {
		s.notify(lease.NeedsAdoptNote(tk))
	}
}

// dropCache forgets the lease hook's cached answers for checkout: its
// holder, or a pane the user's pause was shared with, may have just
// lost it. A cache that cannot be written is left to expire.
func (s *server) dropCache(checkout string) {
	_ = s.leaseCache.Drop(checkout)
}

// goneWaiters reads whether each waiter on checkout has a gone pane,
// before the table is locked.
func (s *server) goneWaiters(checkout string) (map[string]bool, error) {
	st, err := s.leases.Status(lease.Args{Checkout: checkout})
	if err != nil {
		return nil, fmt.Errorf("read the lease queue: %w", err)
	}
	gone := map[string]bool{}
	for _, w := range st.Leases[0].Queue {
		gone[w.Handle] = s.paneGone(w.Handle)
	}
	return gone, nil
}

// leaseAdopt handles the two adopt ops: begin, before the CLI's process
// check and git steps, and adopt, after them.
func (s *server) leaseAdopt(req Request, args lease.Args) Response {
	caller, err := s.adoptCaller(req.Env)
	if err != nil {
		return Response{Error: err.Error()}
	}
	var r lease.Reply
	if req.Op == "lease-adopt-begin" {
		r, err = s.leases.BeginAdopt(caller, args)
	} else {
		r, err = s.leases.Adopt(caller, args)
	}
	r.Caller = caller
	return leaseResponse(r, err)
}

// adoptCaller identifies the caller of an adopt. A caller outside herdr,
// or in a pane that runs no agent, is the user, who may always adopt. A
// request on the hub socket always comes from a pane: its handle.
func (s *server) adoptCaller(env []string) (string, error) {
	_, forwarded := adapter.ForwardedCaller(s.ctx)
	if !forwarded && envValue(env, "HERDR_PANE_ID") == "" && envValue(env, "AGENTBUS_AS_HANDLE") == "" {
		return lease.UserHolder, nil
	}
	caller, _, err := s.reg.WhoAmI(s.ctx, env)
	if err != nil {
		return "", fmt.Errorf("identify the lease caller: %w", err)
	}
	for _, ad := range s.adapters {
		if state, err := ad.State(s.ctx, caller); err == nil && state == adapter.StateNoAgent {
			return lease.UserHolder, nil
		}
	}
	return caller, nil
}

// leaseUser handles the user's ops: pause, unshare, and resume's begin,
// abort, and finish. They are refused from a pane that runs an agent.
func (s *server) leaseUser(req Request, args lease.Args) Response {
	caller, err := s.adoptCaller(req.Env)
	if err != nil {
		return Response{Error: err.Error()}
	}
	if caller != lease.UserHolder {
		verb := strings.TrimPrefix(strings.TrimSuffix(strings.TrimSuffix(req.Op, "-begin"), "-abort"), "lease-")
		return Response{Error: fmt.Sprintf("lease %s is for the user; %s runs an agent. Run it from a plain terminal pane or outside herdr", verb, caller)}
	}
	if req.Op == "lease-pause" || req.Op == "lease-unshare" {
		if args.With, err = s.withHandles(req.Env, args.With); err != nil {
			return Response{Error: err.Error()}
		}
	}
	var r lease.Reply
	switch req.Op {
	case "lease-pause":
		if r, err = s.leases.Pause(args); err == nil && r.Asked != "" {
			// An ask that cannot be queued is logged by the mailbox.
			_, _ = s.box.Post(s.ctx, "agentbus", r.Asked, lease.PauseAskText(args.Checkout))
		}
	case "lease-unshare":
		if r, err = s.leases.Unshare(args); err == nil {
			s.dropCache(args.Checkout)
		}
	case "lease-resume-begin":
		r, err = s.leases.BeginResume(args)
	case "lease-resume-abort":
		r, err = s.leases.AbortResume(args)
	default:
		r, err = s.resumeLease(args)
	}
	r.Caller = caller
	return leaseResponse(r, err)
}

// resumeLease ends the user's hold and grants and wakes the next live
// waiter, like releaseLease.
func (s *server) resumeLease(args lease.Args) (lease.Reply, error) {
	gone, err := s.goneWaiters(args.Checkout)
	if err != nil {
		return lease.Reply{}, err
	}
	r, err := s.leases.Resume(args, func(handle string) bool { return gone[handle] })
	if err != nil {
		return lease.Reply{}, fmt.Errorf("resume: %w", err)
	}
	s.dropCache(args.Checkout)
	s.wake(args.Checkout, r)
	return r, nil
}

// withHandles resolves pause --with and unshare targets to handles: a
// registered name, a handle, or a herdr pane id on this machine. A
// caller on another machine has resolved its own pane ids already, so
// its leftover targets are not looked up as this machine's panes.
func (s *server) withHandles(env []string, targets []string) ([]string, error) {
	_, remote := s.remoteCaller(env)
	out := make([]string, 0, len(targets))
	for _, target := range targets {
		if handle, err := s.reg.Resolve(target); err == nil {
			out = append(out, handle)
			continue
		}
		if remote {
			return nil, fmt.Errorf("--with %s is not a registered name, a handle, or an agent pane on your machine", target)
		}
		paneEnv := []string{"HERDR_PANE_ID=" + target}
		for _, kv := range env {
			if !strings.HasPrefix(kv, "HERDR_PANE_ID=") && !strings.HasPrefix(kv, "AGENTBUS_AS_HANDLE=") {
				paneEnv = append(paneEnv, kv)
			}
		}
		handle, _, err := s.reg.WhoAmI(s.ctx, paneEnv)
		if err != nil {
			return nil, fmt.Errorf("--with %s is not a registered name, a handle, or an agent pane: %w", target, err)
		}
		out = append(out, handle)
	}
	return out, nil
}

func envValue(env []string, key string) string {
	for _, kv := range env {
		if v, ok := strings.CutPrefix(kv, key+"="); ok {
			return v
		}
	}
	return ""
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
