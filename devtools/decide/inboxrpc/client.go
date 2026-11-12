package inboxrpc

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

const (
	// DefaultTimeout bounds one request on a connected session.
	DefaultTimeout = 20 * time.Second
	// firstBackoff and maxBackoff space reconnect attempts.
	firstBackoff = time.Second
	maxBackoff   = 30 * time.Second
	// stderrTail is how much of the server's stderr an error quotes.
	stderrTail = 1024
)

// Conn is one session to the server: requests go to W and responses
// come from R. Close ends the session.
type Conn struct {
	W     io.Writer
	R     io.Reader
	Close func()
}

// Dialer starts a session.
type Dialer func() (*Conn, error)

// HubDialer runs `decide serve --stdio` on hub over one ssh session.
// The session ends with an error that carries the server's stderr.
// stdin and stdout are OS pipes: exec would otherwise wait on a stdin
// copy that never ends, and ssh's exit would never reach the client.
func HubDialer(hub hubcall.Hub) Dialer {
	return func() (*Conn, error) {
		inR, inW, err := os.Pipe()
		if err != nil {
			return nil, fmt.Errorf("stdin pipe: %w", err)
		}
		outR, outW, err := os.Pipe()
		if err != nil {
			inR.Close()
			inW.Close()
			return nil, fmt.Errorf("stdout pipe: %w", err)
		}
		ctx, cancel := context.WithCancel(context.Background())
		stderr := &tail{}
		ended := &endedReader{r: outR}
		go func() {
			code, err := hub.Run(ctx, hubcall.Call{Args: []string{"serve", "--stdio"}, Stdin: inR, Stdout: outW, Stderr: stderr})
			if err == nil {
				err = fmt.Errorf("decide serve --stdio ended with exit %d", code)
				if msg := stderr.String(); msg != "" {
					err = fmt.Errorf("%w: %s", err, msg)
				}
			}
			ended.set(err)
			inR.Close()
			outW.Close()
		}()
		return &Conn{W: inW, R: ended, Close: func() {
			_ = inW.Close()
			cancel()
			_ = outR.Close()
		}}, nil
	}
}

// endedReader reads a session's stdout and, at its end, returns why
// the session ended instead of io.EOF.
type endedReader struct {
	r   io.ReadCloser
	mu  sync.Mutex
	err error
}

func (e *endedReader) set(err error) {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.err = err
}

func (e *endedReader) Read(p []byte) (int, error) {
	n, err := e.r.Read(p)
	if err == io.EOF {
		e.mu.Lock()
		defer e.mu.Unlock()
		if e.err != nil {
			return n, e.err
		}
	}
	return n, err //nolint:wrapcheck // a reader passes its errors through
}

// Client is a tui.Backend on the hub's ledger. Label names the hub in
// messages. Machine is this machine's herdr label, and HubMachine the
// hub's label in this machine's herdr; an owner pane on the hub is
// focused through `herdr --machine <HubMachine>`. Herdr replaces
// decide.ExecHerdr and Now time.Now.
type Client struct {
	Dial       Dialer
	Label      string
	Machine    string
	HubMachine string
	Herdr      decide.HerdrCall
	Timeout    time.Duration
	Now        func() time.Time

	mu      sync.Mutex
	sess    *session
	dialing bool
	nextID  int64
	retryAt time.Time
	backoff time.Duration
}

var _ tui.Backend = (*Client)(nil)

// hubError is a call the hub did not take because it could not be
// reached. It matches tui.ErrUnreachable.
type hubError struct {
	msg string
	err error
}

func (e *hubError) Error() string { return e.msg }

func (e *hubError) Unwrap() error { return e.err }

func (e *hubError) Is(target error) bool { return target == tui.ErrUnreachable }

// Connect starts the session and checks the server's protocol.
func (c *Client) Connect() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.sess != nil {
		return nil
	}
	sess, err := c.open()
	if err != nil {
		return err
	}
	c.sess = sess
	return nil
}

// Close ends the session.
func (c *Client) Close() {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.sess != nil {
		c.sess.close(errors.New("closed"))
		c.sess = nil
	}
}

func (c *Client) now() time.Time {
	if c.Now != nil {
		return c.Now()
	}
	return time.Now()
}

func (c *Client) timeout() time.Duration {
	if c.Timeout > 0 {
		return c.Timeout
	}
	return DefaultTimeout
}

// open dials and says hello. The caller holds c.mu or owns c.dialing.
func (c *Client) open() (*session, error) {
	conn, err := c.Dial()
	if err != nil {
		return nil, c.unreachable(err)
	}
	sess := newSession(conn)
	resp, err := sess.do(context.Background(), Request{ID: 0, Op: OpHello}, c.timeout())
	if err != nil {
		sess.close(err)
		return nil, c.unreachable(err)
	}
	if resp.Error != "" || resp.Version != Version {
		sess.close(errors.New("protocol"))
		return nil, fmt.Errorf("hub %s: decide serve speaks protocol %d, want %d; update decide on the hub", c.Label, resp.Version, Version)
	}
	return sess, nil
}

// unreachable is a failed dial. hubcall's own error already names the hub.
func (c *Client) unreachable(err error) error {
	var ue *hubcall.UnreachableError
	if errors.As(err, &ue) {
		return &hubError{msg: ue.Error(), err: err}
	}
	return &hubError{msg: fmt.Sprintf("hub %s: %v", c.Label, err), err: err}
}

// session is the connected session. While there is none, a write fails
// at once. A list, the inbox's periodic read, dials again once the
// backoff has passed.
func (c *Client) session(op string) (*session, error) {
	c.mu.Lock()
	if c.sess != nil {
		defer c.mu.Unlock()
		return c.sess, nil
	}
	if writeOps[op] {
		c.mu.Unlock()
		return nil, &hubError{msg: fmt.Sprintf("hub %s unreachable; %s not sent", c.Label, op)}
	}
	if op != OpList || c.dialing || c.now().Before(c.retryAt) {
		c.mu.Unlock()
		return nil, &hubError{msg: fmt.Sprintf("hub %s unreachable; retrying", c.Label)}
	}
	c.dialing = true
	c.mu.Unlock()
	sess, err := c.open()
	c.mu.Lock()
	defer c.mu.Unlock()
	c.dialing = false
	if err != nil {
		c.failed()
		return nil, err
	}
	c.sess = sess
	c.backoff = 0
	return sess, nil
}

// failed schedules the next reconnect. The caller holds c.mu.
func (c *Client) failed() {
	c.backoff = min(max(2*c.backoff, firstBackoff), maxBackoff)
	c.retryAt = c.now().Add(c.backoff)
}

// drop forgets sess after it broke and schedules a reconnect.
func (c *Client) drop(sess *session, err error) {
	sess.close(err)
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.sess == sess {
		c.sess = nil
		c.failed()
	}
}

// call sends one request and waits for its response. A response error
// is a plain error; a broken session is a hubError.
func (c *Client) call(req Request) (Response, error) {
	return c.callCtx(context.Background(), req)
}

// callCtx is call that stops waiting when ctx is done. That leaves the
// session up.
func (c *Client) callCtx(ctx context.Context, req Request) (Response, error) {
	sess, err := c.session(req.Op)
	if err != nil {
		return Response{}, err
	}
	c.mu.Lock()
	c.nextID++
	req.ID = c.nextID
	c.mu.Unlock()
	resp, err := sess.do(ctx, req, c.timeout())
	var gaveUp *callerGaveUp
	if errors.As(err, &gaveUp) {
		return Response{}, err
	}
	if err != nil {
		c.drop(sess, err)
		what := "retrying"
		if writeOps[req.Op] {
			what = req.Op + " not confirmed"
		}
		return Response{}, &hubError{msg: fmt.Sprintf("hub %s unreachable (%v); %s", c.Label, err, what), err: err}
	}
	if resp.Error != "" {
		return resp, errors.New(resp.Error)
	}
	return resp, nil
}

func (c *Client) item(req Request) (decide.Item, error) {
	resp, err := c.call(req)
	if err != nil {
		return decide.Item{}, err
	}
	if resp.Item == nil {
		return decide.Item{}, fmt.Errorf("hub %s: %s returned no item", c.Label, req.Op)
	}
	if resp.NotifyError != "" {
		return *resp.Item, &tui.NotifyError{Err: errors.New(resp.NotifyError)}
	}
	return *resp.Item, nil
}

// List implements tui.Backend.
func (c *Client) List() ([]decide.Item, error) {
	resp, err := c.call(Request{Op: OpList})
	if err != nil {
		return nil, err
	}
	return resp.Items, nil
}

// Get is one item.
func (c *Client) Get(id string) (decide.Item, error) {
	return c.item(Request{Op: OpShow, Item: id})
}

// MarkRead implements tui.Backend.
func (c *Client) MarkRead(id string) (decide.Item, error) {
	return c.item(Request{Op: OpMarkRead, Item: id})
}

// Answer implements tui.Backend. The hub records via ui.
func (c *Client) Answer(id, value string) (decide.Item, error) {
	return c.item(Request{Op: OpAnswer, Item: id, Text: value})
}

// Reply implements tui.Backend.
func (c *Client) Reply(id, text string) (decide.Item, error) {
	return c.item(Request{Op: OpReply, Item: id, Text: text})
}

// Done implements tui.Backend.
func (c *Client) Done(id, outcome string) (decide.Item, error) {
	return c.item(Request{Op: OpDone, Item: id, Text: outcome})
}

// Archive marks a notification done with the outcome "archived".
func (c *Client) Archive(id string) (decide.Item, error) {
	return c.item(Request{Op: OpArchive, Item: id})
}

// Withdraw withdraws item id with reason.
func (c *Client) Withdraw(id, reason string) (decide.Item, error) {
	return c.item(Request{Op: OpWithdraw, Item: id, Text: reason})
}

// Snooze implements tui.Backend.
func (c *Client) Snooze(id string, until time.Time) (decide.Item, error) {
	return c.item(Request{Op: OpSnooze, Item: id, Until: &until})
}

// Unsnooze implements tui.Backend.
func (c *Client) Unsnooze(id string) (decide.Item, error) {
	return c.item(Request{Op: OpUnsnooze, Item: id})
}

// Status implements tui.Backend.
func (c *Client) Status() (tui.StatusBoard, error) {
	resp, err := c.call(Request{Op: OpStatus})
	if err != nil {
		return tui.StatusBoard{}, err
	}
	if resp.Status == nil {
		return tui.StatusBoard{}, nil
	}
	return *resp.Status, nil
}

// Targets are the hub's configured recipients and whether it adds
// agentbus names.
func (c *Client) Targets() ([]tui.Target, bool, error) {
	resp, err := c.call(Request{Op: OpTargets})
	if err != nil {
		return nil, false, err
	}
	var out []tui.Target
	for _, tg := range resp.Targets {
		out = append(out, tui.Target{Name: tg.Name, Pane: tg.Pane})
	}
	return out, resp.Agentbus, nil
}

// BusRunner runs `agentbus list --json` on the hub, the only agentbus
// call the inbox makes outside the backend.
func (c *Client) BusRunner(ctx context.Context, args []string) ([]byte, error) {
	if !slices.Equal(args, []string{"list", "--json"}) {
		return nil, fmt.Errorf("agentbus %s does not run on the hub from the inbox", strings.Join(args, " "))
	}
	resp, err := c.callCtx(ctx, Request{Op: OpBusList})
	if err != nil {
		return nil, err
	}
	return []byte(resp.Output), nil
}

// Send implements tui.Backend. The hub looks the recipient up by name.
func (c *Client) Send(forward string, tg tui.Target, text string) error {
	_, err := c.call(Request{Op: OpSend, To: tg.Name, Bus: tg.Bus, Forward: forward, Text: text})
	return err
}

// OwnerPane implements tui.Backend.
func (c *Client) OwnerPane(it *decide.Item) decide.OwnerLocation {
	return c.local(decide.OwnerPane(it, ""))
}

// Locate implements tui.Backend. The hub asks its herdr; an
// unreachable hub leaves the stored pane unknown.
func (c *Client) Locate(ctx context.Context, it *decide.Item) decide.OwnerLocation {
	resp, err := c.callCtx(ctx, Request{Op: OpLocate, Item: it.ID})
	if err != nil || resp.Location == nil {
		return c.OwnerPane(it)
	}
	return c.local(*resp.Location)
}

// local turns a location seen from the hub into one seen from here:
// the hub's own machine becomes HubMachine, and this machine's label
// becomes empty.
func (c *Client) local(loc decide.OwnerLocation) decide.OwnerLocation {
	switch {
	case loc.Pane == "" || strings.HasPrefix(loc.Pane, decide.BusPanePrefix):
	case loc.Machine == "":
		loc.Machine = c.hubMachine()
	case loc.Machine == c.Machine:
		loc.Machine = ""
	}
	return loc
}

// hubMachine is the hub's herdr label, or its ssh target when the
// config does not name one.
func (c *Client) hubMachine() string {
	if c.HubMachine != "" {
		return c.HubMachine
	}
	return c.Label
}

// Focus implements tui.Backend. herdr runs here, with --machine for a
// pane on another machine.
func (c *Client) Focus(ctx context.Context, loc decide.OwnerLocation) string {
	if c.HubMachine == "" && loc.Machine == c.Label && loc.State == decide.LocationLive {
		return "owner is on the hub " + c.Label + `; set "hubMachine" in the decide config to its herdr machine label to jump there`
	}
	msg := decide.LocationResolver{Run: c.Herdr}.Focus(ctx, loc)
	if loc.Machine != "" && strings.HasSuffix(msg, "not focusable from here") {
		msg += " (is " + loc.Machine + " saved in herdr?)"
	}
	return msg
}

// session is one connection with its requests in flight.
type session struct {
	conn    *Conn
	wmu     sync.Mutex
	mu      sync.Mutex
	pending map[int64]chan Response
	done    chan struct{}
	err     error
	once    sync.Once
}

func newSession(conn *Conn) *session {
	s := &session{conn: conn, pending: map[int64]chan Response{}, done: make(chan struct{})}
	go s.read()
	return s
}

func (s *session) read() {
	sc := bufio.NewScanner(s.conn.R)
	sc.Buffer(make([]byte, 64<<10), maxLine)
	for sc.Scan() {
		var resp Response
		if err := json.Unmarshal(sc.Bytes(), &resp); err != nil {
			continue
		}
		s.mu.Lock()
		ch := s.pending[resp.ID]
		delete(s.pending, resp.ID)
		s.mu.Unlock()
		if ch != nil {
			ch <- resp
		}
	}
	err := sc.Err()
	if err == nil {
		err = errors.New("session ended")
	}
	s.close(err)
}

func (s *session) close(err error) {
	s.once.Do(func() {
		s.mu.Lock()
		s.err = err
		s.mu.Unlock()
		close(s.done)
		if s.conn.Close != nil {
			s.conn.Close()
		}
	})
}

func (s *session) do(ctx context.Context, req Request, timeout time.Duration) (Response, error) {
	line, err := json.Marshal(req)
	if err != nil {
		return Response{}, fmt.Errorf("encode request: %w", err)
	}
	ch := make(chan Response, 1)
	s.mu.Lock()
	if s.err != nil {
		err := s.err
		s.mu.Unlock()
		return Response{}, err
	}
	s.pending[req.ID] = ch
	s.mu.Unlock()
	s.wmu.Lock()
	_, err = s.conn.W.Write(append(line, '\n'))
	s.wmu.Unlock()
	if err != nil {
		return Response{}, fmt.Errorf("send request: %w", err)
	}
	timer := time.NewTimer(timeout)
	defer timer.Stop()
	select {
	case resp := <-ch:
		return resp, nil
	case <-s.done:
		s.mu.Lock()
		defer s.mu.Unlock()
		return Response{}, s.err
	case <-timer.C:
		return Response{}, fmt.Errorf("no answer in %s", timeout)
	case <-ctx.Done():
		s.mu.Lock()
		delete(s.pending, req.ID)
		s.mu.Unlock()
		return Response{}, &callerGaveUp{err: ctx.Err()}
	}
}

// callerGaveUp is a request whose caller stopped waiting. The session
// is still good.
type callerGaveUp struct {
	err error
}

func (e *callerGaveUp) Error() string { return "hub request: " + e.err.Error() }

func (e *callerGaveUp) Unwrap() error { return e.err }

// tail keeps the last stderrTail bytes written to it.
type tail struct {
	mu  sync.Mutex
	buf []byte
}

func (t *tail) Write(p []byte) (int, error) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.buf = append(t.buf, p...)
	if len(t.buf) > stderrTail {
		t.buf = t.buf[len(t.buf)-stderrTail:]
	}
	return len(p), nil
}

func (t *tail) String() string {
	t.mu.Lock()
	defer t.mu.Unlock()
	return string(bytes.TrimSpace(t.buf))
}
