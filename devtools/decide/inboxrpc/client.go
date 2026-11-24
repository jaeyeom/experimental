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
// decide.ExecHerdr and Now time.Now. CacheDir overrides the replica
// cache directory when set.
type Client struct {
	Dial       Dialer
	Label      string
	Machine    string
	HubMachine string
	Herdr      decide.HerdrCall
	Timeout    time.Duration
	Now        func() time.Time
	CacheDir   string
	UseOutbox  bool

	mu      sync.Mutex
	sess    *session
	dialing bool
	closed  bool
	nextID  int64
	retryAt time.Time
	backoff time.Duration
	// display is the hub's display settings from the latest connect,
	// and displayGen counts the connects that fetched them.
	display    decide.DisplaySettings
	displayGen uint64

	replicaOnce sync.Once
	replica     *replicaStore
	replicaErr  error

	outboxOnce    sync.Once
	outbox        *outboxStore
	outboxErr     error
	outboxMu      sync.Mutex
	outboxRunning bool
	outboxWake    chan struct{}

	wg sync.WaitGroup

	markReadMu      sync.Mutex
	markReadQueue   []string
	markReadQueued  map[string]bool
	markReadRunning bool
	markReadWake    chan struct{}

	draftFlushMu      sync.Mutex
	draftFlushRunning bool
	draftFlushWake    chan struct{}

	orgTreeUnsupported bool
	orgTreeErr         error
}

var (
	_ tui.Backend        = (*Client)(nil)
	_ tui.OutboxBackend  = (*Client)(nil)
	_ tui.DraftBackend   = (*Client)(nil)
	_ tui.OrgTreeBackend = (*Client)(nil)
)

// hubError is a call the hub did not take because it could not be
// reached. It matches tui.ErrUnreachable.
type hubError struct {
	msg string
	err error
}

func (e *hubError) Error() string { return e.msg }

func (e *hubError) Unwrap() error { return e.err }

func (e *hubError) Is(target error) bool { return target == tui.ErrUnreachable }

func (c *Client) ensureReplica() (*replicaStore, error) {
	c.replicaOnce.Do(func() {
		c.replica, c.replicaErr = newReplicaStore(c.CacheDir, c.Label)
	})
	return c.replica, c.replicaErr
}

func (c *Client) ensureOutbox() (*outboxStore, error) {
	c.outboxOnce.Do(func() {
		c.outbox, c.outboxErr = newOutboxStore(c.CacheDir, c.Label)
	})
	return c.outbox, c.outboxErr
}

// Connect starts the session, checks the server's protocol, and syncs
// the local replica from its last sequence number.
func (c *Client) Connect() error {
	if _, err := c.ensureReplica(); err != nil {
		return err
	}
	if c.UseOutbox {
		if _, err := c.ensureOutbox(); err != nil {
			return err
		}
	}
	c.mu.Lock()
	if c.sess != nil {
		c.mu.Unlock()
		return nil
	}
	c.mu.Unlock()
	sess, display, watchCh, err := c.open()
	if err != nil {
		return err
	}
	c.mu.Lock()
	c.connected(sess, display, watchCh)
	c.mu.Unlock()
	return nil
}

// connected keeps sess, the display settings it fetched, and starts the
// watch loop. The caller holds c.mu.
func (c *Client) connected(sess *session, display decide.DisplaySettings, watchCh <-chan Response) {
	if c.sess != nil || c.closed {
		sess.close(errors.New("superseded"))
		return
	}
	c.sess = sess
	c.display = display
	c.displayGen++
	if watchCh != nil {
		c.wg.Add(1)
		go func() {
			defer c.wg.Done()
			c.watchLoop(sess, watchCh)
		}()
	}
	c.kickMarkRead()
	c.kickDraftFlush()
	if c.UseOutbox {
		c.startOutboxIfPending()
	}
}

// isConnected reports whether the hub session is currently up.
func (c *Client) isConnected() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.sess != nil
}

// SyncStatus implements tui.ReplicaBackend.
func (c *Client) SyncStatus() tui.SyncStatus {
	rep, err := c.ensureReplica()
	var syncedAt time.Time
	if err == nil && rep != nil {
		syncedAt = rep.syncedAt()
	}
	return tui.SyncStatus{
		Connected: c.isConnected(),
		SyncedAt:  syncedAt,
	}
}

// Display is the hub's display settings from the latest connect, and a
// generation that grows with each connect, so a caller sees a
// reconnect's fresh settings. Zero means none were fetched yet.
func (c *Client) Display() (decide.DisplaySettings, uint64) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.display, c.displayGen
}

// Close ends the session and waits for background client goroutines.
func (c *Client) Close() {
	c.mu.Lock()
	c.closed = true
	sess := c.sess
	c.sess = nil
	c.mu.Unlock()
	if sess != nil {
		sess.close(errors.New("closed"))
	}
	c.kickMarkRead()
	c.kickDraftFlush()
	c.kickOutbox()
	c.wg.Wait()
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

// open dials, says hello, fetches display settings, and starts watch
// from the replica's last sequence number.
func (c *Client) open() (*session, decide.DisplaySettings, <-chan Response, error) {
	var none decide.DisplaySettings
	rep, err := c.ensureReplica()
	if err != nil {
		return nil, none, nil, err
	}
	conn, err := c.Dial()
	if err != nil {
		return nil, none, nil, c.unreachable(err)
	}
	sess := newSession(conn)
	resp, err := sess.do(context.Background(), Request{ID: 0, Op: OpHello}, c.timeout())
	if err != nil {
		sess.close(err)
		return nil, none, nil, c.unreachable(err)
	}
	if err := protocolMismatch(c.Label, resp); err != nil {
		sess.close(errors.New("protocol"))
		return nil, none, nil, err
	}
	resp, err = sess.do(context.Background(), Request{ID: 0, Op: OpConfig}, c.timeout())
	if err != nil {
		sess.close(err)
		return nil, none, nil, c.unreachable(err)
	}
	if resp.Error != "" {
		sess.close(errors.New("config"))
		return nil, none, nil, fmt.Errorf("hub %s: display settings: %s", c.Label, resp.Error)
	}
	display := none
	if resp.Display != nil {
		display = *resp.Display
	}
	watchResp, watchCh, err := c.startWatchWithResync(sess, rep)
	if err != nil {
		return nil, none, nil, err
	}
	if hubOrgTreeEnabled(display) || watchResp.OrgSpecModTime != nil {
		c.refreshOrgTree(sess, rep)
	}
	return sess, display, watchCh, nil
}

func (c *Client) startWatchWithResync(sess *session, rep *replicaStore) (Response, <-chan Response, error) {
	watchResp, watchCh, err := sess.startWatch(context.Background(), c.allocID(), rep.seq(), c.timeout())
	if err != nil {
		sess.close(err)
		return Response{}, nil, c.unreachable(err)
	}
	if watchResp.Error != "" {
		if strings.HasPrefix(watchResp.Error, "unknown op ") {
			return watchResp, nil, nil
		}
		sess.close(errors.New("watch"))
		return Response{}, nil, fmt.Errorf("hub %s: watch: %s", c.Label, watchResp.Error)
	}
	if rep.applySync(watchResp, c.now()) {
		watchResp, watchCh, err = sess.startWatch(context.Background(), c.allocID(), 0, c.timeout())
		if err != nil {
			sess.close(err)
			return Response{}, nil, c.unreachable(err)
		}
		if watchResp.Error != "" {
			sess.close(errors.New("watch"))
			return Response{}, nil, fmt.Errorf("hub %s: watch: %s", c.Label, watchResp.Error)
		}
		rep.applySync(watchResp, c.now())
	}
	return watchResp, watchCh, nil
}

func (c *Client) allocID() int64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.nextID++
	return c.nextID
}

// protocolMismatch reports a hello the two sides cannot share. A hub
// error, including one too old to answer hello with a version, keeps
// that text and names the hub. A version gap names the side that is
// behind.
func protocolMismatch(label string, resp Response) error {
	if resp.Error != "" {
		return fmt.Errorf("hub %s: %s; update decide on the hub", label, resp.Error)
	}
	if resp.Version == Version {
		return nil
	}
	if resp.Version > Version {
		return fmt.Errorf("hub %s: decide serve speaks protocol %d, this decide speaks %d; update decide on this machine", label, resp.Version, Version)
	}
	return fmt.Errorf("hub %s: decide serve speaks protocol %d, this decide speaks %d; update decide on the hub", label, resp.Version, Version)
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
// at once. A non-write op returns unreachable and triggers a background
// reconnect once the backoff has passed.
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
	c.mu.Unlock()
	c.maybeReconnect()
	return nil, &hubError{msg: fmt.Sprintf("hub %s unreachable; retrying", c.Label)}
}

// maybeReconnect starts a background reconnect if disconnected and past backoff.
func (c *Client) maybeReconnect() {
	c.mu.Lock()
	if c.sess != nil || c.closed || c.dialing || c.now().Before(c.retryAt) {
		c.mu.Unlock()
		return
	}
	c.dialing = true
	c.wg.Add(1)
	c.mu.Unlock()
	go func() {
		defer c.wg.Done()
		c.reconnect()
	}()
}

func (c *Client) reconnect() {
	sess, display, watchCh, err := c.open()
	c.mu.Lock()
	defer c.mu.Unlock()
	c.dialing = false
	if err != nil {
		c.failed()
		return
	}
	if c.closed {
		sess.close(errors.New("closed"))
		return
	}
	c.connected(sess, display, watchCh)
	c.backoff = 0
}

func (c *Client) watchLoop(sess *session, watchCh <-chan Response) {
	rep, err := c.ensureReplica()
	if err != nil {
		return
	}
	for {
		select {
		case <-sess.done:
			sess.mu.Lock()
			serr := sess.err
			sess.mu.Unlock()
			c.drop(sess, serr)
			return
		case resp := <-watchCh:
			if resp.Error != "" {
				c.drop(sess, errors.New(resp.Error))
				return
			}
			if rep.applySync(resp, c.now()) {
				resyncResp, newCh, err := sess.startWatch(context.Background(), c.allocID(), 0, c.timeout())
				if err != nil {
					c.drop(sess, err)
					return
				}
				if resyncResp.Error != "" {
					c.drop(sess, errors.New(resyncResp.Error))
					return
				}
				rep.applySync(resyncResp, c.now())
				watchCh = newCh
			}
			if resp.OrgSpecModTime != nil {
				tree, mod := rep.orgTree()
				if tree == "" || !resp.OrgSpecModTime.Equal(mod) {
					c.refreshOrgTree(sess, rep)
				}
			}
		}
	}
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
		if rep, rerr := c.ensureReplica(); rerr == nil && rep != nil {
			rep.upsertLocal(*resp.Item)
		}
		return *resp.Item, &tui.NotifyError{Err: errors.New(resp.NotifyError)}
	}
	if rep, rerr := c.ensureReplica(); rerr == nil && rep != nil {
		rep.upsertLocal(*resp.Item)
	}
	return *resp.Item, nil
}

// List implements tui.Backend. It reads from the local replica without
// waiting on the hub, and triggers a background reconnect when down.
func (c *Client) List() ([]decide.Item, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return nil, err
	}
	c.mu.Lock()
	connected := c.sess != nil
	closed := c.closed
	c.mu.Unlock()
	if connected && !closed {
		c.wg.Add(1)
		go func() {
			defer c.wg.Done()
			c.refreshDisplay()
		}()
	} else {
		c.maybeReconnect()
	}
	return rep.items(), nil
}

// SetAvatar saves choice for name on the hub and returns its display
// settings. A hub that does not know the op says to update decide there.
// A transport failure is a hub error and does not write locally.
func (c *Client) SetAvatar(name, choice string) (decide.DisplaySettings, error) {
	if c.UseOutbox {
		if _, err := c.enqueueOutbox(tui.OutboxEntry{Op: OpSetAvatar, Name: name, Choice: choice}); err != nil {
			return decide.DisplaySettings{}, err
		}
		d, _ := c.Display()
		return d, nil
	}
	resp, err := c.call(Request{Op: OpSetAvatar, Name: name, Choice: choice})
	if err != nil {
		if strings.HasPrefix(err.Error(), "unknown op ") {
			return decide.DisplaySettings{}, fmt.Errorf("hub %s: %s; update decide on the hub", c.Label, err.Error())
		}
		return decide.DisplaySettings{}, err
	}
	if resp.Display == nil {
		return decide.DisplaySettings{}, fmt.Errorf("hub %s: set-avatar returned no display settings", c.Label)
	}
	c.noteDisplay(*resp.Display)
	return *resp.Display, nil
}

// SetHidden saves the hidden contact list on the hub and returns its
// display settings. A hub that does not know the op says to update
// decide there. A transport failure is a hub error and does not write
// locally. A nil names list clears the key.
func (c *Client) SetHidden(names []string) (decide.DisplaySettings, error) {
	if names == nil {
		names = []string{}
	}
	raw, err := json.Marshal(names)
	if err != nil {
		return decide.DisplaySettings{}, fmt.Errorf("hidden contacts: %w", err)
	}
	if c.UseOutbox {
		if _, err := c.enqueueOutbox(tui.OutboxEntry{Op: OpSetHidden, Text: string(raw)}); err != nil {
			return decide.DisplaySettings{}, err
		}
		d, _ := c.Display()
		return d, nil
	}
	resp, err := c.call(Request{Op: OpSetHidden, Text: string(raw)})
	if err != nil {
		if strings.HasPrefix(err.Error(), "unknown op ") {
			return decide.DisplaySettings{}, fmt.Errorf("hub %s: %s; update decide on the hub", c.Label, err.Error())
		}
		return decide.DisplaySettings{}, err
	}
	if resp.Display == nil {
		return decide.DisplaySettings{}, fmt.Errorf("hub %s: set-hidden returned no display settings", c.Label)
	}
	c.noteDisplay(*resp.Display)
	return *resp.Display, nil
}

// SetFavorites saves the starred contact list on the hub and returns its
// display settings. A hub that does not know the op says to update
// decide there. A nil names list clears the key.
func (c *Client) SetFavorites(names []string) (decide.DisplaySettings, error) {
	if names == nil {
		names = []string{}
	}
	raw, err := json.Marshal(names)
	if err != nil {
		return decide.DisplaySettings{}, fmt.Errorf("favorite contacts: %w", err)
	}
	if c.UseOutbox {
		if _, err := c.enqueueOutbox(tui.OutboxEntry{Op: OpSetFavorites, Text: string(raw)}); err != nil {
			return decide.DisplaySettings{}, err
		}
		d, _ := c.Display()
		return d, nil
	}
	resp, err := c.call(Request{Op: OpSetFavorites, Text: string(raw)})
	if err != nil {
		if strings.HasPrefix(err.Error(), "unknown op ") {
			return decide.DisplaySettings{}, fmt.Errorf("hub %s: %s; update decide on the hub", c.Label, err.Error())
		}
		return decide.DisplaySettings{}, err
	}
	if resp.Display == nil {
		return decide.DisplaySettings{}, fmt.Errorf("hub %s: set-favorites returned no display settings", c.Label)
	}
	c.noteDisplay(*resp.Display)
	return *resp.Display, nil
}

// SetAliases saves contact aliases on the hub and returns its display
// settings. A hub that does not know the op says to update decide there.
// A nil map clears the key.
func (c *Client) SetAliases(aliases map[string]string) (decide.DisplaySettings, error) {
	if aliases == nil {
		aliases = map[string]string{}
	}
	raw, err := json.Marshal(aliases)
	if err != nil {
		return decide.DisplaySettings{}, fmt.Errorf("contact aliases: %w", err)
	}
	if c.UseOutbox {
		if _, err := c.enqueueOutbox(tui.OutboxEntry{Op: OpSetAliases, Text: string(raw)}); err != nil {
			return decide.DisplaySettings{}, err
		}
		d, _ := c.Display()
		return d, nil
	}
	resp, err := c.call(Request{Op: OpSetAliases, Text: string(raw)})
	if err != nil {
		if strings.HasPrefix(err.Error(), "unknown op ") {
			return decide.DisplaySettings{}, fmt.Errorf("hub %s: %s; update decide on the hub", c.Label, err.Error())
		}
		return decide.DisplaySettings{}, err
	}
	if resp.Display == nil {
		return decide.DisplaySettings{}, fmt.Errorf("hub %s: set-contact-aliases returned no display settings", c.Label)
	}
	c.noteDisplay(*resp.Display)
	return *resp.Display, nil
}

// refreshDisplay fetches config while a session is up and keeps it when
// it changed, so the next display read sees another client's save.
func (c *Client) refreshDisplay() {
	c.mu.Lock()
	up := c.sess != nil
	c.mu.Unlock()
	if !up {
		return
	}
	resp, err := c.call(Request{Op: OpConfig})
	if err != nil || resp.Display == nil {
		return
	}
	c.noteDisplay(*resp.Display)
}

// noteDisplay stores next when it differs from the cached settings.
func (c *Client) noteDisplay(next decide.DisplaySettings) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if displaySame(c.display, next) {
		return
	}
	c.display = next
	c.displayGen++
}

func displaySame(a, b decide.DisplaySettings) bool {
	left, errA := json.Marshal(a)
	right, errB := json.Marshal(b)
	return errA == nil && errB == nil && bytes.Equal(left, right)
}

// Get returns one item from the local replica without waiting on the hub.
func (c *Client) Get(id string) (decide.Item, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return decide.Item{}, err
	}
	it, ok := rep.get(id)
	if !ok {
		return decide.Item{}, fmt.Errorf("decide item %s not found", id)
	}
	return it, nil
}

// MarkRead implements tui.Backend. It updates the local replica at once
// and sends mark-read to the hub in the background, retrying on failure.
func (c *Client) MarkRead(id string) (decide.Item, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return decide.Item{}, err
	}
	it, ok := rep.markReadLocal(id)
	if !ok {
		return decide.Item{}, fmt.Errorf("decide item %s not found", id)
	}
	c.enqueueMarkRead(id)
	return it, nil
}

func (c *Client) enqueueMarkRead(id string) {
	c.markReadMu.Lock()
	if c.markReadQueued == nil {
		c.markReadQueued = make(map[string]bool)
	}
	if !c.markReadQueued[id] {
		c.markReadQueued[id] = true
		c.markReadQueue = append(c.markReadQueue, id)
	}
	startWorker := false
	if !c.markReadRunning {
		c.markReadRunning = true
		if c.markReadWake == nil {
			c.markReadWake = make(chan struct{}, 1)
		}
		startWorker = true
	}
	c.markReadMu.Unlock()
	if startWorker {
		c.wg.Add(1)
		go func() {
			defer c.wg.Done()
			c.markReadWorker()
		}()
	}
	c.kickMarkRead()
	c.kickDraftFlush()
	if c.UseOutbox {
		c.startOutboxIfPending()
	}
}

func (c *Client) kickMarkRead() {
	c.markReadMu.Lock()
	ch := c.markReadWake
	c.markReadMu.Unlock()
	if ch == nil {
		return
	}
	select {
	case ch <- struct{}{}:
	default:
	}
}

func (c *Client) markReadWorker() {
	const retryInterval = 250 * time.Millisecond
	for {
		c.mu.Lock()
		closed := c.closed
		c.mu.Unlock()
		if closed {
			return
		}
		c.markReadMu.Lock()
		if len(c.markReadQueue) == 0 {
			c.markReadRunning = false
			c.markReadMu.Unlock()
			return
		}
		id := c.markReadQueue[0]
		wake := c.markReadWake
		c.markReadMu.Unlock()

		_, err := c.call(Request{Op: OpMarkRead, Item: id})
		if err == nil {
			if rep, rerr := c.ensureReplica(); rerr == nil && rep != nil {
				rep.clearPendingMarkRead(id)
			}
			c.markReadMu.Lock()
			if len(c.markReadQueue) > 0 && c.markReadQueue[0] == id {
				c.markReadQueue = c.markReadQueue[1:]
				delete(c.markReadQueued, id)
			}
			c.markReadMu.Unlock()
			continue
		}
		c.maybeReconnect()
		timer := time.NewTimer(retryInterval)
		select {
		case <-wake:
			timer.Stop()
		case <-timer.C:
		}
	}
}

// Answer implements tui.Backend. The hub records via ui.
func (c *Client) Answer(id, value string) (decide.Item, error) {
	if c.UseOutbox {
		return c.enqueueOutbox(tui.OutboxEntry{Op: OpAnswer, ItemID: id, Text: value})
	}
	return c.item(Request{Op: OpAnswer, Item: id, Text: value})
}

// Reply implements tui.Backend.
func (c *Client) Reply(id, text, route string) (decide.Item, error) {
	if c.UseOutbox {
		return c.enqueueOutbox(tui.OutboxEntry{Op: OpReply, ItemID: id, Text: text, Route: route})
	}
	return c.item(Request{Op: OpReply, Item: id, Text: text, Route: route})
}

// Done implements tui.Backend.
func (c *Client) Done(id, outcome string) (decide.Item, error) {
	if c.UseOutbox {
		return c.enqueueOutbox(tui.OutboxEntry{Op: OpDone, ItemID: id, Text: outcome})
	}
	return c.item(Request{Op: OpDone, Item: id, Text: outcome})
}

// Archive marks a notification done with the outcome "archived" when
// no message is newer than seen. A newer message is ErrNewerMessage.
func (c *Client) Archive(id string, seen time.Time) (decide.Item, error) {
	if c.UseOutbox {
		s := seen
		return c.enqueueOutbox(tui.OutboxEntry{Op: OpArchive, ItemID: id, Seen: &s})
	}
	it, err := c.item(Request{Op: OpArchive, Item: id, Seen: &seen})
	if err != nil && err.Error() == decide.ErrNewerMessage.Error() {
		return decide.Item{}, decide.ErrNewerMessage
	}
	return it, err
}

// Withdraw withdraws item id with reason.
func (c *Client) Withdraw(id, reason string) (decide.Item, error) {
	return c.item(Request{Op: OpWithdraw, Item: id, Text: reason})
}

// Snooze implements tui.Backend.
func (c *Client) Snooze(id string, until time.Time) (decide.Item, error) {
	if c.UseOutbox {
		u := until
		it, err := c.enqueueOutbox(tui.OutboxEntry{Op: OpSnooze, ItemID: id, Until: &u})
		if err == nil {
			it.SnoozedUntil = &u
		}
		return it, err
	}
	return c.item(Request{Op: OpSnooze, Item: id, Until: &until})
}

// Unsnooze implements tui.Backend.
func (c *Client) Unsnooze(id string) (decide.Item, error) {
	if c.UseOutbox {
		return c.enqueueOutbox(tui.OutboxEntry{Op: OpUnsnooze, ItemID: id})
	}
	return c.item(Request{Op: OpUnsnooze, Item: id})
}

// Status implements tui.Backend. It reads from the local replica without
// waiting on the hub.
func (c *Client) Status() (tui.StatusBoard, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return tui.StatusBoard{}, err
	}
	return rep.status(), nil
}

// ListDrafts implements tui.DraftBackend. It reads from the local replica
// without waiting on the hub, and triggers a background reconnect when down.
func (c *Client) ListDrafts() ([]decide.Draft, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return nil, err
	}
	if !c.isConnected() {
		c.maybeReconnect()
	}
	return rep.drafts(), nil
}

// PutDraft implements tui.DraftBackend. It sends live to the hub and updates
// the local replica on success. If the hub is unreachable, it stores the
// pending edit in the local replica and flushes it on reconnect.
func (c *Client) PutDraft(d decide.Draft, baseRev int64) (decide.Draft, bool, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return decide.Draft{}, false, err
	}
	if d.UpdatedBy == "" {
		d.UpdatedBy = c.Machine
	}
	if d.UpdatedAt.IsZero() {
		d.UpdatedAt = c.now().UTC()
	} else {
		d.UpdatedAt = d.UpdatedAt.UTC()
	}
	resp, err := c.call(Request{Op: OpPutDraft, Draft: &d, BaseRev: baseRev})
	if err == nil {
		if resp.Draft == nil {
			return decide.Draft{}, false, fmt.Errorf("hub %s: put-draft returned no draft", c.Label)
		}
		rep.upsertDraftLocal(*resp.Draft)
		return *resp.Draft, resp.DraftConflict, nil
	}
	if errors.Is(err, tui.ErrUnreachable) {
		rep.queuePendingDraft(d, baseRev)
		c.maybeReconnect()
		return d, false, nil
	}
	return decide.Draft{}, false, err
}

// DeleteDraft implements tui.DraftBackend. It sends live to the hub and updates
// the local replica on success.
func (c *Client) DeleteDraft(key, conflictID string) error {
	rep, err := c.ensureReplica()
	if err != nil {
		return err
	}
	if _, err := c.call(Request{Op: OpDeleteDraft, Item: key, ConflictID: conflictID}); err != nil {
		return err
	}
	rep.deleteDraftLocal(key, conflictID)
	return nil
}

func (c *Client) kickDraftFlush() {
	rep, err := c.ensureReplica()
	if err != nil || rep == nil {
		return
	}
	c.draftFlushMu.Lock()
	var wake chan struct{}
	startWorker := false
	if !c.draftFlushRunning {
		if !c.closed && rep.hasPendingDrafts() {
			c.draftFlushRunning = true
			if c.draftFlushWake == nil {
				c.draftFlushWake = make(chan struct{}, 1)
			}
			startWorker = true
		}
	}
	wake = c.draftFlushWake
	c.draftFlushMu.Unlock()
	if startWorker {
		c.wg.Add(1)
		go func() {
			defer c.wg.Done()
			c.draftFlushWorker()
		}()
	}
	if wake != nil {
		select {
		case wake <- struct{}{}:
		default:
		}
	}
}

func (c *Client) draftFlushWorker() {
	const retryInterval = 250 * time.Millisecond
	for {
		c.mu.Lock()
		closed := c.closed
		c.mu.Unlock()
		if closed {
			c.draftFlushMu.Lock()
			c.draftFlushRunning = false
			c.draftFlushMu.Unlock()
			return
		}
		rep, err := c.ensureReplica()
		if err != nil || rep == nil {
			c.draftFlushMu.Lock()
			c.draftFlushRunning = false
			c.draftFlushMu.Unlock()
			return
		}
		pending := rep.pendingDrafts()
		if len(pending) == 0 {
			c.draftFlushMu.Lock()
			c.draftFlushRunning = false
			c.draftFlushMu.Unlock()
			return
		}
		c.draftFlushMu.Lock()
		wake := c.draftFlushWake
		c.draftFlushMu.Unlock()

		unreachable := false
		for _, p := range pending {
			d := p.Draft
			resp, err := c.call(Request{Op: OpPutDraft, Draft: &d, BaseRev: p.BaseRev})
			if err == nil {
				rep.resolvePendingDraft(d.Key, p.Gen, resp.Draft)
				continue
			}
			if errors.Is(err, tui.ErrUnreachable) {
				unreachable = true
				break
			}
			// Non-transient error (e.g. validation/limit); drop so we don't loop forever.
			rep.resolvePendingDraft(d.Key, p.Gen, nil)
		}
		if !unreachable {
			continue
		}
		c.maybeReconnect()
		timer := time.NewTimer(retryInterval)
		select {
		case <-wake:
			timer.Stop()
		case <-timer.C:
		}
	}
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
		out = append(out, tui.Target{Name: tg.Name, Pane: tg.TargetPane()})
	}
	return out, resp.Agentbus, nil
}

// BusRunner runs `agentbus list --json` or `agentbus health --json` on
// the hub, the only agentbus calls the inbox makes outside the backend.
func (c *Client) BusRunner(ctx context.Context, args []string) ([]byte, error) {
	var op string
	switch {
	case slices.Equal(args, []string{"list", "--json"}):
		op = OpBusList
	case slices.Equal(args, []string{"health", "--json"}):
		op = OpBusHealth
	default:
		return nil, fmt.Errorf("agentbus %s does not run on the hub from the inbox", strings.Join(args, " "))
	}
	resp, err := c.callCtx(ctx, Request{Op: op})
	if err != nil {
		return nil, err
	}
	return []byte(resp.Output), nil
}

// CheckDeliveries implements tui.DeliveryChecker. The hub runs the
// status check against its ledger.
func (c *Client) CheckDeliveries(ctx context.Context) error {
	_, err := c.callCtx(ctx, Request{Op: OpDeliveries})
	return err
}

const oldHubOrgTreeNote = "hub does not support org-tree; using local orgctl"

func hubOrgTreeEnabled(display decide.DisplaySettings) bool {
	raw, ok := display.Keys["ui.orgTree"]
	if !ok {
		return false
	}
	var enabled bool
	return json.Unmarshal(raw, &enabled) == nil && enabled
}

func (c *Client) refreshOrgTree(sess *session, rep *replicaStore) {
	resp, err := sess.do(context.Background(), Request{ID: c.allocID(), Op: OpOrgTree}, c.timeout())
	if err != nil {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	if resp.Error != "" {
		if strings.HasPrefix(resp.Error, "unknown op ") {
			c.orgTreeUnsupported = true
			c.orgTreeErr = nil
			return
		}
		c.orgTreeUnsupported = false
		c.orgTreeErr = errors.New(resp.Error)
		return
	}
	c.orgTreeUnsupported = false
	c.orgTreeErr = nil
	var mod time.Time
	if resp.OrgSpecModTime != nil {
		mod = *resp.OrgSpecModTime
	}
	rep.setOrgTree(resp.Output, mod)
}

// OrgTree implements tui.OrgTreeBackend. It serves the cached org tree
// from the local replica when available, or fetches it from the hub.
func (c *Client) OrgTree() (tui.OrgTreeResult, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return tui.OrgTreeResult{}, err
	}
	c.mu.Lock()
	unsupported := c.orgTreeUnsupported
	sess := c.sess
	treeErr := c.orgTreeErr
	c.mu.Unlock()
	if unsupported {
		return tui.OrgTreeResult{Fallback: true, Note: oldHubOrgTreeNote}, nil
	}
	if sess != nil && treeErr != nil {
		return tui.OrgTreeResult{}, treeErr
	}
	if tree, _ := rep.orgTree(); tree != "" {
		return tui.OrgTreeResult{JSON: []byte(tree)}, nil
	}
	if sess != nil {
		c.refreshOrgTree(sess, rep)
		c.mu.Lock()
		unsupported = c.orgTreeUnsupported
		treeErr = c.orgTreeErr
		c.mu.Unlock()
		if unsupported {
			return tui.OrgTreeResult{Fallback: true, Note: oldHubOrgTreeNote}, nil
		}
		if treeErr != nil {
			return tui.OrgTreeResult{}, treeErr
		}
		if tree, _ := rep.orgTree(); tree != "" {
			return tui.OrgTreeResult{JSON: []byte(tree)}, nil
		}
		return tui.OrgTreeResult{}, &hubError{msg: fmt.Sprintf("hub %s: org-tree failed", c.Label)}
	}
	c.maybeReconnect()
	return tui.OrgTreeResult{}, &hubError{msg: fmt.Sprintf("hub %s unreachable; no cached org tree", c.Label)}
}

// Send implements tui.Backend. The hub looks the recipient up by name.
func (c *Client) Send(forward string, tg tui.Target, text string) error {
	if c.UseOutbox {
		_, err := c.enqueueOutbox(tui.OutboxEntry{Op: OpSend, To: tg.Name, Bus: tg.Bus, Forward: forward, Text: text})
		return err
	}
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
	case decide.ParseTarget(loc.Pane, loc.Machine).Scheme != decide.SchemeHerdr:
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
	watchID int64
	watchCh chan Response
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
		if s.watchCh != nil && resp.ID == s.watchID {
			wch := s.watchCh
			s.mu.Unlock()
			select {
			case wch <- resp:
			case <-s.done:
			}
			continue
		}
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

func (s *session) startWatch(ctx context.Context, id, since int64, timeout time.Duration) (Response, <-chan Response, error) {
	req := Request{ID: id, Op: OpWatch, Since: since}
	line, err := json.Marshal(req)
	if err != nil {
		return Response{}, nil, fmt.Errorf("encode request: %w", err)
	}
	wch := make(chan Response, 64)
	s.mu.Lock()
	if s.err != nil {
		err := s.err
		s.mu.Unlock()
		return Response{}, nil, err
	}
	s.watchID = id
	s.watchCh = wch
	s.mu.Unlock()
	s.wmu.Lock()
	_, err = s.conn.W.Write(append(line, '\n'))
	s.wmu.Unlock()
	if err != nil {
		return Response{}, nil, fmt.Errorf("send request: %w", err)
	}
	timer := time.NewTimer(timeout)
	defer timer.Stop()
	select {
	case resp := <-wch:
		return resp, wch, nil
	case <-s.done:
		s.mu.Lock()
		defer s.mu.Unlock()
		return Response{}, nil, s.err
	case <-timer.C:
		return Response{}, nil, fmt.Errorf("no answer in %s", timeout)
	case <-ctx.Done():
		return Response{}, nil, &callerGaveUp{err: ctx.Err()}
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
