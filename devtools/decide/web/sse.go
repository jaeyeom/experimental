package web

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
)

// subBuffer bounds the events queued for one client. A client that falls
// further behind is dropped and resumes with Last-Event-ID, so it can
// never block the watcher.
const subBuffer = 32

// defaultPingEvery keeps idle streams under proxy timeouts.
const defaultPingEvery = 25 * time.Second

type subscriber struct {
	ch      chan inboxrpc.WatchEvent
	dropped chan struct{}
	bus     chan struct{}
	seq     int64
}

// hub fans the shared Watcher out to event streams. The watcher runs only
// while at least one client is subscribed.
type hub struct {
	mu     sync.Mutex
	subs   map[*subscriber]struct{}
	cancel context.CancelFunc
	done   chan struct{}
}

func (h *hub) add(seq int64) *subscriber {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.addLocked(seq)
}

func (h *hub) addLocked(seq int64) *subscriber {
	sub := &subscriber{
		ch:      make(chan inboxrpc.WatchEvent, subBuffer),
		dropped: make(chan struct{}),
		bus:     make(chan struct{}, 1),
		seq:     seq,
	}
	if h.subs == nil {
		h.subs = map[*subscriber]struct{}{}
	}
	h.subs[sub] = struct{}{}
	return sub
}

func (h *hub) subscribers() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return len(h.subs)
}

func (h *hub) running() bool {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.done != nil
}

func (h *hub) broadcast(ev inboxrpc.WatchEvent) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.broadcastLocked(ev)
}

func (h *hub) broadcastLocked(ev inboxrpc.WatchEvent) {
	for sub := range h.subs {
		// An empty ledger (seq 0) always polls as a resync; it carries
		// nothing new.
		if ev.Resync && ev.Seq == 0 && ev.Status == nil && ev.Drafts == nil && ev.OrgSpecModTime == nil {
			continue
		}
		// Skip what a late subscriber's initial frame already covered.
		if !ev.Resync && (ev.Seq < sub.seq || (ev.Seq == sub.seq && len(ev.Changes) > 0)) {
			continue
		}
		select {
		case sub.ch <- ev:
			sub.seq = max(sub.seq, ev.Seq)
		default:
			delete(h.subs, sub)
			close(sub.dropped)
		}
	}
}

func (h *hub) notifyBus() {
	h.mu.Lock()
	defer h.mu.Unlock()
	for sub := range h.subs {
		select {
		case sub.bus <- struct{}{}:
		default:
		}
	}
}

// remove unsubscribes sub and stops the watcher loop with the last one.
func (h *hub) remove(sub *subscriber) {
	h.mu.Lock()
	delete(h.subs, sub)
	var cancel context.CancelFunc
	var done chan struct{}
	if len(h.subs) == 0 && h.done != nil {
		cancel, done = h.cancel, h.done
		h.cancel, h.done = nil, nil
	}
	h.mu.Unlock()
	if cancel != nil {
		cancel()
		<-done
	}
}

// subscribe starts a client at since. It returns the initial event.
func (s *Server) subscribe(since int64) (*subscriber, inboxrpc.WatchEvent, error) {
	w := s.Watcher()
	h := &s.state().hub
	h.mu.Lock()
	defer h.mu.Unlock()
	// Start moves the watcher's baseline, so first hand current
	// subscribers what has changed up to now.
	if ev, ok := w.Poll(); ok {
		h.broadcastLocked(ev)
	}
	ev, err := w.Start(since)
	if err != nil {
		return nil, ev, fmt.Errorf("start event stream: %w", err)
	}
	sub := h.addLocked(ev.Seq)
	if h.done == nil {
		ctx, cancel := context.WithCancel(context.Background())
		done := make(chan struct{})
		h.cancel, h.done = cancel, done
		go func() {
			defer close(done)
			w.Run(ctx, h.broadcast)
		}()
	}
	return sub, ev, nil
}

// lastEventID reads the resume point: Last-Event-ID first, then ?since.
func lastEventID(r *http.Request) (int64, error) {
	raw := r.Header.Get("Last-Event-ID")
	if raw == "" {
		raw = r.URL.Query().Get("since")
	}
	if raw == "" {
		return 0, nil
	}
	n, err := strconv.ParseInt(raw, 10, 64)
	if err != nil || n < 0 {
		return 0, badRequest("invalid event id %q", raw)
	}
	return n, nil
}

// eventStream is one client's SSE connection.
type eventStream struct {
	s       *Server
	w       http.ResponseWriter
	flusher http.Flusher
	lastBus []byte
}

func (e *eventStream) send(name string, id int64, payload any) bool {
	b, err := json.Marshal(payload)
	if err != nil {
		return false
	}
	idLine := ""
	if id > 0 {
		idLine = fmt.Sprintf("id: %d\n", id)
	}
	if _, err := fmt.Fprintf(e.w, "%sevent: %s\ndata: %s\n\n", idLine, name, b); err != nil {
		return false
	}
	e.flusher.Flush()
	return true
}

// frame sends ev as a resync or delta, adding a changed bus health.
func (e *eventStream) frame(ev inboxrpc.WatchEvent) bool {
	bus, changed := e.s.busChanged(e.lastBus)
	if changed {
		e.lastBus = bus
	}
	if ev.Resync {
		body, err := e.s.resyncBody(ev, bus)
		if err != nil {
			return false
		}
		return e.send("resync", ev.Seq, body)
	}
	return e.send("delta", ev.Seq, deltaBody(ev, bus, changed))
}

// busDelta sends a delta carrying only a changed bus health.
func (e *eventStream) busDelta(seq int64) bool {
	bus, changed := e.s.busChanged(e.lastBus)
	if !changed {
		return true
	}
	e.lastBus = bus
	return e.send("delta", seq, deltaBody(inboxrpc.WatchEvent{Seq: seq}, bus, true))
}

func (s *Server) events(w http.ResponseWriter, r *http.Request) {
	since, err := lastEventID(r)
	if err != nil {
		fail(w, err, 0)
		return
	}
	flusher, ok := w.(http.Flusher)
	if !ok || s.Backend == nil {
		writeError(w, http.StatusServiceUnavailable, "event stream unavailable")
		return
	}
	// Long-lived stream: no server write deadline applies.
	_ = http.NewResponseController(w).SetWriteDeadline(time.Time{})
	sub, ev, err := s.subscribe(since)
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	defer s.state().hub.remove(sub)

	h := w.Header()
	h.Set("Content-Type", "text/event-stream")
	h.Set("Cache-Control", "no-cache")
	h.Set("Connection", "keep-alive")
	h.Set("X-Accel-Buffering", "no")
	w.WriteHeader(http.StatusOK)

	e := &eventStream{s: s, w: w, flusher: flusher}
	if !e.frame(ev) {
		return
	}
	ping := s.PingEvery
	if ping <= 0 {
		ping = defaultPingEvery
	}
	tick := time.NewTicker(ping)
	defer tick.Stop()
	seq := ev.Seq
	for {
		alive := true
		select {
		case <-r.Context().Done():
			return
		case <-sub.dropped:
			return
		case ev := <-sub.ch:
			seq = ev.Seq
			alive = e.frame(ev)
		case <-sub.bus:
			alive = e.busDelta(seq)
		case <-tick.C:
			alive = e.send("ping", 0, struct{}{})
		}
		if !alive {
			return
		}
	}
}

// busChanged returns the current bus health and whether it differs from
// last.
func (s *Server) busChanged(last []byte) ([]byte, bool) {
	st := s.state()
	st.mu.Lock()
	defer st.mu.Unlock()
	return append([]byte(nil), st.busHealth...), !bytes.Equal(st.busHealth, last)
}

func deltaBody(ev inboxrpc.WatchEvent, bus []byte, busChanged bool) map[string]any {
	changes := ev.Changes
	if changes == nil {
		changes = []decide.LedgerChange{}
	}
	body := map[string]any{"seq": ev.Seq, "changes": changes}
	if ev.Status != nil {
		body["status"] = ev.Status
	}
	if ev.Drafts != nil {
		body["drafts"] = ev.Drafts
		body["draftSeq"] = ev.DraftSeq
	}
	if ev.OrgSpecModTime != nil {
		body["orgSpecModTime"] = ev.OrgSpecModTime
	}
	if busChanged && len(bus) > 0 {
		body["busHealth"] = json.RawMessage(bus)
	}
	return body
}

// resyncBody is the full snapshot: items plus everything a delta may
// carry, reading status and drafts when the event has none.
func (s *Server) resyncBody(ev inboxrpc.WatchEvent, bus []byte) (map[string]any, error) {
	items := ev.Items
	if items == nil {
		items = []decide.Item{}
	}
	open, err := decide.Select(items, decide.StateOpen)
	if err != nil {
		return nil, fmt.Errorf("select open items: %w", err)
	}
	inbox := decide.InboxEntries(open, nil)
	if inbox == nil {
		inbox = []decide.InboxEntry{}
	}
	status := ev.Status
	if status == nil {
		board, err := s.Backend.Status()
		if err != nil {
			return nil, err //nolint:wrapcheck // the backend names its failure
		}
		status = &board
	}
	drafts, draftSeq := ev.Drafts, ev.DraftSeq
	if drafts == nil {
		if draftSeq, drafts, err = s.drafts(); err != nil {
			return nil, err
		}
	}
	display, err := s.display()
	if err != nil {
		return nil, err
	}
	orgOn, _, err := s.orgEnabled()
	if err != nil {
		return nil, err
	}
	targets := s.Targets
	if targets == nil {
		targets = []decide.ConfigTarget{}
	}
	body := map[string]any{
		"seq":            ev.Seq,
		"items":          items,
		"inbox":          inbox,
		"unreadCount":    decide.UnreadCount(open),
		"status":         status,
		"drafts":         drafts,
		"draftSeq":       draftSeq,
		"display":        display,
		"busHealth":      json.RawMessage(bus),
		"targets":        targets,
		"agentbus":       s.Agentbus,
		"orgTreeEnabled": orgOn,
	}
	if len(bus) == 0 {
		body["busHealth"] = nil
	}
	if ev.OrgSpecModTime != nil {
		body["orgSpecModTime"] = ev.OrgSpecModTime
	}
	return body, nil
}
