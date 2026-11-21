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
	"path/filepath"
	"reflect"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// maxLine bounds one request or response line.
const maxLine = 64 << 20

// Server answers inbox requests against Backend, the hub's own ledger.
// Targets and Agentbus are the hub's compose recipients. Display
// returns the hub's display settings for config; nil sends none.
type Server struct {
	Backend  *tui.LocalBackend
	Targets  []decide.ConfigTarget
	Agentbus bool
	Display  func() (decide.DisplaySettings, error)
	// FollowEvery is how often Serve follows up stale answers on its own.
	// Zero does not. The hub sets it so a session follows up without a
	// list request and without the item open.
	FollowEvery time.Duration
	// WatchEvery is how often Serve polls the ledger for external changes
	// while a watch is active. Zero defaults to 100ms.
	WatchEvery time.Duration
}

const defaultWatchEvery = 100 * time.Millisecond

// Serve answers each request line from r on w until r ends. Requests
// run concurrently; each response is one line.
func (s *Server) Serve(ctx context.Context, r io.Reader, w io.Writer) error {
	var mu sync.Mutex
	var wg sync.WaitGroup
	defer wg.Wait()
	loopCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	if s.FollowEvery > 0 && s.Backend != nil {
		wg.Add(1)
		go func() {
			defer wg.Done()
			s.Backend.FollowLoop(loopCtx, s.FollowEvery, nil)
		}()
	}
	enc := json.NewEncoder(w)
	reply := func(resp Response) {
		mu.Lock()
		defer mu.Unlock()
		_ = enc.Encode(resp) // a closed client has nobody to tell
	}
	watcher := &connWatcher{
		server: s,
		reply:  reply,
		wake:   make(chan struct{}, 1),
	}
	if s.Backend != nil {
		wg.Add(1)
		go func() {
			defer wg.Done()
			watcher.loop(loopCtx)
		}()
	}
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 64<<10), maxLine)
	for sc.Scan() {
		var req Request
		if err := json.Unmarshal(sc.Bytes(), &req); err != nil {
			reply(Response{Error: "decode request: " + err.Error()})
			continue
		}
		if req.Op == OpWatch {
			watcher.start(req)
			continue
		}
		wg.Add(1)
		go func() {
			defer wg.Done()
			resp := s.handle(ctx, req)
			resp.ID = req.ID
			reply(resp)
			if writeOps[req.Op] {
				watcher.signal()
			}
		}()
	}
	if err := sc.Err(); err != nil {
		return fmt.Errorf("read requests: %w", err)
	}
	return nil
}

func (s *Server) handle(ctx context.Context, req Request) Response {
	if resp, ok := s.itemOp(req); ok {
		return resp
	}
	if resp, ok := s.busOp(ctx, req); ok {
		return resp
	}
	if resp, ok := s.draftOp(req); ok {
		return resp
	}
	if resp, ok := s.settingsOp(req); ok {
		return resp
	}
	switch req.Op {
	case OpHello:
		return Response{Version: Version}
	case OpList:
		return s.list()
	case OpStatus:
		return s.status()
	case OpTargets:
		return Response{Targets: s.Targets, Agentbus: s.Agentbus}
	case OpConfig:
		return s.config()
	case OpSend:
		return s.send(req)
	case OpLocate:
		return s.locate(ctx, req)
	default:
		return Response{Error: fmt.Sprintf("unknown op %q", req.Op)}
	}
}

func (s *Server) settingsOp(req Request) (Response, bool) {
	switch req.Op {
	case OpSetAvatar:
		return s.setAvatar(req), true
	case OpSetHidden:
		return s.setHidden(req), true
	case OpSetFavorites:
		return s.setFavorites(req), true
	case OpSetAliases:
		return s.setAliases(req), true
	}
	return Response{}, false
}

func (s *Server) draftOp(req Request) (Response, bool) {
	switch req.Op {
	case OpListDrafts:
		return s.listDrafts(), true
	case OpPutDraft:
		return s.putDraft(req), true
	case OpDeleteDraft:
		return s.deleteDraft(req), true
	}
	return Response{}, false
}

// busOp answers an op that runs agentbus on the hub and reports whether
// req is one.
func (s *Server) busOp(ctx context.Context, req Request) (Response, bool) {
	switch req.Op {
	case OpBusList:
		return s.busList(ctx), true
	case OpBusHealth:
		return s.busHealth(ctx), true
	case OpDeliveries:
		return s.checkDeliveries(ctx), true
	}
	return Response{}, false
}

// itemOp answers an op on one item and reports whether req is one.
func (s *Server) itemOp(req Request) (Response, bool) {
	b := s.Backend
	var writtenAt time.Time
	if req.WrittenAt != nil {
		writtenAt = *req.WrittenAt
	}
	ops := map[string]func() (decide.Item, error){
		OpShow:     func() (decide.Item, error) { return b.Get(req.Item) },
		OpMarkRead: func() (decide.Item, error) { return b.MarkRead(req.Item) },
		OpAnswer:   func() (decide.Item, error) { return b.AnswerAt(req.Item, req.Text, writtenAt) },
		OpReply:    func() (decide.Item, error) { return b.ReplyAt(req.Item, req.Text, req.Route, writtenAt) },
		OpDone:     func() (decide.Item, error) { return b.Done(req.Item, req.Text) },
		OpArchive: func() (decide.Item, error) {
			var seen time.Time
			if req.Seen != nil {
				seen = *req.Seen
			}
			return b.Archive(req.Item, seen)
		},
		OpWithdraw: func() (decide.Item, error) { return b.Withdraw(req.Item, req.Text) },
		OpUnsnooze: func() (decide.Item, error) { return b.Unsnooze(req.Item) },
		OpSnooze: func() (decide.Item, error) {
			if req.Until == nil {
				return decide.Item{}, fmt.Errorf("snooze needs a time")
			}
			return b.Snooze(req.Item, *req.Until)
		},
	}
	op, ok := ops[req.Op]
	if !ok {
		return Response{}, false
	}
	if req.ConflictCheck {
		if cur, err := b.Get(req.Item); err == nil {
			var baseLast time.Time
			if req.BaseLastMessageAt != nil {
				baseLast = *req.BaseLastMessageAt
			}
			if reason := detectConflict(req.Op, req.BaseState, req.BaseMessageCount, baseLast, req.BaseAnswered, cur); reason != "" {
				return Response{Conflict: reason, Item: &cur}, true
			}
		}
	}
	return itemResponse(op()), true
}

func detectConflict(op, baseState string, baseCount int, baseLast time.Time, baseAnswered bool, cur decide.Item) string {
	if cur.State == decide.StateWithdrawn && baseState != decide.StateWithdrawn {
		return "item was withdrawn"
	}
	if (cur.State == decide.StateDone || cur.State == decide.StateDoneUnread) && baseState != cur.State {
		return "item was marked done"
	}
	if op == OpAnswer && (cur.State == decide.StateAnswered || cur.Answer != nil) && !baseAnswered {
		return "item was already answered"
	}
	if len(cur.Messages) > baseCount || (!baseLast.IsZero() && cur.LastMessageAt().After(baseLast)) {
		return "new message arrived"
	}
	return ""
}

func (s *Server) list() Response {
	items, err := s.Backend.List()
	if err != nil {
		return failed(err)
	}
	if items == nil {
		items = []decide.Item{}
	}
	return Response{Items: items}
}

func (s *Server) status() Response {
	board, err := s.Backend.Status()
	if err != nil {
		return failed(err)
	}
	return Response{Status: &board}
}

func (s *Server) locate(ctx context.Context, req Request) Response {
	it, err := s.Backend.Get(req.Item)
	if err != nil {
		return failed(err)
	}
	loc := s.Backend.Locate(ctx, &it)
	return Response{Location: &loc}
}

// setAvatar validates choice the way the local picker does, writes the
// rule to this machine's config, and returns the display settings.
func (s *Server) setAvatar(req Request) Response {
	if _, err := tui.SaveAvatar(req.Name, req.Choice); err != nil {
		return failed(err)
	}
	return s.config()
}

// setHidden replaces the hub's hiddenContacts list and returns the
// display settings. Text is a JSON array of names.
func (s *Server) setHidden(req Request) Response {
	var names []string
	if err := json.Unmarshal([]byte(req.Text), &names); err != nil {
		return failed(fmt.Errorf("hidden contacts: %w", err))
	}
	if _, err := decide.SetHiddenContacts(names); err != nil {
		return failed(err)
	}
	return s.config()
}

// setFavorites replaces the hub's favoriteContacts list and returns the
// display settings. Text is a JSON array of names.
func (s *Server) setFavorites(req Request) Response {
	var names []string
	if err := json.Unmarshal([]byte(req.Text), &names); err != nil {
		return failed(fmt.Errorf("favorite contacts: %w", err))
	}
	if _, err := decide.SetFavoriteContacts(names); err != nil {
		return failed(err)
	}
	return s.config()
}

// setAliases replaces the hub's contactAliases map and returns the
// display settings. Text is a JSON object of ledger name to agent.
func (s *Server) setAliases(req Request) Response {
	var aliases map[string]string
	if err := json.Unmarshal([]byte(req.Text), &aliases); err != nil {
		return failed(fmt.Errorf("contact aliases: %w", err))
	}
	if _, err := decide.SetContactAliases(aliases); err != nil {
		return failed(err)
	}
	return s.config()
}

func (s *Server) config() Response {
	var settings decide.DisplaySettings
	if s.Display != nil {
		var err error
		if settings, err = s.Display(); err != nil {
			return failed(err)
		}
	}
	return Response{Display: &settings}
}

func (s *Server) busRun() decide.AgentbusRunner {
	if s.Backend.BusRun != nil {
		return s.Backend.BusRun
	}
	return decide.ExecAgentbus
}

func (s *Server) busList(ctx context.Context) Response {
	out, err := s.busRun()(ctx, []string{"list", "--json"})
	if err != nil {
		return failed(err)
	}
	return Response{Output: string(out)}
}

// busHealth runs `agentbus health --json` on the hub, where agent
// messages are sent. Health exits nonzero while the bus is down, so
// its report goes back whenever it printed one.
func (s *Server) busHealth(ctx context.Context) Response {
	out, err := s.busRun()(ctx, []string{"health", "--json"})
	if err != nil && len(bytes.TrimSpace(out)) == 0 {
		return failed(err)
	}
	return Response{Output: string(out)}
}

// checkDeliveries runs the batched `agentbus status --json` on the hub,
// where the ledger and the bus messages are, and stores what it says.
func (s *Server) checkDeliveries(ctx context.Context) Response {
	if err := s.Backend.CheckDeliveries(ctx); err != nil {
		return failed(err)
	}
	return Response{}
}

// send resolves the recipient on the hub: a configured target by name,
// or an agentbus name.
func (s *Server) send(req Request) Response {
	tg := tui.Target{Name: req.To, Bus: true}
	if !req.Bus {
		found, ok := decide.FindTarget(s.Targets, req.To)
		if !ok {
			return Response{Error: fmt.Sprintf("decide send recipient %q is unknown on the hub", req.To)}
		}
		tg = tui.Target{Name: found.Name, Pane: found.Pane}
	}
	var writtenAt time.Time
	if req.WrittenAt != nil {
		writtenAt = *req.WrittenAt
	}
	if err := s.Backend.SendAt(req.Forward, tg, req.Text, writtenAt); err != nil {
		return failed(err)
	}
	return Response{}
}

func (s *Server) draftStore() (*decide.DraftStore, error) {
	ds, err := decide.OpenDraftStore(s.Backend.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide draft store: %w", err)
	}
	if s.Backend.Now != nil {
		ds.SetClock(s.Backend.Now)
	}
	return ds, nil
}

func (s *Server) listDrafts() Response {
	ds, err := s.draftStore()
	if err != nil {
		return failed(err)
	}
	seq, drafts, err := ds.List()
	if err != nil {
		return failed(err)
	}
	if drafts == nil {
		drafts = []decide.Draft{}
	}
	return Response{Drafts: drafts, DraftSeq: seq}
}

func (s *Server) putDraft(req Request) Response {
	if req.Draft == nil {
		return Response{Error: "put-draft needs a draft"}
	}
	saved, conflict, err := s.Backend.PutDraft(*req.Draft, req.BaseRev)
	if err != nil {
		return failed(err)
	}
	return Response{Draft: &saved, DraftConflict: conflict}
}

func (s *Server) deleteDraft(req Request) Response {
	if err := s.Backend.DeleteDraft(req.Item, req.ConflictID); err != nil {
		return failed(err)
	}
	return Response{}
}

func itemResponse(it decide.Item, err error) Response {
	var notify *tui.NotifyError
	if errors.As(err, &notify) {
		return Response{Item: &it, NotifyError: notify.Error()}
	}
	if err != nil {
		return failed(err)
	}
	return Response{Item: &it}
}

func failed(err error) Response {
	return Response{Error: err.Error()}
}

type connWatcher struct {
	server *Server
	reply  func(Response)
	wake   chan struct{}

	mu           sync.Mutex
	watchID      int64
	lastSeq      int64
	lastStatus   tui.StatusBoard
	hasStatus    bool
	statusAt     time.Time
	statusMod    time.Time
	lastDraftSeq int64
	hasDrafts    bool
	draftsAt     time.Time
	draftsMod    time.Time
}

func (w *connWatcher) signal() {
	select {
	case w.wake <- struct{}{}:
	default:
	}
}

func (w *connWatcher) ledger() (*decide.Ledger, error) {
	led, err := decide.Open(w.server.Backend.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide ledger: %w", err)
	}
	if w.server.Backend.Now != nil {
		led.SetClock(w.server.Backend.Now)
	}
	return led, nil
}

func (w *connWatcher) start(req Request) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.server.Backend == nil {
		w.reply(Response{ID: req.ID, Error: "no backend"})
		return
	}
	led, err := w.ledger()
	if err != nil {
		w.reply(Response{ID: req.ID, Error: err.Error()})
		return
	}
	seq, changes, items, resync, err := led.ChangesSince(req.Since)
	if err != nil {
		w.reply(Response{ID: req.ID, Error: err.Error()})
		return
	}
	resp := Response{
		ID:      req.ID,
		Seq:     seq,
		Resync:  resync,
		Items:   items,
		Changes: changes,
	}
	if board, err := w.server.Backend.Status(); err == nil {
		resp.Status = &board
		w.lastStatus = board
		w.hasStatus = true
	}
	w.statusAt = time.Now()
	w.statusMod = w.statStatusMod()
	if dseq, drafts, err := w.listDrafts(); err == nil {
		resp.Drafts = drafts
		resp.DraftSeq = dseq
		w.lastDraftSeq = dseq
		w.hasDrafts = true
	}
	w.draftsAt = time.Now()
	w.draftsMod = w.statDraftsMod()
	w.watchID = req.ID
	w.lastSeq = seq
	w.reply(resp)
}

func (w *connWatcher) listDrafts() (int64, []decide.Draft, error) {
	ds, err := w.server.draftStore()
	if err != nil {
		return 0, nil, err
	}
	seq, drafts, err := ds.List()
	if err != nil {
		return 0, nil, fmt.Errorf("list decide drafts: %w", err)
	}
	return seq, drafts, nil
}

func (w *connWatcher) statDraftsMod() time.Time {
	info, err := os.Stat(filepath.Join(w.server.Backend.Dir, "drafts.json"))
	if err != nil {
		return time.Time{}
	}
	return info.ModTime()
}

func (w *connWatcher) statStatusMod() time.Time {
	info, err := os.Stat(filepath.Join(w.server.Backend.Dir, "status.json"))
	if err != nil {
		return time.Time{}
	}
	return info.ModTime()
}

func (w *connWatcher) loop(ctx context.Context) {
	every := w.server.WatchEvery
	if every <= 0 {
		every = defaultWatchEvery
	}
	tick := time.NewTicker(every)
	defer tick.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-w.wake:
			w.poll()
		case <-tick.C:
			w.poll()
		}
	}
}

func (w *connWatcher) poll() {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.watchID == 0 || w.server.Backend == nil {
		return
	}
	led, err := w.ledger()
	if err != nil {
		return
	}
	seq, changes, items, resync, err := led.ChangesSince(w.lastSeq)
	if err != nil {
		return
	}
	statusPtr := w.pollStatus()
	draftsPtr, draftSeq := w.pollDrafts()
	if !resync && len(changes) == 0 && statusPtr == nil && draftsPtr == nil {
		return
	}
	w.lastSeq = seq
	resp := Response{
		ID:      w.watchID,
		Seq:     seq,
		Resync:  resync,
		Items:   items,
		Changes: changes,
		Status:  statusPtr,
	}
	if draftsPtr != nil {
		resp.Drafts = *draftsPtr
		resp.DraftSeq = draftSeq
	}
	w.reply(resp)
}

func (w *connWatcher) pollStatus() *tui.StatusBoard {
	mod := w.statStatusMod()
	if mod.Equal(w.statusMod) && time.Since(w.statusAt) < time.Second {
		return nil
	}
	w.statusMod = mod
	w.statusAt = time.Now()
	board, err := w.server.Backend.Status()
	if err != nil {
		return nil
	}
	if w.hasStatus && reflect.DeepEqual(w.lastStatus, board) {
		return nil
	}
	w.lastStatus = board
	w.hasStatus = true
	return &board
}

func (w *connWatcher) pollDrafts() (*[]decide.Draft, int64) {
	dmod := w.statDraftsMod()
	if dmod.Equal(w.draftsMod) && time.Since(w.draftsAt) < time.Second {
		return nil, 0
	}
	w.draftsMod = dmod
	w.draftsAt = time.Now()
	dseq, drafts, err := w.listDrafts()
	if err != nil {
		return nil, 0
	}
	if w.hasDrafts && dseq == w.lastDraftSeq {
		return nil, 0
	}
	w.lastDraftSeq = dseq
	w.hasDrafts = true
	return &drafts, dseq
}
