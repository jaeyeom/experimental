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
	Backend   *tui.LocalBackend
	Targets   []decide.ConfigTarget
	Agentbus  bool
	Display   func() (decide.DisplaySettings, error)
	OrgConfig func() (enabled bool, spec string, err error)
	Orgctl    tui.OrgctlRunner
	// FollowEvery is how often Serve follows up stale answers on its own.
	// Zero does not. The hub sets it so a session follows up without a
	// list request and without the item open.
	FollowEvery time.Duration
	// WatchEvery is how often Serve polls the ledger for external changes
	// while a watch is active. Zero defaults to 2s.
	WatchEvery time.Duration
}

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
	shared := &Watcher{Backend: s.Backend, OrgConfig: s.OrgConfig, Every: s.WatchEvery}
	watcher := &connWatcher{watcher: shared, reply: reply}
	if s.Backend != nil {
		wg.Add(1)
		go func() {
			defer wg.Done()
			shared.Run(loopCtx, watcher.emit)
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
				shared.Signal()
			}
		}()
	}
	if err := sc.Err(); err != nil {
		return fmt.Errorf("read requests: %w", err)
	}
	return nil
}

func (s *Server) eventsOp(req Request) (Response, bool) {
	switch req.Op {
	case OpReasons:
		return s.reasons(), true
	case OpFlag:
		return s.flag(req), true
	case OpListAsks:
		return s.listAsks(), true
	case OpCloseAsk:
		return s.closeAsk(req), true
	default:
		return Response{}, false
	}
}

func (s *Server) queryOp(ctx context.Context, req Request) (Response, bool) {
	switch req.Op {
	case OpHello:
		return Response{Version: Version}, true
	case OpList:
		return s.list(), true
	case OpStatus:
		return s.status(), true
	case OpTargets:
		return Response{Targets: s.Targets, Agentbus: s.Agentbus}, true
	case OpConfig:
		return s.config(), true
	case OpLocate:
		return s.locate(ctx, req), true
	case OpOrgTree:
		return s.orgTree(ctx), true
	case OpPanels:
		return s.panels(), true
	default:
		return Response{}, false
	}
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
	if resp, ok := s.eventsOp(req); ok {
		return resp
	}
	if resp, ok := s.queryOp(ctx, req); ok {
		return resp
	}
	if req.Op == OpSend {
		return s.send(req)
	}
	return Response{Error: fmt.Sprintf("unknown op %q", req.Op)}
}

func (s *Server) reasons() Response {
	reasons, err := s.Backend.Reasons()
	if err != nil {
		return failed(err)
	}
	return Response{ReasonList: reasons}
}

func (s *Server) flag(req Request) Response {
	ev, err := s.Backend.Flag(req.Item, req.MsgIndex, req.Reasons, req.Text)
	if err != nil {
		return failed(err)
	}
	return Response{Event: &ev}
}

func (s *Server) listAsks() Response {
	enabled, err := s.Backend.AsksEnabled()
	if err != nil {
		return failed(err)
	}
	on := enabled
	if !enabled {
		empty := []decide.AskRecord{}
		return Response{Asks: &empty, AsksEnabled: &on}
	}
	asks, err := s.Backend.ListAsks()
	if err != nil {
		return failed(err)
	}
	if asks == nil {
		asks = []decide.AskRecord{}
	}
	return Response{Asks: &asks, AsksEnabled: &on}
}

func (s *Server) closeAsk(req Request) Response {
	ask, err := s.Backend.CloseAsk(req.Item, req.Choice, req.Text)
	if err != nil {
		return failed(err)
	}
	resp := Response{Ask: &ask}
	if it, gerr := s.Backend.Get(ask.ItemID); gerr == nil {
		resp.Item = &it
	}
	return resp
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
			if reason := DetectConflict(req.Op, req.BaseState, req.BaseMessageCount, baseLast, req.BaseAnswered, cur); reason != "" {
				return Response{Conflict: reason, Item: &cur}, true
			}
		}
	}
	return itemResponse(op()), true
}

// DetectConflict reports why a write made against a stale view of an
// item should not apply to cur, or "" when it is safe. baseState,
// baseCount, baseLast, and baseAnswered describe the item as the writer
// saw it.
func DetectConflict(op, baseState string, baseCount int, baseLast time.Time, baseAnswered bool, cur decide.Item) string {
	if cur.State == decide.StateWithdrawn && baseState != decide.StateWithdrawn {
		return "item was withdrawn"
	}
	if (cur.State == decide.StateDone || cur.State == decide.StateDoneUnread) && baseState != cur.State {
		if op == OpAnswer {
			return decide.AlreadyClosedMessage(cur.Owner)
		}
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

func (s *Server) panels() Response {
	store, err := decide.OpenPanels(s.Backend.Dir)
	if err != nil {
		return failed(err)
	}
	if s.Backend.Now != nil {
		store.SetClock(s.Backend.Now)
	}
	list, err := store.List()
	if err != nil {
		return failed(err)
	}
	return Response{Panels: &list}
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

func (s *Server) orgTree(ctx context.Context) Response {
	if s.OrgConfig == nil {
		return Response{Error: "the org tree is off; set ui.orgTree and ui.orgSpec"}
	}
	enabled, spec, err := s.OrgConfig()
	if err != nil {
		return failed(err)
	}
	if !enabled {
		return Response{Error: "the org tree is off on the hub; set ui.orgTree and ui.orgSpec"}
	}
	if spec == "" {
		return Response{Error: "ui.orgSpec is not set on the hub"}
	}
	var modTime time.Time
	if info, err := os.Stat(spec); err == nil {
		modTime = info.ModTime()
	}
	run := s.Orgctl
	if run == nil {
		run = tui.ExecOrgctl
	}
	runCtx, cancel := context.WithTimeout(ctx, tui.OrgctlTimeout)
	defer cancel()
	out, err := run(runCtx, []string{"tree", "--json", spec})
	if err != nil {
		return failed(err)
	}
	resp := Response{Output: string(out)}
	if !modTime.IsZero() {
		resp.OrgSpecModTime = &modTime
	}
	return resp
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
		tg = tui.Target{Name: found.Name, Pane: found.TargetPane()}
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

// connWatcher feeds one connection's watch request from a shared Watcher.
type connWatcher struct {
	watcher *Watcher
	reply   func(Response)

	mu      sync.Mutex
	watchID int64
}

func (c *connWatcher) response(id int64, ev WatchEvent) Response {
	return Response{
		ID:             id,
		Seq:            ev.Seq,
		Resync:         ev.Resync,
		Items:          ev.Items,
		Changes:        ev.Changes,
		Status:         ev.Status,
		Drafts:         ev.Drafts,
		DraftSeq:       ev.DraftSeq,
		OrgSpecModTime: ev.OrgSpecModTime,
		Panels:         ev.Panels,
		Asks:           ev.Asks,
		AsksEnabled:    ev.AsksEnabled,
	}
}

func (c *connWatcher) start(req Request) {
	c.mu.Lock()
	defer c.mu.Unlock()
	ev, err := c.watcher.Start(req.Since)
	if err != nil {
		c.reply(Response{ID: req.ID, Error: err.Error()})
		return
	}
	c.watchID = req.ID
	c.reply(c.response(req.ID, ev))
}

func (c *connWatcher) emit(ev WatchEvent) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.reply(c.response(c.watchID, ev))
}
