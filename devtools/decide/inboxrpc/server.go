package inboxrpc

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"sync"

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
}

// Serve answers each request line from r on w until r ends. Requests
// run concurrently; each response is one line.
func (s *Server) Serve(ctx context.Context, r io.Reader, w io.Writer) error {
	var mu sync.Mutex
	var wg sync.WaitGroup
	defer wg.Wait()
	enc := json.NewEncoder(w)
	reply := func(resp Response) {
		mu.Lock()
		defer mu.Unlock()
		_ = enc.Encode(resp) // a closed client has nobody to tell
	}
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 64<<10), maxLine)
	for sc.Scan() {
		var req Request
		if err := json.Unmarshal(sc.Bytes(), &req); err != nil {
			reply(Response{Error: "decode request: " + err.Error()})
			continue
		}
		wg.Add(1)
		go func() {
			defer wg.Done()
			resp := s.handle(ctx, req)
			resp.ID = req.ID
			reply(resp)
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
	b := s.Backend
	switch req.Op {
	case OpHello:
		return Response{Version: Version}
	case OpList:
		items, err := b.List()
		if err != nil {
			return failed(err)
		}
		if items == nil {
			items = []decide.Item{}
		}
		return Response{Items: items}
	case OpStatus:
		board, err := b.Status()
		if err != nil {
			return failed(err)
		}
		return Response{Status: &board}
	case OpTargets:
		return Response{Targets: s.Targets, Agentbus: s.Agentbus}
	case OpBusList:
		return s.busList(ctx)
	case OpConfig:
		return s.config()
	case OpSend:
		return s.send(req)
	case OpLocate:
		it, err := b.Get(req.Item)
		if err != nil {
			return failed(err)
		}
		loc := b.Locate(ctx, &it)
		return Response{Location: &loc}
	default:
		return Response{Error: fmt.Sprintf("unknown op %q", req.Op)}
	}
}

// itemOp answers an op on one item and reports whether req is one.
func (s *Server) itemOp(req Request) (Response, bool) {
	b := s.Backend
	ops := map[string]func() (decide.Item, error){
		OpShow:     func() (decide.Item, error) { return b.Get(req.Item) },
		OpMarkRead: func() (decide.Item, error) { return b.MarkRead(req.Item) },
		OpAnswer:   func() (decide.Item, error) { return b.Answer(req.Item, req.Text) },
		OpReply:    func() (decide.Item, error) { return b.Reply(req.Item, req.Text) },
		OpDone:     func() (decide.Item, error) { return b.Done(req.Item, req.Text) },
		OpArchive:  func() (decide.Item, error) { return b.Done(req.Item, "archived") },
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
	return itemResponse(op()), true
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

func (s *Server) busList(ctx context.Context) Response {
	run := s.Backend.BusRun
	if run == nil {
		run = decide.ExecAgentbus
	}
	out, err := run(ctx, []string{"list", "--json"})
	if err != nil {
		return failed(err)
	}
	return Response{Output: string(out)}
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
	if err := s.Backend.Send(req.Forward, tg, req.Text); err != nil {
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
