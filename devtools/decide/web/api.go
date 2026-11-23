package web

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"regexp"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
)

// maxBody bounds a request body. Drafts are the largest legitimate
// payload and cap at decide.MaxDraftTextBytes.
const maxBody = 1 << 20

// maxText bounds the free text of one answer, reply, or send.
const maxText = 256 << 10

// validID matches ledger item ids such as D12 or A3 and draft keys such
// as reply:D12. It keeps path values out of ledger lookups unchecked.
var validID = regexp.MustCompile(`^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$`)

// apiState is the server's shared API state: the watcher the write
// handlers signal, the latest bus health, and the org tree cache.
type apiState struct {
	watcher *inboxrpc.Watcher

	mu        sync.Mutex
	busHealth json.RawMessage
	orgSpec   string
	orgMod    time.Time
	orgTree   json.RawMessage
}

func (s *Server) state() *apiState {
	s.stateOnce.Do(func() {
		s.api = &apiState{watcher: &inboxrpc.Watcher{Backend: s.Backend, OrgConfig: s.OrgConfig}}
	})
	return s.api
}

// Watcher is the shared hub watcher, signalled after every write.
func (s *Server) Watcher() *inboxrpc.Watcher { return s.state().watcher }

// apiRoutes registers /api/v1 on mux. Auth and CSRF wrap the mux.
func (s *Server) apiRoutes(mux *http.ServeMux) {
	mux.HandleFunc("GET /api/v1/snapshot", s.snapshot)
	mux.HandleFunc("GET /api/v1/items/{id}", s.getItem)
	mux.HandleFunc("GET /api/v1/status", s.getStatus)
	mux.HandleFunc("GET /api/v1/contacts", s.contacts)
	mux.HandleFunc("GET /api/v1/org-tree", s.orgTree)
	mux.HandleFunc("GET /api/v1/search", s.search)

	mux.HandleFunc("POST /api/v1/items/{id}/read", s.writeItem(inboxrpc.OpMarkRead, noBody, s.markRead))
	mux.HandleFunc("POST /api/v1/items/{id}/answer", s.writeItem(inboxrpc.OpAnswer, checkAnswer, s.answer))
	mux.HandleFunc("POST /api/v1/items/{id}/reply", s.writeItem(inboxrpc.OpReply, checkReply, s.reply))
	mux.HandleFunc("POST /api/v1/items/{id}/archive", s.writeItem(inboxrpc.OpArchive, needSeen, s.archive))
	mux.HandleFunc("POST /api/v1/items/{id}/done", s.writeItem(inboxrpc.OpDone, checkDone, s.done))
	mux.HandleFunc("POST /api/v1/items/{id}/snooze", s.writeItem(inboxrpc.OpSnooze, s.checkSnooze, s.snooze))
	mux.HandleFunc("POST /api/v1/items/{id}/unsnooze", s.writeItem(inboxrpc.OpUnsnooze, noBody, s.unsnooze))
	mux.HandleFunc("POST /api/v1/notes/archive-all", s.archiveAll)
	mux.HandleFunc("POST /api/v1/send", s.send)
	mux.HandleFunc("POST /api/v1/contacts/favorites", s.setFavorites)
	mux.HandleFunc("PUT /api/v1/drafts/{key}", s.putDraft)
	mux.HandleFunc("DELETE /api/v1/drafts/{key}", s.deleteDraft)
}

type apiError struct {
	status int
	msg    string
	extra  map[string]any
}

func (e *apiError) Error() string { return e.msg }

func badRequest(format string, a ...any) *apiError {
	return &apiError{status: http.StatusBadRequest, msg: fmt.Sprintf(format, a...)}
}

func writeError(w http.ResponseWriter, status int, msg string) {
	writeJSON(w, status, map[string]any{"error": msg})
}

// fail writes err as a JSON error: an *apiError as itself, a missing
// item as 404, and anything else as fallback.
func fail(w http.ResponseWriter, err error, fallback int) {
	var ae *apiError
	switch {
	case errors.As(err, &ae):
		body := map[string]any{"error": ae.msg}
		for k, v := range ae.extra {
			body[k] = v
		}
		writeJSON(w, ae.status, body)
	case errors.Is(err, decide.ErrNotFound):
		writeError(w, http.StatusNotFound, err.Error())
	case errors.Is(err, decide.ErrDraftTooLarge):
		writeError(w, http.StatusRequestEntityTooLarge, err.Error())
	default:
		writeError(w, fallback, err.Error())
	}
}

// decodeBody reads one JSON object from r into v, refusing unknown
// fields, trailing data, and bodies over maxBody. An empty body leaves v
// as it was.
func decodeBody(w http.ResponseWriter, r *http.Request, v any) error {
	dec := json.NewDecoder(http.MaxBytesReader(w, r.Body, maxBody))
	dec.DisallowUnknownFields()
	if err := dec.Decode(v); err != nil {
		var tooBig *http.MaxBytesError
		switch {
		case errors.Is(err, io.EOF):
			return nil
		case errors.As(err, &tooBig):
			return &apiError{status: http.StatusRequestEntityTooLarge, msg: "request body too large"}
		}
		return badRequest("invalid JSON body: %v", err)
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		var tooBig *http.MaxBytesError
		if errors.As(err, &tooBig) {
			return &apiError{status: http.StatusRequestEntityTooLarge, msg: "request body too large"}
		}
		return badRequest("invalid JSON body: unexpected data after the object")
	}
	return nil
}

func itemID(r *http.Request) (string, error) {
	id := r.PathValue("id")
	if !validID.MatchString(id) {
		return "", badRequest("invalid item id %q", id)
	}
	return id, nil
}

// busRun is the agentbus runner the backend uses.
func (s *Server) busRun() decide.AgentbusRunner {
	if s.Backend.BusRun != nil {
		return s.Backend.BusRun
	}
	return decide.ExecAgentbus
}

// refreshBus runs `agentbus health --json` and checks waiting
// deliveries, caching the health report for the snapshot. Health exits
// nonzero while the bus is down, so its report is kept whenever it
// printed one.
func (s *Server) refreshBus(ctx context.Context) error {
	if !s.Agentbus || s.Backend == nil {
		return nil
	}
	out, err := s.busRun()(ctx, []string{"health", "--json"})
	if trimmed := bytes.TrimSpace(out); json.Valid(trimmed) {
		st := s.state()
		st.mu.Lock()
		st.busHealth = append(json.RawMessage(nil), trimmed...)
		st.mu.Unlock()
		err = nil
	}
	led, lerr := decide.Open(s.Backend.Dir)
	if lerr != nil {
		return fmt.Errorf("open decide ledger: %w", lerr)
	}
	items, lerr := led.List()
	if lerr != nil {
		return fmt.Errorf("list decide ledger: %w", lerr)
	}
	for i := range items {
		if decide.HasPending(&items[i]) {
			if cerr := s.Backend.CheckDeliveries(ctx); cerr != nil && err == nil {
				err = cerr
			}
			s.Watcher().Signal()
			break
		}
	}
	return err //nolint:wrapcheck // agentbus names its own failure
}

// busLoop refreshes bus health and deliveries until ctx is done, so a
// snapshot never waits on a slow subprocess.
func (s *Server) busLoop(ctx context.Context) {
	every := s.Config.BusHealthInterval()
	t := time.NewTicker(every)
	defer t.Stop()
	for {
		if err := s.refreshBus(ctx); err != nil && ctx.Err() == nil && s.Report != nil {
			s.Report(err)
		}
		select {
		case <-ctx.Done():
			return
		case <-t.C:
		}
	}
}
