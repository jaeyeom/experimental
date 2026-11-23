package web

import (
	"errors"
	"fmt"
	"net/http"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// conflictCheck describes the item as the writer last saw it. When
// present, the write applies only if inboxrpc.DetectConflict finds the
// item unchanged in ways that matter.
type conflictCheck struct {
	BaseState         string     `json:"baseState"`
	BaseMessageCount  int        `json:"baseMessageCount"`
	BaseLastMessageAt *time.Time `json:"baseLastMessageAt"`
	BaseAnswered      bool       `json:"baseAnswered"`
}

// itemReq is the union of the item write bodies. Each endpoint checks
// the fields it uses; unknown fields are refused.
type itemReq struct {
	WrittenAt     *time.Time     `json:"writtenAt"`
	ConflictCheck *conflictCheck `json:"conflictCheck"`
	Value         string         `json:"value"`
	Note          string         `json:"note"`
	Text          string         `json:"text"`
	Route         string         `json:"route"`
	Seen          *time.Time     `json:"seen"`
	Outcome       string         `json:"outcome"`
	Until         *time.Time     `json:"until"`
	Preset        string         `json:"preset"`
}

func (r *itemReq) writtenAt() time.Time {
	if r.WrittenAt == nil {
		return time.Time{}
	}
	return *r.WrittenAt
}

func noBody(*itemReq) error { return nil }

func tooLong(field, v string) error {
	if len(v) > maxText {
		return badRequest("%s is longer than %d bytes", field, maxText)
	}
	return nil
}

// writeItem serves one write to item {id}: it validates the body, checks
// for a conflict, runs write on the backend, and reports the item. A
// failed notify keeps the write and is reported as notifyError.
func (s *Server) writeItem(op string, check func(*itemReq) error, write func(id string, r *itemReq) (decide.Item, error)) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		id, err := itemID(r)
		if err != nil {
			fail(w, err, 0)
			return
		}
		var body itemReq
		if err := decodeBody(w, r, &body); err != nil {
			fail(w, err, 0)
			return
		}
		if err := check(&body); err != nil {
			fail(w, err, 0)
			return
		}
		if body.ConflictCheck != nil {
			cur, err := s.Backend.Get(id)
			if err != nil {
				fail(w, err, http.StatusInternalServerError)
				return
			}
			cc := body.ConflictCheck
			var baseLast time.Time
			if cc.BaseLastMessageAt != nil {
				baseLast = *cc.BaseLastMessageAt
			}
			if reason := inboxrpc.DetectConflict(op, cc.BaseState, cc.BaseMessageCount, baseLast, cc.BaseAnswered, cur); reason != "" {
				writeJSON(w, http.StatusConflict, map[string]any{"error": "conflict: " + reason, "conflict": reason, "item": cur})
				return
			}
		}
		it, err := write(id, &body)
		s.reportItem(w, it, err)
	}
}

// reportItem writes the result of an item write and wakes the watcher.
func (s *Server) reportItem(w http.ResponseWriter, it decide.Item, err error) {
	var notify *tui.NotifyError
	switch {
	case errors.As(err, &notify):
		s.Watcher().Signal()
		writeJSON(w, http.StatusOK, map[string]any{"item": it, "notifyError": notify.Error()})
	case errors.Is(err, decide.ErrNewerMessage):
		writeJSON(w, http.StatusConflict, map[string]any{"error": "new message: not archived", "code": "newer_message"})
	case err != nil:
		fail(w, err, http.StatusBadRequest)
	default:
		s.Watcher().Signal()
		writeJSON(w, http.StatusOK, map[string]any{"item": it})
	}
}

func (s *Server) markRead(id string, _ *itemReq) (decide.Item, error) {
	return s.Backend.MarkRead(id) //nolint:wrapcheck // the backend names its failure
}

func checkAnswer(r *itemReq) error {
	if r.Value == "" {
		return badRequest("value is required")
	}
	if err := tooLong("value", r.Value); err != nil {
		return err
	}
	return tooLong("note", r.Note)
}

// answer records the option answer, then the human note as a reply, as
// the TUI does with ui.answerAction "edit".
func (s *Server) answer(id string, r *itemReq) (decide.Item, error) {
	it, err := s.Backend.AnswerAt(id, r.Value, r.writtenAt())
	var notify *tui.NotifyError
	if err != nil && !errors.As(err, &notify) {
		return it, err //nolint:wrapcheck // the backend names its failure
	}
	if r.Note == "" {
		return it, err //nolint:wrapcheck // the backend names its failure
	}
	noted, nerr := s.Backend.ReplyAt(id, r.Note, "", r.writtenAt())
	if nerr != nil && !errors.As(nerr, &notify) {
		return it, nerr //nolint:wrapcheck // the backend names its failure
	}
	if err == nil {
		err = nerr
	}
	return noted, err //nolint:wrapcheck // the backend names its failure
}

func checkReply(r *itemReq) error {
	if r.Text == "" {
		return badRequest("text is required")
	}
	if r.Route != "" {
		if err := decide.CheckRoute(r.Route); err != nil {
			return badRequest("%v", err)
		}
	}
	return tooLong("text", r.Text)
}

func (s *Server) reply(id string, r *itemReq) (decide.Item, error) {
	return s.Backend.ReplyAt(id, r.Text, r.Route, r.writtenAt()) //nolint:wrapcheck // the backend names its failure
}

func needSeen(r *itemReq) error {
	if r.Seen == nil || r.Seen.IsZero() {
		return badRequest("seen is required")
	}
	return nil
}

func (s *Server) archive(id string, r *itemReq) (decide.Item, error) {
	return s.Backend.Archive(id, *r.Seen) //nolint:wrapcheck // the backend names its failure
}

func checkDone(r *itemReq) error { return tooLong("outcome", r.Outcome) }

func (s *Server) done(id string, r *itemReq) (decide.Item, error) {
	return s.Backend.Done(id, r.Outcome) //nolint:wrapcheck // the backend names its failure
}

// snoozeAt resolves the wake time of a snooze request: until as given,
// or a preset from the server clock in local time, as the TUI offers.
func (s *Server) snoozeAt(r *itemReq) (time.Time, error) {
	if (r.Until == nil) == (r.Preset == "") {
		return time.Time{}, badRequest("give exactly one of until and preset")
	}
	if r.Until != nil {
		return *r.Until, nil
	}
	presets := decide.SnoozePresets(s.now().In(time.Local))
	switch r.Preset {
	case "1h":
		return presets[0].At, nil
	case "3h":
		return presets[1].At, nil
	case "tomorrow9":
		return presets[2].At, nil
	case "nextmon9":
		return presets[3].At, nil
	}
	return time.Time{}, badRequest("unknown preset %q; use 1h, 3h, tomorrow9, or nextmon9", r.Preset)
}

func (s *Server) checkSnooze(r *itemReq) error {
	at, err := s.snoozeAt(r)
	if err != nil {
		return err
	}
	if !at.After(s.now()) {
		return badRequest("snooze time must be in the future")
	}
	return nil
}

func (s *Server) snooze(id string, r *itemReq) (decide.Item, error) {
	at, err := s.snoozeAt(r)
	if err != nil {
		return decide.Item{}, err
	}
	return s.Backend.Snooze(id, at) //nolint:wrapcheck // the backend names its failure
}

func (s *Server) unsnooze(id string, _ *itemReq) (decide.Item, error) {
	return s.Backend.Unsnooze(id) //nolint:wrapcheck // the backend names its failure
}

func (s *Server) archiveAll(w http.ResponseWriter, r *http.Request) {
	var body itemReq
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, 0)
		return
	}
	if err := needSeen(&body); err != nil {
		fail(w, err, 0)
		return
	}
	n, err := s.Backend.ArchiveNotes(*body.Seen)
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	s.Watcher().Signal()
	writeJSON(w, http.StatusOK, map[string]any{"archived": n})
}

type sendReq struct {
	To        string     `json:"to"`
	Bus       bool       `json:"bus"`
	Forward   string     `json:"forward"`
	Text      string     `json:"text"`
	WrittenAt *time.Time `json:"writtenAt"`
}

// send starts a thread to a configured target or an agentbus name,
// resolving the recipient as inboxrpc.Server does.
func (s *Server) send(w http.ResponseWriter, r *http.Request) {
	var body sendReq
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, 0)
		return
	}
	switch {
	case body.To == "":
		fail(w, badRequest("to is required"), 0)
		return
	case body.Text == "":
		fail(w, badRequest("text is required"), 0)
		return
	case len(body.To) > 256 || len(body.Forward) > 128:
		fail(w, badRequest("to or forward is too long"), 0)
		return
	}
	if err := tooLong("text", body.Text); err != nil {
		fail(w, err, 0)
		return
	}
	tg := tui.Target{Name: body.To, Bus: true}
	if !body.Bus {
		found, ok := decide.FindTarget(s.Targets, body.To)
		if !ok {
			fail(w, badRequest("decide send recipient %q is unknown on the hub", body.To), 0)
			return
		}
		tg = tui.Target{Name: found.Name, Pane: found.Pane}
	}
	var writtenAt time.Time
	if body.WrittenAt != nil {
		writtenAt = *body.WrittenAt
	}
	err := s.Backend.SendAt(body.Forward, tg, body.Text, writtenAt)
	var notify *tui.NotifyError
	switch {
	case errors.As(err, &notify):
		s.Watcher().Signal()
		writeJSON(w, http.StatusOK, map[string]any{"ok": true, "notifyError": notify.Error()})
	case err != nil:
		fail(w, err, http.StatusInternalServerError)
	default:
		s.Watcher().Signal()
		writeJSON(w, http.StatusOK, map[string]any{"ok": true})
	}
}

const maxFavorites = 1000

func (s *Server) setFavorites(w http.ResponseWriter, r *http.Request) {
	var body struct {
		Names []string `json:"names"`
	}
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, 0)
		return
	}
	if body.Names == nil {
		fail(w, badRequest("names is required"), 0)
		return
	}
	if len(body.Names) > maxFavorites {
		fail(w, badRequest("more than %d names", maxFavorites), 0)
		return
	}
	for _, n := range body.Names {
		if len(n) > 256 {
			fail(w, badRequest("a name is longer than 256 bytes"), 0)
			return
		}
	}
	if _, err := decide.SetFavoriteContacts(body.Names); err != nil {
		fail(w, fmt.Errorf("favorite contacts: %w", err), http.StatusInternalServerError)
		return
	}
	display, err := s.display()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	s.Watcher().Signal()
	writeJSON(w, http.StatusOK, map[string]any{"display": display})
}
