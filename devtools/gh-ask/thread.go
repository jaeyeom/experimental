package ghask

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"slices"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

const (
	// statusCacheTTL is how long a status_command result is reused.
	statusCacheTTL = 10 * time.Second
	// maxStateBytes caps the state string from status_command.
	maxStateBytes = 256
	// maxStatusMessages is how many messages GET /status returns.
	maxStatusMessages = 50
	// maxStatusTextBytes caps one message's text, on a UTF-8 boundary.
	maxStatusTextBytes = 16 * 1024
	// maxStatusBodyBytes caps the GET /status response body.
	maxStatusBodyBytes = 256 * 1024
)

// errStatusMessages means the messages array was present but not usable.
// The three required status fields are still returned.
var errStatusMessages = errors.New("invalid status messages")

// StatusMessage is one entry of the optional messages array.
type StatusMessage struct {
	Role string    `json:"role"`
	Time time.Time `json:"time"`
	Text string    `json:"text"`
}

// ThreadStatus is the JSON body of GET /status/<id>. Messages is omitted
// when status_command did not print a usable array.
type ThreadStatus struct {
	State    string          `json:"state"`
	Answered bool            `json:"answered"`
	Closed   bool            `json:"closed"`
	Messages []StatusMessage `json:"messages,omitempty"`
}

type cachedStatus struct {
	status ThreadStatus
	at     time.Time
}

// loadThreadsLocked loads the state file on first use. s.mu must be held.
func (s *Server) loadThreadsLocked() error {
	if s.threads != nil {
		return nil
	}
	threads, err := loadState(s.cfg.StateFile)
	if err != nil {
		return err
	}
	s.threads = threads
	return nil
}

// openThread returns the id stored for the PR unless its last known
// status is closed.
func (s *Server) openThread(repo string, pr int) (string, bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if err := s.loadThreadsLocked(); err != nil {
		return "", false, err
	}
	entry, ok := s.threads[stateKey(repo, pr)]
	if !ok || entry.Closed {
		return "", false, nil
	}
	return entry.ID, true, nil
}

// recordThread remembers id as created here and, with ReusePerPR, stores
// it as the PR's thread. A failed save is logged; the thread exists.
func (s *Server) recordThread(repo string, pr int, id string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.created[id] = true
	if s.cfg.Reuse != ReusePerPR {
		return
	}
	if err := s.loadThreadsLocked(); err != nil {
		s.logger.Error("gh-ask: state file", "err", err)
		return
	}
	s.threads[stateKey(repo, pr)] = threadEntry{ID: id}
	if err := saveState(s.cfg.StateFile, s.threads); err != nil {
		s.logger.Error("gh-ask: state file", "err", err)
	}
}

// knownID reports whether this server created id, in this process or, with
// ReusePerPR, in an earlier one that stored it.
func (s *Server) knownID(id string) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.created[id] {
		return true
	}
	if s.cfg.Reuse != ReusePerPR {
		return false
	}
	if err := s.loadThreadsLocked(); err != nil {
		s.logger.Error("gh-ask: state file", "err", err)
		return false
	}
	for _, entry := range s.threads {
		if entry.ID == id {
			return true
		}
	}
	return false
}

// noteClosed stores the last known closed value for id's PR threads.
func (s *Server) noteClosed(id string, closed bool) {
	if s.cfg.Reuse != ReusePerPR {
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if err := s.loadThreadsLocked(); err != nil {
		s.logger.Error("gh-ask: state file", "err", err)
		return
	}
	changed := false
	for key, entry := range s.threads {
		if entry.ID == id && entry.Closed != closed {
			entry.Closed = closed
			s.threads[key] = entry
			changed = true
		}
	}
	if !changed {
		return
	}
	if err := saveState(s.cfg.StateFile, s.threads); err != nil {
		s.logger.Error("gh-ask: state file", "err", err)
	}
}

// serveStatus answers GET /status/<id> for ids this server created. Any
// other id, and every id when status_command is unset, gets 404.
func (s *Server) serveStatus(w http.ResponseWriter, r *http.Request) {
	id := strings.TrimPrefix(r.URL.Path, statusPrefix)
	if len(s.cfg.StatusCommand) == 0 || !validID(id) || !s.knownID(id) {
		s.logReject(http.StatusNotFound, "unknown id", "", 0)
		writeJSON(w, http.StatusNotFound, errorBody{"not found"})
		return
	}
	st, err := s.status(r.Context(), id)
	if err != nil {
		writeJSON(w, http.StatusBadGateway, errorBody{err.Error()})
		return
	}
	writeJSON(w, http.StatusOK, st)
}

// status returns id's status from the cache, or runs status_command and
// caches a valid result for statusCacheTTL. Failures are not cached.
func (s *Server) status(ctx context.Context, id string) (ThreadStatus, error) {
	s.mu.Lock()
	cached, ok := s.statuses[id]
	s.mu.Unlock()
	if ok && s.now().Sub(cached.at) < statusCacheTTL {
		return cached.status, nil
	}
	out, err := s.run(ctx, "status_command", s.cfg.StatusCommand, map[string]string{"id": id}, "id", id)
	if err != nil {
		return ThreadStatus{}, err
	}
	st, err := parseStatus(out)
	if errors.Is(err, errStatusMessages) {
		s.logger.Error("gh-ask: status messages", "id", id, "err", err)
	} else if err != nil {
		s.logger.Error("gh-ask: status_command output", "id", id, "err", err)
		return ThreadStatus{}, errors.New("status_command printed bad JSON")
	}
	s.mu.Lock()
	s.statuses[id] = cachedStatus{status: st, at: s.now()}
	s.mu.Unlock()
	s.noteClosed(id, st.Closed)
	return st, nil
}

// parseStatus accepts exactly one JSON object with a string state and
// bool answered and closed. An optional messages array is cleaned and
// returned oldest first. A bad messages array returns the three fields
// and errStatusMessages. Other fields are dropped.
func parseStatus(out []byte) (ThreadStatus, error) {
	var raw struct {
		State    *string         `json:"state"`
		Answered *bool           `json:"answered"`
		Closed   *bool           `json:"closed"`
		Messages json.RawMessage `json:"messages"`
	}
	dec := json.NewDecoder(bytes.NewReader(out))
	if err := dec.Decode(&raw); err != nil {
		return ThreadStatus{}, fmt.Errorf("decode: %w", err)
	}
	if err := dec.Decode(&struct{}{}); !errors.Is(err, io.EOF) {
		return ThreadStatus{}, errors.New("trailing data after the object")
	}
	if raw.State == nil || raw.Answered == nil || raw.Closed == nil {
		return ThreadStatus{}, errors.New("state, answered and closed are required")
	}
	if len(*raw.State) > maxStateBytes || strings.ContainsFunc(*raw.State, unicode.IsControl) {
		return ThreadStatus{}, errors.New("state is too long or has control characters")
	}
	st := ThreadStatus{State: *raw.State, Answered: *raw.Answered, Closed: *raw.Closed}
	msgs, err := cleanStatusMessages(raw.Messages)
	if err != nil {
		return st, err
	}
	st.Messages = msgs
	return fitStatusBody(st), nil
}

// cleanStatusMessages validates messages. A missing or null array is
// omitted. The wrong JSON type, a missing field, or an unparseable time
// rejects the whole array.
func cleanStatusMessages(raw json.RawMessage) ([]StatusMessage, error) {
	if len(raw) == 0 || string(raw) == "null" {
		return nil, nil
	}
	var in []struct {
		Role *string `json:"role"`
		Time *string `json:"time"`
		Text *string `json:"text"`
	}
	if err := json.Unmarshal(raw, &in); err != nil {
		return nil, errStatusMessages
	}
	msgs := make([]StatusMessage, 0, len(in))
	for _, m := range in {
		msg, ok, err := oneStatusMessage(m.Role, m.Time, m.Text)
		if err != nil {
			return nil, err
		}
		if ok {
			msgs = append(msgs, msg)
		}
	}
	sort.SliceStable(msgs, func(i, j int) bool {
		return msgs[i].Time.Before(msgs[j].Time)
	})
	if len(msgs) > maxStatusMessages {
		msgs = msgs[len(msgs)-maxStatusMessages:]
	}
	if len(msgs) == 0 {
		return nil, nil
	}
	return msgs, nil
}

func oneStatusMessage(role, when, text *string) (StatusMessage, bool, error) {
	if role == nil || when == nil || text == nil {
		return StatusMessage{}, false, errStatusMessages
	}
	parsed, err := time.Parse(time.RFC3339, *when)
	if err != nil {
		return StatusMessage{}, false, errStatusMessages
	}
	switch *role {
	case "you", "agent", "note":
	default:
		return StatusMessage{}, false, nil
	}
	return StatusMessage{
		Role: *role,
		Time: parsed,
		Text: truncateUTF8(cleanStatusText(*text), maxStatusTextBytes),
	}, true, nil
}

func cleanStatusText(s string) string {
	return strings.Map(func(r rune) rune {
		if r == '\n' || r == '\t' || !unicode.IsControl(r) {
			return r
		}
		return -1
	}, s)
}

func truncateUTF8(s string, maxBytes int) string {
	if len(s) <= maxBytes {
		return s
	}
	i := 0
	for i < len(s) {
		_, size := utf8.DecodeRuneInString(s[i:])
		if i+size > maxBytes {
			break
		}
		i += size
	}
	return s[:i]
}

// fitStatusBody drops the oldest messages until the encoded response,
// including the newline writeJSON adds, fits in maxStatusBodyBytes.
func fitStatusBody(st ThreadStatus) ThreadStatus {
	for len(st.Messages) > 0 {
		body, err := json.Marshal(st)
		if err != nil {
			st.Messages = nil
			return st
		}
		if len(body)+1 <= maxStatusBodyBytes {
			break
		}
		st.Messages = st.Messages[1:]
	}
	if len(st.Messages) == 0 {
		st.Messages = nil
	}
	return st
}

// serveThread answers GET /thread?repo=&pr= from the state file. It runs
// no command. reuse other than per-pr, and a PR with no stored id, are 404.
func (s *Server) serveThread(w http.ResponseWriter, r *http.Request) {
	repo, pr := queryRepoPR(r)
	if s.cfg.Reuse != ReusePerPR {
		s.logReject(http.StatusNotFound, "not found", repo, pr)
		writeJSON(w, http.StatusNotFound, errorBody{"not found"})
		return
	}
	q := r.URL.Query()
	if !slices.Contains(s.cfg.Repos, q.Get("repo")) {
		s.logReject(http.StatusForbidden, "repo not allowed", repo, pr)
		writeJSON(w, http.StatusForbidden, errorBody{"repo not allowed"})
		return
	}
	parsed, err := strconv.Atoi(q.Get("pr"))
	if err != nil || parsed <= 0 {
		s.logReject(http.StatusBadRequest, "bad request", repo, 0)
		writeJSON(w, http.StatusBadRequest, errorBody{"bad pr"})
		return
	}
	id, ok, err := s.storedThreadID(q.Get("repo"), parsed)
	if err != nil {
		s.logger.Error("gh-ask: state file", "err", err)
		writeJSON(w, http.StatusBadGateway, errorBody{"state file"})
		return
	}
	if !ok {
		s.logReject(http.StatusNotFound, "not found", repo, parsed)
		writeJSON(w, http.StatusNotFound, errorBody{"not found"})
		return
	}
	writeJSON(w, http.StatusOK, struct {
		ID string `json:"id"`
	}{ID: id})
}

// storedThreadID returns the id stored for the PR, including a closed one.
// s.mu is taken here. A missing entry is ok false.
func (s *Server) storedThreadID(repo string, pr int) (string, bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if err := s.loadThreadsLocked(); err != nil {
		return "", false, err
	}
	entry, ok := s.threads[stateKey(repo, pr)]
	if !ok || entry.ID == "" {
		return "", false, nil
	}
	return entry.ID, true, nil
}
