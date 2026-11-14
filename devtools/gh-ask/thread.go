package ghask

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
	"unicode"
)

const (
	// statusCacheTTL is how long a status_command result is reused.
	statusCacheTTL = 10 * time.Second
	// maxStateBytes caps the state string from status_command.
	maxStateBytes = 256
)

// ThreadStatus is the JSON body of GET /status/<id>.
type ThreadStatus struct {
	State    string `json:"state"`
	Answered bool   `json:"answered"`
	Closed   bool   `json:"closed"`
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
	if err != nil {
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
// bool answered and closed. Other fields are dropped.
func parseStatus(out []byte) (ThreadStatus, error) {
	var raw struct {
		State    *string `json:"state"`
		Answered *bool   `json:"answered"`
		Closed   *bool   `json:"closed"`
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
	return ThreadStatus{State: *raw.State, Answered: *raw.Answered, Closed: *raw.Closed}, nil
}
