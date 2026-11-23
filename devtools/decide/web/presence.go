package web

import (
	"net/http"
	"time"
)

// presenceTTL is how long an active client counts without a heartbeat.
const presenceTTL = 60 * time.Second

// maxClientID bounds a client id.
const maxClientID = 128

// HasActiveClient reports whether any web client sent an active
// heartbeat within presenceTTL, so Web Push can skip while one is open.
func (s *Server) HasActiveClient() bool {
	st := s.state()
	now := s.now()
	st.mu.Lock()
	defer st.mu.Unlock()
	active := false
	for id, exp := range st.clients {
		if now.Before(exp) {
			active = true
		} else {
			delete(st.clients, id)
		}
	}
	return active
}

func (s *Server) presence(w http.ResponseWriter, r *http.Request) {
	var body struct {
		ClientID string `json:"clientId"`
		Active   bool   `json:"active"`
	}
	if err := decodeBody(w, r, &body); err != nil {
		fail(w, err, http.StatusBadRequest)
		return
	}
	if body.ClientID == "" || len(body.ClientID) > maxClientID {
		fail(w, badRequest("clientId is required (at most %d bytes)", maxClientID), 0)
		return
	}
	st := s.state()
	st.mu.Lock()
	if body.Active {
		if st.clients == nil {
			st.clients = map[string]time.Time{}
		}
		st.clients[body.ClientID] = s.now().Add(presenceTTL)
	} else {
		delete(st.clients, body.ClientID)
	}
	st.mu.Unlock()
	writeJSON(w, http.StatusOK, map[string]any{"ok": true})
}
