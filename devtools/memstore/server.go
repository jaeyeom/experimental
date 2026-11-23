package memstore

import (
	"crypto/subtle"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"slices"
	"strconv"
	"strings"
)

// ServerConfig configures the profile-scoped HTTP Server.
type ServerConfig struct {
	Inbox InboxTransport
	Token string
}

// Server exposes a minimal profile-scoped HTTP API bound to a single resolved
// Service profile at startup. Callers cannot switch profiles per request.
type Server struct {
	svc   *Service
	inbox InboxTransport
	token string
	mux   *http.ServeMux
}

// NewServer constructs a profile-scoped HTTP Server bound to svc.
func NewServer(svc *Service, cfg ServerConfig) *Server {
	s := &Server{
		svc:   svc,
		inbox: cfg.Inbox,
		token: cfg.Token,
		mux:   http.NewServeMux(),
	}
	s.mux.HandleFunc("GET /healthz", s.handleHealthz)
	s.mux.HandleFunc("GET /v1/search", s.requireAuth(s.handleSearch))
	s.mux.HandleFunc("GET /v1/entities/{ref}", s.requireAuth(s.handleGetEntity))
	s.mux.HandleFunc("POST /v1/inbox", s.requireAuth(s.handlePushInbox))
	s.mux.HandleFunc("GET /v1/inbox", s.requireAuth(s.handleListInbox))
	s.mux.HandleFunc("DELETE /v1/inbox/{id}", s.requireAuth(s.handleAckInbox))
	return s
}

// ServeHTTP implements http.Handler.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.mux.ServeHTTP(w, r)
}

func (s *Server) requireAuth(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if s.token != "" {
			auth := r.Header.Get("Authorization")
			got, ok := strings.CutPrefix(auth, "Bearer ")
			if !ok || subtle.ConstantTimeCompare([]byte(got), []byte(s.token)) != 1 {
				writeJSONError(w, http.StatusUnauthorized, "unauthorized")
				return
			}
		}
		next(w, r)
	}
}

func (s *Server) handleHealthz(w http.ResponseWriter, _ *http.Request) {
	profile := ""
	if s.svc != nil {
		profile = s.svc.policy.name
	}
	writeJSONResponse(w, http.StatusOK, map[string]string{
		"status":  "ok",
		"profile": profile,
	})
}

func (s *Server) handleSearch(w http.ResponseWriter, r *http.Request) {
	q := r.URL.Query()
	queryStr := q.Get("q")
	if strings.TrimSpace(queryStr) == "" {
		writeJSONError(w, http.StatusBadRequest, "query parameter q is required")
		return
	}
	var limit int
	if rawLimit := q.Get("limit"); rawLimit != "" {
		n, err := strconv.Atoi(rawLimit)
		if err != nil || n < 0 {
			writeJSONError(w, http.StatusBadRequest, "invalid limit parameter")
			return
		}
		limit = n
	}

	hits, err := NewSQLiteRetriever(s.svc).Search(r.Context(), SearchQuery{
		Query:    queryStr,
		Vault:    q.Get("vault"),
		Kinds:    q["kind"],
		EntityID: q.Get("entity"),
		Limit:    limit,
		Actor:    Actor{Caller: "http", Reason: "GET /v1/search"},
	})
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSONResponse(w, http.StatusOK, hits)
}

func (s *Server) handleGetEntity(w http.ResponseWriter, r *http.Request) {
	ref := r.PathValue("ref")
	if ref == "" {
		writeJSONError(w, http.StatusBadRequest, "entity ref is required")
		return
	}
	actor := Actor{Caller: "http", Reason: "GET /v1/entities"}
	matches, err := s.svc.ResolveEntity(r.Context(), ref, actor)
	if err != nil {
		var amb *AmbiguousError
		if errors.As(err, &amb) {
			writeJSONResponse(w, http.StatusConflict, map[string]any{
				"error":      amb.Error(),
				"candidates": amb.Candidates,
			})
			return
		}
		writeServiceError(w, err)
		return
	}

	view, err := s.svc.GetEntity(r.Context(), matches[0].Vault, matches[0].Entity.ID, actor)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSONResponse(w, http.StatusOK, view)
}

func (s *Server) handlePushInbox(w http.ResponseWriter, r *http.Request) {
	if s.inbox == nil {
		writeJSONError(w, http.StatusNotFound, "inbox transport not configured")
		return
	}
	var env InboxEnvelope
	if err := json.NewDecoder(http.MaxBytesReader(w, r.Body, 1<<20)).Decode(&env); err != nil {
		writeJSONError(w, http.StatusBadRequest, fmt.Sprintf("invalid json body: %v", err))
		return
	}
	if err := s.validateInboxEnvelope(env); err != nil {
		writeJSONError(w, http.StatusForbidden, err.Error())
		return
	}
	if env.ID == "" {
		id, err := newID("env")
		if err != nil {
			writeJSONError(w, http.StatusInternalServerError, err.Error())
			return
		}
		env.ID = id
	}
	if err := s.inbox.Push(r.Context(), env); err != nil {
		writeJSONError(w, http.StatusBadRequest, err.Error())
		return
	}
	writeJSONResponse(w, http.StatusCreated, env)
}

func (s *Server) validateInboxEnvelope(env InboxEnvelope) error {
	if s.svc == nil {
		return nil
	}
	comp := defaultCompartment(env.Compartment)
	if !slices.Contains(s.svc.policy.AllowedCompartments(), comp) {
		return fmt.Errorf("profile %q does not allow compartment %q", s.svc.policy.name, comp)
	}
	sens := defaultSensitivity(env.Sensitivity)
	rank, ok := sens.rank()
	if !ok {
		return fmt.Errorf("unknown sensitivity %q", sens)
	}
	if rank > s.svc.policy.maxRank {
		return fmt.Errorf("profile %q caps sensitivity at %q (requested %q)", s.svc.policy.name, s.svc.policy.maxSensitivity, sens)
	}
	return nil
}

func (s *Server) handleListInbox(w http.ResponseWriter, r *http.Request) {
	if s.inbox == nil {
		writeJSONError(w, http.StatusNotFound, "inbox transport not configured")
		return
	}
	var limit int
	if rawLimit := r.URL.Query().Get("limit"); rawLimit != "" {
		n, err := strconv.Atoi(rawLimit)
		if err != nil || n < 0 {
			writeJSONError(w, http.StatusBadRequest, "invalid limit parameter")
			return
		}
		limit = n
	}
	envelopes, err := s.inbox.List(r.Context(), limit)
	if err != nil {
		writeJSONError(w, http.StatusInternalServerError, err.Error())
		return
	}
	writeJSONResponse(w, http.StatusOK, envelopes)
}

func (s *Server) handleAckInbox(w http.ResponseWriter, r *http.Request) {
	if s.inbox == nil {
		writeJSONError(w, http.StatusNotFound, "inbox transport not configured")
		return
	}
	id := r.PathValue("id")
	if err := validateEnvelopeID(id); err != nil {
		writeJSONError(w, http.StatusBadRequest, err.Error())
		return
	}
	if err := s.inbox.Ack(r.Context(), id); err != nil {
		writeJSONError(w, http.StatusInternalServerError, err.Error())
		return
	}
	writeJSONResponse(w, http.StatusOK, map[string]any{
		"acked": true,
		"id":    id,
	})
}

func writeServiceError(w http.ResponseWriter, err error) {
	switch {
	case errors.Is(err, ErrForbidden):
		writeJSONError(w, http.StatusForbidden, err.Error())
	case errors.Is(err, ErrNotFound):
		writeJSONError(w, http.StatusNotFound, err.Error())
	default:
		writeJSONError(w, http.StatusInternalServerError, err.Error())
	}
}

func writeJSONError(w http.ResponseWriter, status int, msg string) {
	writeJSONResponse(w, status, map[string]string{"error": msg})
}

func writeJSONResponse(w http.ResponseWriter, status int, payload any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(payload)
}
