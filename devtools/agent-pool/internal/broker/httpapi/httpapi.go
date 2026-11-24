// Package httpapi implements the Worker HTTPS API, SSE wakeup stream, and
// Producer REST API for the agent-pool central broker.
package httpapi

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const (
	maxJSONBodyBytes         = 2 << 20 // 2 MiB.
	defaultKeepaliveInterval = 20 * time.Second
)

type tokenContextKey struct{}

// Options configures optional behavior of the broker HTTP handler.
type Options struct {
	KeepaliveInterval time.Duration
	MCPHandler        http.Handler
}

type server struct {
	svc               *broker.Service
	store             broker.Store
	keepaliveInterval time.Duration
}

// NewHandler constructs the broker HTTP ServeMux with Worker HTTPS, SSE,
// Producer REST, healthz, and optional MCP routes.
func NewHandler(svc *broker.Service, store broker.Store, opts Options) http.Handler {
	keepalive := opts.KeepaliveInterval
	if keepalive <= 0 {
		keepalive = defaultKeepaliveInterval
	}
	s := &server{
		svc:               svc,
		store:             store,
		keepaliveInterval: keepalive,
	}

	mux := http.NewServeMux()
	mux.HandleFunc("GET /healthz", s.handleHealthz)

	// Worker endpoints.
	mux.Handle("POST /workers/heartbeat", s.requireRole(broker.TokenRoleWorker, s.handleWorkerHeartbeat))
	mux.Handle("GET /workers/events", s.requireRole(broker.TokenRoleWorker, s.handleWorkerEvents))
	mux.Handle("POST /workers/lease", s.requireRole(broker.TokenRoleWorker, s.handleWorkerLease))
	mux.Handle("POST /workers/jobs/{jobId}/heartbeat", s.requireRole(broker.TokenRoleWorker, s.handleJobHeartbeat))
	mux.Handle("POST /workers/jobs/{jobId}/complete", s.requireRole(broker.TokenRoleWorker, s.handleJobComplete))

	// Producer REST endpoints.
	mux.Handle("GET /v1/status", s.requireRole(broker.TokenRoleProducer, s.handlePoolStatus))
	mux.Handle("POST /v1/jobs", s.requireRole(broker.TokenRoleProducer, s.handleDispatchJob))
	mux.Handle("GET /v1/jobs/{jobId}", s.requireRole(broker.TokenRoleProducer, s.handleGetJob))
	mux.Handle("POST /v1/jobs/{jobId}/cancel", s.requireRole(broker.TokenRoleProducer, s.handleCancelJob))

	if opts.MCPHandler != nil {
		mux.Handle("POST /mcp", opts.MCPHandler)
	}
	return mux
}

func (s *server) requireRole(role broker.TokenRole, next http.HandlerFunc) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		token, ok := extractBearerToken(r.Header.Get("Authorization"))
		if !ok {
			writeAPIError(w, &protocol.APIError{
				Code:    protocol.ErrCodeUnauthorized,
				Message: "missing or invalid Authorization bearer token",
			})
			return
		}
		rec, err := s.store.AuthenticateToken(r.Context(), token)
		if err != nil {
			writeAPIError(w, &protocol.APIError{
				Code:    protocol.ErrCodeUnauthorized,
				Message: "invalid or revoked bearer token",
			})
			return
		}
		if rec.Role != role {
			writeAPIError(w, &protocol.APIError{
				Code:    protocol.ErrCodeForbidden,
				Message: fmt.Sprintf("endpoint requires %s role", role),
			})
			return
		}
		ctx := context.WithValue(r.Context(), tokenContextKey{}, rec)
		next(w, r.WithContext(ctx))
	})
}

func tokenFromContext(ctx context.Context) broker.TokenRecord {
	rec, _ := ctx.Value(tokenContextKey{}).(broker.TokenRecord)
	return rec
}

func (s *server) handleHealthz(w http.ResponseWriter, _ *http.Request) {
	writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
}

func (s *server) handleWorkerHeartbeat(w http.ResponseWriter, r *http.Request) {
	var req protocol.WorkerHeartbeatRequest
	if !decodeJSONBody(w, r, &req) {
		return
	}
	resp, err := s.svc.WorkerHeartbeat(r.Context(), tokenFromContext(r.Context()), req)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSON(w, http.StatusOK, resp)
}

func (s *server) handleWorkerEvents(w http.ResponseWriter, r *http.Request) {
	worker := tokenFromContext(r.Context())
	repos, err := resolveSSERepos(worker.AllowedRepos, r.URL.Query().Get("repos"))
	if err != nil {
		writeServiceError(w, err)
		return
	}

	flusher, ok := w.(http.Flusher)
	if !ok {
		writeAPIError(w, &protocol.APIError{
			Code:    protocol.ErrCodeInternal,
			Message: "streaming unsupported",
		})
		return
	}

	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")
	w.Header().Set("X-Accel-Buffering", "no")
	w.WriteHeader(http.StatusOK)

	ch, unsubscribe := s.svc.Hub().SubscribeWorker(worker.ID, repos)
	defer unsubscribe()

	_, _ = io.WriteString(w, ": connected\n\n")
	flusher.Flush()

	s.streamWorkerEvents(r.Context(), w, flusher, ch)
}

func (s *server) streamWorkerEvents(ctx context.Context, w io.Writer, flusher http.Flusher, ch <-chan broker.WorkerEvent) {
	ticker := time.NewTicker(s.keepaliveInterval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case ev, ok := <-ch:
			if !ok {
				return
			}
			payload, err := json.Marshal(ev.Data)
			if err != nil {
				continue
			}
			var frame strings.Builder
			frame.WriteString("event: ")
			frame.WriteString(string(ev.Type))
			frame.WriteString("\ndata: ")
			frame.Write(payload)
			frame.WriteString("\n\n")
			_, _ = io.WriteString(w, frame.String())
			flusher.Flush()
		case <-ticker.C:
			_, _ = io.WriteString(w, ": keepalive\n\n")
			flusher.Flush()
		}
	}
}

func (s *server) handleWorkerLease(w http.ResponseWriter, r *http.Request) {
	var req protocol.LeaseRequest
	if !decodeJSONBody(w, r, &req) {
		return
	}
	resp, ok, err := s.svc.ClaimLease(r.Context(), tokenFromContext(r.Context()), req)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	if !ok {
		w.WriteHeader(http.StatusNoContent)
		return
	}
	writeJSON(w, http.StatusOK, resp)
}

func (s *server) handleJobHeartbeat(w http.ResponseWriter, r *http.Request) {
	var req protocol.JobHeartbeatRequest
	if !decodeJSONBody(w, r, &req) {
		return
	}
	jobID := r.PathValue("jobId")
	resp, err := s.svc.JobHeartbeat(r.Context(), tokenFromContext(r.Context()), jobID, req)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSON(w, http.StatusOK, resp)
}

func (s *server) handleJobComplete(w http.ResponseWriter, r *http.Request) {
	var req protocol.CompleteJobRequest
	if !decodeJSONBody(w, r, &req) {
		return
	}
	jobID := r.PathValue("jobId")
	resp, err := s.svc.CompleteJob(r.Context(), tokenFromContext(r.Context()), jobID, req)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSON(w, http.StatusOK, resp)
}

func (s *server) handlePoolStatus(w http.ResponseWriter, r *http.Request) {
	resp, err := s.svc.PoolStatus(r.Context(), tokenFromContext(r.Context()))
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSON(w, http.StatusOK, resp)
}

func (s *server) handleDispatchJob(w http.ResponseWriter, r *http.Request) {
	var req protocol.DispatchRequest
	if !decodeJSONBody(w, r, &req) {
		return
	}
	resp, err := s.svc.DispatchSkill(r.Context(), tokenFromContext(r.Context()), req)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSON(w, http.StatusCreated, resp)
}

func (s *server) handleGetJob(w http.ResponseWriter, r *http.Request) {
	waitDur, err := parseWaitParam(r.URL.Query().Get("wait"))
	if err != nil {
		writeServiceError(w, err)
		return
	}
	jobID := r.PathValue("jobId")
	requester := tokenFromContext(r.Context())
	resp, err := s.svc.GetJobResult(r.Context(), requester.ID, jobID, waitDur)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSON(w, http.StatusOK, resp)
}

func (s *server) handleCancelJob(w http.ResponseWriter, r *http.Request) {
	jobID := r.PathValue("jobId")
	requester := tokenFromContext(r.Context())
	resp, err := s.svc.CancelJob(r.Context(), requester.ID, jobID)
	if err != nil {
		writeServiceError(w, err)
		return
	}
	writeJSON(w, http.StatusOK, resp)
}

func extractBearerToken(header string) (string, bool) {
	parts := strings.SplitN(strings.TrimSpace(header), " ", 2)
	if len(parts) != 2 || !strings.EqualFold(parts[0], "Bearer") {
		return "", false
	}
	tok := strings.TrimSpace(parts[1])
	return tok, tok != ""
}

func resolveSSERepos(allowedRepos []string, query string) ([]string, error) {
	requested := splitCSV(query)
	if len(requested) == 0 {
		return allowedRepos, nil
	}
	if len(allowedRepos) > 0 {
		for _, repo := range requested {
			if !slices.Contains(allowedRepos, repo) {
				return nil, &protocol.APIError{
					Code:    protocol.ErrCodeForbidden,
					Message: fmt.Sprintf("worker token is not allowed for repo %q", repo),
				}
			}
		}
	}
	return requested, nil
}

func splitCSV(raw string) []string {
	if strings.TrimSpace(raw) == "" {
		return nil
	}
	var out []string
	for _, part := range strings.Split(raw, ",") {
		trimmed := strings.TrimSpace(part)
		if trimmed != "" {
			out = append(out, trimmed)
		}
	}
	return out
}

func parseWaitParam(raw string) (time.Duration, error) {
	if raw == "" {
		return 0, nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil || d < 0 {
		return 0, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidArgs,
			Message: fmt.Sprintf("invalid wait duration %q", raw),
		}
	}
	return d, nil
}

func decodeJSONBody(w http.ResponseWriter, r *http.Request, dst any) bool {
	r.Body = http.MaxBytesReader(w, r.Body, maxJSONBodyBytes)
	dec := json.NewDecoder(r.Body)
	if err := dec.Decode(dst); err != nil {
		var maxErr *http.MaxBytesError
		if errors.As(err, &maxErr) {
			writeJSON(w, http.StatusRequestEntityTooLarge, protocol.APIError{
				Code:    protocol.ErrCodeInvalidArgs,
				Message: "request body too large",
			})
			return false
		}
		writeAPIError(w, &protocol.APIError{
			Code:    protocol.ErrCodeInvalidArgs,
			Message: fmt.Sprintf("invalid JSON body: %v", err),
		})
		return false
	}
	return true
}

func writeServiceError(w http.ResponseWriter, err error) {
	var apiErr *protocol.APIError
	if errors.As(err, &apiErr) {
		writeAPIError(w, apiErr)
		return
	}
	writeAPIError(w, &protocol.APIError{
		Code:    protocol.ErrCodeInternal,
		Message: "internal server error",
	})
}

func writeAPIError(w http.ResponseWriter, apiErr *protocol.APIError) {
	writeJSON(w, statusCodeForError(apiErr.Code), apiErr)
}

func statusCodeForError(code protocol.ErrorCode) int {
	switch code {
	case protocol.ErrCodeInvalidArgs,
		protocol.ErrCodeUnknownRepo,
		protocol.ErrCodeUnknownSkill,
		protocol.ErrCodeOutputInvalid,
		protocol.ErrCodeCatalogMismatch:
		return http.StatusBadRequest
	case protocol.ErrCodeUnauthorized:
		return http.StatusUnauthorized
	case protocol.ErrCodeForbidden:
		return http.StatusForbidden
	case protocol.ErrCodeNotFound:
		return http.StatusNotFound
	case protocol.ErrCodeLeaseExpired, protocol.ErrCodeInvalidTransition:
		return http.StatusConflict
	case protocol.ErrCodeRateLimited:
		return http.StatusTooManyRequests
	case protocol.ErrCodeNoCapacity:
		return http.StatusServiceUnavailable
	default:
		return http.StatusInternalServerError
	}
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(v)
}
