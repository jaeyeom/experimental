// Package ghask is a loopback HTTP server that hands a question about a
// pull request line range to a locally configured command.
//
// The only endpoint is POST /ask. The request text is written to a file;
// it never reaches the command's argv.
package ghask

import (
	"context"
	"crypto/sha256"
	"crypto/subtle"
	"encoding/json"
	"errors"
	"log/slog"
	"net/http"
	"os"
	"regexp"
	"slices"
	"strconv"
	"time"
)

// TokenHeader carries the shared secret.
const TokenHeader = "X-Gh-Ask-Token" //nolint:gosec // G101: a header name, not a credential

const githubOrigin = "https://github.com"

var extensionOrigin = regexp.MustCompile(`^(chrome-extension|moz-extension|safari-web-extension)://[A-Za-z0-9.-]+$`)

// Server handles POST /ask. Create it with NewServer.
type Server struct {
	cfg       Config
	tokenHash [sha256.Size]byte
	hosts     []string
	limiter   *limiter
	now       func() time.Time
	// tempDir is where message files go. Empty means os.TempDir.
	tempDir string
	logger  *slog.Logger
}

// NewServer returns a handler for a server listening on 127.0.0.1:port.
func NewServer(cfg Config, token string, port int) *Server {
	p := strconv.Itoa(port)
	s := &Server{
		cfg:       cfg,
		tokenHash: sha256.Sum256([]byte(token)),
		hosts:     []string{"127.0.0.1:" + p, "localhost:" + p},
		now:       time.Now,
		logger:    slog.Default(),
	}
	s.limiter = newLimiter(cfg.RateLimit, time.Hour, func() time.Time { return s.now() })
	return s
}

type errorBody struct {
	Error string `json:"error"`
}

// ServeHTTP never sets CORS headers. A browser preflight gets 405, so a
// page cannot add the token header cross-origin.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/ask" {
		writeJSON(w, http.StatusNotFound, errorBody{"not found"})
		return
	}
	if r.Method != http.MethodPost {
		w.Header().Set("Allow", http.MethodPost)
		writeJSON(w, http.StatusMethodNotAllowed, errorBody{"method not allowed"})
		return
	}
	if !slices.Contains(s.hosts, r.Host) {
		writeJSON(w, http.StatusForbidden, errorBody{"bad host"})
		return
	}
	if !allowedOrigin(r.Header.Values("Origin")) {
		writeJSON(w, http.StatusForbidden, errorBody{"bad origin"})
		return
	}
	if !s.tokenOK(r.Header.Get(TokenHeader)) {
		writeJSON(w, http.StatusUnauthorized, errorBody{"bad token"})
		return
	}
	req, status, err := decodeRequest(w, r)
	if err != nil {
		writeJSON(w, status, errorBody{err.Error()})
		return
	}
	if !slices.Contains(s.cfg.Repos, req.Repo) {
		writeJSON(w, http.StatusForbidden, errorBody{"repo not allowed"})
		return
	}
	if !s.limiter.Allow() {
		writeJSON(w, http.StatusTooManyRequests, errorBody{"rate limit"})
		return
	}
	// The command runs to completion even if the browser goes away.
	id, err := s.ask(context.WithoutCancel(r.Context()), req)
	if err != nil {
		writeJSON(w, http.StatusBadGateway, errorBody{err.Error()})
		return
	}
	writeJSON(w, http.StatusOK, struct {
		ID string `json:"id"`
	}{id})
}

func (s *Server) tokenOK(got string) bool {
	if got == "" {
		return false
	}
	// Hashing first keeps the compare length fixed.
	sum := sha256.Sum256([]byte(got))
	return subtle.ConstantTimeCompare(sum[:], s.tokenHash[:]) == 1
}

func allowedOrigin(values []string) bool {
	switch len(values) {
	case 0:
		return true
	case 1:
		return values[0] == githubOrigin || extensionOrigin.MatchString(values[0])
	default:
		return false
	}
}

func decodeRequest(w http.ResponseWriter, r *http.Request) (AskRequest, int, error) {
	var req AskRequest
	body := http.MaxBytesReader(w, r.Body, MaxBodyBytes)
	if err := json.NewDecoder(body).Decode(&req); err != nil {
		var tooBig *http.MaxBytesError
		if errors.As(err, &tooBig) {
			return req, http.StatusRequestEntityTooLarge, errors.New("body too large")
		}
		return req, http.StatusBadRequest, errors.New("bad JSON")
	}
	if err := req.Validate(); err != nil {
		if isTooLarge(err) {
			return req, http.StatusRequestEntityTooLarge, err
		}
		return req, http.StatusBadRequest, err
	}
	return req, 0, nil
}

// ask writes the message file, runs ask_command, and returns the id. Its
// errors are short and safe to show the browser; details go to the log.
func (s *Server) ask(ctx context.Context, req AskRequest) (string, error) {
	path, err := writeMessageFile(s.tempDir, RenderMessage(req))
	if err != nil {
		s.logger.Error("gh-ask: message file", "err", err)
		return "", errors.New("could not write message file")
	}
	defer func() {
		if err := os.Remove(path); err != nil {
			s.logger.Warn("gh-ask: remove message file", "err", err)
		}
	}()
	argv, err := expandArgv(s.cfg.AskCommand, map[string]string{
		"file": path,
		"pr":   strconv.Itoa(req.PR),
		"repo": req.Repo,
	})
	if err != nil {
		s.logger.Error("gh-ask: ask_command", "err", err)
		return "", errors.New("bad ask_command")
	}
	out, stderr, err := runCommand(ctx, argv, s.cfg.CommandTimeout)
	if err != nil {
		s.logger.Error("gh-ask: ask_command", "repo", req.Repo, "pr", req.PR, "err", err, "stderr", stderr)
		return "", errors.New("ask_command failed")
	}
	id, ok := extractID(s.cfg.IDPattern, out)
	if !ok {
		s.logger.Error("gh-ask: no id in ask_command output", "repo", req.Repo, "pr", req.PR)
		return "", errors.New("ask_command printed no id")
	}
	return id, nil
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	h := w.Header()
	h.Set("Content-Type", "application/json")
	h.Set("X-Content-Type-Options", "nosniff")
	h.Set("Cache-Control", "no-store")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(v)
}
