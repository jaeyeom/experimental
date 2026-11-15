// Package ghask is a loopback HTTP server that hands a question about a
// pull request line range to a locally configured command.
//
// The endpoints are POST /ask, GET /status/<id> and GET /thread. The
// request text is written to a file; it never reaches a command's argv.
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
	"strings"
	"sync"
	"time"
)

const statusPrefix = "/status/"

// TokenHeader carries the shared secret.
const TokenHeader = "X-Gh-Ask-Token" //nolint:gosec // G101: a header name, not a credential

const githubOrigin = "https://github.com"

var extensionOrigin = regexp.MustCompile(`^(chrome-extension|moz-extension|safari-web-extension)://[A-Za-z0-9.-]+$`)

// Server handles POST /ask, GET /status/<id> and GET /thread. Create it
// with NewServer.
type Server struct {
	cfg       Config
	tokenHash [sha256.Size]byte
	hosts     []string
	limiter   *limiter
	now       func() time.Time
	// tempDir is where message files go. Empty means os.TempDir.
	tempDir string
	logger  *slog.Logger

	mu sync.Mutex
	// threads is the state file's content, loaded on first use. It is
	// used only with ReusePerPR.
	threads map[string]threadEntry
	// created holds the ids ask_command printed in this process.
	created  map[string]bool
	statuses map[string]cachedStatus
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
		created:   map[string]bool{},
		statuses:  map[string]cachedStatus{},
	}
	s.limiter = newLimiter(cfg.RateLimit, time.Hour, func() time.Time { return s.now() })
	return s
}

type errorBody struct {
	Error string `json:"error"`
}

// ServeHTTP never sets CORS headers. A browser preflight gets 405, so a
// page cannot add the token header cross-origin. Every route gets the
// same Host, Origin and token checks.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	var method string
	var handle func(http.ResponseWriter, *http.Request)
	switch {
	case r.URL.Path == "/ask":
		method, handle = http.MethodPost, s.serveAsk
	case r.URL.Path == "/thread":
		method, handle = http.MethodGet, s.serveThread
	case strings.HasPrefix(r.URL.Path, statusPrefix):
		method, handle = http.MethodGet, s.serveStatus
	default:
		s.logReject(http.StatusNotFound, "not found", "", 0)
		writeJSON(w, http.StatusNotFound, errorBody{"not found"})
		return
	}
	if r.Method != method {
		w.Header().Set("Allow", method)
		reason := "method"
		if r.Method == http.MethodOptions {
			reason = "preflight"
		}
		repo, pr := queryRepoPR(r)
		s.logReject(http.StatusMethodNotAllowed, reason, repo, pr)
		writeJSON(w, http.StatusMethodNotAllowed, errorBody{"method not allowed"})
		return
	}
	repo, pr := queryRepoPR(r)
	if !slices.Contains(s.hosts, r.Host) {
		s.logReject(http.StatusForbidden, "host", repo, pr)
		writeJSON(w, http.StatusForbidden, errorBody{"bad host"})
		return
	}
	if !allowedOrigin(r.Header.Values("Origin")) {
		s.logReject(http.StatusForbidden, "origin", repo, pr)
		writeJSON(w, http.StatusForbidden, errorBody{"bad origin"})
		return
	}
	if !s.tokenOK(r.Header.Get(TokenHeader)) {
		s.logReject(http.StatusUnauthorized, "bad token", repo, pr)
		writeJSON(w, http.StatusUnauthorized, errorBody{"bad token"})
		return
	}
	handle(w, r)
}

// logReject writes one warn line for a rejected request.
// It records status, reason, and repo and pr when those are known.
// It does not record the token, the question, the selection, or the page URL.
func (s *Server) logReject(status int, reason, repo string, pr int) {
	if s.logger == nil {
		return
	}
	args := []any{"status", status, "reason", reason}
	if repoPattern.MatchString(repo) {
		args = append(args, "repo", repo)
	}
	if pr > 0 {
		args = append(args, "pr", pr)
	}
	s.logger.Warn("gh-ask: rejected", args...)
}

// queryRepoPR returns the repo and PR from a /thread query when they are safe to log.
func queryRepoPR(r *http.Request) (string, int) {
	if r == nil || r.URL == nil || r.URL.Path != "/thread" {
		return "", 0
	}
	repo := r.URL.Query().Get("repo")
	if !repoPattern.MatchString(repo) {
		repo = ""
	}
	pr, err := strconv.Atoi(r.URL.Query().Get("pr"))
	if err != nil || pr <= 0 {
		return repo, 0
	}
	return repo, pr
}

func (s *Server) serveAsk(w http.ResponseWriter, r *http.Request) {
	req, err := decodeAskJSON(w, r)
	if err != nil {
		status := http.StatusBadRequest
		reason := "bad request"
		if errors.Is(err, errBodyTooLarge) {
			status = http.StatusRequestEntityTooLarge
			reason = "too large"
		}
		s.logReject(status, reason, "", 0)
		writeJSON(w, status, errorBody{err.Error()})
		return
	}
	if err := req.Validate(); err != nil {
		status := http.StatusBadRequest
		reason := "bad request"
		if isTooLarge(err) {
			status = http.StatusRequestEntityTooLarge
			reason = "too large"
		}
		s.logReject(status, reason, req.Repo, req.PR)
		writeJSON(w, status, errorBody{err.Error()})
		return
	}
	if !slices.Contains(s.cfg.Repos, req.Repo) {
		s.logReject(http.StatusForbidden, "repo not allowed", req.Repo, req.PR)
		writeJSON(w, http.StatusForbidden, errorBody{"repo not allowed"})
		return
	}
	if !s.limiter.Allow() {
		s.logReject(http.StatusTooManyRequests, "rate limited", req.Repo, req.PR)
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

var errBodyTooLarge = errors.New("body too large")

// decodeAskJSON reads the ask body. A body over the cap is errBodyTooLarge.
// Validation is separate, so a decode error does not leave a request to log.
func decodeAskJSON(w http.ResponseWriter, r *http.Request) (AskRequest, error) {
	var req AskRequest
	body := http.MaxBytesReader(w, r.Body, MaxBodyBytes)
	if err := json.NewDecoder(body).Decode(&req); err != nil {
		var tooBig *http.MaxBytesError
		if errors.As(err, &tooBig) {
			return AskRequest{}, errBodyTooLarge
		}
		return AskRequest{}, errors.New("bad JSON")
	}
	return req, nil
}

// ask writes the message file and returns the thread id. With
// ReusePerPR it reuses an open stored thread through followup_command.
// Otherwise it runs ask_command and stores the new id. Its errors are
// short and safe to show the browser; details go to the log.
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
	vars := map[string]string{
		"file": path,
		"pr":   strconv.Itoa(req.PR),
		"repo": req.Repo,
	}
	if s.cfg.Reuse == ReusePerPR {
		if id, done, err := s.followStored(ctx, req, vars); done || err != nil {
			return id, err
		}
	}
	out, err := s.run(ctx, "ask_command", s.cfg.AskCommand, vars, "repo", req.Repo, "pr", req.PR)
	if err != nil {
		return "", err
	}
	id, ok := extractID(s.cfg.IDPattern, out)
	if !ok {
		s.logger.Error("gh-ask: no id in ask_command output", "repo", req.Repo, "pr", req.PR)
		return "", errors.New("ask_command printed no id")
	}
	s.recordThread(req.Repo, req.PR, id)
	return id, nil
}

// followStored runs followup_command for the PR's stored thread when
// that thread is still open. done is false when the caller should start
// a new thread: nothing is stored, the stored thread is closed, or its
// status could not be checked. With status_command set, a cached status
// younger than statusCacheTTL is used; otherwise the command runs. A
// failed check is logged and starts a new thread, which is safer than a
// question posted into a thread that might already be closed. Without
// status_command, the stored id is reused as it is.
func (s *Server) followStored(ctx context.Context, req AskRequest, vars map[string]string) (string, bool, error) {
	id, ok, err := s.openThread(req.Repo, req.PR)
	if err != nil {
		s.logger.Error("gh-ask: state file", "err", err)
		return "", true, errors.New("could not read state file")
	}
	if !ok {
		return "", false, nil
	}
	if len(s.cfg.StatusCommand) > 0 {
		st, err := s.status(ctx, id)
		if err != nil {
			s.logger.Error("gh-ask: status check failed; starting a new thread", "id", id, "err", err)
			return "", false, nil
		}
		if st.Closed {
			return "", false, nil
		}
	}
	vars["id"] = id
	if _, err := s.run(ctx, "followup_command", s.cfg.FollowupCommand, vars, "repo", req.Repo, "pr", req.PR); err != nil {
		return "", true, err
	}
	return id, true, nil
}

// run expands tmpl with vars and runs it. Its error is short and safe to
// show the browser; details, with logArgs, go to the log.
func (s *Server) run(ctx context.Context, name string, tmpl []string, vars map[string]string, logArgs ...any) ([]byte, error) {
	argv, err := expandArgv(tmpl, vars)
	if err != nil {
		s.logger.Error("gh-ask: "+name, "err", err)
		return nil, errors.New("bad " + name)
	}
	out, stderr, err := runCommand(ctx, argv, s.cfg.CommandTimeout)
	if err != nil {
		s.logger.Error("gh-ask: "+name, append(logArgs, "err", err, "stderr", stderr)...)
		return nil, errors.New(name + " failed")
	}
	return out, nil
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	h := w.Header()
	h.Set("Content-Type", "application/json")
	h.Set("X-Content-Type-Options", "nosniff")
	h.Set("Cache-Control", "no-store")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(v)
}
