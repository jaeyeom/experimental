// Package web is the HTTP server behind `decide web`: embedded static
// assets, a JSON API under /api/v1, and token-to-cookie authentication
// with CSRF protection.
package web

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/netip"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// contentSecurityPolicy is sent on every response. The UI is same-origin
// scripts and styles only.
const contentSecurityPolicy = "default-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'"

// shutdownGrace is how long Serve waits for in-flight requests.
const shutdownGrace = 5 * time.Second

// Server is the `decide web` HTTP server.
type Server struct {
	Backend *tui.LocalBackend
	Config  decide.Config
	// Display and OrgConfig are the live config readers, as in
	// inboxrpc.Server.
	Display   func() (decide.DisplaySettings, error)
	OrgConfig func() (bool, string, error)
	Orgctl    tui.OrgctlRunner
	// FollowEvery is how often Serve follows up stale answers on the
	// backend. Zero or negative disables it.
	FollowEvery time.Duration
	// AllowInsecureUnauth permits auth mode none on a non-loopback
	// address.
	AllowInsecureUnauth bool
	// Clock and Rand are test hooks. Nil means time.Now and crypto/rand.
	Clock func() time.Time
	Rand  io.Reader
	// Report receives background errors. Nil drops them.
	Report func(error)
	// Targets and Agentbus are the compose recipients, as in
	// inboxrpc.Server.
	Targets  []decide.ConfigTarget
	Agentbus bool
	// PingEvery is the idle keep-alive interval of the event stream.
	// Zero means 25s, under common proxy idle timeouts.
	PingEvery time.Duration
	// WatchEvery is the watcher poll interval. Zero means 100ms.
	WatchEvery time.Duration

	// PushPath is the Web Push state file. Empty means
	// $XDG_STATE_HOME/decide/webpush.json.
	PushPath string
	// PushClient is the push-service HTTP client. Nil means a client that
	// refuses non-public addresses and times out.
	PushClient *http.Client
	// AllowInsecurePush permits http and loopback push endpoints, for
	// tests.
	AllowInsecurePush bool
	// PushAfter is a test hook for the grace and wake timers. Nil means
	// time.AfterFunc.
	PushAfter func(d time.Duration, f func()) (stop func() bool)

	stateOnce      sync.Once
	api            *apiState
	pushOnce       sync.Once
	push           *PushStore
	pushErr        error
	pushClientOnce sync.Once
	pushHTTP       *http.Client
}

func (s *Server) now() time.Time {
	if s.Clock != nil {
		return s.Clock()
	}
	return time.Now()
}

// Listen checks the configured address against the auth mode and binds
// it. It refuses an open server on a non-loopback address.
func (s *Server) Listen() (net.Listener, error) {
	addr := s.Config.WebAddr()
	if err := checkListenAddr(addr, s.authMode(), s.AllowInsecureUnauth); err != nil {
		return nil, err
	}
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		return nil, fmt.Errorf("decide web: %w", err)
	}
	return ln, nil
}

func (s *Server) authMode() string {
	if s.Config.Web.Auth.Mode == "" {
		return decide.WebAuthNone
	}
	return s.Config.Web.Auth.Mode
}

// checkListenAddr refuses auth mode none off the loopback interface
// unless allowInsecure.
func checkListenAddr(addr, mode string, allowInsecure bool) error {
	host, _, err := net.SplitHostPort(addr)
	if err != nil {
		return fmt.Errorf("decide web: listen address %q: %w", addr, err)
	}
	if mode != decide.WebAuthNone || allowInsecure || isLoopbackHost(host) {
		return nil
	}
	return fmt.Errorf("decide web: refusing auth mode %q on non-loopback address %q; "+
		"set web.auth.mode to %q (or pass --token-file), or pass --allow-insecure-unauth",
		decide.WebAuthNone, addr, decide.WebAuthToken)
}

func isLoopbackHost(host string) bool {
	if host == "localhost" {
		return true
	}
	ip, err := netip.ParseAddr(host)
	return err == nil && ip.IsLoopback()
}

// Serve serves on ln until ctx is done, then shuts down gracefully. It
// also follows up stale answers every FollowEvery.
func (s *Server) Serve(ctx context.Context, ln net.Listener) error {
	h, err := s.Handler()
	if err != nil {
		return err
	}
	if s.Backend != nil && s.FollowEvery > 0 {
		go s.Backend.FollowLoop(ctx, s.FollowEvery, func(err error) {
			if s.Report != nil {
				s.Report(err)
			}
		})
	}
	if s.Backend != nil {
		go s.busLoop(ctx)
		if s.Config.Web.Push.Enabled {
			go newPushDispatcher(s).run(ctx)
		}
	}
	srv := &http.Server{Handler: h, ReadHeaderTimeout: 10 * time.Second}
	errc := make(chan error, 1)
	go func() { errc <- srv.Serve(ln) }()
	select {
	case err := <-errc:
		return fmt.Errorf("decide web: %w", err)
	case <-ctx.Done():
	}
	shutdownCtx, cancel := context.WithTimeout(context.Background(), shutdownGrace)
	defer cancel()
	if err := srv.Shutdown(shutdownCtx); err != nil {
		return fmt.Errorf("decide web: shutdown: %w", err)
	}
	if err := <-errc; !errors.Is(err, http.ErrServerClosed) {
		return fmt.Errorf("decide web: %w", err)
	}
	return nil
}

// Handler builds the full handler: security headers, CSRF, then auth in
// front of the routes. Auth and CSRF wrap the whole mux, so a route
// added later is protected without opting in.
func (s *Server) Handler() (http.Handler, error) {
	a, err := newAuth(s)
	if err != nil {
		return nil, err
	}
	mux := http.NewServeMux()
	staticRoutes(mux)
	mux.HandleFunc("GET /api/v1/health", func(w http.ResponseWriter, _ *http.Request) {
		writeJSON(w, http.StatusOK, map[string]any{"ok": true})
	})
	a.routes(mux)
	s.apiRoutes(mux)
	s.pushRoutes(mux)
	return securityHeaders(a.csrf(a.require(mux))), nil
}

func securityHeaders(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		h := w.Header()
		h.Set("Content-Security-Policy", contentSecurityPolicy)
		h.Set("X-Content-Type-Options", "nosniff")
		h.Set("Referrer-Policy", "no-referrer")
		next.ServeHTTP(w, r)
	})
}

func writeJSON(w http.ResponseWriter, code int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Cache-Control", "no-store")
	w.WriteHeader(code)
	_ = json.NewEncoder(w).Encode(v)
}
