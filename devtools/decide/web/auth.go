package web

import (
	"crypto/hmac"
	"crypto/rand"
	"crypto/sha256"
	"crypto/subtle"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/decide"
)

const (
	// SessionCookie is the signed session cookie's name.
	SessionCookie = "decide_session"
	// TokenEnv is the environment variable that overrides the token file.
	TokenEnv = "DECIDE_WEB_TOKEN"
	// SessionTTL is how long a login lasts.
	SessionTTL = 30 * 24 * time.Hour

	minTokenLen  = 16
	tokenBytes   = 32
	maxLoginBody = 4 << 10
)

// auth authenticates requests and guards mutating ones against CSRF.
// Every failure path denies.
type auth struct {
	required bool
	token    []byte // nil unless required
	key      []byte // session HMAC key, derived from the token
	now      func() time.Time
	rand     io.Reader
	public   *url.URL // Config.Web.PublicURL, nil if unset

	mu      sync.Mutex
	revoked map[string]time.Time // session nonce -> its expiry
}

func newAuth(s *Server) (*auth, error) {
	a := &auth{now: s.now, rand: s.Rand, revoked: map[string]time.Time{}}
	if a.rand == nil {
		a.rand = rand.Reader
	}
	if raw := s.Config.Web.PublicURL; raw != "" {
		u, err := url.Parse(raw)
		if err != nil || u.Host == "" {
			return nil, fmt.Errorf("decide web: web.publicUrl %q is not an absolute URL", raw)
		}
		a.public = u
	}
	switch s.authMode() {
	case decide.WebAuthNone:
	case decide.WebAuthToken:
		tok, err := loadToken(s.Config.Web.Auth.TokenFile, a.rand)
		if err != nil {
			return nil, err
		}
		a.required = true
		a.token = []byte(tok)
		mac := hmac.New(sha256.New, a.token)
		mac.Write([]byte("decide-web-session-key"))
		a.key = mac.Sum(nil)
	default:
		return nil, fmt.Errorf("decide web: unknown auth mode %q", s.Config.Web.Auth.Mode)
	}
	return a, nil
}

// loadToken reads the token from TokenEnv, else file. A configured file
// that does not exist is created with a random token (mode 0600).
func loadToken(file string, r io.Reader) (string, error) {
	tok := strings.TrimSpace(os.Getenv(TokenEnv))
	if tok == "" {
		if file == "" {
			return "", fmt.Errorf("decide web: token auth needs web.auth.tokenFile (--token-file) or $%s", TokenEnv)
		}
		data, err := os.ReadFile(file)
		switch {
		case errors.Is(err, fs.ErrNotExist):
			return generateToken(file, r)
		case err != nil:
			return "", fmt.Errorf("decide web: read token file: %w", err)
		}
		tok = strings.TrimSpace(string(data))
	}
	if len(tok) < minTokenLen {
		return "", fmt.Errorf("decide web: token must be at least %d characters", minTokenLen)
	}
	return tok, nil
}

func generateToken(file string, r io.Reader) (string, error) {
	b := make([]byte, tokenBytes)
	if _, err := io.ReadFull(r, b); err != nil {
		return "", fmt.Errorf("decide web: generate token: %w", err)
	}
	tok := hex.EncodeToString(b)
	if err := renameio.WriteFile(file, []byte(tok+"\n"), 0o600); err != nil {
		return "", fmt.Errorf("decide web: write token file: %w", err)
	}
	return tok, nil
}

// tokenEqual compares in constant time. Hashing first hides the length.
func (a *auth) tokenEqual(got string) bool {
	g := sha256.Sum256([]byte(got))
	w := sha256.Sum256(a.token)
	return subtle.ConstantTimeCompare(g[:], w[:]) == 1
}

func (a *auth) routes(mux *http.ServeMux) {
	mux.HandleFunc("GET /api/v1/auth/status", a.status)
	mux.HandleFunc("POST /api/v1/auth/login", a.login)
	mux.HandleFunc("POST /api/v1/auth/logout", a.logout)
}

// bearer reports whether the request carries the valid token as a
// Bearer credential.
func (a *auth) bearer(r *http.Request) bool {
	scheme, tok, ok := strings.Cut(r.Header.Get("Authorization"), " ")
	return a.required && ok && strings.EqualFold(scheme, "Bearer") && a.tokenEqual(tok)
}

// authenticated reports whether the request is allowed API access.
func (a *auth) authenticated(r *http.Request) bool {
	if !a.required {
		return true
	}
	if a.bearer(r) {
		return true
	}
	c, err := r.Cookie(SessionCookie)
	return err == nil && a.validSession(c.Value)
}

// require rejects unauthenticated /api/v1 calls other than health and
// the auth endpoints.
func (a *auth) require(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		p := r.URL.Path
		open := p == "/api/v1/health" || strings.HasPrefix(p, "/api/v1/auth/")
		if strings.HasPrefix(p, "/api/v1") && !open && !a.authenticated(r) {
			writeJSON(w, http.StatusUnauthorized, map[string]any{"error": "unauthorized"})
			return
		}
		next.ServeHTTP(w, r)
	})
}

// csrf refuses cross-origin browser requests that change state. A valid
// Bearer credential is exempt: a page cannot make the browser send one.
// A request with neither fetch metadata nor an Origin is refused.
func (a *auth) csrf(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.Method {
		case http.MethodGet, http.MethodHead, http.MethodOptions:
		default:
			if !a.bearer(r) && !a.sameOrigin(r) {
				writeJSON(w, http.StatusForbidden, map[string]any{"error": "cross-origin request refused"})
				return
			}
		}
		next.ServeHTTP(w, r)
	})
}

func (a *auth) sameOrigin(r *http.Request) bool {
	if site := r.Header.Get("Sec-Fetch-Site"); site != "" {
		return site == "same-origin" || site == "none"
	}
	origin := r.Header.Get("Origin")
	if origin == "" || origin == "null" {
		return false
	}
	u, err := url.Parse(origin)
	if err != nil || u.Host == "" {
		return false
	}
	return u.Host == r.Host || (a.public != nil && u.Host == a.public.Host && u.Scheme == a.public.Scheme)
}

func (a *auth) status(w http.ResponseWriter, r *http.Request) {
	writeJSON(w, http.StatusOK, map[string]any{
		"authRequired":  a.required,
		"authenticated": a.authenticated(r),
	})
}

func (a *auth) login(w http.ResponseWriter, r *http.Request) {
	var body struct {
		Token string `json:"token"`
	}
	r.Body = http.MaxBytesReader(w, r.Body, maxLoginBody)
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"error": "bad request"})
		return
	}
	if a.required {
		if body.Token == "" || !a.tokenEqual(body.Token) {
			writeJSON(w, http.StatusUnauthorized, map[string]any{"error": "unauthorized"})
			return
		}
		value, exp, err := a.newSession()
		if err != nil {
			writeJSON(w, http.StatusInternalServerError, map[string]any{"error": "internal error"})
			return
		}
		http.SetCookie(w, a.cookie(r, value, exp, int(SessionTTL/time.Second)))
	}
	writeJSON(w, http.StatusOK, map[string]any{"ok": true})
}

func (a *auth) logout(w http.ResponseWriter, r *http.Request) {
	if c, err := r.Cookie(SessionCookie); err == nil && a.required {
		a.revoke(c.Value)
	}
	http.SetCookie(w, a.cookie(r, "", time.Unix(0, 0), -1))
	writeJSON(w, http.StatusOK, map[string]any{"ok": true})
}

func isSecureRequest(r *http.Request) bool {
	return r.TLS != nil || strings.EqualFold(r.Header.Get("X-Forwarded-Proto"), "https")
}

func (a *auth) cookie(r *http.Request, value string, exp time.Time, maxAge int) *http.Cookie {
	// nosemgrep: go.lang.security.audit.net.cookie-missing-secure.cookie-missing-secure
	return &http.Cookie{ //nolint:gosec // G124: Secure follows the request scheme, as the proxy may terminate TLS.
		Name:     SessionCookie,
		Value:    value,
		Path:     "/",
		Expires:  exp,
		MaxAge:   maxAge,
		HttpOnly: true,
		SameSite: http.SameSiteLaxMode,
		Secure:   isSecureRequest(r),
	}
}

// A session value is "<expiry unix>.<nonce hex>.<base64url HMAC>"; the
// MAC covers the first two fields.

func (a *auth) sign(payload string) string {
	mac := hmac.New(sha256.New, a.key)
	mac.Write([]byte(payload))
	return base64.RawURLEncoding.EncodeToString(mac.Sum(nil))
}

func (a *auth) newSession() (string, time.Time, error) {
	nonce := make([]byte, 16)
	if _, err := io.ReadFull(a.rand, nonce); err != nil {
		return "", time.Time{}, fmt.Errorf("session nonce: %w", err)
	}
	exp := a.now().Add(SessionTTL)
	payload := strconv.FormatInt(exp.Unix(), 10) + "." + hex.EncodeToString(nonce)
	return payload + "." + a.sign(payload), exp, nil
}

// parseSession verifies the MAC and returns the nonce and expiry.
func (a *auth) parseSession(value string) (string, time.Time, bool) {
	i := strings.LastIndex(value, ".")
	if i < 0 {
		return "", time.Time{}, false
	}
	payload, sig := value[:i], value[i+1:]
	if !hmac.Equal([]byte(sig), []byte(a.sign(payload))) {
		return "", time.Time{}, false
	}
	expStr, nonce, ok := strings.Cut(payload, ".")
	exp, err := strconv.ParseInt(expStr, 10, 64)
	if !ok || err != nil {
		return "", time.Time{}, false
	}
	return nonce, time.Unix(exp, 0), true
}

func (a *auth) validSession(value string) bool {
	nonce, exp, ok := a.parseSession(value)
	if !ok || !a.now().Before(exp) {
		return false
	}
	a.mu.Lock()
	defer a.mu.Unlock()
	_, revoked := a.revoked[nonce]
	return !revoked
}

func (a *auth) revoke(value string) {
	nonce, exp, ok := a.parseSession(value)
	if !ok {
		return
	}
	a.mu.Lock()
	defer a.mu.Unlock()
	now := a.now()
	for n, e := range a.revoked {
		if !now.Before(e) {
			delete(a.revoked, n)
		}
	}
	a.revoked[nonce] = exp
}
