package web

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

const testToken = "test-token-test-token-test-token" // gitleaks:allow

func testNow() time.Time { return time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC) }

func newHandler(t *testing.T, cfg decide.Config, mutate func(*Server)) http.Handler {
	t.Helper()
	s := &Server{Config: cfg, Clock: testNow}
	if mutate != nil {
		mutate(s)
	}
	h, err := s.Handler()
	if err != nil {
		t.Fatalf("Handler: %v", err)
	}
	return h
}

func tokenHandler(t *testing.T) http.Handler {
	t.Helper()
	t.Setenv(TokenEnv, testToken)
	return newHandler(t, decide.Config{Web: decide.WebConfig{Auth: decide.WebAuthConfig{Mode: decide.WebAuthToken}}}, nil)
}

type req struct {
	method, path, body string
	header             map[string]string
}

func do(h http.Handler, r req) *httptest.ResponseRecorder {
	hr := httptest.NewRequest(r.method, r.path, strings.NewReader(r.body))
	hr.Host = "decide.test"
	for k, v := range r.header {
		hr.Header.Set(k, v)
	}
	w := httptest.NewRecorder()
	h.ServeHTTP(w, hr)
	return w
}

var sameOrigin = map[string]string{"Sec-Fetch-Site": "same-origin"}

func with(base map[string]string, kv ...string) map[string]string {
	out := map[string]string{}
	for k, v := range base {
		out[k] = v
	}
	for i := 0; i+1 < len(kv); i += 2 {
		out[kv[i]] = kv[i+1]
	}
	return out
}

func login(h http.Handler, token string) *httptest.ResponseRecorder {
	b, _ := json.Marshal(map[string]string{"token": token})
	return do(h, req{"POST", "/api/v1/auth/login", string(b), sameOrigin})
}

func sessionCookie(t *testing.T, w *httptest.ResponseRecorder) *http.Cookie {
	t.Helper()
	for _, c := range w.Result().Cookies() {
		if c.Name == SessionCookie {
			return c
		}
	}
	t.Fatalf("no %s cookie; status %d", SessionCookie, w.Code)
	return nil
}

func wantStatus(t *testing.T, w *httptest.ResponseRecorder, code int) {
	t.Helper()
	if w.Code != code {
		t.Fatalf("status = %d, want %d; body %s", w.Code, code, w.Body)
	}
}

func jsonBody(t *testing.T, w *httptest.ResponseRecorder) map[string]any {
	t.Helper()
	var m map[string]any
	if err := json.Unmarshal(w.Body.Bytes(), &m); err != nil {
		t.Fatalf("body %q: %v", w.Body, err)
	}
	return m
}

func TestSecurityHeadersEverywhere(t *testing.T) {
	h := tokenHandler(t)
	for _, r := range []req{
		{"GET", "/", "", nil},
		{"GET", "/api/v1/health", "", nil},
		{"GET", "/api/v1/nope", "", nil},
		{"GET", "/missing", "", nil},
		{"POST", "/api/v1/auth/login", "{}", nil}, // CSRF refusal
	} {
		w := do(h, r)
		for k, v := range map[string]string{
			"Content-Security-Policy": "default-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'",
			"X-Content-Type-Options":  "nosniff",
			"Referrer-Policy":         "no-referrer",
		} {
			if got := w.Header().Get(k); got != v {
				t.Errorf("%s %s: %s = %q, want %q", r.method, r.path, k, got, v)
			}
		}
	}
}

func TestIndexAndHealthAreOpen(t *testing.T) {
	h := tokenHandler(t)
	w := do(h, req{"GET", "/", "", nil})
	wantStatus(t, w, 200)
	if !strings.Contains(w.Body.String(), "<title>decide</title>") {
		t.Errorf("index body = %q", w.Body)
	}
	w = do(h, req{"GET", "/api/v1/health", "", nil})
	wantStatus(t, w, 200)
	if jsonBody(t, w)["ok"] != true {
		t.Errorf("health = %s", w.Body)
	}
}

func TestNoneModeIsOpen(t *testing.T) {
	h := newHandler(t, decide.Config{}, nil)
	w := do(h, req{"GET", "/api/v1/auth/status", "", nil})
	wantStatus(t, w, 200)
	m := jsonBody(t, w)
	if m["authRequired"] != false || m["authenticated"] != true {
		t.Errorf("status = %v", m)
	}
	// Past auth: an unknown API route is a 404, not a 401.
	wantStatus(t, do(h, req{"GET", "/api/v1/nope", "", nil}), 404)
}

func TestTokenModeRejectsUnauthenticated(t *testing.T) {
	h := tokenHandler(t)
	w := do(h, req{"GET", "/api/v1/nope", "", nil})
	wantStatus(t, w, 401)
	if jsonBody(t, w)["error"] != "unauthorized" {
		t.Errorf("body = %s", w.Body)
	}
	w = do(h, req{"GET", "/api/v1/auth/status", "", nil})
	wantStatus(t, w, 200)
	m := jsonBody(t, w)
	if m["authRequired"] != true || m["authenticated"] != false {
		t.Errorf("status = %v", m)
	}
}

func TestLoginRejections(t *testing.T) {
	h := tokenHandler(t)
	for name, body := range map[string]string{ //nolint:gosec // G101: test inputs, not credentials.
		"wrong token":     `{"token":"nope"}`,
		"prefix of token": `{"token":"` + testToken[:10] + `"}`,
		"longer token":    `{"token":"` + testToken + `x"}`,
		"empty token":     `{"token":""}`,
		"missing token":   `{}`,
	} {
		w := do(h, req{"POST", "/api/v1/auth/login", body, sameOrigin})
		wantStatus(t, w, 401)
		if len(w.Result().Cookies()) != 0 {
			t.Errorf("%s: set a cookie", name)
		}
	}
	wantStatus(t, do(h, req{"POST", "/api/v1/auth/login", `not json`, sameOrigin}), 400)
	wantStatus(t, do(h, req{"GET", "/api/v1/auth/login", "", sameOrigin}), 405)
	wantStatus(t, do(h, req{"POST", "/api/v1/auth/login", `{"token":"` + strings.Repeat("a", 1<<20) + `"}`, sameOrigin}), 400)
}

func TestLoginSetsSignedCookie(t *testing.T) {
	h := tokenHandler(t)
	w := login(h, testToken)
	wantStatus(t, w, 200)
	c := sessionCookie(t, w)
	if !c.HttpOnly || c.SameSite != http.SameSiteLaxMode || c.Path != "/" || c.Secure {
		t.Errorf("cookie flags = %+v", c)
	}
	if strings.Contains(c.Value, testToken) {
		t.Error("cookie contains the token")
	}
	status := do(h, req{"GET", "/api/v1/nope", "", map[string]string{"Cookie": c.String()}})
	wantStatus(t, status, 404) // authenticated
	st := jsonBody(t, do(h, req{"GET", "/api/v1/auth/status", "", map[string]string{"Cookie": c.String()}}))
	if st["authenticated"] != true {
		t.Errorf("status = %v", st)
	}
}

func TestCookieSecureBehindHTTPSProxy(t *testing.T) {
	h := tokenHandler(t)
	b := `{"token":"` + testToken + `"}`
	w := do(h, req{"POST", "/api/v1/auth/login", b, with(sameOrigin, "X-Forwarded-Proto", "https")})
	if c := sessionCookie(t, w); !c.Secure {
		t.Error("cookie not Secure behind https proxy")
	}
}

func TestTamperedAndForeignCookiesRejected(t *testing.T) {
	h := tokenHandler(t)
	c := sessionCookie(t, login(h, testToken))
	for name, v := range map[string]string{
		"flipped sig":  c.Value[:len(c.Value)-2] + "AA",
		"empty":        "",
		"no dot":       "abcdef",
		"garbage":      "x.y.z",
		"forged exp":   "9999999999.aaaa." + c.Value[strings.LastIndex(c.Value, ".")+1:],
		"unsigned":     c.Value[:strings.LastIndex(c.Value, ".")],
		"other secret": "1.2.3",
	} {
		w := do(h, req{"GET", "/api/v1/nope", "", map[string]string{"Cookie": SessionCookie + "=" + v}})
		if w.Code != 401 {
			t.Errorf("%s: status = %d, want 401", name, w.Code)
		}
	}
	// A cookie signed under another token is not valid here.
	t.Setenv(TokenEnv, "other-token-other-token-other-tk")
	other := newHandler(t, decide.Config{Web: decide.WebConfig{Auth: decide.WebAuthConfig{Mode: decide.WebAuthToken}}}, nil)
	wantStatus(t, do(other, req{"GET", "/api/v1/nope", "", map[string]string{"Cookie": c.String()}}), 401)
}

func TestExpiredCookieRejected(t *testing.T) {
	now := testNow()
	t.Setenv(TokenEnv, testToken)
	h := newHandler(t, decide.Config{Web: decide.WebConfig{Auth: decide.WebAuthConfig{Mode: decide.WebAuthToken}}},
		func(s *Server) { s.Clock = func() time.Time { return now } })
	c := sessionCookie(t, login(h, testToken))
	now = now.Add(SessionTTL + time.Second)
	wantStatus(t, do(h, req{"GET", "/api/v1/nope", "", map[string]string{"Cookie": c.String()}}), 401)
}

func TestBearer(t *testing.T) {
	h := tokenHandler(t)
	wantStatus(t, do(h, req{"GET", "/api/v1/nope", "", map[string]string{"Authorization": "Bearer " + testToken}}), 404)
	for _, a := range []string{"Bearer wrong", "Bearer ", "Bearer", testToken, "Basic " + testToken, "bearer"} {
		wantStatus(t, do(h, req{"GET", "/api/v1/nope", "", map[string]string{"Authorization": a}}), 401)
	}
}

func TestLogoutInvalidatesSession(t *testing.T) {
	h := tokenHandler(t)
	c := sessionCookie(t, login(h, testToken))
	hdr := with(sameOrigin, "Cookie", c.String())
	w := do(h, req{"POST", "/api/v1/auth/logout", "", hdr})
	wantStatus(t, w, 200)
	if cleared := sessionCookie(t, w); cleared.MaxAge >= 0 || cleared.Value != "" {
		t.Errorf("logout cookie = %+v", cleared)
	}
	// The old cookie, replayed, no longer works.
	wantStatus(t, do(h, req{"GET", "/api/v1/nope", "", map[string]string{"Cookie": c.String()}}), 401)
	wantStatus(t, do(h, req{"GET", "/api/v1/auth/logout", "", hdr}), 405)
}

func TestCSRF(t *testing.T) {
	h := tokenHandler(t)
	body := `{"token":"` + testToken + `"}`
	for name, hdr := range map[string]map[string]string{
		"cross-site fetch metadata":           {"Sec-Fetch-Site": "cross-site"},
		"same-site fetch metadata":            {"Sec-Fetch-Site": "same-site"},
		"unknown fetch metadata":              {"Sec-Fetch-Site": "bogus"},
		"evil origin":                         {"Origin": "https://evil.example"},
		"null origin":                         {"Origin": "null"},
		"malformed origin":                    {"Origin": "::::"},
		"evil origin, same-origin lie absent": {"Origin": "http://decide.test.evil.example"},
		"no origin, no fetch metadata":        {},
		"cross-site beats matching origin":    {"Sec-Fetch-Site": "cross-site", "Origin": "http://decide.test"},
	} {
		for _, method := range []string{"POST", "PUT", "DELETE", "PATCH"} {
			w := do(h, req{method, "/api/v1/auth/login", body, hdr})
			if w.Code != 403 {
				t.Errorf("%s %s: status = %d, want 403", name, method, w.Code)
			}
			if len(w.Result().Cookies()) != 0 {
				t.Errorf("%s %s: set a cookie", name, method)
			}
		}
	}
	for _, hdr := range map[string]map[string]string{
		"same-origin":  {"Sec-Fetch-Site": "same-origin"},
		"none":         {"Sec-Fetch-Site": "none"},
		"origin match": {"Origin": "http://decide.test"},
	} {
		wantStatus(t, do(h, req{"POST", "/api/v1/auth/login", body, hdr}), 200)
	}
}

func TestCSRFPublicURL(t *testing.T) {
	t.Setenv(TokenEnv, testToken)
	h := newHandler(t, decide.Config{Web: decide.WebConfig{
		PublicURL: "https://decide.example.com",
		Auth:      decide.WebAuthConfig{Mode: decide.WebAuthToken},
	}}, nil)
	body := `{"token":"` + testToken + `"}`
	wantStatus(t, do(h, req{"POST", "/api/v1/auth/login", body, map[string]string{"Origin": "https://decide.example.com"}}), 200)
	wantStatus(t, do(h, req{"POST", "/api/v1/auth/login", body, map[string]string{"Origin": "https://evil.example"}}), 403)
}

func TestCSRFAppliesInNoneMode(t *testing.T) {
	h := newHandler(t, decide.Config{}, nil)
	wantStatus(t, do(h, req{"POST", "/api/v1/auth/logout", "", map[string]string{"Origin": "https://evil.example"}}), 403)
	wantStatus(t, do(h, req{"POST", "/api/v1/auth/logout", "", sameOrigin}), 200)
}

func TestBearerSkipsCSRFButWrongBearerDoesNot(t *testing.T) {
	h := tokenHandler(t)
	// A native client sends no Origin; a valid Bearer is not forgeable by a page.
	w := do(h, req{"POST", "/api/v1/auth/logout", "", map[string]string{"Authorization": "Bearer " + testToken}})
	wantStatus(t, w, 200)
	w = do(h, req{"POST", "/api/v1/auth/logout", "", map[string]string{"Authorization": "Bearer wrong"}})
	wantStatus(t, w, 403)
}

func TestTokenSources(t *testing.T) {
	dir := t.TempDir()
	tokenMode := func(file string) decide.Config {
		return decide.Config{Web: decide.WebConfig{Auth: decide.WebAuthConfig{Mode: decide.WebAuthToken, TokenFile: file}}}
	}
	build := func(cfg decide.Config) error {
		_, err := (&Server{Config: cfg}).Handler()
		return err
	}

	t.Setenv(TokenEnv, "")
	if err := build(tokenMode("")); err == nil {
		t.Error("token mode with no token source started")
	}

	existing := filepath.Join(dir, "existing")
	if err := os.WriteFile(existing, []byte(testToken+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	h := newHandler(t, tokenMode(existing), nil)
	wantStatus(t, login(h, testToken), 200)

	short := filepath.Join(dir, "short")
	if err := os.WriteFile(short, []byte("abc\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := build(tokenMode(short)); err == nil {
		t.Error("short token accepted")
	}
	empty := filepath.Join(dir, "empty")
	if err := os.WriteFile(empty, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	if err := build(tokenMode(empty)); err == nil {
		t.Error("empty token file accepted")
	}

	generated := filepath.Join(dir, "sub", "generated")
	if err := os.MkdirAll(filepath.Dir(generated), 0o700); err != nil {
		t.Fatal(err)
	}
	h = newHandler(t, tokenMode(generated), nil)
	info, err := os.Stat(generated)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode().Perm() != 0o600 {
		t.Errorf("generated token mode = %v, want 0600", info.Mode().Perm())
	}
	data, _ := os.ReadFile(generated)
	tok := strings.TrimSpace(string(data))
	if len(tok) != 64 {
		t.Errorf("generated token %q is not 32 bytes of hex", tok)
	}
	wantStatus(t, login(h, tok), 200)

	// The env token wins over the file.
	t.Setenv(TokenEnv, testToken)
	h = newHandler(t, tokenMode(generated), nil)
	wantStatus(t, login(h, testToken), 200)
	wantStatus(t, login(h, tok), 401)
}

func TestUnknownAuthModeRefused(t *testing.T) {
	_, err := (&Server{Config: decide.Config{Web: decide.WebConfig{Auth: decide.WebAuthConfig{Mode: "bogus"}}}}).Handler()
	if err == nil {
		t.Error("unknown auth mode accepted")
	}
}

func TestCheckListenAddr(t *testing.T) {
	for addr, loopback := range map[string]bool{
		"127.0.0.1:8787":   true,
		"127.1.2.3:1":      true,
		"[::1]:8787":       true,
		"localhost:8787":   true,
		"0.0.0.0:8787":     false,
		":8787":            false,
		"[::]:8787":        false,
		"192.168.1.5:8787": false,
		"example.com:80":   false,
	} {
		err := checkListenAddr(addr, decide.WebAuthNone, false)
		if loopback && err != nil {
			t.Errorf("%s: %v", addr, err)
		}
		if !loopback && err == nil {
			t.Errorf("%s with auth none started without --allow-insecure-unauth", addr)
		}
		if err := checkListenAddr(addr, decide.WebAuthNone, true); err != nil {
			t.Errorf("%s with allow-insecure-unauth: %v", addr, err)
		}
		if err := checkListenAddr(addr, decide.WebAuthToken, false); err != nil {
			t.Errorf("%s in token mode: %v", addr, err)
		}
	}
	if err := checkListenAddr("nonsense", decide.WebAuthToken, false); err == nil {
		t.Error("addr without a port accepted")
	}
}

func TestListenRefusesOpenPublicBind(t *testing.T) {
	s := &Server{Config: decide.Config{Web: decide.WebConfig{Addr: "0.0.0.0:0"}}}
	ln, err := s.Listen()
	if err == nil {
		ln.Close()
		t.Fatal("bound 0.0.0.0 with auth none")
	}
	if !strings.Contains(err.Error(), "allow-insecure-unauth") {
		t.Errorf("error %q does not name the flag", err)
	}
}

func TestServeAndShutdown(t *testing.T) {
	s := &Server{Config: decide.Config{Web: decide.WebConfig{Addr: "127.0.0.1:0"}}}
	ln, err := s.Listen()
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- s.Serve(ctx, ln) }()
	resp, err := http.Get("http://" + ln.Addr().String() + "/api/v1/health")
	if err != nil {
		t.Fatal(err)
	}
	_, _ = io.Copy(io.Discard, resp.Body)
	resp.Body.Close()
	cancel()
	select {
	case err := <-done:
		if err != nil {
			t.Errorf("Serve = %v", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("Serve did not return after cancel")
	}
	if _, err := http.Get("http://" + ln.Addr().String() + "/"); err == nil {
		t.Error("server still listening after shutdown")
	}
}
