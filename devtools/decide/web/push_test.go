package web

import (
	"crypto/ecdh"
	"crypto/rand"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	_ "time/tzdata"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// fakePushService records pushes and answers with status.
type fakePushService struct {
	*httptest.Server
	mu     sync.Mutex
	bodies [][]byte
	status int
}

func newFakePushService(t *testing.T) *fakePushService {
	t.Helper()
	f := &fakePushService{status: http.StatusCreated}
	f.Server = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		buf := make([]byte, r.ContentLength)
		_, _ = r.Body.Read(buf)
		f.mu.Lock()
		f.bodies = append(f.bodies, buf)
		st := f.status
		f.mu.Unlock()
		w.WriteHeader(st)
	}))
	t.Cleanup(f.Close)
	return f
}

func (f *fakePushService) count() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.bodies)
}

func (f *fakePushService) setStatus(code int) {
	f.mu.Lock()
	f.status = code
	f.mu.Unlock()
}

// testBrowser is a test browser: its keys decrypt what the server sends.
type testBrowser struct {
	priv *ecdh.PrivateKey
	auth []byte
}

func newTestBrowser(t *testing.T) testBrowser {
	t.Helper()
	p, err := ecdh.P256().GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	a := make([]byte, 16)
	_, _ = rand.Read(a)
	return testBrowser{priv: p, auth: a}
}

func (s testBrowser) keys() SubscriptionKeys {
	return SubscriptionKeys{P256dh: encodeB64(s.priv.PublicKey().Bytes()), Auth: encodeB64(s.auth)}
}

func (s testBrowser) decrypt(t *testing.T, body []byte) map[string]any {
	t.Helper()
	plain, err := decryptAES128GCM(body, s.priv, s.auth)
	if err != nil {
		t.Fatal(err)
	}
	var m map[string]any
	if err := json.Unmarshal(plain, &m); err != nil {
		t.Fatalf("payload %q: %v", plain, err)
	}
	return m
}

func pushServer(t *testing.T) (*Server, string) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "state", "webpush.json")
	s := &Server{
		Config:            decide.Config{Web: decide.WebConfig{Push: decide.WebPushConfig{Enabled: true, Subject: "mailto:me@example.com"}}},
		Clock:             testNow,
		PushPath:          path,
		AllowInsecurePush: true,
	}
	return s, path
}

func pushHandler(t *testing.T, s *Server) http.Handler {
	t.Helper()
	h, err := s.Handler()
	if err != nil {
		t.Fatal(err)
	}
	return h
}

func TestPushVAPIDKeyEndpoint(t *testing.T) {
	s, path := pushServer(t)
	h := pushHandler(t, s)
	w := do(h, req{method: "GET", path: "/api/v1/push/vapid-key"})
	var got struct {
		PublicKey string
		Enabled   bool
	}
	_ = json.Unmarshal(w.Body.Bytes(), &got)
	if w.Code != 200 || !got.Enabled || len(b64(t, got.PublicKey)) != 65 {
		t.Fatalf("vapid-key = %d %s", w.Code, w.Body)
	}
	fi, err := os.Stat(path)
	if err != nil || fi.Mode().Perm() != 0o600 {
		t.Fatalf("state file = %v, %v", fi, err)
	}
	// The key survives a restart.
	s2, _ := pushServer(t)
	s2.PushPath = path
	w2 := do(pushHandler(t, s2), req{method: "GET", path: "/api/v1/push/vapid-key"})
	if !strings.Contains(w2.Body.String(), got.PublicKey) {
		t.Fatalf("restart changed the key: %s", w2.Body)
	}
}

func TestPushDisabled(t *testing.T) {
	s, path := pushServer(t)
	s.Config.Web.Push.Enabled = false
	h := pushHandler(t, s)
	w := do(h, req{method: "GET", path: "/api/v1/push/vapid-key"})
	if !strings.Contains(w.Body.String(), `"enabled":false`) {
		t.Fatalf("body = %s", w.Body)
	}
	w = do(h, req{method: "POST", path: "/api/v1/push/subscribe", header: sameOrigin, body: `{}`})
	if w.Code != http.StatusServiceUnavailable {
		t.Fatalf("subscribe code = %d", w.Code)
	}
	if _, err := os.Stat(path); err == nil {
		t.Fatal("disabled push created a state file")
	}
}

func subscribeBody(endpoint string, sub testBrowser) string {
	b, _ := json.Marshal(map[string]any{"endpoint": endpoint, "keys": sub.keys()})
	return string(b)
}

func TestPushSubscribeValidatesAndUpserts(t *testing.T) {
	s, _ := pushServer(t)
	s.AllowInsecurePush = false
	h := pushHandler(t, s)
	sub := newTestBrowser(t)
	bad := []string{
		"http://push.example.com/x",
		"https://127.0.0.1/x",
		"https://localhost/x",
		"https://10.0.0.5/x",
		"https://169.254.169.254/x",
		"https://[::1]/x",
		"https://192.168.1.1/x",
		"https://user:pw@push.example.com/x",
		"",
	}
	for _, ep := range bad {
		w := do(h, req{method: "POST", path: "/api/v1/push/subscribe", header: sameOrigin, body: subscribeBody(ep, sub)})
		if w.Code != http.StatusBadRequest {
			t.Errorf("endpoint %q accepted: %d", ep, w.Code)
		}
	}
	w := do(h, req{
		method: "POST", path: "/api/v1/push/subscribe", header: sameOrigin,
		body: `{"endpoint":"https://push.example.com/x","keys":{"p256dh":"AAAA","auth":"AAAA"}}`,
	})
	if w.Code != http.StatusBadRequest {
		t.Errorf("bad keys accepted: %d", w.Code)
	}
	good := subscribeBody("https://push.example.com/x", sub)
	for range 2 {
		w = do(h, req{method: "POST", path: "/api/v1/push/subscribe", header: sameOrigin, body: good})
		if w.Code != 200 {
			t.Fatalf("subscribe = %d %s", w.Code, w.Body)
		}
	}
	if strings.Contains(w.Body.String(), "push.example.com") || strings.Contains(w.Body.String(), sub.keys().Auth) {
		t.Fatalf("response leaks endpoint or keys: %s", w.Body)
	}
	var got Subscription
	_ = json.Unmarshal(w.Body.Bytes(), &got)
	if !got.Enabled || slices.Contains(got.Classes, "note") || len(got.Classes) != 6 {
		t.Fatalf("defaults = %+v", got)
	}
	st, _ := s.pushStore()
	if n := len(st.List()); n != 1 {
		t.Fatalf("upsert stored %d subscriptions", n)
	}
}

func TestPushRoutesNeedCSRF(t *testing.T) {
	s, _ := pushServer(t)
	h := pushHandler(t, s)
	w := do(h, req{method: "POST", path: "/api/v1/push/subscribe", body: `{}`})
	if w.Code != http.StatusForbidden {
		t.Fatalf("cross-origin subscribe = %d", w.Code)
	}
}

func TestPushPreferencesStripNote(t *testing.T) {
	s, _ := pushServer(t)
	h := pushHandler(t, s)
	sub := newTestBrowser(t)
	ep := "https://push.example.com/x"
	do(h, req{method: "POST", path: "/api/v1/push/subscribe", header: sameOrigin, body: subscribeBody(ep, sub)})
	put := func(body string) *httptest.ResponseRecorder {
		return do(h, req{method: "PUT", path: "/api/v1/push/preferences", header: sameOrigin, body: body})
	}
	w := put(`{"endpoint":"` + ep + `","enabled":false,"classes":["note","approval","Approval"],"quietStart":"22:00","quietEnd":"08:00","tz":"Asia/Seoul"}`)
	if w.Code != 200 {
		t.Fatalf("put = %d %s", w.Code, w.Body)
	}
	var got Subscription
	_ = json.Unmarshal(w.Body.Bytes(), &got)
	if got.Enabled || !slices.Equal(got.Classes, []string{"approval"}) || got.QuietStart != "22:00" || got.TZ != "Asia/Seoul" {
		t.Fatalf("prefs = %+v", got)
	}
	for _, body := range []string{
		`{"endpoint":"` + ep + `","classes":["bogus"]}`,
		`{"endpoint":"` + ep + `","quietStart":"25:00","quietEnd":"08:00"}`,
		`{"endpoint":"` + ep + `","quietStart":"22:00","quietEnd":""}`,
		`{"endpoint":"` + ep + `","tz":"Not/AZone"}`,
	} {
		if w := put(body); w.Code != http.StatusBadRequest {
			t.Errorf("%s accepted: %d", body, w.Code)
		}
	}
	if w := put(`{"endpoint":"https://other.example.com/x","enabled":true}`); w.Code != http.StatusNotFound {
		t.Errorf("unknown endpoint = %d", w.Code)
	}
}

func TestPushDeleteSubscription(t *testing.T) {
	s, _ := pushServer(t)
	h := pushHandler(t, s)
	sub := newTestBrowser(t)
	ep := "https://push.example.com/x"
	do(h, req{method: "POST", path: "/api/v1/push/subscribe", header: sameOrigin, body: subscribeBody(ep, sub)})
	w := do(h, req{method: "DELETE", path: "/api/v1/push/subscribe", header: sameOrigin, body: `{"endpoint":"` + ep + `"}`})
	if w.Code != 200 {
		t.Fatalf("delete = %d", w.Code)
	}
	st, _ := s.pushStore()
	if len(st.List()) != 0 {
		t.Fatal("subscription not removed")
	}
}

func TestPushTestSendsAndPrunesGone(t *testing.T) {
	svc := newFakePushService(t)
	s, _ := pushServer(t)
	s.PushClient = svc.Client()
	h := pushHandler(t, s)
	sub := newTestBrowser(t)
	ep := svc.URL + "/x"
	do(h, req{method: "POST", path: "/api/v1/push/subscribe", header: sameOrigin, body: subscribeBody(ep, sub)})
	body := `{"endpoint":"` + ep + `"}`
	w := do(h, req{method: "POST", path: "/api/v1/push/test", header: sameOrigin, body: body})
	if w.Code != 200 || svc.count() != 1 {
		t.Fatalf("test push = %d, %d sent", w.Code, svc.count())
	}
	if m := sub.decrypt(t, svc.bodies[0]); m["title"] == "" {
		t.Fatalf("payload = %v", m)
	}
	svc.setStatus(http.StatusGone)
	w = do(h, req{method: "POST", path: "/api/v1/push/test", header: sameOrigin, body: body})
	if w.Code != http.StatusGone {
		t.Fatalf("gone test push = %d", w.Code)
	}
	st, _ := s.pushStore()
	if len(st.List()) != 0 {
		t.Fatal("410 did not prune the subscription")
	}
}

func TestIsPublicIP(t *testing.T) {
	for ip, want := range map[string]bool{
		"8.8.8.8": true, "2606:4700::1": true,
		"127.0.0.1": false, "10.1.2.3": false, "172.16.0.1": false, "192.168.0.1": false,
		"169.254.1.1": false, "::1": false, "fe80::1": false, "fc00::1": false, "0.0.0.0": false,
		"100.64.0.1": false, "::ffff:127.0.0.1": false,
	} {
		if got := isPublicIP(netip.MustParseAddr(ip)); got != want {
			t.Errorf("isPublicIP(%s) = %v", ip, got)
		}
	}
}
