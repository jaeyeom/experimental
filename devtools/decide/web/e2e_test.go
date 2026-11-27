package web

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// lockedTimers is fakeTimers that the dispatcher goroutine and the test
// may use together.
type lockedTimers struct {
	mu sync.Mutex
	fakeTimers
}

func (l *lockedTimers) after(d time.Duration, fn func()) func() bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.fakeTimers.after(d, fn)
}

// pending reports the armed timers.
func (l *lockedTimers) pending() int {
	l.mu.Lock()
	defer l.mu.Unlock()
	n := 0
	for _, fn := range l.fns {
		if fn != nil {
			n++
		}
	}
	return n
}

// fire runs the armed timers outside the lock, as time would.
func (l *lockedTimers) fire() {
	l.mu.Lock()
	var run []func()
	for i, fn := range l.fns {
		if fn != nil {
			run = append(run, fn)
			l.fns[i] = nil
		}
	}
	l.mu.Unlock()
	for _, fn := range run {
		fn()
	}
}

// e2eClock is a settable clock shared by the server and the ledger.
type e2eClock struct {
	mu sync.Mutex
	t  time.Time
}

func (c *e2eClock) now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.t
}

func (c *e2eClock) advance(d time.Duration) {
	c.mu.Lock()
	c.t = c.t.Add(d)
	c.mu.Unlock()
}

// e2e is the whole stack on a real listener: token auth, the REST API,
// the event stream, and the push dispatcher, over a temporary ledger.
type e2e struct {
	t      *testing.T
	srv    *httptest.Server
	server *Server
	dir    string
	clock  *e2eClock
	timers *lockedTimers
	push   *fakePushService
	phone  testBrowser
	cookie string
	d      *pushDispatcher
}

func newE2E(t *testing.T) *e2e {
	t.Helper()
	t.Setenv(TokenEnv, testToken)
	e := &e2e{t: t, dir: t.TempDir(), clock: &e2eClock{t: testNow()}, timers: &lockedTimers{}, push: newFakePushService(t), phone: newTestBrowser(t)}
	e.server = &Server{
		Backend: &tui.LocalBackend{
			Dir: e.dir, Now: e.clock.now,
			RunSend: func([]string) error { return nil },
		},
		Config: decide.Config{Web: decide.WebConfig{
			Auth: decide.WebAuthConfig{Mode: decide.WebAuthToken},
			Push: decide.WebPushConfig{Enabled: true, Subject: "mailto:me@example.com"},
		}},
		Clock:             e.clock.now,
		Targets:           []decide.ConfigTarget{{Name: "builder", Pane: "w2:p3"}},
		WatchEvery:        5 * time.Millisecond,
		PingEvery:         time.Hour,
		PushPath:          filepath.Join(t.TempDir(), "webpush.json"),
		PushClient:        e.push.Client(),
		AllowInsecurePush: true,
		PushAfter:         e.timers.after,
	}
	h, err := e.server.Handler()
	if err != nil {
		t.Fatal(err)
	}
	e.srv = httptest.NewServer(h)
	t.Cleanup(e.srv.Close)
	return e
}

// startPush runs the push dispatcher, as Serve does, and waits until it
// has seeded the items present now.
func (e *e2e) startPush(seeded string) {
	e.t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	e.d = newPushDispatcher(e.server)
	done := make(chan struct{})
	go func() { defer close(done); e.d.run(ctx) }()
	e.t.Cleanup(func() { cancel(); <-done })
	eventually(e.t, "dispatcher seeded", func() bool {
		e.d.mu.Lock()
		defer e.d.mu.Unlock()
		_, ok := e.d.msgs[seeded]
		return ok
	})
}

// eventually polls cond for a bounded time.
func eventually(t *testing.T, what string, cond func() bool) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for !cond() {
		if time.Now().After(deadline) {
			t.Fatalf("timed out waiting for %s", what)
		}
		time.Sleep(2 * time.Millisecond)
	}
}

// call sends one request as the page would (same-origin, session cookie)
// unless hdr says otherwise, and decodes a JSON object body.
func (e *e2e) call(method, path, body string, hdr map[string]string) (int, map[string]any, *http.Response) {
	e.t.Helper()
	r, err := http.NewRequest(method, e.srv.URL+path, strings.NewReader(body))
	if err != nil {
		e.t.Fatal(err)
	}
	for k, v := range hdr {
		r.Header.Set(k, v)
	}
	resp, err := e.srv.Client().Do(r)
	if err != nil {
		e.t.Fatal(err)
	}
	defer resp.Body.Close()
	raw, _ := io.ReadAll(resp.Body)
	var m map[string]any
	_ = json.Unmarshal(raw, &m)
	return resp.StatusCode, m, resp
}

func (e *e2e) page() map[string]string {
	h := map[string]string{"Sec-Fetch-Site": "same-origin"}
	if e.cookie != "" {
		h["Cookie"] = e.cookie
	}
	return h
}

func (e *e2e) want(code int, method, path, body string) map[string]any {
	e.t.Helper()
	got, m, _ := e.call(method, path, body, e.page())
	if got != code {
		e.t.Fatalf("%s %s = %d (%v), want %d", method, path, got, m, code)
	}
	return m
}

func (e *e2e) ledger() *decide.Ledger {
	e.t.Helper()
	led, err := decide.Open(e.dir)
	if err != nil {
		e.t.Fatal(err)
	}
	led.SetClock(e.clock.now)
	return led
}

func (e *e2e) create(it decide.Item) decide.Item {
	e.t.Helper()
	if it.Owner == "" {
		it.Owner = "ada"
	}
	if it.Kind == "" {
		it.Kind = decide.KindRelay
	}
	it.State = decide.StateOpen
	got, err := e.ledger().Create(&it)
	if err != nil {
		e.t.Fatal(err)
	}
	return got
}

func (e *e2e) item(id string) decide.Item {
	e.t.Helper()
	it, err := e.ledger().Get(id)
	if err != nil {
		e.t.Fatal(err)
	}
	return it
}

// pushedIDs decrypts everything the fake push service received.
func (e *e2e) pushedIDs() []string {
	e.push.mu.Lock()
	bodies := append([][]byte(nil), e.push.bodies...)
	e.push.mu.Unlock()
	var ids []string
	for _, b := range bodies {
		ids = append(ids, e.phone.decrypt(e.t, b)["itemId"].(string))
	}
	return ids
}

// nextChange reads frames until every id in ids has appeared in a delta frame.
func nextChange(t *testing.T, c *sseClient, ids ...string) sseFrame {
	t.Helper()
	need := make(map[string]bool, len(ids))
	for _, id := range ids {
		need[id] = true
	}
	for {
		f := c.next(true)
		var seen []any
		if f.event == "delta" {
			seen, _ = f.data["changes"].([]any)
		}
		for _, ch := range seen {
			if id, _ := ch.(map[string]any)["id"].(string); need[id] {
				delete(need, id)
			}
		}
		if len(need) == 0 {
			return f
		}
	}
}

func TestE2EFullStack(t *testing.T) {
	e := newE2E(t)
	opts := []decide.Option{{Key: "yes", Label: "Approve"}, {Key: "no", Label: "Decline"}}

	// Authentication and CSRF.
	t.Run("auth", func(t *testing.T) {
		if code, _, _ := e.call("GET", "/api/v1/health", "", nil); code != 200 {
			t.Errorf("health = %d", code)
		}
		if code, _, _ := e.call("GET", "/", "", nil); code != 200 {
			t.Errorf("shell = %d, want it public", code)
		}
		for _, p := range []string{"/api/v1/snapshot", "/api/v1/events", "/api/v1/status"} {
			if code, _, _ := e.call("GET", p, "", map[string]string{"Sec-Fetch-Site": "same-origin"}); code != 401 {
				t.Errorf("anonymous GET %s = %d, want 401", p, code)
			}
		}
		if code, _, _ := e.call("GET", "/api/v1/snapshot", "", map[string]string{"Authorization": "Bearer wrong-token-wrong-token"}); code != 401 {
			t.Errorf("wrong bearer = %d, want 401", code)
		}
		login := `{"token":"` + testToken + `"}`
		for name, hdr := range map[string]map[string]string{
			"no origin":    {},
			"cross-site":   {"Sec-Fetch-Site": "cross-site"},
			"evil origin":  {"Origin": "https://evil.example"},
			"wrong origin": {"Origin": "http://127.0.0.1:1"},
		} {
			if code, _, resp := e.call("POST", "/api/v1/auth/login", login, hdr); code != 403 || len(resp.Cookies()) != 0 {
				t.Errorf("login with %s = %d, cookies %v; want 403, none", name, code, resp.Cookies())
			}
		}
		same := map[string]string{"Sec-Fetch-Site": "same-origin"}
		for _, bad := range []string{`{"token":"nope-nope-nope-nope"}`, `{"token":""}`, `{}`} {
			if code, _, resp := e.call("POST", "/api/v1/auth/login", bad, same); code != 401 || len(resp.Cookies()) != 0 {
				t.Errorf("login %s = %d, want 401 and no cookie", bad, code)
			}
		}
		code, _, resp := e.call("POST", "/api/v1/auth/login", login, same)
		if code != 200 || len(resp.Cookies()) != 1 {
			t.Fatalf("login = %d, cookies %v", code, resp.Cookies())
		}
		c := resp.Cookies()[0]
		if c.Name != SessionCookie || !c.HttpOnly || c.SameSite != http.SameSiteLaxMode {
			t.Errorf("cookie = %+v", c)
		}
		e.cookie = c.Name + "=" + c.Value
		m := e.want(200, "GET", "/api/v1/auth/status", "")
		if m["authenticated"] != true || m["authRequired"] != true {
			t.Errorf("status = %v", m)
		}
		// A state change needs same-origin even with a valid session.
		if code, _, _ := e.call("POST", "/api/v1/presence", `{"clientId":"x","active":false}`,
			map[string]string{"Cookie": e.cookie, "Sec-Fetch-Site": "cross-site"}); code != 403 {
			t.Errorf("cross-site write with session = %d, want 403", code)
		}
		// A Bearer token needs no browser metadata.
		if code, _, _ := e.call("POST", "/api/v1/presence", `{"clientId":"x","active":false}`,
			map[string]string{"Authorization": "Bearer " + testToken}); code != 200 {
			t.Errorf("bearer write = %d, want 200", code)
		}
	})

	parent := e.create(decide.Item{Question: "deploy?", Class: decide.ClassRequest, NotifyPane: "w1:p1"})
	e.startPush(parent.ID)

	// Subscribe a phone to push.
	m := e.want(200, "GET", "/api/v1/push/vapid-key", "")
	if m["enabled"] != true || m["publicKey"] == "" {
		t.Fatalf("vapid-key = %v", m)
	}
	endpoint := e.push.URL + "/sub"
	e.want(200, "POST", "/api/v1/push/subscribe", subscribeBody(endpoint, e.phone))

	events := openSSE(t, e.srv, "/api/v1/events?since=0", map[string]string{"Cookie": e.cookie})
	resync := events.next(true)
	if resync.event != "resync" {
		t.Fatalf("first frame = %+v", resync)
	}
	if items, _ := resync.data["items"].([]any); len(items) != 1 {
		t.Fatalf("resync items = %v", resync.data["items"])
	}

	// A note and an approval filed straight into the ledger reach the
	// stream; only the approval is pushed.
	note := e.create(decide.Item{Question: "fyi", Class: decide.ClassNote})
	card := e.create(decide.Item{
		Question: "run rm -rf?", Verbatim: "rm -rf /tmp/x", Class: decide.ClassApproval, Parent: parent.ID,
		Options: opts, NotifyPane: "w1:p1",
	})
	nextChange(t, events, note.ID, card.ID)
	eventually(t, "grace timer", func() bool { return e.timers.pending() > 0 })
	if len(e.pushedIDs()) != 0 {
		t.Fatal("pushed before the grace delay")
	}
	e.timers.fire()
	pushed := e.pushedIDs()
	if len(pushed) == 0 || pushed[0] != card.ID {
		t.Fatalf("pushed %v, want the approval first", pushed)
	}
	for _, id := range pushed {
		if id == note.ID {
			t.Fatal("a note was pushed")
		}
	}
	e.push.mu.Lock()
	payload := e.phone.decrypt(t, e.push.bodies[0])
	e.push.mu.Unlock()
	acts, _ := payload["actions"].([]any)
	if len(acts) != 1 || acts[0].(map[string]any)["action"] != "snooze-1h" || acts[0].(map[string]any)["title"] != "Snooze 1h" {
		t.Errorf("actions = %v, want only Snooze 1h", payload["actions"])
	}
	if strings.Contains(string(mustJSON(t, payload)), "Approve") || strings.Contains(string(mustJSON(t, payload)), "Decline") {
		t.Errorf("payload offers a decision: %v", payload)
	}
	base := len(e.pushedIDs())

	// Read in the TUI inside the grace window: suppressed.
	read := e.create(decide.Item{Question: "pick one?", Class: decide.ClassDecision, Options: opts})
	nextChange(t, events, read.ID)
	eventually(t, "grace timer", func() bool { return e.timers.pending() > 0 })
	if _, err := e.server.Backend.MarkRead(read.ID); err != nil {
		t.Fatal(err)
	}
	e.timers.fire()
	if got := len(e.pushedIDs()); got != base {
		t.Fatalf("pushed an item read within the grace window (%d -> %d)", base, got)
	}

	// An active web client suppresses; once its heartbeat lapses, the
	// next item pushes.
	e.want(200, "POST", "/api/v1/presence", `{"clientId":"tab-1","active":true}`)
	busy := e.create(decide.Item{Question: "while you look", Class: decide.ClassDecision, Options: opts})
	nextChange(t, events, busy.ID)
	eventually(t, "grace timer", func() bool { return e.timers.pending() > 0 })
	e.timers.fire()
	if got := len(e.pushedIDs()); got != base {
		t.Fatalf("pushed while a client was active (%d -> %d)", base, got)
	}
	e.clock.advance(presenceTTL + time.Second)
	after := e.create(decide.Item{Question: "after you left", Class: decide.ClassDecision, Options: opts})
	nextChange(t, events, after.ID)
	eventually(t, "grace timer", func() bool { return e.timers.pending() > 0 })
	e.timers.fire()
	ids := e.pushedIDs()
	if len(ids) != base+1 || ids[base] != after.ID {
		t.Fatalf("pushed %v, want %s after the heartbeat expired", ids, after.ID)
	}

	// Approval answer with a conflict check: a stale view is refused.
	if _, err := e.ledger().Reply(card.ID, "ops/bot", "news", nil, false); err != nil {
		t.Fatal(err)
	}
	stale := `{"value":"yes","conflictCheck":{"baseState":"open","baseMessageCount":` + strconv.Itoa(len(card.Messages)) + `}}`
	m = e.want(409, "POST", "/api/v1/items/"+card.ID+"/answer", stale)
	if m["conflict"] != "new message arrived" {
		t.Errorf("conflict = %v", m)
	}
	if e.item(card.ID).Answer != nil {
		t.Fatal("a conflicting answer was applied")
	}
	cur := e.item(card.ID)
	fresh := `{"value":"yes","conflictCheck":{"baseState":"open","baseMessageCount":` + strconv.Itoa(len(cur.Messages)) + `}}`
	before := len(e.item(parent.ID).Messages)
	e.want(200, "POST", "/api/v1/items/"+card.ID+"/answer", fresh)
	got := e.item(card.ID)
	if got.Answer == nil || got.Answer.Via != decide.ViaUI || got.Answer.Value != "yes" {
		t.Fatalf("answer = %+v", got.Answer)
	}
	if len(e.item(parent.ID).Messages) <= before {
		t.Error("the parent thread has no approval line")
	}
	nextChange(t, events, card.ID)

	// Reply and draft conflict.
	e.want(200, "POST", "/api/v1/items/"+parent.ID+"/reply", `{"text":"go ahead"}`)
	if last := e.item(parent.ID).Messages; last[len(last)-1].Via != decide.ViaUI {
		t.Errorf("reply via = %q", last[len(last)-1].Via)
	}
	key := "reply:" + parent.ID
	put := func(text string, baseRev int) (int, map[string]any) {
		b, _ := json.Marshal(map[string]any{"draft": map[string]any{"key": key, "text": text}, "baseRev": baseRev})
		code, m, _ := e.call("PUT", "/api/v1/drafts/"+key, string(b), e.page())
		return code, m
	}
	if code, m := put("first", 0); code != 200 || m["draftConflict"] != false {
		t.Fatalf("draft put = %d %v", code, m)
	}
	code, m := put("from the other client", 0)
	if code != 409 || m["draftConflict"] != true {
		t.Fatalf("stale draft put = %d %v, want 409 conflict", code, m)
	}
	if d := m["draft"].(map[string]any); d["text"] != "first" || len(d["conflicts"].([]any)) != 1 {
		t.Errorf("draft after conflict = %v", d)
	}

	// Triage: snooze (and its delta on the stream), unsnooze, archive, done.
	e.want(200, "POST", "/api/v1/items/"+busy.ID+"/snooze", `{"preset":"1h"}`)
	f := nextChange(t, events, busy.ID)
	if f.event != "delta" || f.id == "" {
		t.Errorf("snooze frame = %+v", f)
	}
	if e.item(busy.ID).SnoozedUntil == nil {
		t.Error("snooze did not apply")
	}
	e.want(200, "POST", "/api/v1/items/"+busy.ID+"/unsnooze", "")
	if e.item(busy.ID).SnoozedUntil != nil {
		t.Error("unsnooze did not apply")
	}
	e.want(200, "POST", "/api/v1/items/"+read.ID+"/read", "")
	seen := e.clock.now().Format(time.RFC3339Nano)
	e.want(200, "POST", "/api/v1/items/"+note.ID+"/archive", `{"seen":"`+seen+`"}`)
	if e.item(note.ID).State == decide.StateOpen {
		t.Error("archive left the note open")
	}
	e.want(200, "POST", "/api/v1/items/"+after.ID+"/done", `{"outcome":"handled"}`)
	e.want(200, "POST", "/api/v1/send", `{"to":"builder","text":"build it"}`)
	e.want(400, "POST", "/api/v1/send", `{"to":"stranger","text":"hi"}`)
	e.want(200, "POST", "/api/v1/contacts/favorites", `{"names":["ada"]}`)

	// A client that reconnects with the last id it saw gets a delta of
	// what it missed, not a resync.
	lastID := nextChange(t, events, after.ID).id
	events.close()
	missed := e.create(decide.Item{Question: "while offline", Class: decide.ClassNote})
	again := openSSE(t, e.srv, "/api/v1/events", map[string]string{"Cookie": e.cookie, "Last-Event-ID": lastID})
	if f := nextChange(t, again, missed.ID); f.event != "delta" {
		t.Errorf("resume frame = %+v, want a delta", f)
	}

	// Logout ends the session.
	e.want(200, "POST", "/api/v1/auth/logout", "")
	if code, _, _ := e.call("GET", "/api/v1/snapshot", "", e.page()); code != 401 {
		t.Errorf("snapshot after logout = %d, want 401", code)
	}
}

func mustJSON(t *testing.T, v any) []byte {
	t.Helper()
	b, err := json.Marshal(v)
	if err != nil {
		t.Fatal(err)
	}
	return b
}

var assetRef = regexp.MustCompile(`(?:src|href)="(/[^"#]+)"|from '\./([\w./-]+)'|import '\./([\w./-]+)'`)

// TestE2EEmbeddedAssets crawls the shell: every asset it references,
// directly or through ES module imports, is served with the security
// headers, and the manifest and worker are well formed.
func TestE2EEmbeddedAssets(t *testing.T) {
	e := newE2E(t)
	get := func(p string) (string, http.Header) {
		t.Helper()
		resp, err := e.srv.Client().Get(e.srv.URL + p)
		if err != nil {
			t.Fatal(err)
		}
		defer resp.Body.Close()
		b, _ := io.ReadAll(resp.Body)
		if resp.StatusCode != 200 {
			t.Errorf("GET %s = %d, want 200", p, resp.StatusCode)
		}
		if !strings.Contains(resp.Header.Get("Content-Security-Policy"), "default-src 'self'") ||
			resp.Header.Get("X-Content-Type-Options") != "nosniff" {
			t.Errorf("GET %s lacks security headers: %v", p, resp.Header)
		}
		return string(b), resp.Header
	}
	seen := map[string]bool{}
	queue := []string{"/", "/manifest.webmanifest", "/sw.js"}
	for len(queue) > 0 {
		p := queue[0]
		queue = queue[1:]
		if seen[p] {
			continue
		}
		seen[p] = true
		body, _ := get(p)
		if !strings.HasSuffix(p, ".js") && p != "/" {
			continue
		}
		for _, m := range assetRef.FindAllStringSubmatch(body, -1) {
			switch {
			case m[1] != "":
				queue = append(queue, m[1])
			case m[2] != "":
				queue = append(queue, "/static/"+m[2])
			case m[3] != "":
				queue = append(queue, "/static/"+m[3])
			}
		}
	}
	for _, want := range []string{"/static/app.js", "/static/styles.css", "/static/store.js", "/static/thread_view.js", "/static/settings_view.js"} {
		if !seen[want] {
			t.Errorf("crawl never reached %s", want)
		}
	}
	// Manifest icons are served too.
	body, _ := get("/manifest.webmanifest")
	var man struct {
		Icons []struct{ Src string }
	}
	if err := json.Unmarshal([]byte(body), &man); err != nil || len(man.Icons) == 0 {
		t.Fatalf("manifest: %v, %d icons", err, len(man.Icons))
	}
	for _, ic := range man.Icons {
		get(ic.Src)
	}
	sw, h := get("/sw.js")
	if bytes.Contains([]byte(sw), []byte("__BUILD__")) || h.Get("Service-Worker-Allowed") != "/" {
		t.Errorf("service worker not built: allowed=%q", h.Get("Service-Worker-Allowed"))
	}
}

func TestNextChangeBatched(t *testing.T) {
	t.Run("single_frame", func(t *testing.T) {
		c := &sseClient{t: t, frames: make(chan sseFrame, 4)}
		c.frames <- sseFrame{
			id:    "1",
			event: "delta",
			data: map[string]any{
				"changes": []any{
					map[string]any{"id": "note-1"},
					map[string]any{"id": "card-1"},
				},
			},
		}
		f := nextChange(t, c, "note-1", "card-1")
		if f.id != "1" {
			t.Errorf("frame id = %q, want 1", f.id)
		}
	})

	t.Run("multiple_frames", func(t *testing.T) {
		c := &sseClient{t: t, frames: make(chan sseFrame, 4)}
		c.frames <- sseFrame{
			id:    "1",
			event: "delta",
			data:  map[string]any{"changes": []any{map[string]any{"id": "note-1"}}},
		}
		c.frames <- sseFrame{event: "ping"}
		c.frames <- sseFrame{
			id:    "2",
			event: "delta",
			data:  map[string]any{"changes": []any{map[string]any{"id": "card-1"}}},
		}
		f := nextChange(t, c, "note-1", "card-1")
		if f.id != "2" {
			t.Errorf("frame id = %q, want 2", f.id)
		}
	})
}
