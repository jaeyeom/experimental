package web

import (
	"bufio"
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"runtime"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
)

type sseFrame struct {
	id, event string
	data      map[string]any
}

type sseClient struct {
	t      *testing.T
	resp   *http.Response
	frames chan sseFrame
	cancel context.CancelFunc
}

func openSSE(t *testing.T, srv *httptest.Server, path string, header map[string]string) *sseClient {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	r, err := http.NewRequestWithContext(ctx, "GET", srv.URL+path, nil)
	if err != nil {
		t.Fatal(err)
	}
	r.Header.Set("Sec-Fetch-Site", "same-origin")
	for k, v := range header {
		r.Header.Set(k, v)
	}
	resp, err := srv.Client().Do(r)
	if err != nil {
		cancel()
		t.Fatal(err)
	}
	c := &sseClient{t: t, resp: resp, frames: make(chan sseFrame, 64), cancel: cancel}
	go func() {
		defer close(c.frames)
		sc := bufio.NewScanner(resp.Body)
		var f sseFrame
		for sc.Scan() {
			line := sc.Text()
			switch {
			case line == "":
				if f.event != "" {
					c.frames <- f
				}
				f = sseFrame{}
			case strings.HasPrefix(line, "id: "):
				f.id = line[4:]
			case strings.HasPrefix(line, "event: "):
				f.event = line[7:]
			case strings.HasPrefix(line, "data: "):
				_ = json.Unmarshal([]byte(line[6:]), &f.data)
			}
		}
	}()
	t.Cleanup(c.close)
	return c
}

func (c *sseClient) close() {
	c.cancel()
	_ = c.resp.Body.Close()
}

func (c *sseClient) next(skipPing bool) sseFrame {
	c.t.Helper()
	for {
		select {
		case f, ok := <-c.frames:
			if !ok {
				c.t.Fatal("stream closed")
			}
			if skipPing && f.event == "ping" {
				continue
			}
			return f
		case <-time.After(5 * time.Second):
			c.t.Fatal("timed out waiting for frame")
		}
	}
}

func newSSE(t *testing.T) (*api, *httptest.Server) {
	t.Helper()
	a := newAPI(t)
	a.server.PingEvery = 50 * time.Millisecond
	a.server.WatchEvery = 10 * time.Millisecond
	h, err := a.server.Handler()
	if err != nil {
		t.Fatal(err)
	}
	srv := httptest.NewServer(h)
	t.Cleanup(srv.Close)
	return a, srv
}

func TestEventsResyncThenDelta(t *testing.T) {
	a, srv := newSSE(t)
	a.create(&decide.Item{Question: "one?"})
	c := openSSE(t, srv, "/api/v1/events", nil)
	h := c.resp.Header
	if h.Get("Content-Type") != "text/event-stream" || h.Get("Cache-Control") != "no-cache" || h.Get("X-Accel-Buffering") != "no" {
		t.Errorf("headers = %v", h)
	}
	f := c.next(true)
	if f.event != "resync" || f.id == "" {
		t.Fatalf("first frame = %+v", f)
	}
	for _, k := range []string{"seq", "items", "inbox", "unreadCount", "status", "panels", "drafts", "draftSeq", "display", "busHealth", "targets", "agentbus", "orgTreeEnabled"} {
		if _, ok := f.data[k]; !ok {
			t.Errorf("resync lacks %q", k)
		}
	}
	first, _ := strconv.Atoi(f.id)
	r := a.create(&decide.Item{Question: "two?"})
	a.server.Watcher().Signal()
	d := c.next(true)
	if d.event != "delta" {
		t.Fatalf("frame = %+v", d)
	}
	if id, _ := strconv.Atoi(d.id); id <= first {
		t.Errorf("id %s not above %d", d.id, first)
	}
	ch, _ := d.data["changes"].([]any)
	if len(ch) != 1 || ch[0].(map[string]any)["id"] != r.ID {
		t.Errorf("changes = %v", d.data["changes"])
	}
}

func TestEventsLastEventIDResume(t *testing.T) {
	a, srv := newSSE(t)
	a.create(&decide.Item{Question: "one?"})
	c1 := openSSE(t, srv, "/api/v1/events", nil)
	id := c1.next(true).id
	c1.close()
	two := a.create(&decide.Item{Question: "two?"})
	c2 := openSSE(t, srv, "/api/v1/events", map[string]string{"Last-Event-ID": id})
	f := c2.next(true)
	if f.event != "delta" {
		t.Fatalf("frame = %+v", f)
	}
	ch, _ := f.data["changes"].([]any)
	if len(ch) != 1 || ch[0].(map[string]any)["id"] != two.ID {
		t.Errorf("changes = %v", f.data["changes"])
	}
	// The since query is the fallback, and a future seq is a gap.
	c3 := openSSE(t, srv, "/api/v1/events?since=99999", nil)
	if f := c3.next(true); f.event != "resync" {
		t.Errorf("unknown since gave %+v", f)
	}
}

func TestEventsPingAndCleanup(t *testing.T) {
	a, srv := newSSE(t)
	base := runtime.NumGoroutine()
	c := openSSE(t, srv, "/api/v1/events", nil)
	c.next(false)
	if f := c.next(false); f.event != "ping" {
		t.Fatalf("idle frame = %+v", f)
	}
	if n := a.server.state().hub.subscribers(); n != 1 {
		t.Fatalf("subscribers = %d", n)
	}
	c.close()
	deadline := time.Now().Add(5 * time.Second)
	for a.server.state().hub.subscribers() != 0 || a.server.state().hub.running() {
		if time.Now().After(deadline) {
			t.Fatal("hub still running after disconnect")
		}
		time.Sleep(10 * time.Millisecond)
	}
	srv.CloseClientConnections()
	for runtime.NumGoroutine() > base+2 {
		if time.Now().After(deadline) {
			t.Fatalf("goroutines %d, base %d", runtime.NumGoroutine(), base)
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func TestEventsRequiresAuth(t *testing.T) {
	h := tokenHandler(t)
	w := do(h, req{"GET", "/api/v1/events", "", sameOrigin})
	if w.Code != http.StatusUnauthorized {
		t.Errorf("status = %d", w.Code)
	}
}

func TestHubDropsSlowSubscriber(t *testing.T) {
	h := &hub{}
	sub := h.add(0)
	for i := int64(1); i <= subBuffer+1; i++ {
		h.broadcast(inboxrpc.WatchEvent{Seq: i})
	}
	select {
	case <-sub.dropped:
	default:
		t.Fatal("slow subscriber not dropped")
	}
	h.remove(sub)
}

func TestPresence(t *testing.T) {
	a := newAPI(t)
	now := testNow()
	a.server.Clock = func() time.Time { return now }
	if a.server.HasActiveClient() {
		t.Fatal("active before any heartbeat")
	}
	a.wantOK(a.post("/api/v1/presence", `{"clientId":"c1","active":true}`), 200)
	if !a.server.HasActiveClient() {
		t.Fatal("not active")
	}
	now = now.Add(59 * time.Second)
	if !a.server.HasActiveClient() {
		t.Fatal("expired early")
	}
	now = now.Add(2 * time.Second)
	if a.server.HasActiveClient() {
		t.Fatal("stale entry not expired")
	}
	a.wantOK(a.post("/api/v1/presence", `{"clientId":"c1","active":true}`), 200)
	a.wantOK(a.post("/api/v1/presence", `{"clientId":"c1","active":false}`), 200)
	if a.server.HasActiveClient() {
		t.Fatal("still active after clear")
	}
	a.wantErr(a.post("/api/v1/presence", `{"active":true}`), 400)
}

func TestEventsPanelChangeEmitsDeltaOnce(t *testing.T) {
	a := newAPI(t)
	a.server.PingEvery = time.Hour
	a.server.WatchEvery = time.Hour
	h, err := a.server.Handler()
	if err != nil {
		t.Fatal(err)
	}
	srv := httptest.NewServer(h)
	defer srv.Close()

	c := openSSE(t, srv, "/api/v1/events", nil)
	defer c.close()
	resync := c.next(true)
	if resync.event != "resync" {
		t.Fatalf("first event = %q, want resync", resync.event)
	}
	if p, ok := resync.data["panels"].([]any); !ok || len(p) != 0 {
		t.Fatalf("initial panels = %v", resync.data["panels"])
	}

	// Idle stream emits nothing.
	select {
	case f := <-c.frames:
		t.Fatalf("unexpected idle frame: %+v", f)
	case <-time.After(80 * time.Millisecond):
	}

	store, err := decide.OpenPanels(a.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.Set(decide.Panel{
		ID:    "build",
		Title: "Build",
		Kind:  "kv",
		Data:  json.RawMessage(`{"ok":true}`),
	}); err != nil {
		t.Fatal(err)
	}

	d := c.next(true)
	if d.event != "delta" {
		t.Fatalf("frame = %+v, want delta", d)
	}
	panels, ok := d.data["panels"].([]any)
	if !ok || len(panels) != 1 || panels[0].(map[string]any)["id"] != "build" {
		t.Fatalf("delta panels = %v", d.data["panels"])
	}
}
