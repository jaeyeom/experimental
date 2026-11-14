package registry

import (
	"context"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

const (
	bootSock   = "/run/user/1000/herdr.sock"
	bootHandle = "herdr:local/s1/p1"
)

// bootHerdr is a herdr runner whose server is missing until up is set,
// as at boot when agentbusd starts before the herdr server.
type bootHerdr struct {
	mu    sync.Mutex
	up    bool
	lists int
}

func (h *bootHerdr) setUp(up bool) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.up = up
}

func (h *bootHerdr) agentLists() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.lists
}

func (h *bootHerdr) run(_ context.Context, args []string) (string, string, int, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	cmd := strings.Join(args[:min(2, len(args))], " ")
	if cmd == "agent list" {
		h.lists++
	}
	if !h.up {
		return `{"id":"cli:agent:list","error":{"code":"server_not_running","message":"no herdr server is running at ` + bootSock + `; run ` + "`herdr`" + ` to start or attach it"}}`, "", 1, nil
	}
	switch cmd {
	case "agent list":
		return `{"result":{"agents":[{"pane_id":"p1","tab_id":"t1","agent":"claude","agent_status":"idle","agent_session":{"value":"s1"}}]}}`, "", 0, nil
	case "tab list":
		return `{"result":{"tabs":[{"tab_id":"t1","label":"reviews"}]}}`, "", 0, nil
	default:
		return "", "unexpected " + cmd, 1, nil
	}
}

type stepClock struct {
	mu sync.Mutex
	t  time.Time
}

func (c *stepClock) now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.t
}

func (c *stepClock) add(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.t = c.t.Add(d)
}

func healthEvents(t *testing.T, lg *eventlog.Log) []eventlog.Event {
	t.Helper()
	lines, err := lg.Read(eventlog.Filter{Name: "herdr"})
	if err != nil {
		t.Fatal(err)
	}
	var out []eventlog.Event
	for _, line := range lines {
		ev, err := eventlog.Parse(line)
		if err != nil {
			t.Fatal(err)
		}
		if ev.Event == eventlog.EventAdapterError || ev.Event == eventlog.EventAdapterReachable {
			out = append(out, ev)
		}
	}
	return out
}

// TestHerdrDownAtStartRecoversWithoutRestart is a daemon that started
// before the herdr server: the transport is retried with backoff, and
// once herdr answers, search and liveness work with the same registry.
func TestHerdrDownAtStartRecoversWithoutRestart(t *testing.T) {
	dir := t.TempDir()
	h := &bootHerdr{up: true}
	ads := []adapter.Adapter{&herdr.Adapter{Run: h.run}}
	first, err := Open(dir, ads, nil)
	if err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	if _, err := first.Register(ctx, envPane("p1"), "ops/x", "reviews"); err != nil {
		t.Fatal(err)
	}

	// Boot: the daemon starts again while the herdr server is not up.
	h.setUp(false)
	lg := eventlog.Open(dir)
	reg, err := Open(dir, []adapter.Adapter{&herdr.Adapter{Run: h.run}}, lg)
	if err != nil {
		t.Fatal(err)
	}
	clock := &stepClock{t: time.Date(2026, 11, 14, 8, 0, 0, 0, time.UTC)}
	reg.Now = clock.now

	reg.pollOnce(ctx)
	rows, err := reg.Search(ctx, "")
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 0 {
		t.Fatalf("search while herdr is down = %+v", rows)
	}
	if got := liveOf(t, reg, "ops/x"); got != "unreachable" {
		t.Fatalf("liveness while down = %s", got)
	}

	// Each failure doubles the wait, up to healthBackoffMax.
	wait := healthBackoff
	for range 8 {
		before := h.agentLists()
		clock.add(wait - time.Millisecond)
		reg.pollOnce(ctx)
		if h.agentLists() != before {
			t.Fatalf("retried before the %s backoff ended", wait)
		}
		clock.add(time.Millisecond)
		reg.pollOnce(ctx)
		if h.agentLists() == before {
			t.Fatalf("no retry after the %s backoff", wait)
		}
		wait = min(2*wait, healthBackoffMax)
	}
	if wait != healthBackoffMax || healthBackoffMax > 30*time.Second {
		t.Fatalf("backoff = %s, max %s", wait, healthBackoffMax)
	}

	h.setUp(true)
	clock.add(healthBackoffMax)
	reg.pollOnce(ctx)
	rows, err = reg.Search(ctx, "reviews")
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 || rows[0].Handle != bootHandle {
		t.Fatalf("search after herdr came up = %+v", rows)
	}
	if got := liveOf(t, reg, "ops/x"); got != "alive" {
		t.Fatalf("liveness after herdr came up = %s", got)
	}

	events := healthEvents(t, lg)
	if len(events) != 2 {
		t.Fatalf("health events = %+v, want one down and one up", events)
	}
	down, up := events[0], events[1]
	if down.Event != eventlog.EventAdapterError || down.Detail != "herdr unreachable" || !strings.Contains(down.Reason, bootSock) {
		t.Fatalf("down event = %+v", down)
	}
	if up.Event != eventlog.EventAdapterReachable || up.Detail != "herdr reachable" {
		t.Fatalf("up event = %+v", up)
	}
}

func liveOf(t *testing.T, reg *Registry, name string) string {
	t.Helper()
	rows, err := reg.List(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	for _, row := range rows {
		if row.Name == name {
			return row.Liveness
		}
	}
	t.Fatalf("list has no %s: %+v", name, rows)
	return ""
}

func TestTransportProblems(t *testing.T) {
	dir := t.TempDir()
	lg := eventlog.Open(dir)
	appendAll := func(events ...eventlog.Event) {
		t.Helper()
		for _, ev := range events {
			if err := lg.Append(ev); err != nil {
				t.Fatal(err)
			}
		}
	}
	start := eventlog.Event{Event: eventlog.EventStart, Actor: "daemon", Detail: "listening"}
	down := eventlog.Event{Event: eventlog.EventAdapterError, Actor: "agentbus", Name: "herdr", Detail: "herdr unreachable", Reason: "no herdr server is running at " + bootSock}
	up := eventlog.Event{Event: eventlog.EventAdapterReachable, Actor: "agentbus", Name: "herdr", Detail: "herdr reachable"}

	check := func(want string) {
		t.Helper()
		got, err := TransportProblems(dir)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Join(got, "\n") != want {
			t.Fatalf("problems = %q, want %q", got, want)
		}
	}
	check("")
	appendAll(start, down)
	check("herdr transport unreachable: no herdr server is running at " + bootSock)
	appendAll(up)
	check("")
	appendAll(down, start)
	check("")
}
