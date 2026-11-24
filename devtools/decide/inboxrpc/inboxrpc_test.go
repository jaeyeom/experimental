package inboxrpc

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func TestMain(m *testing.M) {
	dir, err := os.MkdirTemp("", "inboxrpc-config")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	// No test may run a live notify command from the user's config.
	if err := os.Setenv("XDG_CONFIG_HOME", dir); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	cacheDir, err := os.MkdirTemp("", "inboxrpc-cache")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := os.Setenv("XDG_CACHE_HOME", cacheDir); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	code := m.Run()
	_ = os.RemoveAll(dir)
	_ = os.RemoveAll(cacheDir)
	os.Exit(code)
}

// hub is a server on a temporary ledger with recording send, agentbus,
// and herdr fakes.
type hub struct {
	dir    string
	server *Server
	mu     sync.Mutex
	sends  [][]string
	bus    [][]string
	herdr  []string
	dials  int
	conns  []*Conn
	wg     sync.WaitGroup
}

func newHub(t *testing.T) *hub {
	t.Helper()
	h := &hub{dir: t.TempDir()}
	t.Cleanup(func() {
		h.mu.Lock()
		conns := append([]*Conn(nil), h.conns...)
		h.mu.Unlock()
		for _, conn := range conns {
			conn.Close()
		}
		h.wg.Wait()
	})
	h.server = &Server{
		Backend: &tui.LocalBackend{
			Dir: h.dir,
			RunSend: func(argv []string) error {
				h.mu.Lock()
				defer h.mu.Unlock()
				h.sends = append(h.sends, argv)
				return nil
			},
			BusRun: func(_ context.Context, args []string) ([]byte, error) {
				h.mu.Lock()
				defer h.mu.Unlock()
				h.bus = append(h.bus, args)
				if args[0] == "list" {
					return []byte(`[{"name":"coord/primary","liveness":"live","handle":"herdr:hub/s/p9"}]`), nil
				}
				return []byte("m-1 queued"), nil
			},
			Herdr: func(_ context.Context, machine string, args []string) (string, error) {
				h.mu.Lock()
				defer h.mu.Unlock()
				h.herdr = append(h.herdr, machine+":"+args[0])
				switch args[0] {
				case "pane":
					return `{"result":{"panes":[{"pane_id":"w1:p1","tab_id":"w1:t1","workspace_id":"w1"}]}}`, nil
				case "tab":
					return `{"result":{"tabs":[{"tab_id":"w1:t1","label":"review"}]}}`, nil
				default:
					return `{"result":{"workspaces":[{"workspace_id":"w1","label":"exp"}]}}`, nil
				}
			},
		},
		Targets:    []decide.ConfigTarget{{Name: "builder", Pane: "w2:p3"}},
		Agentbus:   true,
		WatchEvery: 5 * time.Millisecond,
	}
	return h
}

// dial connects a client to the server in this process.
func (h *hub) dial() (*Conn, error) {
	reqR, reqW := io.Pipe()
	respR, respW := io.Pipe()
	h.wg.Add(1)
	go func() {
		defer h.wg.Done()
		err := h.server.Serve(context.Background(), reqR, respW)
		respW.CloseWithError(err)
	}()
	conn := &Conn{W: reqW, R: respR, Close: func() {
		_ = reqW.Close()
		_ = respR.Close()
	}}
	h.mu.Lock()
	h.dials++
	h.conns = append(h.conns, conn)
	h.mu.Unlock()
	return conn, nil
}

func (h *hub) create(t *testing.T, item *decide.Item) decide.Item {
	t.Helper()
	led, err := decide.Open(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	if item.Owner == "" {
		item.Owner = "ada"
	}
	if item.Kind == "" {
		item.Kind = decide.KindRelay
	}
	item.State = decide.StateOpen
	got, err := led.Create(item)
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func TestServeFollowsUpAStaleAnswerWithoutAList(t *testing.T) {
	h := newHub(t)
	start := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	now := start
	h.server.Backend.Now = func() time.Time { return now }
	prev := h.server.Backend.Herdr
	h.server.Backend.Herdr = func(ctx context.Context, machine string, args []string) (string, error) {
		if len(args) > 0 && args[0] == "agent" {
			return `{"result":{"agents":[{"pane_id":"w1:p1","agent_status":"idle"}]}}`, nil
		}
		return prev(ctx, machine, args)
	}
	led, err := decide.Open(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return now })
	item, err := led.Create(&decide.Item{
		Question:   "ship",
		Owner:      "ada",
		Kind:       decide.KindRelay,
		NotifyPane: "w1:p1",
		Options:    []decide.Option{{Key: "a", Label: "Alpha"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(item.ID, "a", decide.ViaCLI); err != nil {
		t.Fatal(err)
	}
	now = start.Add(31 * time.Minute)
	h.server.FollowEvery = 15 * time.Millisecond
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	pr, pw := io.Pipe()
	defer pw.Close()
	errc := make(chan error, 1)
	go func() {
		errc <- h.server.Serve(ctx, pr, io.Discard)
	}()
	deadline := time.Now().Add(2 * time.Second)
	var got decide.Item
	for {
		got, err = led.Get(item.ID)
		resent := err == nil && len(got.Delivery) > 0 && got.Delivery[len(got.Delivery)-1].Outcome == decide.OutcomeResend
		if resent {
			break
		}
		if time.Now().After(deadline) {
			t.Fatalf("delivery = %+v err = %v", got.Delivery, err)
		}
		time.Sleep(10 * time.Millisecond)
	}
	cancel()
	_ = pw.Close()
	select {
	case err := <-errc:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("serve did not return")
	}
}

func connect(t *testing.T, h *hub) *Client {
	t.Helper()
	c := &Client{Dial: h.dial, Label: "me@hub", Machine: "mac", HubMachine: "hub", CacheDir: t.TempDir()}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(c.Close)
	return c
}

func eventually(t *testing.T, cond func() bool, msg string) {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for !cond() {
		if time.Now().After(deadline) {
			t.Fatal(msg)
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func TestEveryOpRoundTrips(t *testing.T) {
	h := newHub(t)
	relay := h.create(t, &decide.Item{Question: "ship?", Options: []decide.Option{{Key: "y", Label: "Yes"}}, NotifyPane: "w1:p1", Unread: true})
	other := h.create(t, &decide.Item{Question: "other", NotifyPane: "w1:p1"})
	board, err := decide.OpenStatus(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := board.SetAgent("builder", "working", "compiling"); err != nil {
		t.Fatal(err)
	}
	c := connect(t, h)

	items, err := c.List()
	if err != nil || len(items) != 2 {
		t.Fatalf("list = %d items, %v", len(items), err)
	}
	if got, err := c.Get(relay.ID); err != nil || got.Question != "ship?" {
		t.Fatalf("show = %+v, %v", got, err)
	}
	if got, err := c.MarkRead(relay.ID); err != nil || got.Unread {
		t.Fatalf("mark read = %+v, %v", got, err)
	}
	if got, err := c.Reply(relay.ID, "why?", ""); err != nil || got.Messages[len(got.Messages)-1].Text != "why?" || got.Messages[len(got.Messages)-1].Via != decide.ViaUI || len(h.sends) != 1 {
		t.Fatalf("reply = %+v, %v, sends %q", got, err, h.sends)
	}
	if got, err := c.Answer(relay.ID, "y"); err != nil || got.Answer == nil || got.Answer.Via != decide.ViaUI || got.Messages[len(got.Messages)-1].Via != decide.ViaUI {
		t.Fatalf("answer = %+v, %v", got.Answer, err)
	}
	led, err := decide.Open(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Reply(relay.ID, "ops/bot", "agent note", nil, false); err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		got, gerr := c.Get(relay.ID)
		return gerr == nil && len(got.Messages) > 0 && got.Messages[len(got.Messages)-1].By == "ops/bot"
	}, "replica did not receive pushed reply")
	if got, err := c.Done(relay.ID, "shipped"); err != nil || got.State != decide.StateDone || got.Outcome != "shipped" {
		t.Fatalf("done = %+v, %v", got, err)
	}
	until := time.Now().Add(time.Hour).Truncate(time.Second)
	if got, err := c.Snooze(other.ID, until); err != nil || got.SnoozedUntil == nil || !got.SnoozedUntil.Equal(until) {
		t.Fatalf("snooze = %+v, %v", got.SnoozedUntil, err)
	}
	if got, err := c.Unsnooze(other.ID); err != nil || got.SnoozedUntil != nil {
		t.Fatalf("unsnooze = %+v, %v", got.SnoozedUntil, err)
	}
	archived := h.create(t, &decide.Item{Question: "fyi"})
	if got, err := c.Archive(archived.ID, time.Now()); err != nil || got.Outcome != "archived" {
		t.Fatalf("archive = %+v, %v", got, err)
	}
	if got, err := c.Withdraw(other.ID, "moot"); err != nil || got.State != decide.StateWithdrawn {
		t.Fatalf("withdraw = %+v, %v", got, err)
	}
	st, err := c.Status()
	if err != nil || len(st.Agents) != 1 || st.Agents[0].Name != "builder" {
		t.Fatalf("status = %+v, %v", st, err)
	}
	targets, agentbus, err := c.Targets()
	if err != nil || !agentbus || !slices.Equal(targets, []tui.Target{{Name: "builder", Pane: "w2:p3"}}) {
		t.Fatalf("targets = %+v %v %v", targets, agentbus, err)
	}
	out, err := c.BusRunner(context.Background(), []string{"list", "--json"})
	if err != nil || !strings.Contains(string(out), "coord/primary") {
		t.Fatalf("bus list = %s, %v", out, err)
	}
	if err := c.Send("", tui.Target{Name: "builder", Pane: "ignored"}, "build it"); err != nil {
		t.Fatal(err)
	}
	if items, err := led.List(); err != nil {
		t.Fatal(err)
	} else {
		var found bool
		for _, it := range items {
			if it.Question == "build it" {
				found = true
				if it.Class != decide.ClassRequest || !strings.HasPrefix(it.ID, "R-") {
					t.Fatalf("compose item = %+v, want request with R- prefix", it)
				}
			}
		}
		if !found {
			t.Fatal("composed item 'build it' not found")
		}
	}
	if last := h.sends[len(h.sends)-1]; last[len(last)-2] != "w2:p3" {
		t.Fatalf("send argv = %q", last)
	}
	if err := c.Send(relay.ID, tui.Target{Name: "coord/primary", Bus: true}, "look"); err != nil {
		t.Fatal(err)
	}
	if last := h.bus[len(h.bus)-1]; last[0] != "send" || last[1] != "coord/primary" {
		t.Fatalf("bus send = %q", last)
	}
	if err := c.Send("", tui.Target{Name: "nobody"}, "x"); err == nil || !strings.Contains(err.Error(), "unknown on the hub") {
		t.Fatalf("unknown recipient = %v", err)
	}
	loc := c.Locate(context.Background(), &relay)
	if loc.State != decide.LocationLive || loc.Machine != "hub" || loc.Tab != "review" {
		t.Fatalf("locate = %+v", loc)
	}
	if !slices.Contains(h.herdr, ":pane") {
		t.Fatalf("locate did not ask the hub's herdr: %q", h.herdr)
	}
	if _, err := c.Get("D-404"); err == nil {
		t.Fatal("show of a missing item succeeded")
	}
	if h.dials != 1 {
		t.Fatalf("dials = %d, want one session for every op", h.dials)
	}
}

func TestReplyRouteReachesTheWorkerThroughTheHub(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{Question: "q", Owner: "ada", NotifyPane: "w1:p1"})
	led, err := decide.Open(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Assign(it.ID, decide.Assign{Worker: "bea", Pane: "w2:p3"}); err != nil {
		t.Fatal(err)
	}
	c := connect(t, h)
	if _, err := c.Reply(it.ID, "hi", decide.RouteWorker); err != nil {
		t.Fatal(err)
	}
	if len(h.sends) != 1 || h.sends[0][len(h.sends[0])-2] != "w2:p3" {
		t.Fatalf("sends = %q", h.sends)
	}
}

func TestNotifyFailureKeepsTheWrite(t *testing.T) {
	h := newHub(t)
	h.server.Backend.RunSend = func([]string) error { return errors.New("pane gone") }
	it := h.create(t, &decide.Item{Question: "q", NotifyPane: "w1:p1"})
	c := connect(t, h)
	got, err := c.Reply(it.ID, "hi", "")
	var notify *tui.NotifyError
	if !errors.As(err, &notify) || !strings.Contains(err.Error(), "pane gone") || len(got.Messages) == 0 {
		t.Fatalf("reply = %+v, %v", got, err)
	}
}

func TestDisconnectFailsWritesAndListReconnectsAfterBackoff(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{Question: "q", Options: []decide.Option{{Key: "y", Label: "Yes"}}})
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var mu sync.Mutex
	c := &Client{Dial: h.dial, Label: "me@hub", CacheDir: t.TempDir(), Now: func() time.Time {
		mu.Lock()
		defer mu.Unlock()
		return now
	}, Timeout: 2 * time.Second}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	h.conns[0].Close() // the ssh session drops
	eventually(t, func() bool { return !c.SyncStatus().Connected }, "client did not notice drop")
	items, err := c.List()
	if err != nil || len(items) != 1 {
		t.Fatalf("list from replica after drop = %d items, %v", len(items), err)
	}
	_, err = c.Answer(it.ID, "y")
	if !errors.Is(err, tui.ErrUnreachable) || err.Error() != "hub me@hub unreachable; answer not sent" {
		t.Fatalf("answer while down = %v", err)
	}
	if items, err := c.List(); err != nil || len(items) != 1 || h.dials != 1 {
		t.Fatalf("list within backoff = %d items, %v, dials %d", len(items), err, h.dials)
	}
	mu.Lock()
	now = now.Add(firstBackoff)
	mu.Unlock()
	if _, err := c.List(); err != nil {
		t.Fatalf("list after backoff = %v", err)
	}
	eventually(t, func() bool { return c.SyncStatus().Connected && h.dials == 2 }, "client did not reconnect after backoff")
	if got, err := c.Answer(it.ID, "y"); err != nil || got.State != decide.StateAnswered {
		t.Fatalf("answer after reconnect = %+v, %v", got, err)
	}
	led, _ := decide.Open(h.dir)
	if got, _ := led.Get(it.ID); got.State != decide.StateAnswered {
		t.Fatalf("hub ledger state = %s", got.State)
	}
}

func TestBackoffGrowsWhileTheHubStaysDown(t *testing.T) {
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var nowMu sync.Mutex
	var dialMu sync.Mutex
	dials := 0
	c := &Client{Label: "me@hub", CacheDir: t.TempDir(), Now: func() time.Time {
		nowMu.Lock()
		defer nowMu.Unlock()
		return now
	}, Dial: func() (*Conn, error) {
		dialMu.Lock()
		dials++
		dialMu.Unlock()
		return nil, errors.New("host unreachable")
	}}
	for i, wait := range []time.Duration{0, time.Second, 2 * time.Second, 4 * time.Second} {
		nowMu.Lock()
		now = now.Add(wait)
		nowMu.Unlock()
		if _, err := c.List(); err != nil {
			t.Fatalf("list = %v", err)
		}
		want := i + 1
		eventually(t, func() bool {
			dialMu.Lock()
			defer dialMu.Unlock()
			return dials == want
		}, fmt.Sprintf("dials did not reach %d", want))
		// Wait for the failed dial to record its next retryAt before advancing now.
		eventually(t, func() bool {
			c.mu.Lock()
			defer c.mu.Unlock()
			return !c.dialing
		}, "dial still in progress")
	}
	nowMu.Lock()
	now = now.Add(7 * time.Second)
	nowMu.Unlock()
	_, _ = c.List()
	dialMu.Lock()
	gotDials := dials
	dialMu.Unlock()
	if gotDials != 4 {
		t.Fatalf("dialed before the 8s backoff: %d", gotDials)
	}
}

func TestSlowBusListKeepsTheSession(t *testing.T) {
	h := newHub(t)
	release := make(chan struct{})
	h.server.Backend.BusRun = func(context.Context, []string) ([]byte, error) {
		<-release
		return []byte("[]"), nil
	}
	c := connect(t, h)
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()
	if _, err := c.BusRunner(ctx, []string{"list", "--json"}); !errors.Is(err, context.DeadlineExceeded) || errors.Is(err, tui.ErrUnreachable) {
		t.Fatalf("slow bus list = %v", err)
	}
	close(release)
	if _, err := c.List(); err != nil || h.dials != 1 {
		t.Fatalf("list after a slow bus list = %v, dials %d", err, h.dials)
	}
	if _, err := c.BusRunner(context.Background(), []string{"send", "x", "y"}); err == nil {
		t.Fatal("bus send ran through the bus runner")
	}
}

func TestBusHealthRunsOnTheHub(t *testing.T) {
	h := newHub(t)
	down := `{"daemon":"down","transport":"unknown","problems":["daemon unreachable"]}`
	h.server.Backend.BusRun = func(_ context.Context, args []string) ([]byte, error) {
		h.mu.Lock()
		defer h.mu.Unlock()
		h.bus = append(h.bus, args)
		return []byte(down + "\n"), errors.New("agentbus health: exit status 1")
	}
	c := connect(t, h)
	out, err := c.BusRunner(context.Background(), []string{"health", "--json"})
	if err != nil || strings.TrimSpace(string(out)) != down {
		t.Fatalf("health = %q, %v", out, err)
	}
	if len(h.bus) != 1 || strings.Join(h.bus[0], " ") != "health --json" {
		t.Fatalf("hub agentbus calls = %q", h.bus)
	}
	h.server.Backend.BusRun = func(context.Context, []string) ([]byte, error) {
		return nil, errors.New("agentbus: not found")
	}
	if _, err := c.BusRunner(context.Background(), []string{"health", "--json"}); err == nil || !strings.Contains(err.Error(), "not found") {
		t.Fatalf("missing agentbus on the hub = %v", err)
	}
}

func TestDeliveryCheckRunsOnTheHub(t *testing.T) {
	h := newHub(t)
	c := connect(t, h)
	if err := c.Send("", tui.Target{Name: "coord/primary", Bus: true}, "hello"); err != nil {
		t.Fatal(err)
	}
	led, err := decide.Open(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil || len(items) != 1 || !decide.HasPending(&items[0]) {
		t.Fatalf("items = %+v, %v", items, err)
	}
	h.server.Backend.BusRun = func(_ context.Context, args []string) ([]byte, error) {
		h.mu.Lock()
		defer h.mu.Unlock()
		h.bus = append(h.bus, args)
		return []byte(`{"id":"m-1","state":"delivered","delivered_at":"2026-11-14T12:05:00.000Z"}` + "\n"), nil
	}
	if err := c.CheckDeliveries(context.Background()); err != nil {
		t.Fatal(err)
	}
	if last := h.bus[len(h.bus)-1]; strings.Join(last, " ") != "status --json m-1" {
		t.Fatalf("hub agentbus calls = %q", h.bus)
	}
	got, err := led.Get(items[0].ID)
	if err != nil {
		t.Fatal(err)
	}
	if mark, ok := decide.MessageMark(&got, got.Messages[0]); !ok || mark.State != decide.OutcomeDelivered {
		t.Fatalf("mark = %+v, %v", mark, ok)
	}
}

func TestOwnerLocationIsSeenFromThisMachine(t *testing.T) {
	c := &Client{Label: "me@hub", Machine: "mac", HubMachine: "hub"}
	cases := []struct {
		stored, want string
	}{
		{"", "hub"},        // asked on the hub itself
		{"mac", ""},        // asked from this machine
		{"linux", "linux"}, // a third machine
	}
	for _, tc := range cases {
		got := c.OwnerPane(&decide.Item{NotifyPane: "w1:p1", NotifyMachine: tc.stored})
		if got.Machine != tc.want {
			t.Fatalf("stored %q: machine %q, want %q", tc.stored, got.Machine, tc.want)
		}
	}
}

func TestFocusRunsHerdrHereWithTheHubMachine(t *testing.T) {
	var calls []string
	herdr := func(_ context.Context, machine string, args []string) (string, error) {
		calls = append(calls, machine+" "+strings.Join(args, " "))
		return "", nil
	}
	live := decide.OwnerLocation{Machine: "hub", Pane: "w1:p1", State: decide.LocationLive, WorkspaceID: "w1", TabID: "w1:t1", Workspace: "exp", Tab: "review"}
	c := &Client{Label: "me@hub", HubMachine: "hub", Herdr: herdr}
	if msg := c.Focus(context.Background(), live); msg != "focused hub: exp / review" {
		t.Fatalf("focus = %q", msg)
	}
	if !slices.Equal(calls, []string{"hub workspace focus w1", "hub tab focus w1:t1"}) {
		t.Fatalf("herdr calls = %q", calls)
	}

	calls = nil
	c = &Client{Label: "me@hub", Herdr: herdr}
	loc := c.OwnerPane(&decide.Item{NotifyPane: "w1:p1"})
	loc.State, loc.WorkspaceID, loc.TabID = decide.LocationLive, "w1", "w1:t1"
	if msg := c.Focus(context.Background(), loc); !strings.Contains(msg, `set "hubMachine"`) || len(calls) != 0 {
		t.Fatalf("focus without hubMachine = %q, calls %q", msg, calls)
	}

	c = &Client{Label: "me@hub", HubMachine: "hub", Herdr: func(context.Context, string, []string) (string, error) {
		return "", errors.New("unknown machine")
	}}
	if msg := c.Focus(context.Background(), live); !strings.Contains(msg, "saved in herdr") {
		t.Fatalf("focus on an unsaved machine = %q", msg)
	}
}

func helloClient(body string) *Client {
	return &Client{Label: "me@hub", Dial: func() (*Conn, error) {
		r, w := io.Pipe()
		go func() {
			fmt.Fprintln(w, body)
		}()
		return &Conn{W: io.Discard, R: r, Close: func() { _ = w.Close() }}, nil
	}}
}

func TestOldServerFailsTheHandshake(t *testing.T) {
	err := helloClient(`{"id":0,"error":"unknown op \"hello\""}`).Connect()
	want := `hub me@hub: unknown op "hello"; update decide on the hub`
	if err == nil || err.Error() != want {
		t.Fatalf("connect = %v, want %s", err, want)
	}
}

func TestNewerHubTellsYouToUpdateThisMachine(t *testing.T) {
	err := helloClient(fmt.Sprintf(`{"id":0,"version":%d}`, Version+1)).Connect()
	want := fmt.Sprintf("hub me@hub: decide serve speaks protocol %d, this decide speaks %d; update decide on this machine", Version+1, Version)
	if err == nil || err.Error() != want || strings.Contains(err.Error(), "on the hub") {
		t.Fatalf("connect = %v, want %s", err, want)
	}
}

func TestOlderHubTellsYouToUpdateTheHub(t *testing.T) {
	err := helloClient(fmt.Sprintf(`{"id":0,"version":%d}`, Version-1)).Connect()
	want := fmt.Sprintf("hub me@hub: decide serve speaks protocol %d, this decide speaks %d; update decide on the hub", Version-1, Version)
	if err == nil || err.Error() != want {
		t.Fatalf("connect = %v, want %s", err, want)
	}
}

func TestSetAvatarWritesTheHubAndASecondClientSeesIt(t *testing.T) {
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", home)
	path := filepath.Join(home, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	h := newHub(t)
	h.server.Display = func() (decide.DisplaySettings, error) {
		f, err := decide.LoadConfigFile()
		if err != nil {
			return decide.DisplaySettings{}, fmt.Errorf("load config: %w", err)
		}
		return f.Display(), nil
	}
	first := connect(t, h)
	second := connect(t, h)
	if _, err := first.SetAvatar("ops/x", "nope"); err == nil || !strings.Contains(err.Error(), `unknown built-in avatar "nope"`) {
		t.Fatalf("invalid choice: %v", err)
	}
	if data, err := os.ReadFile(path); err != nil || strings.Contains(string(data), "ops/x") {
		t.Fatalf("rejected choice wrote %s, %v", data, err)
	}
	settings, err := first.SetAvatar("ops/x", "owl")
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(settings.Keys["avatars"]), "owl") {
		t.Fatalf("display = %s", settings.Keys["avatars"])
	}
	data, err := os.ReadFile(path)
	if err != nil || !strings.Contains(string(data), `"builtin": "owl"`) || !strings.Contains(string(data), `"match": "ops/x"`) {
		t.Fatalf("hub config = %s, %v", data, err)
	}
	if _, err := second.List(); err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		got, gen := second.Display()
		return gen >= 2 && strings.Contains(string(got.Keys["avatars"]), "owl")
	}, "second client did not refresh avatar display")
}

func TestSetHiddenWritesTheHubAndASecondClientSeesIt(t *testing.T) {
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", home)
	path := filepath.Join(home, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	h := newHub(t)
	h.server.Display = func() (decide.DisplaySettings, error) {
		f, err := decide.LoadConfigFile()
		if err != nil {
			return decide.DisplaySettings{}, fmt.Errorf("load config: %w", err)
		}
		return f.Display(), nil
	}
	first := connect(t, h)
	second := connect(t, h)
	settings, err := first.SetHidden([]string{"cara", "zed"})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(settings.Keys["hiddenContacts"]), "cara") {
		t.Fatalf("display = %s", settings.Keys["hiddenContacts"])
	}
	data, err := os.ReadFile(path)
	if err != nil || !strings.Contains(string(data), "cara") || !strings.Contains(string(data), "zed") {
		t.Fatalf("hub config = %s, %v", data, err)
	}
	if _, err := second.List(); err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		got, gen := second.Display()
		return gen >= 2 && strings.Contains(string(got.Keys["hiddenContacts"]), "zed")
	}, "second client did not refresh hiddenContacts display")
	if _, err := first.SetHidden(nil); err != nil {
		t.Fatal(err)
	}
	data, err = os.ReadFile(path)
	if err != nil || strings.Contains(string(data), "cara") {
		t.Fatalf("cleared config = %s, %v", data, err)
	}
}

func TestSetFavoritesWritesTheHubAndASecondClientSeesIt(t *testing.T) {
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", home)
	path := filepath.Join(home, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	h := newHub(t)
	h.server.Display = func() (decide.DisplaySettings, error) {
		f, err := decide.LoadConfigFile()
		if err != nil {
			return decide.DisplaySettings{}, fmt.Errorf("load config: %w", err)
		}
		return f.Display(), nil
	}
	first := connect(t, h)
	second := connect(t, h)
	settings, err := first.SetFavorites([]string{"ops/inbox", "builder"})
	if err != nil {
		t.Fatal(err)
	}
	if got := string(settings.Keys["favoriteContacts"]); !strings.Contains(got, "ops/inbox") {
		t.Fatalf("display = %s", got)
	}
	data, err := os.ReadFile(path)
	if err != nil || !strings.Contains(string(data), "builder") {
		t.Fatalf("hub config = %s, %v", data, err)
	}
	if _, err := second.List(); err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		got, gen := second.Display()
		return gen >= 2 && strings.Contains(string(got.Keys["favoriteContacts"]), "builder")
	}, "second client did not refresh favoriteContacts display")
	if _, err := first.SetFavorites(nil); err != nil {
		t.Fatal(err)
	}
	data, err = os.ReadFile(path)
	if err != nil || strings.Contains(string(data), "builder") {
		t.Fatalf("cleared config = %s, %v", data, err)
	}
}

func TestSetAliasesWritesTheHubAndASecondClientSeesIt(t *testing.T) {
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", home)
	path := filepath.Join(home, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	h := newHub(t)
	h.server.Display = func() (decide.DisplaySettings, error) {
		f, err := decide.LoadConfigFile()
		if err != nil {
			return decide.DisplaySettings{}, fmt.Errorf("load config: %w", err)
		}
		return f.Display(), nil
	}
	first := connect(t, h)
	settings, err := first.SetAliases(map[string]string{"primary": "coord/primary"})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(settings.Keys["contactAliases"]), "primary") {
		t.Fatalf("display = %s", settings.Keys["contactAliases"])
	}
	data, err := os.ReadFile(path)
	if err != nil || !strings.Contains(string(data), "coord/primary") {
		t.Fatalf("hub config = %s, %v", data, err)
	}
}

func TestSetAvatarOnAnOldHubSaysToUpdateIt(t *testing.T) {
	c := &Client{Label: "me@hub", Dial: scriptedHub(func(req Request) Response {
		switch req.Op {
		case OpHello:
			return Response{Version: Version}
		case OpConfig:
			return Response{Display: &decide.DisplaySettings{}}
		default:
			return Response{Error: fmt.Sprintf("unknown op %q", req.Op)}
		}
	})}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(c.Close)
	_, err := c.SetAvatar("ops/x", "owl")
	want := `hub me@hub: unknown op "set-avatar"; update decide on the hub`
	if err == nil || err.Error() != want {
		t.Fatalf("set-avatar = %v", err)
	}
}

// scriptedHub answers each request with respond.
func scriptedHub(respond func(Request) Response) Dialer {
	return func() (*Conn, error) {
		reqR, reqW := io.Pipe()
		respR, respW := io.Pipe()
		go func() {
			sc := bufio.NewScanner(reqR)
			enc := json.NewEncoder(respW)
			for sc.Scan() {
				var req Request
				if err := json.Unmarshal(sc.Bytes(), &req); err != nil {
					_ = respW.CloseWithError(err)
					return
				}
				resp := respond(req)
				resp.ID = req.ID
				if err := enc.Encode(resp); err != nil {
					return
				}
			}
		}()
		return &Conn{W: reqW, R: respR, Close: func() {
			_ = reqW.Close()
			_ = respR.Close()
		}}, nil
	}
}

func TestConfigRequestReturnsTheHubDisplaySettings(t *testing.T) {
	h := newHub(t)
	h.server.Display = func() (decide.DisplaySettings, error) {
		return decide.DisplaySettings{
			Keys:  map[string]json.RawMessage{"avatarDefault": json.RawMessage(`true`)},
			Files: map[string]decide.AvatarFile{"hub:0.txt": {Data: []byte("AB")}},
		}, nil
	}
	c := connect(t, h)
	got, gen := c.Display()
	if gen != 1 || string(got.Keys["avatarDefault"]) != "true" || string(got.Files["hub:0.txt"].Data) != "AB" {
		t.Fatalf("display = %+v, generation %d", got, gen)
	}
	resp, err := c.call(Request{Op: OpConfig})
	if err != nil || resp.Display == nil || string(resp.Display.Files["hub:0.txt"].Data) != "AB" {
		t.Fatalf("config = %+v, %v", resp, err)
	}
}

func TestReconnectFetchesTheDisplaySettingsAgain(t *testing.T) {
	h := newHub(t)
	var mu sync.Mutex
	value := "false"
	h.server.Display = func() (decide.DisplaySettings, error) {
		mu.Lock()
		defer mu.Unlock()
		return decide.DisplaySettings{Keys: map[string]json.RawMessage{"avatarDefault": json.RawMessage(value)}}, nil
	}
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var nowMu sync.Mutex
	c := &Client{Dial: h.dial, Label: "me@hub", CacheDir: t.TempDir(), Now: func() time.Time {
		nowMu.Lock()
		defer nowMu.Unlock()
		return now
	}, Timeout: 2 * time.Second}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	h.conns[0].Close()
	eventually(t, func() bool { return !c.SyncStatus().Connected }, "client did not notice drop")
	_, _ = c.List()
	mu.Lock()
	value = "true"
	mu.Unlock()
	nowMu.Lock()
	now = now.Add(firstBackoff)
	nowMu.Unlock()
	if _, err := c.List(); err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		got, gen := c.Display()
		return gen == 2 && string(got.Keys["avatarDefault"]) == "true"
	}, "display after reconnect did not update")
}

func TestArchiveRPCRefusesStaleSeen(t *testing.T) {
	h := newHub(t)
	item := h.create(t, &decide.Item{Question: "fyi"})
	c := connect(t, h)
	shown, err := c.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	seen := shown.Messages[len(shown.Messages)-1].Time
	if _, err := c.Archive(item.ID, seen.Add(-time.Second)); !errors.Is(err, decide.ErrNewerMessage) {
		t.Fatalf("stale seen = %v", err)
	}
	still, err := c.Get(item.ID)
	if err != nil || still.State != decide.StateOpen {
		t.Fatalf("state %s err %v", still.State, err)
	}
	if got, err := c.Archive(item.ID, seen); err != nil || got.State != decide.StateDone || got.Outcome != "archived" {
		t.Fatalf("current seen = %+v %v", got, err)
	}
}

func TestReplicaReadsAndMarkReadDoNotWaitOnBlockedHub(t *testing.T) {
	item := decide.Item{ID: "D-001", Question: "ship?", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, Unread: true}
	block := make(chan struct{})
	defer close(block)
	var dialMu sync.Mutex
	var encMu sync.Mutex
	var watchID int64
	c := &Client{
		Label:    "me@hub",
		CacheDir: t.TempDir(),
		Timeout:  5 * time.Second,
		Dial: func() (*Conn, error) {
			reqR, reqW := io.Pipe()
			respR, respW := io.Pipe()
			enc := json.NewEncoder(respW)
			send := func(r Response) {
				encMu.Lock()
				defer encMu.Unlock()
				_ = enc.Encode(r)
			}
			go func() {
				sc := bufio.NewScanner(reqR)
				for sc.Scan() {
					var req Request
					if err := json.Unmarshal(sc.Bytes(), &req); err != nil {
						continue
					}
					switch req.Op {
					case OpHello:
						send(Response{ID: req.ID, Version: Version})
					case OpConfig:
						send(Response{ID: req.ID, Display: &decide.DisplaySettings{}})
					case OpWatch:
						dialMu.Lock()
						watchID = req.ID
						dialMu.Unlock()
						send(Response{
							ID:     req.ID,
							Seq:    1,
							Resync: true,
							Items:  []decide.Item{item},
							Status: &tui.StatusBoard{Agents: []decide.AgentStatus{{Name: "ada", State: decide.StatusWorking}}},
						})
					default:
						// Block all other RPCs (like mark-read or show) to simulate a stalled link.
						<-block
					}
				}
			}()
			return &Conn{W: reqW, R: respR, Close: func() {
				_ = reqW.Close()
				_ = respR.Close()
				_ = respW.Close()
			}}, nil
		},
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	dialMu.Lock()
	gotWatchID := watchID
	dialMu.Unlock()
	if gotWatchID == 0 {
		t.Fatal("watch was not started")
	}

	done := make(chan struct{})
	go func() {
		defer close(done)
		items, err := c.List()
		if err != nil || len(items) != 1 || items[0].ID != "D-001" {
			t.Errorf("List = %+v, %v", items, err)
		}
		got, err := c.Get("D-001")
		if err != nil || got.Question != "ship?" {
			t.Errorf("Get = %+v, %v", got, err)
		}
		st, err := c.Status()
		if err != nil || len(st.Agents) != 1 || st.Agents[0].Name != "ada" {
			t.Errorf("Status = %+v, %v", st, err)
		}
		mr, err := c.MarkRead("D-001")
		if err != nil || mr.Unread {
			t.Errorf("MarkRead = %+v, %v", mr, err)
		}
	}()
	select {
	case <-done:
	case <-time.After(500 * time.Millisecond):
		t.Fatal("reads or mark-read blocked on stalled hub")
	}
}

func TestReplicaPushedChangesApplyInSequenceOrder(t *testing.T) {
	h := newHub(t)
	first := h.create(t, &decide.Item{Question: "one"})
	c := connect(t, h)

	second := h.create(t, &decide.Item{Question: "two"})
	led, err := decide.Open(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Reply(first.ID, "ops/bot", "follow up", nil, false); err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		items, lerr := c.List()
		if lerr != nil || len(items) != 2 {
			return false
		}
		got1, err1 := c.Get(first.ID)
		got2, err2 := c.Get(second.ID)
		return err1 == nil && err2 == nil && len(got1.Messages) == 2 && got2.Question == "two"
	}, "pushed changes did not apply in sequence order")
}

func TestReplicaSequenceGapTriggersFullResync(t *testing.T) {
	item1 := decide.Item{ID: "D-001", Question: "one", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen}
	item2 := decide.Item{ID: "D-002", Question: "two", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen}
	item3 := decide.Item{ID: "D-003", Question: "three", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen}

	var mu sync.Mutex
	var watchSinces []int64
	var sendFn func(Response)
	var watchID int64

	c := &Client{
		Label:    "me@hub",
		CacheDir: t.TempDir(),
		Timeout:  2 * time.Second,
		Dial: func() (*Conn, error) {
			reqR, reqW := io.Pipe()
			respR, respW := io.Pipe()
			enc := json.NewEncoder(respW)
			var encMu sync.Mutex
			send := func(r Response) {
				encMu.Lock()
				defer encMu.Unlock()
				_ = enc.Encode(r)
			}
			mu.Lock()
			sendFn = send
			mu.Unlock()
			go func() {
				sc := bufio.NewScanner(reqR)
				for sc.Scan() {
					var req Request
					if err := json.Unmarshal(sc.Bytes(), &req); err != nil {
						continue
					}
					switch req.Op {
					case OpHello:
						send(Response{ID: req.ID, Version: Version})
					case OpConfig:
						send(Response{ID: req.ID, Display: &decide.DisplaySettings{}})
					case OpWatch:
						mu.Lock()
						watchSinces = append(watchSinces, req.Since)
						watchID = req.ID
						n := len(watchSinces)
						mu.Unlock()
						if n == 1 {
							send(Response{ID: req.ID, Seq: 1, Resync: true, Items: []decide.Item{item1}})
						} else {
							send(Response{ID: req.ID, Seq: 3, Resync: true, Items: []decide.Item{item1, item2, item3}})
						}
					}
				}
			}()
			return &Conn{W: reqW, R: respR, Close: func() {
				_ = reqW.Close()
				_ = respR.Close()
				_ = respW.Close()
			}}, nil
		},
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	// Push a change with seq=3 when client is at seq=1 (gap: missing seq 2).
	mu.Lock()
	wid := watchID
	send := sendFn
	mu.Unlock()
	send(Response{
		ID:      wid,
		Seq:     3,
		Changes: []decide.LedgerChange{{Seq: 3, ID: item3.ID, Item: &item3}},
	})

	eventually(t, func() bool {
		items, err := c.List()
		return err == nil && len(items) == 3
	}, "sequence gap did not trigger full resync")

	mu.Lock()
	sinces := append([]int64(nil), watchSinces...)
	mu.Unlock()
	if !slices.Equal(sinces, []int64{0, 0}) {
		t.Fatalf("watch sinces = %v, want [0 0]", sinces)
	}
}

func TestReplicaSurvivesClientRestartAndResumesFromSeq(t *testing.T) {
	h := newHub(t)
	first := h.create(t, &decide.Item{Question: "one"})
	cacheDir := t.TempDir()

	c1 := &Client{Dial: h.dial, Label: "me@hub", CacheDir: cacheDir, Timeout: 2 * time.Second}
	if err := c1.Connect(); err != nil {
		t.Fatal(err)
	}
	items, err := c1.List()
	if err != nil || len(items) != 1 || items[0].ID != first.ID {
		t.Fatalf("c1 list = %+v, %v", items, err)
	}
	c1.Close()

	// Hub creates a second item while client is stopped.
	second := h.create(t, &decide.Item{Question: "two"})

	// Wrap h.dial to record the watch Since sent on restart.
	var sinces []int64
	var mu sync.Mutex
	recordingDial := func() (*Conn, error) {
		conn, err := h.dial()
		if err != nil {
			return nil, err
		}
		origW := conn.W
		pr, pw := io.Pipe()
		go func() {
			sc := bufio.NewScanner(pr)
			for sc.Scan() {
				line := sc.Bytes()
				var req Request
				if json.Unmarshal(line, &req) == nil && req.Op == OpWatch {
					mu.Lock()
					sinces = append(sinces, req.Since)
					mu.Unlock()
				}
				if _, werr := origW.Write(append(line, '\n')); werr != nil {
					return
				}
			}
		}()
		origClose := conn.Close
		conn.W = pw
		conn.Close = func() {
			_ = pw.Close()
			origClose()
		}
		return conn, nil
	}

	c2 := &Client{Dial: recordingDial, Label: "me@hub", CacheDir: cacheDir, Timeout: 2 * time.Second}
	if err := c2.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c2.Close()

	mu.Lock()
	gotSinces := append([]int64(nil), sinces...)
	mu.Unlock()
	if len(gotSinces) != 1 || gotSinces[0] != 1 {
		t.Fatalf("watch since on restart = %v, want [1]", gotSinces)
	}
	items2, err := c2.List()
	if err != nil || len(items2) != 2 {
		t.Fatalf("c2 list = %+v, %v", items2, err)
	}
	if got, err := c2.Get(second.ID); err != nil || got.Question != "two" {
		t.Fatalf("c2 get second = %+v, %v", got, err)
	}
}

func TestMarkReadRetriesInBackgroundAndDoesNotError(t *testing.T) {
	h := newHub(t)
	item := h.create(t, &decide.Item{Question: "unread", Unread: true})
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	var nowMu sync.Mutex
	c := &Client{Dial: h.dial, Label: "me@hub", CacheDir: t.TempDir(), Now: func() time.Time {
		nowMu.Lock()
		defer nowMu.Unlock()
		return now
	}, Timeout: 2 * time.Second}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	// Drop the hub connection before MarkRead.
	h.conns[0].Close()
	eventually(t, func() bool { return !c.SyncStatus().Connected }, "client did not notice drop")

	got, err := c.MarkRead(item.ID)
	if err != nil || got.Unread {
		t.Fatalf("MarkRead while down = %+v, %v", got, err)
	}

	// Reconnect after backoff and verify background retry marks it read on the hub.
	nowMu.Lock()
	now = now.Add(firstBackoff)
	nowMu.Unlock()
	_, _ = c.List()

	led, err := decide.Open(h.dir)
	if err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		hubItem, gerr := led.Get(item.ID)
		return gerr == nil && !hubItem.Unread
	}, "background mark-read retry did not update hub ledger")
}

func TestDraftSyncAcrossClientsViaWatch(t *testing.T) {
	h := newHub(t)
	cA := &Client{Dial: h.dial, Label: "me@hub", Machine: "alpha", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	cB := &Client{Dial: h.dial, Label: "me@hub", Machine: "beta", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	if err := cA.Connect(); err != nil {
		t.Fatal(err)
	}
	defer cA.Close()
	if err := cB.Connect(); err != nil {
		t.Fatal(err)
	}
	defer cB.Close()

	key := decide.ReplyDraftKey("D-001")
	saved, conflict, err := cA.PutDraft(decide.Draft{
		Key:  key,
		Text: "hello from A",
	}, 0)
	if err != nil {
		t.Fatalf("PutDraft() error = %v", err)
	}
	if conflict || saved.Rev != 1 || saved.UpdatedBy != "alpha" {
		t.Fatalf("PutDraft() = %+v, conflict=%v", saved, conflict)
	}

	eventually(t, func() bool {
		drafts, err := cB.ListDrafts()
		return err == nil && len(drafts) == 1 && drafts[0].Key == key && drafts[0].Text == "hello from A" && drafts[0].Rev == 1
	}, "client B did not receive draft via watch")
}

func TestDraftConcurrentPutCreatesConflictVisibleOnBothClients(t *testing.T) {
	h := newHub(t)
	cA := &Client{Dial: h.dial, Label: "me@hub", Machine: "alpha", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	cB := &Client{Dial: h.dial, Label: "me@hub", Machine: "beta", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	if err := cA.Connect(); err != nil {
		t.Fatal(err)
	}
	defer cA.Close()
	if err := cB.Connect(); err != nil {
		t.Fatal(err)
	}
	defer cB.Close()

	key := decide.ReplyDraftKey("D-002")
	initial, _, err := cA.PutDraft(decide.Draft{Key: key, Text: "v1"}, 0)
	if err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		drafts, err := cB.ListDrafts()
		return err == nil && len(drafts) == 1 && drafts[0].Rev == initial.Rev
	}, "client B did not receive initial draft")

	// Both clients edit against baseRev = initial.Rev (1).
	savedA, conflictA, err := cA.PutDraft(decide.Draft{Key: key, Text: "edit from A"}, initial.Rev)
	if err != nil || conflictA || savedA.Rev != 2 {
		t.Fatalf("client A PutDraft() = %+v, conflict=%v, err=%v", savedA, conflictA, err)
	}
	savedB, conflictB, err := cB.PutDraft(decide.Draft{Key: key, Text: "edit from B"}, initial.Rev)
	if err != nil || !conflictB {
		t.Fatalf("client B PutDraft() = %+v, conflict=%v, err=%v (want conflict)", savedB, conflictB, err)
	}
	if savedB.Text != "edit from A" || len(savedB.Conflicts) != 1 || savedB.Conflicts[0].Text != "edit from B" {
		t.Fatalf("unexpected conflict draft on B: %+v", savedB)
	}

	eventually(t, func() bool {
		drafts, err := cA.ListDrafts()
		return err == nil && len(drafts) == 1 && len(drafts[0].Conflicts) == 1 && drafts[0].Conflicts[0].Text == "edit from B"
	}, "client A did not receive conflict copy via watch")
}

func TestListDraftsReadsFromReplicaWhenHubUnreachable(t *testing.T) {
	h := newHub(t)
	cacheDir := t.TempDir()
	c := &Client{Dial: h.dial, Label: "me@hub", Machine: "alpha", CacheDir: cacheDir, Timeout: 2 * time.Second}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}

	key := decide.AnswerDraftKey("D-003")
	if _, _, err := c.PutDraft(decide.Draft{Key: key, Text: "cached draft"}, 0); err != nil {
		t.Fatal(err)
	}
	c.Close()

	// Restart client with a dialer that blocks/fails immediately; ListDrafts must
	// return immediately from the local replica without blocking.
	offline := &Client{
		Dial: func() (*Conn, error) {
			return nil, errors.New("network down")
		},
		Label:    "me@hub",
		Machine:  "alpha",
		CacheDir: cacheDir,
		Timeout:  2 * time.Second,
	}
	defer offline.Close()

	done := make(chan []decide.Draft, 1)
	go func() {
		drafts, err := offline.ListDrafts()
		if err != nil {
			t.Errorf("ListDrafts() error = %v", err)
			return
		}
		done <- drafts
	}()

	select {
	case drafts := <-done:
		if len(drafts) != 1 || drafts[0].Key != key || drafts[0].Text != "cached draft" {
			t.Fatalf("offline ListDrafts() = %+v", drafts)
		}
	case <-time.After(time.Second):
		t.Fatal("ListDrafts() blocked while hub unreachable")
	}
}

func TestOfflineDraftEditSurvivesRestartAndDetectsConflictOnReconnect(t *testing.T) {
	h := newHub(t)
	cacheDirA := t.TempDir()
	cA := &Client{Dial: h.dial, Label: "me@hub", Machine: "alpha", CacheDir: cacheDirA, Timeout: 2 * time.Second}
	cB := &Client{Dial: h.dial, Label: "me@hub", Machine: "beta", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	if err := cA.Connect(); err != nil {
		t.Fatal(err)
	}
	if err := cB.Connect(); err != nil {
		t.Fatal(err)
	}
	defer cB.Close()

	key := decide.ReplyDraftKey("D-004")
	v1, _, err := cA.PutDraft(decide.Draft{Key: key, Text: "shared v1"}, 0)
	if err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		drafts, err := cB.ListDrafts()
		return err == nil && len(drafts) == 1 && drafts[0].Rev == v1.Rev
	}, "client B did not receive v1")

	// Client A goes offline.
	cA.Close()

	offlineA := &Client{
		Dial: func() (*Conn, error) {
			return nil, errors.New("offline")
		},
		Label:    "me@hub",
		Machine:  "alpha",
		CacheDir: cacheDirA,
		Timeout:  2 * time.Second,
	}
	if _, _, err := offlineA.PutDraft(decide.Draft{Key: key, Text: "offline edit A"}, v1.Rev); err != nil {
		t.Fatalf("offline PutDraft() error = %v", err)
	}
	drafts, err := offlineA.ListDrafts()
	if err != nil || len(drafts) != 1 || drafts[0].Text != "offline edit A" {
		t.Fatalf("offline ListDrafts() = %+v, %v", drafts, err)
	}
	offlineA.Close()

	// Meanwhile client B updates the same draft on the hub.
	if _, _, err := cB.PutDraft(decide.Draft{Key: key, Text: "online edit B"}, v1.Rev); err != nil {
		t.Fatalf("client B PutDraft() error = %v", err)
	}

	// Client A restarts and reconnects to the hub; the pending edit from the
	// replica must flush on reconnect and trigger conflict detection.
	restartedA := &Client{Dial: h.dial, Label: "me@hub", Machine: "alpha", CacheDir: cacheDirA, Timeout: 2 * time.Second}
	if err := restartedA.Connect(); err != nil {
		t.Fatal(err)
	}
	defer restartedA.Close()

	eventually(t, func() bool {
		ds, err := restartedA.ListDrafts()
		return err == nil && len(ds) == 1 && ds[0].Text == "online edit B" && len(ds[0].Conflicts) == 1 && ds[0].Conflicts[0].Text == "offline edit A"
	}, "restarted client A did not flush pending draft and detect conflict")

	eventually(t, func() bool {
		ds, err := cB.ListDrafts()
		return err == nil && len(ds) == 1 && len(ds[0].Conflicts) == 1 && ds[0].Conflicts[0].Text == "offline edit A"
	}, "client B did not receive conflict from restarted client A")
}

func TestDeleteDraftRemovesDraftAndConflictCopyAcrossClients(t *testing.T) {
	h := newHub(t)
	cA := &Client{Dial: h.dial, Label: "me@hub", Machine: "alpha", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	cB := &Client{Dial: h.dial, Label: "me@hub", Machine: "beta", CacheDir: t.TempDir(), Timeout: 2 * time.Second}
	if err := cA.Connect(); err != nil {
		t.Fatal(err)
	}
	defer cA.Close()
	if err := cB.Connect(); err != nil {
		t.Fatal(err)
	}
	defer cB.Close()

	key := decide.ReplyDraftKey("D-005")
	v1, _, err := cA.PutDraft(decide.Draft{Key: key, Text: "base"}, 0)
	if err != nil {
		t.Fatal(err)
	}
	_, _, err = cA.PutDraft(decide.Draft{Key: key, Text: "from A"}, v1.Rev)
	if err != nil {
		t.Fatal(err)
	}
	saved, conflict, err := cB.PutDraft(decide.Draft{Key: key, Text: "from B"}, v1.Rev)
	if err != nil || !conflict || len(saved.Conflicts) != 1 {
		t.Fatalf("PutDraft conflict = %+v, conflict=%v, err=%v", saved, conflict, err)
	}
	conflictID := saved.Conflicts[0].ID

	// Delete only the conflict copy from client A.
	if err := cA.DeleteDraft(key, conflictID); err != nil {
		t.Fatalf("DeleteDraft(conflict) error = %v", err)
	}
	eventually(t, func() bool {
		ds, err := cB.ListDrafts()
		return err == nil && len(ds) == 1 && len(ds[0].Conflicts) == 0 && ds[0].Text == "from A"
	}, "client B did not see conflict copy deleted")

	// Delete the entire draft from client B.
	if err := cB.DeleteDraft(key, ""); err != nil {
		t.Fatalf("DeleteDraft(whole) error = %v", err)
	}
	eventually(t, func() bool {
		ds, err := cA.ListDrafts()
		return err == nil && len(ds) == 0
	}, "client A did not see whole draft deleted")
}

func TestOrgTreeFetchesAndCachesOfflineAndRefreshesOnMtimeChange(t *testing.T) {
	specPath := filepath.Join(t.TempDir(), "desk.json")
	if err := os.WriteFile(specPath, []byte(`{}`), 0o600); err != nil {
		t.Fatal(err)
	}
	t1 := time.Date(2026, 11, 22, 10, 0, 0, 0, time.UTC)
	if err := os.Chtimes(specPath, t1, t1); err != nil {
		t.Fatal(err)
	}

	var calls int
	treeV1 := `{"label":"user","kind":"root","children":[{"label":"desk/lead","kind":"role","children":[]}]}`
	treeV2 := `{"label":"user","kind":"root","children":[{"label":"desk/new","kind":"role","children":[]}]}`
	treeOut := treeV1

	h := newHub(t)
	h.server.WatchEvery = 20 * time.Millisecond
	h.server.Display = func() (decide.DisplaySettings, error) {
		return decide.DisplaySettings{Keys: map[string]json.RawMessage{"ui.orgTree": json.RawMessage(`true`)}}, nil
	}
	h.server.OrgConfig = func() (bool, string, error) {
		return true, specPath, nil
	}
	h.server.Orgctl = func(_ context.Context, args []string) ([]byte, error) {
		calls++
		if len(args) != 3 || args[0] != "tree" || args[1] != "--json" || args[2] != specPath {
			return nil, fmt.Errorf("unexpected args: %v", args)
		}
		return []byte(treeOut), nil
	}

	cacheDir := t.TempDir()
	c := &Client{
		Label:    "me@hub",
		CacheDir: cacheDir,
		Dial:     h.dial,
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	res, err := c.OrgTree()
	if err != nil || res.Fallback || string(res.JSON) != treeV1 {
		t.Fatalf("OrgTree() = %+v, err=%v", res, err)
	}
	if calls != 1 {
		t.Fatalf("orgctl calls = %d, want 1", calls)
	}

	// Update spec mtime and output while connected; watch refreshes the replica.
	treeOut = treeV2
	t2 := t1.Add(time.Minute)
	if err := os.Chtimes(specPath, t2, t2); err != nil {
		t.Fatal(err)
	}
	eventually(t, func() bool {
		r, err := c.OrgTree()
		return err == nil && string(r.JSON) == treeV2
	}, "client did not refresh org tree after spec mtime change")
	c.Close()

	// Offline client with the same replica cache still serves the cached tree.
	offline := &Client{
		Label:    "me@hub",
		CacheDir: cacheDir,
	}
	cached, err := offline.OrgTree()
	if err != nil || string(cached.JSON) != treeV2 {
		t.Fatalf("offline OrgTree() = %+v, err=%v", cached, err)
	}
}

func TestOrgTreeOldHubFallsBackWithNote(t *testing.T) {
	c := &Client{Label: "me@hub", CacheDir: t.TempDir(), Dial: scriptedHub(func(req Request) Response {
		switch req.Op {
		case OpHello:
			return Response{Version: Version}
		case OpConfig:
			return Response{Display: &decide.DisplaySettings{}}
		default:
			return Response{Error: fmt.Sprintf("unknown op %q", req.Op)}
		}
	})}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(c.Close)
	res, err := c.OrgTree()
	if err != nil || !res.Fallback || res.Note == "" {
		t.Fatalf("OrgTree() = %+v, err=%v; want Fallback=true with Note", res, err)
	}
}

func TestOrgTreeHubFailureReturnsHubMessage(t *testing.T) {
	specPath := filepath.Join(t.TempDir(), "desk.json")
	if err := os.WriteFile(specPath, []byte(`{}`), 0o600); err != nil {
		t.Fatal(err)
	}
	h := newHub(t)
	h.server.Display = func() (decide.DisplaySettings, error) {
		return decide.DisplaySettings{Keys: map[string]json.RawMessage{"ui.orgTree": json.RawMessage(`true`)}}, nil
	}
	h.server.OrgConfig = func() (bool, string, error) {
		return true, specPath, nil
	}
	h.server.Orgctl = func(context.Context, []string) ([]byte, error) {
		return nil, errors.New("orgctl: reportsTo cycle: a -> b -> a")
	}
	c := &Client{
		Label:    "me@hub",
		CacheDir: t.TempDir(),
		Dial:     h.dial,
	}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(c.Close)
	_, err := c.OrgTree()
	if err == nil || !strings.Contains(err.Error(), "orgctl: reportsTo cycle: a -> b -> a") {
		t.Fatalf("OrgTree() err = %v, want hub orgctl error", err)
	}
}
