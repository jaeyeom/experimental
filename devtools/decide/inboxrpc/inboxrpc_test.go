package inboxrpc

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
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
	code := m.Run()
	_ = os.RemoveAll(dir)
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
}

func newHub(t *testing.T) *hub {
	t.Helper()
	h := &hub{dir: t.TempDir()}
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
		Targets:  []decide.ConfigTarget{{Name: "builder", Pane: "w2:p3"}},
		Agentbus: true,
	}
	return h
}

// dial connects a client to the server in this process.
func (h *hub) dial() (*Conn, error) {
	reqR, reqW := io.Pipe()
	respR, respW := io.Pipe()
	go func() {
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

func connect(t *testing.T, h *hub) *Client {
	t.Helper()
	c := &Client{Dial: h.dial, Label: "me@hub", Machine: "mac", HubMachine: "hub"}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(c.Close)
	return c
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
	if got, err := c.Reply(relay.ID, "why?"); err != nil || got.Messages[len(got.Messages)-1].Text != "why?" || got.Messages[len(got.Messages)-1].Via != decide.ViaUI || len(h.sends) != 1 {
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
	if got, err := c.Get(relay.ID); err != nil || got.Messages[len(got.Messages)-1].By != "ops/bot" {
		t.Fatalf("show by = %+v, %v", got.Messages, err)
	}
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
	if got, err := c.Archive(archived.ID); err != nil || got.Outcome != "archived" {
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

func TestNotifyFailureKeepsTheWrite(t *testing.T) {
	h := newHub(t)
	h.server.Backend.RunSend = func([]string) error { return errors.New("pane gone") }
	it := h.create(t, &decide.Item{Question: "q", NotifyPane: "w1:p1"})
	c := connect(t, h)
	got, err := c.Reply(it.ID, "hi")
	var notify *tui.NotifyError
	if !errors.As(err, &notify) || !strings.Contains(err.Error(), "pane gone") || len(got.Messages) == 0 {
		t.Fatalf("reply = %+v, %v", got, err)
	}
}

func TestDisconnectFailsWritesAndListReconnectsAfterBackoff(t *testing.T) {
	h := newHub(t)
	it := h.create(t, &decide.Item{Question: "q", Options: []decide.Option{{Key: "y", Label: "Yes"}}})
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	c := &Client{Dial: h.dial, Label: "me@hub", Now: func() time.Time { return now }, Timeout: 2 * time.Second}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()

	h.conns[0].Close() // the ssh session drops
	if _, err := c.List(); !errors.Is(err, tui.ErrUnreachable) {
		t.Fatalf("list after drop = %v", err)
	}
	_, err := c.Answer(it.ID, "y")
	if !errors.Is(err, tui.ErrUnreachable) || err.Error() != "hub me@hub unreachable; answer not sent" {
		t.Fatalf("answer while down = %v", err)
	}
	if _, err := c.List(); !errors.Is(err, tui.ErrUnreachable) || h.dials != 1 {
		t.Fatalf("list within backoff = %v, dials %d", err, h.dials)
	}
	now = now.Add(firstBackoff)
	items, err := c.List()
	if err != nil || len(items) != 1 || h.dials != 2 {
		t.Fatalf("list after backoff = %d items, %v, dials %d", len(items), err, h.dials)
	}
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
	dials := 0
	c := &Client{Label: "me@hub", Now: func() time.Time { return now }, Dial: func() (*Conn, error) {
		dials++
		return nil, errors.New("host unreachable")
	}}
	for _, wait := range []time.Duration{0, time.Second, 2 * time.Second, 4 * time.Second} {
		now = now.Add(wait)
		if _, err := c.List(); !errors.Is(err, tui.ErrUnreachable) {
			t.Fatalf("list = %v", err)
		}
	}
	if dials != 4 {
		t.Fatalf("dials = %d", dials)
	}
	now = now.Add(7 * time.Second)
	_, _ = c.List()
	if dials != 4 {
		t.Fatalf("dialed before the 8s backoff: %d", dials)
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
	c := &Client{Dial: h.dial, Label: "me@hub", Now: func() time.Time { return now }, Timeout: 2 * time.Second}
	if err := c.Connect(); err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	h.conns[0].Close()
	_, _ = c.List()
	mu.Lock()
	value = "true"
	mu.Unlock()
	now = now.Add(firstBackoff)
	if _, err := c.List(); err != nil {
		t.Fatal(err)
	}
	got, gen := c.Display()
	if gen != 2 || string(got.Keys["avatarDefault"]) != "true" {
		t.Fatalf("display after reconnect = %+v, generation %d", got, gen)
	}
}
