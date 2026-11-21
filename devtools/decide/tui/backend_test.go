package tui

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// fakeHub is a hub backend in memory: a LocalBackend on the hub's
// directory that records each call and fails every call while down.
// A non-nil block holds every call until it is closed, as a slow or
// reconnecting hub connection does, and then fails it.
type fakeHub struct {
	hub   *LocalBackend
	mu    sync.Mutex
	ops   []string
	down  bool
	focus []decide.OwnerLocation
	block chan struct{}
}

func (f *fakeHub) call(op string) error {
	if f.block != nil {
		<-f.block
		return fmt.Errorf("hub me@hub reconnecting; %s not sent: %w", op, ErrUnreachable)
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ops = append(f.ops, op)
	if f.down {
		return fmt.Errorf("hub me@hub unreachable; %s not sent: %w", op, ErrUnreachable)
	}
	return nil
}

func (f *fakeHub) List() ([]decide.Item, error) {
	if err := f.call("list"); err != nil {
		return nil, err
	}
	return f.hub.List()
}

func (f *fakeHub) MarkRead(id string) (decide.Item, error) {
	if err := f.call("mark-read"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.MarkRead(id)
}

func (f *fakeHub) Answer(id, value string) (decide.Item, error) {
	if err := f.call("answer"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Answer(id, value)
}

func (f *fakeHub) Reply(id, text, route string) (decide.Item, error) {
	if err := f.call("reply"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Reply(id, text, route)
}

func (f *fakeHub) Done(id, outcome string) (decide.Item, error) {
	if err := f.call("done"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Done(id, outcome)
}

func (f *fakeHub) Archive(id string, seen time.Time) (decide.Item, error) {
	if err := f.call("archive"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Archive(id, seen)
}

func (f *fakeHub) Snooze(id string, until time.Time) (decide.Item, error) {
	if err := f.call("snooze"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Snooze(id, until)
}

func (f *fakeHub) Unsnooze(id string) (decide.Item, error) {
	if err := f.call("unsnooze"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Unsnooze(id)
}

func (f *fakeHub) Status() (StatusBoard, error) {
	if err := f.call("status"); err != nil {
		return StatusBoard{}, err
	}
	return f.hub.Status()
}

func (f *fakeHub) Send(forward string, tg Target, text string) error {
	if err := f.call("send"); err != nil {
		return err
	}
	return f.hub.Send(forward, tg, text)
}

func (f *fakeHub) OwnerPane(it *decide.Item) decide.OwnerLocation {
	return decide.OwnerLocation{Machine: "hub", Pane: it.NotifyPane, State: decide.LocationUnknown}
}

func (f *fakeHub) Locate(_ context.Context, it *decide.Item) decide.OwnerLocation {
	f.mu.Lock()
	f.ops = append(f.ops, "locate")
	f.mu.Unlock()
	return decide.OwnerLocation{Machine: "hub", Pane: it.NotifyPane, State: decide.LocationLive, Workspace: "exp", Tab: "review", WorkspaceID: "w1", TabID: "w1:t1"}
}

func (f *fakeHub) Focus(_ context.Context, loc decide.OwnerLocation) string {
	f.focus = append(f.focus, loc)
	return "focused " + loc.Machine + ": " + loc.Workspace + " / " + loc.Tab
}

// hubModel is an inbox on a fake hub with one relay item, and the local
// directory the inbox must never touch.
func hubModel(t *testing.T) (*Model, *fakeHub, decide.Item, string) {
	t.Helper()
	hubDir := t.TempDir()
	item := mustCreate(t, openLedger(t, hubDir), &decide.Item{
		Question: "ship it? https://example.com/pr/1", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen,
		Options: []decide.Option{{Key: "y", Label: "Yes"}}, FreeText: true, NotifyPane: "w1:p1",
	})
	var sent [][]string
	fake := &fakeHub{hub: &LocalBackend{Dir: hubDir, RunSend: func(argv []string) error {
		sent = append(sent, argv)
		return nil
	}}}
	local := t.TempDir()
	m, err := prepare(Options{Dir: local, In: strings.NewReader(""), Out: &strings.Builder{}, Backend: fake})
	if err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width, m.height = 100, 30
	return m, fake, item, local
}

func assertUntouched(t *testing.T, dir string) {
	t.Helper()
	if entries, _ := os.ReadDir(dir); len(entries) != 0 {
		t.Fatalf("the inbox wrote the local ledger: %v", entries)
	}
}

func TestHubBackendCarriesEveryInboxWrite(t *testing.T) {
	m, fake, item, local := hubModel(t)
	if len(m.items) != 1 || m.items[0].ID != item.ID {
		t.Fatalf("items = %+v", m.items)
	}
	press(t, m, "\r")
	if m.mode != modeDetail {
		t.Fatalf("not opened: %q", m.status)
	}
	press(t, m, "0again\t\r")
	if m.status != "replied "+item.ID {
		t.Fatalf("reply status %q", m.status)
	}
	press(t, m, "1")
	got, err := fake.hub.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Via != decide.ViaUI {
		t.Fatalf("answer = %+v", got.Answer)
	}
	press(t, m, "ga\r")
	press(t, m, "b\r")
	if !strings.HasPrefix(m.status, "snoozed "+item.ID) {
		t.Fatalf("snooze status %q", m.status)
	}
	press(t, m, "gz\rbk\r")
	if m.status != "unsnoozed "+item.ID {
		t.Fatalf("unsnooze status %q", m.status)
	}
	press(t, m, "ga\rd")
	press(t, m, "shipped\r")
	got, _ = fake.hub.Get(item.ID)
	if got.State != decide.StateDone || got.Outcome != "shipped" {
		t.Fatalf("done = %s %q", got.State, got.Outcome)
	}
	for _, op := range []string{"list", "mark-read", "reply", "answer", "snooze", "unsnooze", "done"} {
		if !slices.Contains(fake.ops, op) {
			t.Fatalf("%s did not go through the backend: %q", op, fake.ops)
		}
	}
	assertUntouched(t, local)
}

func TestHubReloadRunsOffTheUpdateLoop(t *testing.T) {
	m, fake, _, _ := hubModel(t)
	fake.ops = nil
	_, cmd := m.Update(reloadMsg{})
	if len(fake.ops) != 0 || cmd == nil {
		t.Fatalf("reload ran on the update loop: %q", fake.ops)
	}
	msg := cmd()
	if _, ok := msg.(listMsg); !ok || !slices.Equal(fake.ops, []string{"list"}) {
		t.Fatalf("reload = %T, ops %q", msg, fake.ops)
	}
}

func TestHubDisconnectKeepsTheViewAndFailsWrites(t *testing.T) {
	m, fake, item, _ := hubModel(t)
	fake.down = true
	_, cmd := m.Update(reloadMsg{})
	next, _ := m.Update(cmd())
	if next.(*Model).runErr != nil || m.status != unreachableStatus {
		t.Fatalf("status %q, err %v", m.status, m.runErr)
	}
	if len(m.items) != 1 || !strings.Contains(m.View(), "ship it?") || !strings.Contains(m.View(), unreachableStatus) {
		t.Fatalf("view lost\n%s", m.View())
	}
	press(t, m, "\r")
	if m.mode != modeList || !strings.Contains(m.status, "not sent") {
		t.Fatalf("open while down: mode %d status %q", m.mode, m.status)
	}
	m.mode = modeDetail
	m.detail = &m.items[0]
	press(t, m, "1")
	got, _ := fake.hub.Get(item.ID)
	if got.State != decide.StateOpen || !strings.Contains(m.status, "unreachable") {
		t.Fatalf("answer while down: state %s status %q", got.State, m.status)
	}
	m.backToList()

	fake.down = false
	_, cmd = m.Update(reloadMsg{})
	m.Update(cmd())
	if m.status != "hub reconnected" || len(m.items) != 1 {
		t.Fatalf("after reconnect: status %q items %d", m.status, len(m.items))
	}
	press(t, m, "\r")
	if m.mode != modeDetail {
		t.Fatalf("open after reconnect: %q", m.status)
	}
}

func TestHubLinkOpensWithTheLocalOpener(t *testing.T) {
	m, _, _, _ := hubModel(t)
	var argv []string
	m.runOpen = func(_ context.Context, a []string) error {
		argv = a
		return nil
	}
	m.OpenCommand = []string{"my-open"}
	press(t, m, "\r")
	_, cmd := m.Update(runeKey('l'))
	if cmd == nil {
		t.Fatalf("l ran nothing: %q", m.status)
	}
	drain(cmd)
	if !slices.Equal(argv, []string{"my-open", "https://example.com/pr/1"}) {
		t.Fatalf("opener argv = %q", argv)
	}
}

func TestHubOwnerJumpGoesThroughTheBackend(t *testing.T) {
	m, fake, _, _ := hubModel(t)
	_, cmd := m.Update(runeKey('\r'))
	for _, msg := range drain(cmd) {
		m.Update(msg)
	}
	_, cmd = m.Update(runeKey('o'))
	for _, msg := range drain(cmd) {
		m.Update(msg)
	}
	if len(fake.focus) != 1 || fake.focus[0].Machine != "hub" || m.status != "focused hub: exp / review" {
		t.Fatalf("focus %+v status %q", fake.focus, m.status)
	}
}

// drain runs cmd and any batch inside it and returns the messages.
func drain(cmd tea.Cmd) []tea.Msg {
	if cmd == nil {
		return nil
	}
	msg := cmd()
	if batch, ok := msg.(tea.BatchMsg); ok {
		var out []tea.Msg
		for _, c := range batch {
			out = append(out, drain(c)...)
		}
		return out
	}
	return []tea.Msg{msg}
}

func TestLocalSendAgentbusPaneUsesTheBus(t *testing.T) {
	dir := t.TempDir()
	var calls [][]string
	var templateRan bool
	b := &LocalBackend{
		Dir:          dir,
		SendTemplate: "missing-pane-cmd {pane}",
		RunSend: func([]string) error {
			templateRan = true
			return fmt.Errorf("herdr ran")
		},
		BusRun: func(_ context.Context, args []string) ([]byte, error) {
			calls = append(calls, append([]string(nil), args...))
			return []byte("m-7f3a queued\n"), nil
		},
	}
	if err := b.Send("", Target{Name: "team/desk", Pane: "agentbus:team/desk"}, "hello"); err != nil {
		t.Fatal(err)
	}
	if templateRan {
		t.Fatal("the herdr template ran for an agentbus pane")
	}
	if len(calls) != 1 || len(calls[0]) < 2 || calls[0][0] != "send" || calls[0][1] != "team/desk" {
		t.Fatalf("calls = %q", calls)
	}
	led, err := decide.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 1 || items[0].Owner != "team/desk" || items[0].NotifyPane != "agentbus:team/desk" || items[0].BusMessage != "m-7f3a" {
		t.Fatalf("thread = %+v", items)
	}
}

func TestLocalSendTemplateFailureDropsTheThread(t *testing.T) {
	dir := t.TempDir()
	b := &LocalBackend{
		Dir: dir,
		RunSend: func([]string) error {
			return fmt.Errorf("pane is gone")
		},
	}
	err := b.Send("", Target{Name: "alpha", Pane: "pane-a"}, "hello")
	if err == nil || !strings.Contains(err.Error(), "pane is gone") {
		t.Fatalf("err = %v", err)
	}
	led, openErr := decide.Open(dir)
	if openErr != nil {
		t.Fatal(openErr)
	}
	items, listErr := led.List()
	if listErr != nil {
		t.Fatal(listErr)
	}
	if len(items) != 0 {
		t.Fatalf("items = %+v", items)
	}
}

type replicaFakeHub struct {
	*fakeHub
	mu        sync.Mutex
	connected bool
	syncedAt  time.Time
}

func (r *replicaFakeHub) SyncStatus() SyncStatus {
	r.mu.Lock()
	defer r.mu.Unlock()
	return SyncStatus{Connected: r.connected, SyncedAt: r.syncedAt}
}

func (r *replicaFakeHub) MarkRead(id string) (decide.Item, error) {
	// Replica marks read locally without waiting on the hub.
	return r.hub.MarkRead(id)
}

func TestReplicaFreshnessTextAndImmediateOpen(t *testing.T) {
	hubDir := t.TempDir()
	item := mustCreate(t, openLedger(t, hubDir), &decide.Item{
		Question: "ship it?", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, Unread: true,
		Options: []decide.Option{{Key: "y", Label: "Yes"}},
	})
	loc := time.FixedZone("PST", -8*3600)
	now := time.Date(2026, 11, 20, 14, 17, 0, 0, loc)
	fake := &replicaFakeHub{
		fakeHub:   &fakeHub{hub: &LocalBackend{Dir: hubDir}},
		connected: true,
		syncedAt:  now.Add(-12 * time.Second),
	}
	m, err := prepare(Options{
		Dir:     t.TempDir(),
		In:      strings.NewReader(""),
		Out:     &strings.Builder{},
		Backend: fake,
		Now:     func() time.Time { return now },
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width, m.height = 100, 30
	if view := m.View(); !strings.Contains(view, "synced 12s ago") {
		t.Fatalf("synced view missing freshness:\n%s", view)
	}

	fake.mu.Lock()
	fake.connected = false
	fake.syncedAt = time.Date(2026, 11, 20, 14, 5, 0, 0, loc)
	fake.mu.Unlock()
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if view := m.View(); !strings.Contains(view, "hub unreachable; showing copy from 14:05") {
		t.Fatalf("unreachable view missing copy time:\n%s", view)
	}

	// Open renders at once from the replica even when hub writes are down.
	fake.down = true
	press(t, m, "\r")
	if m.mode != modeDetail || m.detail == nil || m.detail.ID != item.ID {
		t.Fatalf("open while hub down: mode=%d detail=%v status=%q", m.mode, m.detail, m.status)
	}
}

func TestLocalInboxMakesNoReplica(t *testing.T) {
	cacheDir := t.TempDir()
	t.Setenv("XDG_CACHE_HOME", cacheDir)
	dir := t.TempDir()
	mustCreate(t, openLedger(t, dir), &decide.Item{
		Question: "local item", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen, Unread: true,
	})
	m, err := prepare(Options{Dir: dir, In: strings.NewReader(""), Out: &strings.Builder{}})
	if err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width, m.height = 100, 30
	press(t, m, "\r")
	if _, err := os.Stat(cacheDir + "/decide/replicas"); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("local inbox created replica directory: %v", err)
	}
}

func TestLocalBackendDraftBackend(t *testing.T) {
	dir := t.TempDir()
	var b Backend = &LocalBackend{Dir: dir, Machine: "dragon"}
	db, ok := b.(DraftBackend)
	if !ok {
		t.Fatal("*LocalBackend does not implement DraftBackend")
	}

	saved, conflict, err := db.PutDraft(decide.Draft{
		Key:  decide.ReplyDraftKey("D-001"),
		Text: "hello",
	}, 0)
	if err != nil {
		t.Fatalf("PutDraft() error = %v", err)
	}
	if conflict || saved.Rev != 1 || saved.UpdatedBy != "dragon" {
		t.Fatalf("saved = %+v, conflict=%v", saved, conflict)
	}

	list, err := db.ListDrafts()
	if err != nil {
		t.Fatalf("ListDrafts() error = %v", err)
	}
	if len(list) != 1 || list[0].Text != "hello" {
		t.Fatalf("ListDrafts() = %+v", list)
	}

	if err := db.DeleteDraft(decide.ReplyDraftKey("D-001"), ""); err != nil {
		t.Fatalf("DeleteDraft() error = %v", err)
	}
	list, err = db.ListDrafts()
	if err != nil {
		t.Fatalf("ListDrafts() after delete error = %v", err)
	}
	if len(list) != 0 {
		t.Fatalf("ListDrafts() after delete = %+v, want empty", list)
	}
}
