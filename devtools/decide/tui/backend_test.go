package tui

import (
	"context"
	"fmt"
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// fakeHub is a hub backend in memory: a LocalBackend on the hub's
// directory that records each call and fails every call while down.
type fakeHub struct {
	hub   *LocalBackend
	ops   []string
	down  bool
	focus []decide.OwnerLocation
}

func (f *fakeHub) call(op string) error {
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

func (f *fakeHub) Reply(id, text string) (decide.Item, error) {
	if err := f.call("reply"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Reply(id, text)
}

func (f *fakeHub) Done(id, outcome string) (decide.Item, error) {
	if err := f.call("done"); err != nil {
		return decide.Item{}, err
	}
	return f.hub.Done(id, outcome)
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
	f.ops = append(f.ops, "locate")
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
	press(t, m, "0again\r")
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
