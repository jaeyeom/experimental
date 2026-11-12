package tui

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// busList is a fake `agentbus list --json`: twelve names, two dead.
// coord/primary holds the same pane as the configured target "main".
const busList = `[
 {"name":"coord/primary","when":"Primary coordinator","liveness":"alive","transport":"herdr","handle":"herdr:local/default/w1:p1"},
 {"name":"coord/backup","when":"standing by","liveness":"alive","transport":"herdr","handle":"herdr:local/default/w1:p2"},
 {"name":"pm/alpha","when":"planning alpha","liveness":"alive","handle":"herdr:local/default/w2:p1"},
 {"name":"pm/beta","when":"planning beta","liveness":"alive","handle":"herdr:local/default/w2:p2"},
 {"name":"pm/gamma","when":"planning gamma","liveness":"unknown","handle":"herdr:local/default/w2:p3"},
 {"name":"tools/build","when":"running builds","liveness":"alive","handle":"herdr:local/default/w3:p1"},
 {"name":"tools/lint","when":"linting","liveness":"alive","handle":"herdr:local/default/w3:p2"},
 {"name":"tools/deploy","when":"ships PRIority fixes","liveness":"alive","handle":"herdr:local/default/w3:p3"},
 {"name":"ops/inbox","when":"reviewing the queue","liveness":"alive","handle":"herdr:local/default/w4:p1"},
 {"name":"ops/pager","when":"on call","liveness":"alive","handle":"herdr:local/default/w4:p2"},
 {"name":"coord/old","when":"gone","liveness":"dead","handle":"herdr:local/default/w5:p1"},
 {"name":"pm/retired","when":"gone too","liveness":"dead","handle":"herdr:local/default/w5:p2"}
]`

// fakeBus answers `agentbus list --json` and `agentbus send` and records
// every call.
type fakeBus struct {
	list    string
	listErr error
	sendOut string
	sendErr error
	calls   [][]string
}

func (f *fakeBus) run(_ context.Context, args []string) ([]byte, error) {
	f.calls = append(f.calls, append([]string(nil), args...))
	switch {
	case len(args) == 2 && args[0] == "list" && args[1] == "--json":
		return []byte(f.list), f.listErr
	case len(args) == 3 && args[0] == "send":
		return []byte(f.sendOut), f.sendErr
	default:
		return nil, fmt.Errorf("unexpected agentbus %q", args)
	}
}

func configuredTargets() []Target {
	return []Target{{Name: "main", Pane: "w1:p1"}, {Name: "inbox", Pane: "w9:p9"}}
}

func busModel(t *testing.T, bus *fakeBus) *Model {
	t.Helper()
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	m, err := prepare(Options{
		Dir:       t.TempDir(),
		In:        strings.NewReader(""),
		Out:       io.Discard,
		Targets:   configuredTargets(),
		Agentbus:  true,
		BusRunner: bus.run,
	})
	if err != nil {
		t.Fatal(err)
	}
	m.Now = func() time.Time { return time.Date(2026, 11, 10, 0, 0, 0, 0, time.UTC) }
	m.runSend = func([]string) error {
		t.Error("a bus name was sent through the pane command")
		return nil
	}
	return m
}

func candidateNames(m *Model) []string {
	var names []string
	for _, tg := range m.candidates() {
		names = append(names, tg.Name)
	}
	return names
}

func visibleNames(m *Model) []string {
	cands := m.candidates()
	var names []string
	for _, i := range m.visibleTargets() {
		names = append(names, cands[i].Name)
	}
	return names
}

func TestAgentbusOffNeverRunsAgentbus(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	m, err := prepare(Options{
		Dir:     t.TempDir(),
		In:      strings.NewReader(""),
		Out:     io.Discard,
		Targets: configuredTargets(),
		BusRunner: func(context.Context, []string) ([]byte, error) {
			t.Fatal("agentbus ran with the config off")
			return nil, nil
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	press(t, m, "c")
	if got := strings.Join(candidateNames(m), ","); got != "main,inbox" {
		t.Fatalf("candidates = %s", got)
	}
}

func TestAgentbusCandidatesMergeDedupeAndPutDeadLast(t *testing.T) {
	bus := &fakeBus{list: busList}
	m := busModel(t, bus)
	loadBus(t, m)
	press(t, m, "c")
	if len(bus.calls) != 1 || strings.Join(bus.calls[0], " ") != "list --json" {
		t.Fatalf("calls = %q", bus.calls)
	}
	names := candidateNames(m)
	want := []string{
		"inbox",
		"coord/backup", "coord/primary",
		"ops/inbox", "ops/pager",
		"pm/alpha", "pm/beta", "pm/gamma",
		"tools/build", "tools/deploy", "tools/lint",
		"coord/old", "pm/retired",
	}
	if strings.Join(names, ",") != strings.Join(want, ",") {
		t.Fatalf("candidates =\n%q\nwant\n%q", names, want)
	}
	cands := m.candidates()
	if !cands[len(cands)-1].Dead || !cands[len(cands)-2].Dead || cands[len(cands)-3].Dead {
		t.Fatalf("dead flags = %+v", cands)
	}
	if cands[0].Bus || !cands[1].Bus {
		t.Fatalf("bus flags = %+v", cands[:2])
	}

	m.width, m.height = 100, 40
	lines, _ := m.pickerLines()
	if len(lines) > 8 {
		t.Fatalf("40-row dropdown has %d rows", len(lines))
	}
	m.height = 15
	lines, _ = m.pickerLines()
	if len(lines) > 5 {
		t.Fatalf("15-row dropdown has %d rows", len(lines))
	}

	// The last row is selected after the walk, so the window shows it.
	m.height = 40
	for range len(cands) {
		m.Update(tea.KeyMsg{Type: tea.KeyDown})
	}
	if m.target != len(cands)-1 || !strings.Contains(m.status, "pm/retired") || !strings.Contains(m.status, "dead") {
		t.Fatalf("target %d status %q", m.target, m.status)
	}
	view := visibleLine(m.View())
	if !strings.Contains(view, "pm/retired") || !strings.Contains(view, "✗ dead") {
		t.Fatalf("selected dead row is not visible\n%s", view)
	}
	if strings.Contains(view, "main") {
		t.Fatalf("deduped target shown\n%s", view)
	}
}

func TestPickerTypeToFilter(t *testing.T) {
	bus := &fakeBus{list: busList}
	m := busModel(t, bus)
	m.width, m.height = 100, 40
	loadBus(t, m)
	press(t, m, "cpri")
	if got := strings.Join(visibleNames(m), ","); got != "coord/primary,tools/deploy" {
		t.Fatalf("pri matches = %s", got)
	}
	if !strings.Contains(visibleLine(m.View()), "2/13") {
		t.Fatalf("match count missing\n%s", m.View())
	}
	m.Update(tea.KeyMsg{Type: tea.KeyBackspace})
	if m.pickFilter != "pr" {
		t.Fatalf("filter after backspace = %q", m.pickFilter)
	}
	press(t, m, "\x1b")
	if m.pickFilter != "" || !m.composing || len(visibleNames(m)) != 13 {
		t.Fatalf("esc filter %q composing %v", m.pickFilter, m.composing)
	}
	press(t, m, "\x1b")
	if m.composing {
		t.Fatal("second esc kept compose open")
	}
}

func TestPickerKeysMoveAndEnterPicks(t *testing.T) {
	m := New(t.TempDir())
	m.Targets = []Target{{Name: "alpha", Pane: "p1"}, {Name: "beta", Pane: "p2"}, {Name: "gamma", Pane: "p3"}}
	press(t, m, "c")
	m.Update(tea.KeyMsg{Type: tea.KeyCtrlN})
	m.Update(tea.KeyMsg{Type: tea.KeyDown})
	if m.target != 2 {
		t.Fatalf("down target = %d", m.target)
	}
	m.Update(tea.KeyMsg{Type: tea.KeyCtrlP})
	m.Update(tea.KeyMsg{Type: tea.KeyUp})
	if m.target != 0 {
		t.Fatalf("up target = %d", m.target)
	}
	press(t, m, "ga\r")
	if m.target != 2 || m.composeFocus != focusText {
		t.Fatalf("enter target %d focus %d", m.target, m.composeFocus)
	}
}

// loadBus runs one background bus load to the end and hands its result
// to the model, as the program would.
func loadBus(t *testing.T, m *Model) {
	t.Helper()
	cmd := m.busLoad()
	if cmd == nil {
		t.Fatal("no bus load started")
	}
	m.Update(cmd())
}

func TestComposeDoesNotWaitForAgentbus(t *testing.T) {
	release := make(chan struct{})
	bus := &fakeBus{list: busList}
	m := busModel(t, bus)
	m.busRun = func(ctx context.Context, args []string) ([]byte, error) {
		<-release
		return bus.run(ctx, args)
	}
	cmd := m.busLoad()
	if cmd == nil {
		t.Fatal("no bus load started")
	}
	done := make(chan tea.Msg, 1)
	go func() { done <- cmd() }()

	start := time.Now()
	press(t, m, "c")
	m.Update(tea.KeyMsg{Type: tea.KeyDown})
	if elapsed := time.Since(start); elapsed > time.Second {
		t.Fatalf("compose waited %s", elapsed)
	}
	if got := strings.Join(candidateNames(m), ","); got != "main,inbox" {
		t.Fatalf("candidates before the load = %s", got)
	}
	if tg, _ := m.selectedTarget(); tg.Name != "inbox" {
		t.Fatalf("selected %q", tg.Name)
	}
	if m.busLoad() != nil {
		t.Fatal("a second load started while one runs")
	}

	close(release)
	m.Update(<-done)
	if got := len(m.candidates()); got != 13 {
		t.Fatalf("candidates after the load = %q", candidateNames(m))
	}
	if tg, _ := m.selectedTarget(); tg.Name != "inbox" || !m.composing || m.status != "" {
		t.Fatalf("selected %q composing %v status %q", tg.Name, m.composing, m.status)
	}
}

func TestInitStartsTheBusLoad(t *testing.T) {
	m := busModel(t, &fakeBus{list: busList})
	m.Init()
	if !m.busLoading {
		t.Fatal("Init did not start a bus load")
	}
}

func TestAgentbusOffStartsNoLoad(t *testing.T) {
	m := busModel(t, &fakeBus{list: busList})
	m.busOn = false
	if m.busLoad() != nil {
		t.Fatal("a bus load started with the config off")
	}
}

func TestReloadStartsABusLoadEachInterval(t *testing.T) {
	bus := &fakeBus{list: busList}
	m := busModel(t, bus)
	now := time.Date(2026, 11, 10, 0, 0, 0, 0, time.UTC)
	m.Now = func() time.Time { return now }
	loadBus(t, m)
	m.Update(reloadMsg{})
	if m.busLoading {
		t.Fatal("a reload within the interval started a bus load")
	}
	now = now.Add(busReloadInterval + time.Second)
	m.Update(reloadMsg{})
	if !m.busLoading {
		t.Fatal("a reload after the interval did not start a bus load")
	}
}

func TestAgentbusTimeoutKeepsNamesAndSaysSlow(t *testing.T) {
	bus := &fakeBus{list: busList}
	m := busModel(t, bus)
	loadBus(t, m)
	if m.busTimeout != decide.DefaultBusListTimeout {
		t.Fatalf("default timeout = %s", m.busTimeout)
	}
	m.busTimeout = 20 * time.Millisecond
	m.busRun = func(ctx context.Context, _ []string) ([]byte, error) {
		<-ctx.Done()
		return nil, errors.New("signal: killed")
	}
	loadBus(t, m)
	if got := len(m.candidates()); got != 13 {
		t.Fatalf("names after a timeout = %q", candidateNames(m))
	}
	if m.status != "agentbus: slow (>20ms)" {
		t.Fatalf("status = %q", m.status)
	}

	// The note shows once.
	m.status = ""
	loadBus(t, m)
	if m.status != "" {
		t.Fatalf("second timeout status = %q", m.status)
	}

	// A good load clears the note, so the next failure shows again.
	m.busRun = bus.run
	loadBus(t, m)
	m.busRun = func(ctx context.Context, _ []string) ([]byte, error) {
		<-ctx.Done()
		return nil, ctx.Err()
	}
	loadBus(t, m)
	if m.status != "agentbus: slow (>20ms)" {
		t.Fatalf("status after recovery = %q", m.status)
	}
}

func TestAgentbusErrorSaysUnavailable(t *testing.T) {
	bus := &fakeBus{list: busList, listErr: errors.New("daemon unreachable")}
	m := busModel(t, bus)
	press(t, m, "c")
	loadBus(t, m)
	if got := strings.Join(candidateNames(m), ","); got != "main,inbox" {
		t.Fatalf("candidates %s", got)
	}
	if m.status != "agentbus: unavailable (daemon unreachable)" {
		t.Fatalf("status = %q", m.status)
	}
}

func TestRunOptionsSetTheBusTimeout(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	m, err := prepare(Options{
		Dir:               t.TempDir(),
		In:                strings.NewReader(""),
		Out:               io.Discard,
		Agentbus:          true,
		RecipientsTimeout: 5 * time.Second,
	})
	if err != nil {
		t.Fatal(err)
	}
	if m.busTimeout != 5*time.Second {
		t.Fatalf("busTimeout = %s", m.busTimeout)
	}
}

func TestSendToBusNameRunsAgentbusSend(t *testing.T) {
	bus := &fakeBus{list: busList, sendOut: "m-7f3a queued\n"}
	m := busModel(t, bus)
	loadBus(t, m)
	press(t, m, "cprimary\rhello bus\t\r")
	if len(bus.calls) != 2 {
		t.Fatalf("calls = %q", bus.calls)
	}
	send := bus.calls[1]
	if send[0] != "send" || send[1] != "coord/primary" {
		t.Fatalf("send argv = %q", send)
	}
	led, err := decide.Open(m.Dir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 1 {
		t.Fatalf("items = %+v", items)
	}
	it := items[0]
	if send[2] != decide.DeliveryNote(it.ID, "human", "hello bus") || !strings.Contains(send[2], it.ID) {
		t.Fatalf("note = %q", send[2])
	}
	if it.Owner != "coord/primary" || it.NotifyPane != "agentbus:coord/primary" || it.BusMessage != "m-7f3a" {
		t.Fatalf("thread = %+v", it)
	}
	if m.note != "" || m.composing || m.status != "sent to coord/primary" {
		t.Fatalf("note %q composing %v status %q", m.note, m.composing, m.status)
	}

	// A reply in that thread goes back through the bus.
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o0again\r")
	if len(bus.calls) != 3 {
		t.Fatalf("reply calls = %q status %q", bus.calls, m.status)
	}
	want := []string{"send", "coord/primary", decide.DeliveryNote(it.ID, "human", "again")}
	if strings.Join(bus.calls[2], "\x00") != strings.Join(want, "\x00") || m.status != "replied "+it.ID {
		t.Fatalf("reply argv %q status %q", bus.calls[2], m.status)
	}
}

func TestSendToBusNameFailureKeepsTheDraft(t *testing.T) {
	bus := &fakeBus{list: busList, sendErr: errors.New("agentbus: no such name")}
	m := busModel(t, bus)
	loadBus(t, m)
	press(t, m, "cprimary\rhello bus\t\r")
	if m.note != "hello bus" || !m.composing || m.status != "agentbus: no such name" {
		t.Fatalf("note %q composing %v status %q", m.note, m.composing, m.status)
	}
	led, err := decide.Open(m.Dir)
	if err != nil {
		t.Fatal(err)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(items) != 0 {
		t.Fatalf("a failed send left a thread: %+v", items)
	}
}
