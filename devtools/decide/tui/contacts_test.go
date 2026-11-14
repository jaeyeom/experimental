package tui

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func pressUpdate(t *testing.T, m *Model, keys string) tea.Cmd {
	t.Helper()
	var cmd tea.Cmd
	for _, r := range keys {
		_, cmd = m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{r}})
	}
	return cmd
}

func contactNames(rows []contact) []string {
	out := make([]string, len(rows))
	for i, c := range rows {
		out[i] = c.Name
	}
	return out
}

func TestContactsMergeBusAndLedger(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 12, 15, 0, 0, 0, time.UTC)
	mustCreate(t, led, &decide.Item{
		Question: "older", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen,
		CreatedAt: now.Add(-2 * time.Hour),
	})
	mustCreate(t, led, &decide.Item{
		Question: "newer", Owner: "cara", Kind: decide.KindRelay, State: decide.StateOpen,
		CreatedAt: now.Add(-time.Hour),
	})
	bus := &fakeBus{list: `[
		{"name":"ada","liveness":"alive"},
		{"name":"zed","liveness":"dead"},
		{"name":"ada","liveness":"alive"}
	]`}
	m := New(dir)
	m.Now = func() time.Time { return now }
	m.UTC = true
	m.busOn = false
	m.busRun = bus.run
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 120
	m.height = 24
	cmd := pressUpdate(t, m, "gc")
	if cmd == nil {
		t.Fatal("contacts did not start a bus load")
	}
	if m.mode != modeContacts {
		t.Fatalf("mode %d", m.mode)
	}
	view := m.View()
	if !strings.Contains(view, "ada") || !strings.Contains(view, "cara") {
		t.Fatalf("ledger agents missing before the bus returns\n%s", view)
	}
	m.Update(cmd())
	if len(bus.calls) != 1 || strings.Join(bus.calls[0], " ") != "list --json" {
		t.Fatalf("bus calls %q", bus.calls)
	}
	if got := contactNames(m.contacts); strings.Join(got, ",") != "ada,cara,zed" {
		t.Fatalf("contacts %q", got)
	}
	byName := map[string]contact{}
	for _, c := range m.contacts {
		if _, ok := byName[c.Name]; ok {
			t.Fatalf("duplicate %s", c.Name)
		}
		byName[c.Name] = c
	}
	if byName["ada"].Offline || byName["ada"].Liveness != "alive" || byName["ada"].Open != 1 || byName["ada"].stateLabel() != "active" {
		t.Fatalf("ada %+v", byName["ada"])
	}
	if !byName["cara"].Offline || byName["cara"].Open != 1 || byName["cara"].stateLabel() != "dead" {
		t.Fatalf("cara %+v", byName["cara"])
	}
	if byName["zed"].Offline || byName["zed"].Liveness != "dead" || byName["zed"].Open != 0 || byName["zed"].stateLabel() != "dead" {
		t.Fatalf("zed %+v", byName["zed"])
	}
	assertFolderCount(t, m, len(Folders)+1, "1", false)
	view = m.View()
	if !strings.Contains(view, "(offline)") || !strings.Contains(view, "cara") {
		t.Fatalf("history-only agent not marked offline\n%s", view)
	}
	if strings.Contains(visibleLine(lineContaining(view, "ada")), "(offline)") {
		t.Fatalf("ada marked offline\n%s", view)
	}
	checkGolden(t, "contacts_120.golden", view)
	m.width = 70
	checkGolden(t, "contacts_70.golden", m.View())

	m.width = 0
	m.height = 0
	press(t, m, "?")
	help := m.View()
	for _, needle := range []string{
		"c contacts", "enter filters every folder", "esc clears the agent filter", "A picks this agent's avatar",
		"x hides this contact", "H shows hidden contacts", "X hides every dead contact", "R releases a dead bus name",
	} {
		if !strings.Contains(help, needle) {
			t.Fatalf("help missing %q\n%s", needle, help)
		}
	}
}

func TestEnterFiltersEveryFolderToTheAgent(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 12, 15, 0, 0, 0, time.UTC)
	mustCreate(t, led, &decide.Item{
		Question: "ada-open", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen,
		CreatedAt: now.Add(-time.Hour),
	})
	mustCreate(t, led, &decide.Item{
		Question: "bea-open", Owner: "bea", Kind: decide.KindRelay, State: decide.StateOpen,
		CreatedAt: now.Add(-time.Hour),
		Messages: []decide.Message{{
			Author: "bea", By: "cara", Time: now.Add(-time.Hour), Text: "bea-open",
		}},
	})
	mustCreate(t, led, &decide.Item{
		Question: "ada-answered", Owner: "ada", Kind: decide.KindRelay, State: decide.StateAnswered,
		CreatedAt: now.Add(-2 * time.Hour), AnsweredAt: now.Add(-2 * time.Hour),
	})
	mustCreate(t, led, &decide.Item{
		Question: "bea-done", Owner: "bea", Kind: decide.KindRelay, State: decide.StateDone,
		CreatedAt: now.Add(-3 * time.Hour), DoneAt: now.Add(-3 * time.Hour),
		Messages: []decide.Message{{
			Author: "bea", By: "ada", Time: now.Add(-3 * time.Hour), Text: "bea-done",
		}},
	})
	m := New(dir)
	m.Now = func() time.Time { return now }
	m.UTC = true
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 120
	m.height = 24
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	m.contactCursor = -1
	for i, c := range m.contacts {
		if c.Name == "ada" {
			m.contactCursor = i
		}
	}
	if m.contactCursor < 0 {
		t.Fatalf("ada missing from %q", contactNames(m.contacts))
	}
	press(t, m, "\r")
	if !strings.Contains(m.View(), "agent: ada") {
		t.Fatalf("no agent header\n%s", m.View())
	}
	checks := []struct {
		keys  string
		index int
		count string
		dim   bool
	}{
		{"gi", 0, "1/2", false},
		{"gz", folderIndex(t, "Snoozed"), "0/0", true},
		{"ga", folderIndex(t, "Answered"), "1/1", false},
		{"gd", folderIndex(t, "Done"), "1/1", false},
		{"gw", folderIndex(t, "Withdrawn"), "0/0", true},
	}
	for _, c := range checks {
		press(t, m, c.keys)
		assertFolderCount(t, m, c.index, c.count, c.dim)
		view := m.View()
		if !strings.Contains(view, "agent: ada") {
			t.Fatalf("after %s the agent header is gone\n%s", c.keys, view)
		}
		if c.keys == "gi" && (!strings.Contains(view, "ada-open") || strings.Contains(view, "bea-open")) {
			t.Fatalf("inbox did not keep only ada's items\n%s", view)
		}
		if c.keys == "gd" && !strings.Contains(view, "bea-done") {
			t.Fatalf("done hid the thread ada sent on\n%s", view)
		}
	}
	press(t, m, "/open\r")
	assertFolderCount(t, m, 0, "1/2", false)
	assertFolderCount(t, m, folderIndex(t, "Answered"), "0/1", true)
	if !strings.Contains(m.View(), "agent: ada") || !strings.Contains(m.View(), "/open") {
		t.Fatalf("search did not keep the agent filter\n%s", m.View())
	}
	press(t, m, "\x1b")
	if strings.Contains(m.View(), "agent:") {
		t.Fatalf("esc kept the agent filter\n%s", m.View())
	}
	assertFolderCount(t, m, 0, "2/2", false)
	assertFolderCount(t, m, folderIndex(t, "Answered"), "0/1", true)
	assertFolderCount(t, m, folderIndex(t, "Done"), "0/1", true)
}

func TestContactsAvatarSavesOnTheHub(t *testing.T) {
	path := pickConfig(t, `{"zeta": true}`)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question: "ship", Owner: "ops/x", Kind: decide.KindRelay, State: decide.StateOpen,
	})
	m := New(dir)
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	hub := &hubAvatars{settings: decide.DisplaySettings{Keys: map[string]json.RawMessage{
		"avatars": json.RawMessage(`[{"match":"ops/x","builtin":"owl"}]`),
	}}}
	m.backend = hub
	m.width = 100
	m.height = 40
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "A")
	if !m.avatarPick || m.avatarOwner != "ops/x" {
		t.Fatalf("picker %v owner %q", m.avatarPick, m.avatarOwner)
	}
	pickEntry(t, m, "owl")
	if hub.name != "ops/x" || hub.choice != "owl" {
		t.Fatalf("set-avatar name %q choice %q", hub.name, hub.choice)
	}
	if m.status != "avatar for ops/x: owl (saved on hub)" {
		t.Fatalf("status %q", m.status)
	}
	got, err := os.ReadFile(path)
	if err != nil || string(got) != string(before) {
		t.Fatalf("client config = %s, %v", got, err)
	}
}

func TestUnreachableBusStillListsLedgerAgents(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question: "hello", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen,
	})
	m := New(dir)
	m.busOn = false
	m.busRun = func(context.Context, []string) ([]byte, error) {
		return nil, errors.New("daemon unreachable")
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 80
	m.height = 20
	start := time.Now()
	cmd := pressUpdate(t, m, "gc")
	if cmd == nil {
		t.Fatal("contacts did not start a bus load")
	}
	if !strings.Contains(m.View(), "ada") {
		t.Fatalf("ledger agents missing while the bus is in flight\n%s", m.View())
	}
	msg := cmd()
	if time.Since(start) > time.Second {
		t.Fatal("the bus load blocked the inbox")
	}
	m.Update(msg)
	if !strings.Contains(m.View(), "ada") {
		t.Fatalf("ledger agents dropped after the bus failed\n%s", m.View())
	}
	if !strings.Contains(m.status, "agentbus: unavailable") || !strings.Contains(m.status, "daemon unreachable") {
		t.Fatalf("status %q", m.status)
	}
	if !strings.Contains(m.View(), "(offline)") {
		t.Fatalf("ledger agent was not marked offline\n%s", m.View())
	}
}

func TestContactsSortByStateAndCountMatches(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 12, 15, 0, 0, 0, time.UTC)
	open := func(owner string, age time.Duration) {
		t.Helper()
		mustCreate(t, led, &decide.Item{
			Question: owner, Owner: owner, Kind: decide.KindRelay, State: decide.StateOpen,
			CreatedAt: now.Add(-age),
		})
	}
	open("ada", 2*time.Hour)
	open("cara", time.Hour)
	open("bea", 3*time.Hour)
	bus := &fakeBus{list: `[
		{"name":"ada","liveness":"alive"},
		{"name":"eve","liveness":"alive"},
		{"name":"bea","liveness":"hibernated"},
		{"name":"neo","liveness":"waking"},
		{"name":"una","liveness":"unreachable"},
		{"name":"unk","liveness":"unknown"},
		{"name":"zed","liveness":"dead"},
		{"name":"gon","liveness":"gone"}
	]`}
	m := New(dir)
	m.Now = func() time.Time { return now }
	m.busRun = bus.run
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 120
	m.height = 30
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	finishContactBus(t, m)
	want := []string{"ada", "eve", "bea", "neo", "una", "unk", "cara", "gon", "zed"}
	if got := contactNames(m.contacts); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("contacts %q", got)
	}
	byName := map[string]contact{}
	for _, c := range m.contacts {
		byName[c.Name] = c
	}
	if byName["cara"].OnBus || !byName["cara"].Offline || byName["cara"].stateLabel() != "dead" {
		t.Fatalf("history-only cara %+v", byName["cara"])
	}
	for _, name := range []string{"bea", "neo", "una", "unk"} {
		if byName[name].stateLabel() != "unresponsive" {
			t.Fatalf("%s %+v", name, byName[name])
		}
	}
	if byName["gon"].stateLabel() != "dead" || byName["zed"].stateLabel() != "dead" {
		t.Fatalf("gon %+v zed %+v", byName["gon"], byName["zed"])
	}
	assertFolderCount(t, m, len(Folders)+1, "2", false)
	press(t, m, "/ada\r")
	assertFolderCount(t, m, len(Folders)+1, "1/2", false)
	press(t, m, "/zzz\r")
	assertFolderCount(t, m, len(Folders)+1, "0/2", true)
	press(t, m, "/\r")
	selectContact(t, m, "ada")
	press(t, m, "\r")
	assertFolderCount(t, m, len(Folders)+1, "2/2", false)
}

func TestHideContactPersistsOnTheHubAndReturnsWhenActive(t *testing.T) {
	path := pickConfig(t, `{"zeta": true}`)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question: "from cara", Owner: "cara", Kind: decide.KindRelay, State: decide.StateOpen,
	})
	mustCreate(t, led, &decide.Item{
		Question: "from ada", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen,
	})
	bus := &fakeBus{list: `[
		{"name":"ada","liveness":"alive"},
		{"name":"cara","liveness":"dead"}
	]`}
	m := New(dir)
	m.busRun = bus.run
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	hub := &hubHidden{}
	m.backend = hub
	m.width = 100
	m.height = 30
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	finishContactBus(t, m)
	selectContact(t, m, "ada")
	press(t, m, "x")
	if len(hub.calls) != 0 {
		t.Fatalf("hiding an active contact wrote %q", hub.calls)
	}
	if !strings.Contains(m.status, "active") {
		t.Fatalf("status %q", m.status)
	}
	selectContact(t, m, "cara")
	press(t, m, "x")
	if len(hub.calls) != 1 || strings.Join(hub.calls[0], ",") != "cara" {
		t.Fatalf("hub hidden %q", hub.calls)
	}
	got, err := os.ReadFile(path)
	if err != nil || string(got) != string(before) {
		t.Fatalf("client config = %s, %v", got, err)
	}
	if names := contactNames(m.visibleContacts()); strings.Join(names, ",") != "ada" {
		t.Fatalf("visible %q", names)
	}
	press(t, m, "H")
	if names := contactNames(m.visibleContacts()); strings.Join(names, ",") != "ada,cara" {
		t.Fatalf("show hidden %q", names)
	}
	press(t, m, "H")
	if names := contactNames(m.visibleContacts()); strings.Join(names, ",") != "ada" {
		t.Fatalf("hide again %q", names)
	}
	bus.list = `[{"name":"ada","liveness":"alive"},{"name":"cara","liveness":"alive"}]`
	finishContactBus(t, m)
	names := contactNames(m.visibleContacts())
	if len(names) != 2 || !strings.Contains(strings.Join(names, ","), "ada") || !strings.Contains(strings.Join(names, ","), "cara") {
		t.Fatalf("active again %q", names)
	}
	last := hub.calls[len(hub.calls)-1]
	if len(last) != 0 {
		t.Fatalf("still hidden %q", last)
	}
	items, err := led.List()
	if err != nil || len(items) != 2 {
		t.Fatalf("ledger items %d %v", len(items), err)
	}
}

func TestHideAllDeadAsksThenHidesOnlyDead(t *testing.T) {
	path := pickConfig(t, `{"zeta": true}`)
	dir := t.TempDir()
	led := openLedger(t, dir)
	for _, owner := range []string{"ada", "bea", "cara"} {
		mustCreate(t, led, &decide.Item{
			Question: owner, Owner: owner, Kind: decide.KindRelay, State: decide.StateOpen,
		})
	}
	bus := &fakeBus{list: `[
		{"name":"ada","liveness":"alive"},
		{"name":"bea","liveness":"hibernated"},
		{"name":"zed","liveness":"dead"}
	]`}
	m := New(dir)
	m.busRun = bus.run
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 100
	m.height = 30
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	finishContactBus(t, m)
	press(t, m, "X")
	if !strings.Contains(m.status, "dead") || !strings.Contains(m.status, "y/n") {
		t.Fatalf("status %q", m.status)
	}
	if names := contactNames(m.visibleContacts()); strings.Join(names, ",") != "ada,bea,cara,zed" {
		t.Fatalf("hidden before confirm %q", names)
	}
	press(t, m, "n")
	if names := contactNames(m.visibleContacts()); strings.Join(names, ",") != "ada,bea,cara,zed" {
		t.Fatalf("n hid %q", names)
	}
	data, err := os.ReadFile(path)
	if err != nil || strings.Contains(string(data), "hiddenContacts") {
		t.Fatalf("config after cancel %s %v", data, err)
	}
	press(t, m, "X")
	press(t, m, "y")
	if names := contactNames(m.visibleContacts()); strings.Join(names, ",") != "ada,bea" {
		t.Fatalf("after confirm %q", names)
	}
	saved, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(saved), "cara") || !strings.Contains(string(saved), "zed") || strings.Contains(string(saved), "ada") || strings.Contains(string(saved), "bea") {
		t.Fatalf("config %s", saved)
	}
	items, err := led.List()
	if err != nil || len(items) != 3 {
		t.Fatalf("ledger items %d %v", len(items), err)
	}
	press(t, m, "H")
	if names := contactNames(m.visibleContacts()); strings.Join(names, ",") != "ada,bea,cara,zed" {
		t.Fatalf("show hidden %q", names)
	}
}

func TestReleaseDeadNameRefusesLive(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	for _, owner := range []string{"ada", "bea", "cara"} {
		mustCreate(t, led, &decide.Item{
			Question: owner, Owner: owner, Kind: decide.KindRelay, State: decide.StateOpen,
		})
	}
	bus := &fakeBus{list: `[
		{"name":"ada","liveness":"alive"},
		{"name":"bea","liveness":"hibernated"},
		{"name":"zed","liveness":"dead"}
	]`}
	m := New(dir)
	m.busRun = bus.run
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 100
	m.height = 30
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	finishContactBus(t, m)
	selectContact(t, m, "zed")
	press(t, m, "R")
	if !strings.Contains(m.status, "zed") || !strings.Contains(m.status, "y/n") {
		t.Fatalf("status %q", m.status)
	}
	if releases(bus) != 0 {
		t.Fatalf("released before confirm %q", bus.calls)
	}
	press(t, m, "y")
	if releases(bus) != 1 || strings.Join(bus.calls[len(bus.calls)-1], " ") != "release zed" {
		t.Fatalf("calls %q", bus.calls)
	}
	if !strings.Contains(m.status, "zed") || strings.Contains(m.status, "refused") {
		t.Fatalf("status %q", m.status)
	}
	selectContact(t, m, "ada")
	press(t, m, "R")
	if !strings.Contains(m.status, "refused") || !strings.Contains(m.status, "active") || releases(bus) != 1 {
		t.Fatalf("status %q calls %q", m.status, bus.calls)
	}
	selectContact(t, m, "bea")
	press(t, m, "R")
	if !strings.Contains(m.status, "refused") || !strings.Contains(m.status, "unresponsive") || releases(bus) != 1 {
		t.Fatalf("status %q calls %q", m.status, bus.calls)
	}
	selectContact(t, m, "cara")
	press(t, m, "R")
	if !strings.Contains(m.status, "refused") || releases(bus) != 1 {
		t.Fatalf("status %q calls %q", m.status, bus.calls)
	}
}

func finishContactBus(t *testing.T, m *Model) {
	t.Helper()
	m.busWant = true
	cmd := m.takeBusCmd()
	if cmd == nil {
		t.Fatal("no bus load")
	}
	m.Update(cmd())
}

func selectContact(t *testing.T, m *Model, name string) {
	t.Helper()
	for i, c := range m.visibleContacts() {
		if c.Name == name {
			m.contactCursor = i
			return
		}
	}
	t.Fatalf("%s missing from %q", name, contactNames(m.visibleContacts()))
}

func releases(bus *fakeBus) int {
	n := 0
	for _, call := range bus.calls {
		if len(call) > 0 && call[0] == "release" {
			n++
		}
	}
	return n
}

type hubHidden struct {
	*LocalBackend
	calls [][]string
}

func (h *hubHidden) SetHidden(names []string) (decide.DisplaySettings, error) {
	h.calls = append(h.calls, append([]string(nil), names...))
	raw, err := json.Marshal(names)
	if err != nil {
		return decide.DisplaySettings{}, fmt.Errorf("hidden contacts: %w", err)
	}
	return decide.DisplaySettings{Keys: map[string]json.RawMessage{"hiddenContacts": raw}}, nil
}
