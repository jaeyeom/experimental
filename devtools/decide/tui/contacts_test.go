package tui

import (
	"context"
	"encoding/json"
	"errors"
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
	if got := contactNames(m.contacts); strings.Join(got, ",") != "cara,ada,zed" {
		t.Fatalf("contacts %q", got)
	}
	byName := map[string]contact{}
	for _, c := range m.contacts {
		if _, ok := byName[c.Name]; ok {
			t.Fatalf("duplicate %s", c.Name)
		}
		byName[c.Name] = c
	}
	if byName["ada"].Offline || byName["ada"].Liveness != "alive" || byName["ada"].Open != 1 {
		t.Fatalf("ada %+v", byName["ada"])
	}
	if !byName["cara"].Offline || byName["cara"].Open != 1 {
		t.Fatalf("cara %+v", byName["cara"])
	}
	if byName["zed"].Offline || byName["zed"].Liveness != "dead" || byName["zed"].Open != 0 {
		t.Fatalf("zed %+v", byName["zed"])
	}
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
	for _, needle := range []string{"c contacts", "enter filters every folder", "esc clears the agent filter", "A picks this agent's avatar"} {
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
