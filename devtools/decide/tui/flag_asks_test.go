package tui

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestFlagOverlayFromListAndDetailRequiresNoteForOther(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	it := mustCreate(t, led, &decide.Item{
		Owner:    "worker/alice",
		Kind:     decide.KindRelay,
		Question: "Pick it cleanly.",
		Options: []decide.Option{
			{Key: "a", Label: "Do it"},
			{Key: "b", Label: "Skip it"},
		},
	})

	m := New(dir)
	m.Now = func() time.Time { return now }
	m.width = 120
	m.height = 35
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Trigger flag from list view via '!'.
	press(t, m, "!")
	if !m.flagging {
		t.Fatal("expected flag modal to open from list via !")
	}
	view := visibleLine(m.View())
	if !strings.Contains(view, "flag "+it.ID) || !strings.Contains(view, "vague-pronoun") {
		t.Fatalf("flag modal missing header/reason:\n%s", view)
	}

	// Move down to "other".
	otherIdx := -1
	for i, r := range m.flagReasons {
		if r.ID == decide.ReasonOther {
			otherIdx = i
			break
		}
	}
	if otherIdx < 0 {
		t.Fatal("missing ReasonOther in flagReasons")
	}
	for range otherIdx {
		m.Update(tea.KeyMsg{Type: tea.KeyDown})
	}
	// Toggle "other" on.
	m.Update(tea.KeyMsg{Type: tea.KeySpace})
	// Attempt submit without note -> blocked with error and moved to note.
	m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	if !m.flagging {
		t.Fatal("flagging should stay open when other lacks note")
	}
	if !strings.Contains(strings.ToLower(m.flagErr), "note") {
		t.Fatalf("flagErr = %q, want note required", m.flagErr)
	}

	// Type a note and submit with Enter.
	for _, r := range "ambiguous pronouns in prompt" {
		m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{r}})
	}
	m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	if m.flagging {
		t.Fatalf("flagging did not close after submitting note: err=%q", m.flagErr)
	}

	// Also open detail view, focus message and flag via '!' with a non-other reason.
	press(t, m, "o")
	if m.mode != modeDetail {
		t.Fatalf("mode = %v, want modeDetail", m.mode)
	}
	press(t, m, "!")
	if !m.flagging || m.flagMsgIdx == nil || *m.flagMsgIdx != 0 {
		t.Fatalf("detail flag: flagging=%v msgIdx=%v", m.flagging, m.flagMsgIdx)
	}
	m.Update(tea.KeyMsg{Type: tea.KeySpace}) // toggle first reason
	m.Update(tea.KeyMsg{Type: tea.KeyEnter}) // submit directly
	if m.flagging {
		t.Fatalf("detail flag did not close: err=%q", m.flagErr)
	}

	events, err := led.ListRawEvents()
	if err != nil {
		t.Fatal(err)
	}
	if len(events) != 2 {
		t.Fatalf("events count = %d, want 2: %+v", len(events), events)
	}
	if events[0].Kind != decide.EventFlag || events[0].SendingAgent != "worker/alice" || events[0].Note != "ambiguous pronouns in prompt" {
		t.Fatalf("event[0] = %+v", events[0])
	}
	if events[1].Kind != decide.EventFlag || events[1].MessageIndex == nil || *events[1].MessageIndex != 0 {
		t.Fatalf("event[1] = %+v", events[1])
	}
}

func TestAsksDisabledHidesNavPanelAndGoTo(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	it := mustCreate(t, led, &decide.Item{
		Owner:    "worker/alice",
		Kind:     decide.KindRelay,
		Question: "Pick one?",
		Options: []decide.Option{
			{Key: "a", Label: "Ask Bob", Ask: "Bob: review PR"},
			{Key: "b", Label: "Skip"},
		},
	})
	if _, err := led.Answer(it.ID, "a", decide.ViaCLI); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.width = 120
	m.height = 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if m.hasAsks() {
		t.Fatal("hasAsks() should be false when asks config is off")
	}
	asks, err := led.ListAsks()
	if err != nil || len(asks) != 0 {
		t.Fatalf("asks with feature disabled = %+v, err=%v", asks, err)
	}
	// Choice event still logged cleanly.
	events, err := led.ListRawEvents()
	if err != nil || len(events) != 1 || events[0].Kind != decide.EventChoice {
		t.Fatalf("events with asks disabled = %+v, err=%v", events, err)
	}
	view := visibleLine(m.View())
	if strings.Contains(view, "Asks") {
		t.Fatalf("sidebar should hide Asks when disabled:\n%s", view)
	}
	press(t, m, "gk")
	if m.mode == modeAsks || m.status != "unknown go-to key" {
		t.Fatalf("gk when asks disabled: mode=%v status=%q", m.mode, m.status)
	}
	// Detail toolbar remains unchanged (6 buttons).
	if err := m.showFolder(2); err != nil { // answered
		t.Fatal(err)
	}
	press(t, m, "o")
	if m.mode != modeDetail {
		t.Fatalf("mode = %v, want modeDetail", m.mode)
	}
	wantToolbar := "[Back (u)] [Reply (0)] [Archive (e)] [Archive with note (E)] [Forward (f)] [Avatar (A)]"
	if title := pinnedTitle(m.View()); !strings.HasPrefix(title, wantToolbar+" ") {
		t.Fatalf("detail toolbar = %q, want prefix %q", title, wantToolbar+" ")
	}
}

func TestAsksEnabledShowsNavStatusPanelSortAndClose(t *testing.T) {
	t.Setenv("NO_COLOR", "1")
	xdg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", xdg)
	cfgPath := filepath.Join(xdg, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, []byte(`{"asks":{"enabled":true}}`), 0o600); err != nil {
		t.Fatal(err)
	}

	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	it1 := mustCreate(t, led, &decide.Item{
		Owner:    "worker/alice",
		Kind:     decide.KindRelay,
		Question: "First?",
		Options: []decide.Option{
			{Key: "a", Label: "Ask Bob", Ask: "Bob: check build", AskLinks: []string{"https://example.com/pr/1"}},
		},
	})
	it2 := mustCreate(t, led, &decide.Item{
		Owner:    "worker/bob",
		Kind:     decide.KindRelay,
		Question: "Second?",
		Options: []decide.Option{
			{Key: "a", Label: "Ask Carol", Ask: "Carol: review doc"},
		},
	})
	if _, err := led.Answer(it1.ID, "a", decide.ViaCLI); err != nil {
		t.Fatal(err)
	}
	now = now.Add(time.Minute)
	if _, err := led.Answer(it2.ID, "a", decide.ViaCLI); err != nil {
		t.Fatal(err)
	}
	// Close ASK-001 -> open asks (ASK-002) should appear before done asks (ASK-001).
	if _, err := led.CloseAsk("ASK-001", decide.AskStatusDone, "already merged", decide.Human, decide.ViaCLI); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Now = func() time.Time { return now }
	m.width = 140
	m.height = 35
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if !m.hasAsks() {
		t.Fatal("expected hasAsks() == true")
	}

	// Sidebar shows Asks with open count 1.
	view := visibleLine(m.View())
	asksLine := lineContaining(view, "Asks")
	if !strings.Contains(asksLine, "1") {
		t.Fatalf("sidebar Asks row = %q, want open count 1\n%s", asksLine, view)
	}

	// Status view includes Asks built-in panel with open count and ASK-002.
	press(t, m, "gs")
	statusView := visibleLine(m.View())
	if !strings.Contains(statusView, "Asks") || !strings.Contains(statusView, "ASK-002") {
		t.Fatalf("status view missing Asks panel:\n%s", statusView)
	}

	// Navigate to Asks via g k.
	press(t, m, "gk")
	if m.mode != modeAsks {
		t.Fatalf("mode = %v, want modeAsks", m.mode)
	}
	asksView := visibleLine(m.View())
	idxOpen := strings.Index(asksView, "ASK-002")
	idxDone := strings.Index(asksView, "ASK-001")
	if idxOpen < 0 || idxDone < 0 || idxOpen > idxDone {
		t.Fatalf("expected open ASK-002 before done ASK-001:\n%s", asksView)
	}

	// Drop selected ask (ASK-002) with x inline prompt.
	press(t, m, "x")
	if m.askClosing != decide.AskStatusDropped {
		t.Fatalf("askClosing = %q, want dropped", m.askClosing)
	}
	for _, r := range "no longer needed" {
		m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{r}})
	}
	m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	if m.askClosing != "" {
		t.Fatalf("askClosing did not finish: err=%q", m.askCloseErr)
	}

	got, err := led.GetAsk("ASK-002")
	if err != nil {
		t.Fatal(err)
	}
	if got.Status != decide.AskStatusDropped || got.CloseReason != "no longer needed" {
		t.Fatalf("ASK-002 after drop = %+v", got)
	}
	// Source item thread gets a system note.
	it, err := led.Get(it2.ID)
	if err != nil {
		t.Fatalf("get %s: err=%v", it2.ID, err)
	}
	if n := len(it.Messages); n == 0 || it.Messages[n-1].Author != "system" || !strings.Contains(it.Messages[n-1].Text, "ASK-002") {
		t.Fatalf("expected system note on %s, got messages: %+v", it2.ID, it.Messages)
	}
}
