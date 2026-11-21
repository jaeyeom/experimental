package tui

import (
	"context"
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
)

// setHealth hands m a finished health check, the cached state the
// banner and the send prompt read.
func setHealth(t *testing.T, m *Model, out string) {
	t.Helper()
	report, err := checkHealth(t.Context(), func(_ context.Context, _ []string) ([]byte, error) {
		return []byte(out), nil
	})
	if err != nil {
		t.Fatal(err)
	}
	m.Update(healthMsg{report: report})
}

// sendCalls counts the `agentbus send` runs.
func sendCalls(bus *fakeBus) int {
	n := 0
	for _, c := range bus.calls {
		if c[0] == "send" {
			n++
		}
		if c[0] == "health" {
			panic("a send ran a new health check")
		}
	}
	return n
}

func TestBusDownPromptsBeforeComposeToABusName(t *testing.T) {
	bus := &fakeBus{list: busList, sendOut: "m-7f3a queued\n"}
	m := busModel(t, bus)
	m.width, m.height = 140, 30
	loadBus(t, m)
	setHealth(t, m, healthDaemonDown)
	press(t, m, "cprimary\rhello bus\t\r")
	if n := sendCalls(bus); n != 0 {
		t.Fatalf("sent %d times before the prompt was answered", n)
	}
	if m.confirmKind != confirmBusDown {
		t.Fatalf("no prompt; status %q", m.status)
	}
	if got := footer(t, m); !strings.Contains(got, busDownPrompt) || !strings.Contains(got, "[Send anyway y]") || !strings.Contains(got, "Cancel n") {
		t.Fatalf("footer = %q", got)
	}
	// The prompt takes only keys: a click on the screen sends nothing.
	m.mouseOn = true
	for y := range m.height {
		for x := 0; x < m.width; x += 4 {
			m.Update(tea.MouseMsg{X: x, Y: y, Button: tea.MouseButtonLeft, Action: tea.MouseActionPress})
		}
	}
	if n := sendCalls(bus); n != 0 || m.confirmKind != confirmBusDown {
		t.Fatalf("a click sent %d times; confirm %d", n, m.confirmKind)
	}
	// Send anyway has the focus, so Enter sends.
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
	if n := sendCalls(bus); n != 1 {
		t.Fatalf("Send anyway sent %d times", n)
	}
	if m.confirmKind != confirmNone || m.note != "" || m.composing || m.status != "sent to coord/primary" {
		t.Fatalf("confirm %d note %q composing %v status %q", m.confirmKind, m.note, m.composing, m.status)
	}
}

func TestBusDownCancelKeepsTheComposeDraft(t *testing.T) {
	for _, key := range []struct {
		name string
		msg  tea.KeyMsg
	}{
		{"esc", tea.KeyMsg{Type: tea.KeyEsc}},
		{"n", tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'n'}}},
		{"tab enter", tea.KeyMsg{Type: tea.KeyTab}},
	} {
		t.Run(key.name, func(t *testing.T) {
			bus := &fakeBus{list: busList}
			m := busModel(t, bus)
			m.width, m.height = 140, 30
			loadBus(t, m)
			setHealth(t, m, healthTransportDown)
			press(t, m, "cprimary\rhello bus\t\r")
			sendKey(t, m, key.msg)
			if key.msg.Type == tea.KeyTab {
				if got := footer(t, m); !strings.Contains(got, "[Cancel n]") {
					t.Fatalf("Tab did not move the focus: %q", got)
				}
				sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
			}
			if n := sendCalls(bus); n != 0 {
				t.Fatalf("Cancel sent %d times", n)
			}
			if m.confirmKind != confirmNone || m.note != "hello bus" || !m.composing || m.composeFocus != focusSend {
				t.Fatalf("confirm %d note %q composing %v focus %d", m.confirmKind, m.note, m.composing, m.composeFocus)
			}
		})
	}
}

func TestBusDownPromptsBeforeReplyToABusThread(t *testing.T) {
	bus := &fakeBus{list: busList, sendOut: "m-7f3a queued\n"}
	m := busModel(t, bus)
	loadBus(t, m)
	press(t, m, "cprimary\rhello bus\t\r")
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	setHealth(t, m, healthDaemonDown)
	press(t, m, "o0again")
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyCtrlS})
	if n := sendCalls(bus); n != 1 || m.confirmKind != confirmBusDown {
		t.Fatalf("sends %d confirm %d status %q", n, m.confirmKind, m.status)
	}
	// The prompt takes the keys: typing does not edit the draft.
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyBackspace})
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEsc})
	if n := sendCalls(bus); n != 1 || m.reply != "again" || !m.replySend {
		t.Fatalf("cancel: sends %d reply %q on [Send] %v", n, m.reply, m.replySend)
	}
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEnter})
	if m.confirmKind != confirmBusDown {
		t.Fatal("the second send did not ask again")
	}
	press(t, m, "y")
	if n := sendCalls(bus); n != 2 || m.reply != "" || !strings.HasPrefix(m.status, "replied ") {
		t.Fatalf("send anyway: sends %d reply %q status %q", n, m.reply, m.status)
	}
}

func TestBusOKSendsWithoutPrompt(t *testing.T) {
	bus := &fakeBus{list: busList, sendOut: "m-7f3a queued\n"}
	m := busModel(t, bus)
	loadBus(t, m)
	setHealth(t, m, healthOK)
	press(t, m, "cprimary\rhello bus\t\r")
	if n := sendCalls(bus); n != 1 || m.confirmKind != confirmNone {
		t.Fatalf("sends %d confirm %d", n, m.confirmKind)
	}
}

func TestBusDownDoesNotPromptForAPaneTarget(t *testing.T) {
	bus := &fakeBus{list: busList}
	m := busModel(t, bus)
	var sent [][]string
	m.runSend = func(argv []string) error {
		sent = append(sent, argv)
		return nil
	}
	loadBus(t, m)
	setHealth(t, m, healthDaemonDown)
	press(t, m, "cinbox\rhello pane\t\r")
	if m.confirmKind != confirmNone || len(sent) != 1 || m.status != "sent to inbox" {
		t.Fatalf("confirm %d sent %q status %q", m.confirmKind, sent, m.status)
	}
	// A reply in that thread goes to the pane too.
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o0again\t\r")
	if m.confirmKind != confirmNone || !strings.HasPrefix(m.status, "replied ") {
		t.Fatalf("reply: confirm %d status %q", m.confirmKind, m.status)
	}
}
