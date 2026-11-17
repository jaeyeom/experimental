package tui

import (
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"

	"github.com/jaeyeom/experimental/devtools/decide"
)

const workerWarning = "This reply goes to ops/x (owner); bea is working on it."

// seedWorkedThread is a bus thread owned by ops/x that bea works on.
func seedWorkedThread(t *testing.T, dir string) decide.Item {
	t.Helper()
	item := seedBusThread(t, dir)
	got, err := openLedger(t, dir).Assign(item.ID, decide.Assign{Worker: "bea"})
	if err != nil {
		t.Fatal(err)
	}
	return got
}

// busSends are the names each agentbus send went to.
func busSends(bus *markBus) []string {
	var out []string
	for _, c := range bus.calls {
		if rest, ok := strings.CutPrefix(c, "send "); ok {
			name, _, _ := strings.Cut(rest, " ")
			out = append(out, name)
		}
	}
	return out
}

func TestReplyToAWorkedItemAsksWhoToNotify(t *testing.T) {
	cases := []struct {
		key  string
		want []string
	}{
		{"w", []string{"bea"}},
		{"b", []string{"ops/x", "bea"}},
		{"o", []string{"ops/x"}},
	}
	for _, tc := range cases {
		t.Run(tc.key, func(t *testing.T) {
			t.Setenv("XDG_CONFIG_HOME", t.TempDir())
			dir := t.TempDir()
			item := seedWorkedThread(t, dir)
			bus := &markBus{send: "m-5 delivered"}
			m := markModel(t, dir, bus)
			press(t, m, "o")
			press(t, m, "0hello\t\r")
			if got := busSends(bus); len(got) != 0 {
				t.Fatalf("sent before the choice: %q", got)
			}
			if m.confirmKind != confirmRoute || !strings.Contains(m.status, workerWarning) {
				t.Fatalf("confirm %d status %q", m.confirmKind, m.status)
			}
			press(t, m, tc.key)
			if got := busSends(bus); strings.Join(got, ",") != strings.Join(tc.want, ",") {
				t.Fatalf("sends = %q, want %q", got, tc.want)
			}
			if m.confirmKind != confirmNone || m.reply != "" || !strings.HasPrefix(m.status, "replied ") {
				t.Fatalf("confirm %d reply %q status %q", m.confirmKind, m.reply, m.status)
			}
			got, err := openLedger(t, dir).Get(item.ID)
			if err != nil {
				t.Fatal(err)
			}
			if len(got.Delivery) != len(tc.want) {
				t.Fatalf("delivery = %+v", got.Delivery)
			}
			// The next reply asks again.
			press(t, m, "0again\t\r")
			if m.confirmKind != confirmRoute {
				t.Fatalf("second reply did not ask; status %q", m.status)
			}
		})
	}
}

func TestReplyRouteCancelKeepsTheDraft(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	seedWorkedThread(t, dir)
	bus := &markBus{send: "m-5 delivered"}
	m := markModel(t, dir, bus)
	press(t, m, "o")
	press(t, m, "0hello\t\r")
	sendKey(t, m, tea.KeyMsg{Type: tea.KeyEsc})
	if got := busSends(bus); len(got) != 0 {
		t.Fatalf("cancel sent %q", got)
	}
	if m.confirmKind != confirmNone || m.reply != "hello" {
		t.Fatalf("confirm %d reply %q", m.confirmKind, m.reply)
	}
}

func TestHeaderShowsWhoIsWorkingOnIt(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	seedWorkedThread(t, dir)
	m := markModel(t, dir, &markBus{})
	m.width, m.height = 140, 40
	press(t, m, "o")
	view := m.View()
	for _, line := range strings.Split(view, "\n") {
		if strings.Contains(line, "Working on it:") && strings.Contains(line, " bea ") {
			return
		}
	}
	t.Fatalf("header has no worker\n%s", view)
}
