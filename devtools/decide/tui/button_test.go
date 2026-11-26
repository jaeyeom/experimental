package tui

import (
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestButtonLabelsAndBindings(t *testing.T) {
	t.Run("compose send", func(t *testing.T) {
		m, _, _ := composerModel(t, composerNote)
		m.width, m.height = 80, 30
		view := visibleLine(m.View())
		if !strings.Contains(view, "[Send (^⏎)]") {
			t.Fatalf("compose send missing [Send (^⏎)]:\n%s", view)
		}
		m.composeFocus = focusSend
		view = visibleLine(m.View())
		if !strings.Contains(view, "> [Send (^⏎)]") {
			t.Fatalf("focused compose send missing > [Send (^⏎)]:\n%s", view)
		}
	})

	t.Run("reply send", func(t *testing.T) {
		m, _, _ := composerModel(t, composerReply)
		m.width, m.height = 80, 30
		view := visibleLine(m.View())
		if !strings.Contains(view, "[Send (^⏎)]") {
			t.Fatalf("reply send missing [Send (^⏎)]:\n%s", view)
		}
		press(t, m, "\t")
		view = visibleLine(m.View())
		if !strings.Contains(view, "> [Send (^⏎)]") {
			t.Fatalf("focused reply send missing > [Send (^⏎)]:\n%s", view)
		}
	})

	t.Run("mark done", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		mustCreate(t, led, &decide.Item{
			Owner:    "test",
			Kind:     decide.KindTypeIn,
			Question: "Run command",
			TypeText: "make test",
			Where:    "pane-1",
		})
		m := New(dir)
		m.width, m.height = 80, 30
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		if err := m.openRow(); err != nil {
			t.Fatal(err)
		}
		if got := visibleLine(m.View()); !strings.Contains(got, "[mark done (E)]") {
			t.Fatalf("mark done missing [mark done (E)]:\n%s", got)
		}
	})

	t.Run("route prompt", func(t *testing.T) {
		dir := t.TempDir()
		seedWorkedThread(t, dir)
		m := markModel(t, dir, &markBus{send: "m-5 delivered"})
		press(t, m, "o")
		press(t, m, "0hello\t\r")
		for _, want := range []string{"[worker (w)]", "[both (b)]", "[owner only (o)]", "[cancel (Esc)]"} {
			if !strings.Contains(m.status, want) {
				t.Fatalf("route status = %q, missing %q", m.status, want)
			}
		}
	})

	t.Run("bus down prompt", func(t *testing.T) {
		bus := &fakeBus{list: busList}
		m := busModel(t, bus)
		m.width, m.height = 140, 30
		loadBus(t, m)
		setHealth(t, m, healthDaemonDown)
		press(t, m, "cprimary\rhello bus\t\r")
		got := footer(t, m)
		if !strings.Contains(got, "[Send anyway (y)]") || !strings.Contains(got, "Cancel (n)") {
			t.Fatalf("bus-down footer = %q", got)
		}
	})

	t.Run("confirm quit", func(t *testing.T) {
		dir := t.TempDir()
		now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
		ob := &fakeOutboxBackend{
			LocalBackend: &LocalBackend{Dir: dir, Now: func() time.Time { return now }},
			entries:      []OutboxEntry{{ID: "OB-001", Op: "reply", ItemID: "D-001", Text: "a", WrittenAt: now, Status: OutboxPending}},
		}
		m := New(dir)
		m.backend = ob
		m.width, m.height = 80, 24
		m.Now = func() time.Time { return now }
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		if _, err := m.Key('q'); err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(m.status, "[Yes (y)] [No (n)]") {
			t.Fatalf("quit confirm status = %q, want [Yes (y)] [No (n)]", m.status)
		}
	})
}

func TestButtonLabelFollowsRemappedKey(t *testing.T) {
	km, err := decide.ResolveKeyMap(map[string][]string{
		"compose.send":  {"ctrl+g"},
		"edit.external": {"ctrl+x ctrl+e"},
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, kind := range []composerKind{composerNote, composerReply} {
		m, _, _ := composerModel(t, kind)
		m.SetKeys(km)
		m.width, m.height = 80, 30
		if got := visibleLine(m.View()); !strings.Contains(got, "[Send (^G)]") {
			t.Fatalf("composer %v remapped view missing [Send (^G)]:\n%s", kind, got)
		}
	}
}

func TestButtonASCIIFallback(t *testing.T) {
	t.Run("config asciiKeys", func(t *testing.T) {
		m, _, _ := composerModel(t, composerNote)
		m.ASCIIKeys = true
		m.width, m.height = 80, 30
		if got := visibleLine(m.View()); !strings.Contains(got, "[Send (C-Enter)]") {
			t.Fatalf("ASCIIKeys compose missing [Send (C-Enter)]:\n%s", got)
		}

		km, err := decide.ResolveKeyMap(map[string][]string{
			"compose.send":  {"ctrl+g"},
			"edit.external": {"ctrl+x ctrl+e"},
		})
		if err != nil {
			t.Fatal(err)
		}
		m.SetKeys(km)
		if got := visibleLine(m.View()); !strings.Contains(got, "[Send (C-g)]") {
			t.Fatalf("ASCIIKeys ctrl+g missing [Send (C-g)]:\n%s", got)
		}
	})

	t.Run("non-utf8 locale", func(t *testing.T) {
		t.Setenv("LC_ALL", "C")
		m, _, _ := composerModel(t, composerReply)
		m.width, m.height = 80, 30
		if got := visibleLine(m.View()); !strings.Contains(got, "[Send (C-Enter)]") {
			t.Fatalf("LC_ALL=C reply missing [Send (C-Enter)]:\n%s", got)
		}
	})
}

func TestButtonNarrowWidthDropsHintBeforeName(t *testing.T) {
	if got := buttonLabel("Send", "^⏎", false, 11); got != "[Send (^⏎)]" {
		t.Errorf("width 11 = %q, want [Send (^⏎)]", got)
	}
	if got := buttonLabel("Send", "^⏎", false, 10); got != "[Send]" {
		t.Errorf("width 10 = %q, want [Send]", got)
	}
	if got := buttonLabel("Send", "^⏎", false, 6); got != "[Send]" {
		t.Errorf("width 6 = %q, want [Send]", got)
	}
	if got := buttonLabel("Send", "^⏎", false, 5); got != "[Sen…" {
		t.Errorf("width 5 = %q, want [Sen…", got)
	}

	m, _, _ := composerModel(t, composerReply)
	// Outer frame (2) + detail box (2) leaves 6 inner columns: [Send (^⏎)] (11)
	// drops its hint to [Send] (6) without truncating the name.
	m.width, m.height = 10, 20
	view := visibleLine(m.View())
	if !strings.Contains(view, "[Send]") || strings.Contains(view, "^⏎") {
		t.Fatalf("narrow reply should drop hint and keep [Send]:\n%s", view)
	}
	// With 5 inner columns, [Send] itself is truncated.
	m.width = 9
	view = visibleLine(m.View())
	if !strings.Contains(view, "[Sen…") {
		t.Fatalf("very narrow reply should truncate [Send] after hint is gone:\n%s", view)
	}
}

func TestButtonWithNoDirectKeyShowsNoHint(t *testing.T) {
	if got := buttonLabel("Send", "", false, 0); got != "[Send]" {
		t.Errorf("empty hint = %q, want [Send]", got)
	}

	km, err := decide.ResolveKeyMap(map[string][]string{
		"compose.send": {},
	})
	if err != nil {
		t.Fatal(err)
	}
	m, _, _ := composerModel(t, composerNote)
	m.SetKeys(km)
	m.width, m.height = 80, 30
	view := visibleLine(m.View())
	if !strings.Contains(view, "[Send]") || strings.Contains(view, "[Send (") {
		t.Fatalf("unbound compose.send should show [Send] with no hint:\n%s", view)
	}

	// Close button on owner-gone item has no direct key.
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Owner:    "test",
		Kind:     decide.KindRelay,
		Question: "Check?",
		FollowUp: decide.FollowUpOwnerGone,
	})
	m2 := New(dir)
	m2.width, m2.height = 80, 30
	if err := m2.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m2.openRow(); err != nil {
		t.Fatal(err)
	}
	m2.detail.FollowUp = decide.FollowUpOwnerGone
	view2 := visibleLine(m2.View())
	closeRow := screenRow(view2, "Close")
	if closeRow == 0 {
		t.Fatalf("missing Close button:\n%s", view2)
	}
	closeLine := strings.Trim(strings.Split(view2, "\n")[closeRow-1], "│ ")
	if closeLine != "Close" {
		t.Fatalf("Close button should have no shortcut hint, got %q", closeLine)
	}
}

func TestButtonClickAreaCoversHint(t *testing.T) {
	t.Run("compose send", func(t *testing.T) {
		m, _, _ := composerModel(t, composerNote)
		m.width, m.height = 80, 30
		var sent []string
		m.runSend = func(argv []string) error {
			sent = append([]string(nil), argv...)
			return nil
		}
		press(t, m, "hello")
		view := m.View()
		row := screenRow(view, "[Send (^⏎)]")
		line := strings.Split(view, "\n")[row-1]
		// Click on the closing bracket after the ^⏎ hint.
		hintCol := visibleCol(line, "^⏎)]") + 2
		if err := m.Click(hintCol, row); err != nil {
			t.Fatal(err)
		}
		if len(sent) == 0 || m.composing {
			t.Fatalf("click on hint did not send: sent=%v composing=%v", sent, m.composing)
		}
	})

	t.Run("reply send", func(t *testing.T) {
		m, led, item := composerModel(t, composerReply)
		m.width, m.height = 80, 30
		press(t, m, "reply text")
		view := m.View()
		row := screenRow(view, "[Send (^⏎)]")
		line := strings.Split(view, "\n")[row-1]
		// Click on the ^⏎ hint itself.
		hintCol := visibleCol(line, "^⏎)]")
		if err := m.Click(hintCol, row); err != nil {
			t.Fatal(err)
		}
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if len(got.Messages) < 2 || got.Messages[len(got.Messages)-1].Text != "reply text" {
			t.Fatalf("click on reply hint did not send: messages=%+v", got.Messages)
		}
	})

	_ = tea.KeyEnter
}
