package tui

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestBottomHelpLineOmitsButtonKeysAndHidesWhenAllCovered(t *testing.T) {
	t.Parallel()
	t.Run("thread view", func(t *testing.T) {
		m, _, _ := toolbarModel(t, &decide.Item{Question: "thread help line"}, 160, 24)
		m.status = ""
		vis := visibleLine(m.View())
		for _, btn := range []string{"[Back (u)]", "[Reply (0)]", "[Archive (e)]", "[Archive with note (E)]", "[Forward (f)]", "[Avatar (A)]"} {
			if !strings.Contains(vis, btn) {
				t.Fatalf("thread view missing button %q:\n%s", btn, vis)
			}
		}

		foot := footer(t, m)
		for _, covered := range []string{"u list", "0 reply", "e/y archive", "E archive", "E outcome", "f forward", "A avatar"} {
			if strings.Contains(foot, covered) {
				t.Fatalf("thread footer contains button-covered hint %q: %q", covered, foot)
			}
		}
		for _, uncovered := range []string{"q quit", "j/k pgup/pgdn home/end", "n/p", "/find", "b snooze", "g go to", "o owner", "! flag", "? help"} {
			if !strings.Contains(foot, uncovered) {
				t.Fatalf("thread footer missing uncovered hint %q: %q", uncovered, foot)
			}
		}

		// ? help still lists every key, including toolbar-button keys.
		m.height = 60
		press(t, m, "?")
		help := visibleLine(m.View())
		for _, want := range []string{"u returns to the list", "0 reply", "f forward", "e/y archive", "E archive with note", "A — set avatar"} {
			if !strings.Contains(help, want) {
				t.Fatalf("thread ? help missing %q:\n%s", want, help)
			}
		}
		press(t, m, "?")

		// When every non-button key is unbound (only button-covered keys remain),
		// there is no help line.
		km, err := decide.ResolveKeyMap(map[string][]string{
			"quit":               {},
			"help":               {},
			"search":             {},
			"goto.inbox":         {},
			"goto.snoozed":       {},
			"goto.answered":      {},
			"goto.done":          {},
			"goto.withdrawn":     {},
			"goto.status":        {},
			"goto.contacts":      {},
			"goto.outbox":        {},
			"goto.asks":          {},
			"goto.settings":      {},
			"goto.top":           {},
			"detail.scrollDown":  {},
			"detail.scrollUp":    {},
			"detail.pageDown":    {},
			"detail.pageUp":      {},
			"detail.top":         {},
			"detail.bottom":      {},
			"detail.nextMessage": {},
			"detail.prevMessage": {},
			"detail.snooze":      {},
			"detail.owner":       {},
			"detail.flag":        {},
		})
		if err != nil {
			t.Fatal(err)
		}
		m.SetKeys(km)
		if got := m.detailHintLine(); got != "" {
			t.Fatalf("detailHintLine = %q, want empty when all keys covered", got)
		}
		if got := m.detailOptionalHints(""); len(got) != 0 {
			t.Fatalf("detailOptionalHints = %v, want empty when all keys covered", got)
		}
		if got := footer(t, m); got != "mouse on" {
			t.Fatalf("thread footer when all keys covered = %q, want %q", got, "mouse on")
		}
	})

	t.Run("reply", func(t *testing.T) {
		m, _, _ := composerModel(t, composerReply)
		m.width, m.height = 160, 30
		m.status = ""
		vis := visibleLine(m.View())
		for _, btn := range []string{"[Send (^⏎)]", "[Discard (M-d)]", "[Save draft (Esc)]", "[Edit in editor (^G)]"} {
			if !strings.Contains(vis, btn) {
				t.Fatalf("reply view missing button %q:\n%s", btn, vis)
			}
		}

		foot := footer(t, m)
		for _, covered := range []string{"editor", "send", "save draft"} {
			if strings.Contains(foot, covered) {
				t.Fatalf("reply footer contains button-covered hint %q: %q", covered, foot)
			}
		}
		for _, uncovered := range []string{"C-a/C-e line", "C-k kill", "C-j newline", "C-o open", "Alt+q fill", "Tab [Send]"} {
			if !strings.Contains(foot, uncovered) {
				t.Fatalf("reply footer missing uncovered hint %q: %q", uncovered, foot)
			}
		}

		// ? help still lists every key.
		m.height = 60
		press(t, m, "\x1b?")
		help := visibleLine(m.View())
		for _, want := range []string{
			"C-x C-e or C-g edits the draft",
			"Ctrl+Enter, Alt+Enter or C-s sends the draft",
			"Alt+d discards",
			"esc keeps the draft",
		} {
			if !strings.Contains(help, want) {
				t.Fatalf("reply ? help missing %q:\n%s", want, help)
			}
		}
		press(t, m, "?0")

		// Tab to reply buttons shows only button navigation, not detail browse hints.
		press(t, m, "\t")
		if !m.replySend {
			t.Fatal("tab did not focus reply buttons")
		}
		btnFoot := footer(t, m)
		if strings.Contains(btnFoot, "q quit") || strings.Contains(btnFoot, "? help") {
			t.Fatalf("reply button-focus footer leaked detail hints: %q", btnFoot)
		}
		if !strings.Contains(btnFoot, "Tab/Shift+Tab move") || !strings.Contains(btnFoot, "Enter select") {
			t.Fatalf("reply button-focus footer = %q", btnFoot)
		}

		// When every non-button key in reply is unbound, there is no help line.
		m.focusReply()
		km, err := decide.ResolveKeyMap(map[string][]string{
			"edit.lineStart":    {},
			"edit.lineEnd":      {},
			"edit.killLine":     {},
			"edit.newline":      {},
			"edit.openLine":     {},
			"edit.fill":         {},
			"compose.nextField": {},
		})
		if err != nil {
			t.Fatal(err)
		}
		m.SetKeys(km)
		if got := m.composerHintLine(composerReply); got != "" {
			t.Fatalf("composerHintLine(composerReply) = %q, want empty", got)
		}
		if got := footer(t, m); got != "mouse on" {
			t.Fatalf("reply footer when all keys covered = %q, want %q", got, "mouse on")
		}
	})

	t.Run("compose", func(t *testing.T) {
		m, _, _ := composerModel(t, composerNote)
		m.width, m.height = 160, 30
		m.status = ""
		vis := visibleLine(m.View())
		for _, btn := range []string{"[Send (^⏎)]", "[Discard (M-d)]", "[Save draft (Esc)]", "[Edit in editor (^G)]"} {
			if !strings.Contains(vis, btn) {
				t.Fatalf("compose view missing button %q:\n%s", btn, vis)
			}
		}

		foot := footer(t, m)
		for _, covered := range []string{"editor", "send", "save draft"} {
			if strings.Contains(foot, covered) {
				t.Fatalf("compose footer contains button-covered hint %q: %q", covered, foot)
			}
		}
		for _, uncovered := range []string{"C-a/C-e line", "C-k kill", "C-j newline", "C-o open", "Alt+q fill", "Tab [Send]"} {
			if !strings.Contains(foot, uncovered) {
				t.Fatalf("compose footer missing uncovered hint %q: %q", uncovered, foot)
			}
		}

		// Recipient picker omits Esc save draft when filter is empty (covered by [Save draft (Esc)]),
		// and shows Esc clear while a filter is typed.
		m.ShiftTab()
		if m.composeFocus != focusRecipient {
			t.Fatalf("composeFocus = %d, want focusRecipient", m.composeFocus)
		}
		pickerFoot := footer(t, m)
		if strings.Contains(pickerFoot, "save draft") || strings.Contains(pickerFoot, "Esc") {
			t.Fatalf("picker footer with empty filter should omit Esc save draft: %q", pickerFoot)
		}
		for _, uncovered := range []string{"type to filter", "↑/↓ C-n/C-p move", "Enter pick", "* star", "Tab text"} {
			if !strings.Contains(pickerFoot, uncovered) {
				t.Fatalf("picker footer missing %q: %q", uncovered, pickerFoot)
			}
		}
		press(t, m, "a")
		if got := footer(t, m); !strings.Contains(got, "Esc clear") || strings.Contains(got, "save draft") {
			t.Fatalf("filtered picker footer = %q, want Esc clear without save draft", got)
		}
		press(t, m, "\x1b")

		// Tab to compose buttons shows button navigation, not list browse hints.
		press(t, m, "\t\t")
		if m.composeFocus != focusSend {
			t.Fatalf("composeFocus = %d, want focusSend", m.composeFocus)
		}
		btnFoot := footer(t, m)
		if strings.Contains(btnFoot, "g go to") || strings.Contains(btnFoot, "? help") {
			t.Fatalf("compose button-focus footer leaked list hints: %q", btnFoot)
		}
		if !strings.Contains(btnFoot, "Tab/Shift+Tab move") || !strings.Contains(btnFoot, "Enter select") {
			t.Fatalf("compose button-focus footer = %q", btnFoot)
		}

		// ? help still lists every key.
		press(t, m, "\x1b")
		m.height = 60
		press(t, m, "?")
		help := visibleLine(m.View())
		for _, want := range []string{
			"C-x C-e or C-g edits the draft",
			"Ctrl+Enter, Alt+Enter or C-s sends the draft",
			"Alt+d discards",
			"esc keeps the draft",
		} {
			if !strings.Contains(help, want) {
				t.Fatalf("compose ? help missing %q:\n%s", want, help)
			}
		}
		press(t, m, "?")

		// When every non-button key in compose text is unbound, there is no help line.
		press(t, m, "c\t")
		km, err := decide.ResolveKeyMap(map[string][]string{
			"edit.lineStart":    {},
			"edit.lineEnd":      {},
			"edit.killLine":     {},
			"edit.newline":      {},
			"edit.openLine":     {},
			"edit.fill":         {},
			"compose.nextField": {},
		})
		if err != nil {
			t.Fatal(err)
		}
		m.SetKeys(km)
		if got := m.composerHintLine(composerNote); got != "" {
			t.Fatalf("composerHintLine(composerNote) = %q, want empty", got)
		}
		if got := footer(t, m); got != "mouse on" {
			t.Fatalf("compose footer when all keys covered = %q, want %q", got, "mouse on")
		}
	})
}
