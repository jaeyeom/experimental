package tui

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// replyButtonModel is an opened relay thread, 100x30.
func replyButtonModel(t *testing.T) *Model {
	t.Helper()
	dir := t.TempDir()
	mustCreate(t, openLedger(t, dir), &decide.Item{
		Owner:    "test",
		Kind:     decide.KindRelay,
		Question: "Check?",
	})
	m := New(dir)
	m.width, m.height = 100, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m.openRow(); err != nil {
		t.Fatal(err)
	}
	return m
}

func TestReplyButtonFocusesReplyBox(t *testing.T) {
	t.Run("renders", func(t *testing.T) {
		m := replyButtonModel(t)
		if got := visibleLine(m.View()); !strings.Contains(got, "[Reply (0)]") {
			t.Fatalf("thread view missing [Reply (0)]:\n%s", got)
		}
	})
	t.Run("key", func(t *testing.T) {
		m := replyButtonModel(t)
		press(t, m, "0")
		if !m.replyFocus {
			t.Fatal("0 did not focus the reply box")
		}
	})
	t.Run("click", func(t *testing.T) {
		m := replyButtonModel(t)
		view := m.View()
		row := screenRow(view, "[Reply (0)]")
		if row == 0 {
			t.Fatalf("missing Reply button:\n%s", visibleLine(view))
		}
		col := visibleCol(strings.Split(view, "\n")[row-1], "Reply")
		if err := m.Click(col, row); err != nil {
			t.Fatal(err)
		}
		if !m.replyFocus {
			t.Fatal("click on [Reply] did not focus the reply box")
		}
	})
	t.Run("unbound shows no parentheses", func(t *testing.T) {
		km, err := decide.ResolveKeyMap(map[string][]string{"detail.reply": {}})
		if err != nil {
			t.Fatal(err)
		}
		m := replyButtonModel(t)
		m.SetKeys(km)
		got := visibleLine(m.View())
		if !strings.Contains(got, "[Reply]") {
			t.Fatalf("unbound reply should show [Reply]:\n%s", got)
		}
	})
	t.Run("remapped", func(t *testing.T) {
		km, err := decide.ResolveKeyMap(map[string][]string{"detail.reply": {"R"}})
		if err != nil {
			t.Fatal(err)
		}
		m := replyButtonModel(t)
		m.SetKeys(km)
		if got := visibleLine(m.View()); !strings.Contains(got, "[Reply (R)]") {
			t.Fatalf("remapped reply missing [Reply (R)]:\n%s", got)
		}
	})
}

func TestActiveButtonHintsAreNotFaint(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	t.Run("reply focused has bright reply hints and faint toolbar hints", func(t *testing.T) {
		m, _, _ := composerModel(t, composerReply)
		m.width, m.height = 120, 30
		raw := m.View()
		vis := visibleLine(raw)
		for _, want := range []string{"[Send (^⏎)]", "[Discard (M-d)]", "[Save draft (Esc)]", "[Edit in editor (^G)]"} {
			if !strings.Contains(vis, want) {
				t.Fatalf("missing %q:\n%s", want, vis)
			}
		}
		// Save draft sits right after Discard in the reply box.
		if !strings.Contains(vis, "[Discard (M-d)] [Save draft (Esc)]") {
			t.Fatalf("expected [Save draft (Esc)] next to Discard:\n%s", vis)
		}
		btnLine := lineContaining(raw, "[Send (^⏎)]")
		if lineHasFaint(btnLine) {
			t.Fatalf("active reply button line should not be faint: %q", btnLine)
		}
		toolbarLine := lineContaining(raw, "[Back")
		if !sgrFaint(sgrBeforeLine(t, toolbarLine, "(u)")) {
			t.Fatalf("toolbar hint should be faint while reply focused: %q", toolbarLine)
		}

		// After Esc with draft kept: toolbar is active (not faint), unfocused reply buttons are faint.
		press(t, m, "draft reply\x1b")
		rawAfter := m.View()
		toolbarAfter := lineContaining(rawAfter, "[Back")
		if lineHasFaint(toolbarAfter) {
			t.Fatalf("active toolbar line should not be faint after Esc: %q", toolbarAfter)
		}
		btnAfter := lineContaining(rawAfter, "[Send")
		for _, hint := range []string{"(^⏎)", "(M-d)", "(Esc)", "(^G)"} {
			if !sgrFaint(sgrBeforeLine(t, btnAfter, hint)) {
				t.Fatalf("unfocused reply button hint %s should be faint: %q", hint, btnAfter)
			}
		}

		m.ASCIIKeys = true
		m.focusReply()
		if got := visibleLine(m.View()); !strings.Contains(got, "[Send (C-Enter)]") {
			t.Fatalf("ascii missing [Send (C-Enter)]:\n%s", got)
		}
	})

	t.Run("compose active hints are not faint and picker disables editor hints", func(t *testing.T) {
		m, _, _ := composerModel(t, composerNote)
		m.width, m.height = 100, 30
		btnLine := lineContaining(m.View(), "[Send (^⏎)]")
		if lineHasFaint(btnLine) {
			t.Fatalf("active compose button line should not be faint: %q", btnLine)
		}

		// When recipient picker is focused, Save draft (Esc) is active (not faint),
		// while Send / Discard / Edit in editor do not work yet and are faint.
		m.ShiftTab()
		if m.composeFocus != focusRecipient {
			t.Fatalf("composeFocus = %d, want focusRecipient", m.composeFocus)
		}
		pickerBtns := lineContaining(m.View(), "[Send")
		if sgrFaint(sgrBeforeLine(t, pickerBtns, "(Esc)")) {
			t.Fatalf("Save draft (Esc) should not be faint in picker: %q", pickerBtns)
		}
		for _, hint := range []string{"(^⏎)", "(M-d)", "(^G)"} {
			if !sgrFaint(sgrBeforeLine(t, pickerBtns, hint)) {
				t.Fatalf("compose hint %s should be faint in picker: %q", hint, pickerBtns)
			}
		}
	})

	t.Run("outbox failed thread buttons follow active rule", func(t *testing.T) {
		m, _ := outboxFolderModel(t,
			OutboxEntry{ID: "OB-001", Op: "answer", Text: "y", Status: OutboxFailed, Error: "done"},
		)
		if _, err := m.Key('\r'); err != nil {
			t.Fatal(err)
		}
		raw := m.View()
		outboxLine := lineContaining(raw, "[Send as message (s)]")
		if lineHasFaint(outboxLine) {
			t.Fatalf("active outbox button line should not be faint: %q", outboxLine)
		}
		press(t, m, "0")
		outboxDim := lineContaining(m.View(), "[Send as message")
		if !sgrFaint(sgrBeforeLine(t, outboxDim, "(s)")) || !sgrFaint(sgrBeforeLine(t, outboxDim, "(x)")) {
			t.Fatalf("outbox button hints should be faint while reply focused: %q", outboxDim)
		}
	})

	t.Run("bus down prompt buttons are not faint", func(t *testing.T) {
		bus := &fakeBus{list: busList}
		m := busModel(t, bus)
		m.width, m.height = 140, 30
		loadBus(t, m)
		setHealth(t, m, healthDaemonDown)
		press(t, m, "cprimary\rhello bus\t\r")
		got := footer(t, m)
		if lineHasFaint(got) {
			t.Fatalf("bus-down prompt should not be faint: %q", got)
		}
	})
}
