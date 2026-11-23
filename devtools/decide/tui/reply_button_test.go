package tui

import (
	"regexp"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// replyButtonModel is an opened relay thread, 80x30.
func replyButtonModel(t *testing.T) *Model {
	t.Helper()
	dir := t.TempDir()
	mustCreate(t, openLedger(t, dir), &decide.Item{
		Owner:    "test",
		Kind:     decide.KindRelay,
		Question: "Check?",
	})
	m := New(dir)
	m.width, m.height = 80, 30
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

func TestButtonHintsAreDimmedInParentheses(t *testing.T) {
	dimmed := regexp.MustCompile(`\x1b\[(?:\d+;)*2(?:;\d+)*m \(\^⏎\)`)
	m, _, _ := composerModel(t, composerReply)
	m.width, m.height = 80, 30
	raw := m.View()
	if !strings.Contains(visibleLine(raw), "[Send (^⏎)]") {
		t.Fatalf("missing [Send (^⏎)]:\n%s", visibleLine(raw))
	}
	if !dimmed.MatchString(raw) {
		t.Fatalf("hint not dimmed: %q", raw)
	}
	m.ASCIIKeys = true
	if got := visibleLine(m.View()); !strings.Contains(got, "[Send (C-Enter)]") {
		t.Fatalf("ascii missing [Send (C-Enter)]:\n%s", got)
	}
	if !strings.Contains(visibleLine(m.View()), "[Discard (") {
		t.Fatalf("missing [Discard (...)]:\n%s", visibleLine(m.View()))
	}
}
