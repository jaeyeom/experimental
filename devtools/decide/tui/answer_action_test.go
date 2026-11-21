package tui

import (
	"strings"
	"testing"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestAnswerActionStayAndList(t *testing.T) {
	t.Run("stay is default", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question: "ship it?",
			Owner:    "ada",
			Kind:     decide.KindRelay,
			State:    decide.StateOpen,
			Options:  []decide.Option{{Key: "y", Label: "Yes"}, {Key: "n", Label: "No"}},
		})
		m := New(dir)
		m.width, m.height = 100, 30
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		press(t, m, "\r1")
		if m.mode != modeDetail || m.detail == nil || m.detail.ID != item.ID {
			t.Fatalf("stay left detail: mode=%d detail=%v", m.mode, m.detail)
		}
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateAnswered || !got.DoneAt.IsZero() || got.Answer == nil || got.Answer.Value != "y" {
			t.Fatalf("got = state %s doneAt %v answer %+v", got.State, got.DoneAt, got.Answer)
		}
	})

	t.Run("list returns to list", func(t *testing.T) {
		dir := t.TempDir()
		led := openLedger(t, dir)
		item := mustCreate(t, led, &decide.Item{
			Question: "ship it?",
			Owner:    "ada",
			Kind:     decide.KindRelay,
			State:    decide.StateOpen,
			Options:  []decide.Option{{Key: "y", Label: "Yes"}},
		})
		m := New(dir)
		m.width, m.height = 100, 30
		m.answerAction = decide.AnswerActionList
		if err := m.Reload(); err != nil {
			t.Fatal(err)
		}
		press(t, m, "\r1")
		if m.mode != modeList {
			t.Fatalf("list stayed in mode %d, want modeList", m.mode)
		}
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateAnswered || !got.DoneAt.IsZero() || got.Answer == nil || got.Answer.Value != "y" {
			t.Fatalf("got = state %s doneAt %v answer %+v", got.State, got.DoneAt, got.Answer)
		}
	})
}

func TestAnswerActionEdit(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "y", Label: "Yes, merge"}, {Key: "n", Label: "No"}},
	})
	m := New(dir)
	m.width, m.height = 100, 30
	m.answerAction = decide.AnswerActionEdit
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "\r1")
	if !m.replyFocus || m.reply != "Yes, merge" || m.pendingAnswerKey != "y" {
		t.Fatalf("edit prefill: focus=%v reply=%q key=%q", m.replyFocus, m.reply, m.pendingAnswerKey)
	}
	// Open question in edit mode must not show Send and archive.
	if strings.Contains(m.View(), "Send and archive") {
		t.Fatalf("open question showed Send and archive:\n%s", m.View())
	}
	press(t, m, "\rplease squash first\t\r")
	if m.mode != modeDetail || m.reply != "" || m.pendingAnswerKey != "" {
		t.Fatalf("after send: mode=%d reply=%q key=%q", m.mode, m.reply, m.pendingAnswerKey)
	}
	got, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != decide.StateAnswered || !got.DoneAt.IsZero() {
		t.Fatalf("state = %s doneAt = %v, want answered and not done", got.State, got.DoneAt)
	}
	if got.Answer == nil || got.Answer.Value != "y" {
		t.Fatalf("answer = %+v, want key y", got.Answer)
	}
	n := len(got.Messages)
	if n < 3 {
		t.Fatalf("messages = %+v, want question + answer + added text", got.Messages)
	}
	ansMsg := got.Messages[n-2]
	addMsg := got.Messages[n-1]
	if ansMsg.Author != "human" || ansMsg.OptionKey != "y" {
		t.Fatalf("answer message = %+v, want human option y", ansMsg)
	}
	if addMsg.Author != "human" || addMsg.Text != "please squash first" {
		t.Fatalf("added message = %+v, want human 'please squash first'", addMsg)
	}
}

func TestSendAndArchiveButtonVisibilityAndAction(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	question := mustCreate(t, led, &decide.Item{
		Question: "open question?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Options:  []decide.Option{{Key: "y", Label: "Yes"}},
	})
	note := mustCreate(t, led, &decide.Item{
		Question: "FYI build green",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
	})
	answered := mustCreate(t, led, &decide.Item{
		Question: "already answered?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateAnswered,
		Options:  []decide.Option{{Key: "y", Label: "Yes"}},
	})

	m := New(dir)
	m.width, m.height = 120, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Open question hides Send and archive.
	for i, it := range m.items {
		if it.ID == question.ID {
			m.cursor = i
			break
		}
	}
	press(t, m, "\r0")
	view := visibleLine(m.View())
	if strings.Contains(view, "Send and archive") {
		t.Fatalf("open question %s showed Send and archive:\n%s", question.ID, view)
	}
	if !strings.Contains(view, "[Send ^⏎]") || !strings.Contains(view, "[Discard M-d]") {
		t.Fatalf("missing Send/Discard hints:\n%s", view)
	}

	// Note (no options, no free text) shows Send and archive, and sending archives it.
	press(t, m, "\x1b\x1bu")
	for i, it := range m.items {
		if it.ID == note.ID {
			m.cursor = i
			break
		}
	}
	press(t, m, "\r0thanks")
	view = visibleLine(m.View())
	if !strings.Contains(view, "[Send and archive M-e]") {
		t.Fatalf("note missing [Send and archive M-e]:\n%s", view)
	}
	// Tab to Send, Tab to Send and archive, Enter.
	press(t, m, "\t\t\r")
	if m.mode != modeList {
		t.Fatalf("send and archive stayed in mode %d, want modeList", m.mode)
	}
	gotNote, err := led.Get(note.ID)
	if err != nil {
		t.Fatal(err)
	}
	if gotNote.State != decide.StateDone || gotNote.Outcome != "archived" {
		t.Fatalf("note after send and archive = state %s outcome %q", gotNote.State, gotNote.Outcome)
	}
	if len(gotNote.Messages) == 0 || gotNote.Messages[len(gotNote.Messages)-1].Text != "thanks" {
		t.Fatalf("note messages = %+v, want 'thanks'", gotNote.Messages)
	}

	// Answered thread shows Send and archive.
	press(t, m, "ga")
	for i, it := range m.items {
		if it.ID == answered.ID {
			m.cursor = i
			break
		}
	}
	press(t, m, "\r0noted")
	view = visibleLine(m.View())
	if !strings.Contains(view, "[Send and archive M-e]") {
		t.Fatalf("answered thread missing [Send and archive M-e]:\n%s", view)
	}
}

func TestDiscardAndEscDraftBehavior(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{
		Question: "FYI",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
	})
	m := New(dir)
	m.width, m.height = 120, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Esc keeps the draft.
	press(t, m, "\r0short draft\x1b")
	if m.reply != "short draft" || m.replyFocus {
		t.Fatalf("Esc did not keep draft: focus=%v reply=%q", m.replyFocus, m.reply)
	}

	// Short draft (<= 3 lines) clears at once on Discard without confirmation.
	press(t, m, "0")
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'d'}, Alt: true})
	if m.confirmKind != confirmNone || m.reply != "" || m.replyFocus {
		t.Fatalf("short discard: confirm=%d reply=%q focus=%v", m.confirmKind, m.reply, m.replyFocus)
	}

	// Long draft (> 3 lines) asks for confirmation; n keeps it, y discards it.
	press(t, m, "0line1\rline2\rline3\rline4")
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'d'}, Alt: true})
	if m.confirmKind != confirmDiscard || !strings.Contains(m.status, "discard draft?") {
		t.Fatalf("long discard did not ask: confirm=%d status=%q", m.confirmKind, m.status)
	}
	press(t, m, "n")
	if m.confirmKind != confirmNone || !strings.Contains(m.reply, "line4") {
		t.Fatalf("cancel discard lost draft: confirm=%d reply=%q", m.confirmKind, m.reply)
	}
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'d'}, Alt: true})
	press(t, m, "y")
	if m.confirmKind != confirmNone || m.reply != "" || m.replyFocus {
		t.Fatalf("confirm discard did not clear: confirm=%d reply=%q focus=%v", m.confirmKind, m.reply, m.replyFocus)
	}
}
