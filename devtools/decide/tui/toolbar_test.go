package tui

import (
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

func toolbarModel(t *testing.T, it *decide.Item, width, height int) (*Model, *decide.Ledger, decide.Item) {
	t.Helper()
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	if it.Owner == "" {
		it.Owner = "ada"
	}
	if it.Kind == "" {
		it.Kind = decide.KindRelay
	}
	if it.CreatedAt.IsZero() {
		it.CreatedAt = now.Add(-time.Hour)
	}
	created := mustCreate(t, led, it)
	m := New(dir)
	m.Now = func() time.Time { return now }
	m.Targets = []Target{{Name: "ada", Pane: "pane-1"}}
	m.runSend = func([]string) error { return nil }
	m.width, m.height = width, height
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	return m, led, created
}

func clickMouse(t *testing.T, m *Model, substr string) tea.Cmd {
	t.Helper()
	view := m.View()
	row := screenRow(view, substr)
	if row == 0 {
		t.Fatalf("missing %q in view:\n%s", substr, visibleLine(view))
	}
	col := visibleCol(strings.Split(view, "\n")[row-1], substr)
	if col == 0 {
		t.Fatalf("missing col for %q in row %d:\n%s", substr, row, visibleLine(view))
	}
	_, cmd := m.Update(tea.MouseMsg{
		X:      col - 1,
		Y:      row - 1,
		Button: tea.MouseButtonLeft,
		Action: tea.MouseActionPress,
	})
	return cmd
}

func TestThreadViewRemovesReplyPromptAndDrawsTitleRule(t *testing.T) {
	m, _, _ := toolbarModel(t, &decide.Item{Question: longBody()}, 120, 20)
	view := m.View()
	plain := visibleLine(view)
	if strings.Contains(plain, "reply: ___") {
		t.Fatalf("thread view still contains reply: ___:\n%s", plain)
	}

	// Row 0 = outer frame top, Row 1 = detail box top, Row 2 = floating title + toolbar, Row 3 = horizontal rule.
	lines := strings.Split(plain, "\n")
	if len(lines) < 5 {
		t.Fatalf("too few lines:\n%s", plain)
	}
	ruleRow := strings.TrimSuffix(strings.TrimPrefix(lines[3], "││"), "││")
	wantRule := strings.Repeat("─", 120-4)
	if ruleRow != wantRule {
		t.Fatalf("row under floating title = %q, want horizontal rule %q\n%s", ruleRow, wantRule, plain)
	}

	// Rule stays pinned below the floating title after scrolling down.
	m.scrollEnd()
	scrolledLines := strings.Split(visibleLine(m.View()), "\n")
	scrolledRule := strings.TrimSuffix(strings.TrimPrefix(scrolledLines[3], "││"), "││")
	if scrolledRule != wantRule {
		t.Fatalf("scrolled row under floating title = %q, want %q", scrolledRule, wantRule)
	}

	// Toolbar actions drop out of the bottom key-help line; the rest stays.
	m.status = ""
	footer := lineContaining(visibleLine(m.View()), "? help")
	for _, dropped := range []string{"0 reply", "f forward", "u list", "e/y archive", "E archive with note"} {
		if strings.Contains(footer, dropped) {
			t.Fatalf("footer still contains toolbar action %q: %q", dropped, footer)
		}
	}
	for _, kept := range []string{"q quit", "j/k", "n/p", "? help"} {
		if !strings.Contains(footer, kept) {
			t.Fatalf("footer missing %q: %q", kept, footer)
		}
	}
}

func TestThreadToolbarButtonsClickAndCustomKeymap(t *testing.T) {
	t.Run("default buttons and clicks", func(t *testing.T) {
		m, _, _ := toolbarModel(t, &decide.Item{Question: "hello toolbar"}, 120, 24)
		title := pinnedTitle(m.View())
		wantToolbar := "[Back (u)] [Reply (0)] [Archive (e)] [Archive with note (E)] [Forward (f)] [Avatar (A)]"
		if !strings.HasPrefix(title, wantToolbar+" ") {
			t.Fatalf("title line = %q, want prefix %q", title, wantToolbar+" ")
		}

		// Reply
		clickOn(t, m, "[Reply (0)]", 1)
		if !m.replyFocus {
			t.Fatal("clicking [Reply (0)] did not focus reply box")
		}
		press(t, m, "\x1b")

		// Archive with note
		clickOn(t, m, "[Archive with note (E)]", 1)
		if m.mode != modeOutcome {
			t.Fatalf("clicking [Archive with note (E)] mode = %d, want modeOutcome", m.mode)
		}
		press(t, m, "\x1b")

		// Forward
		clickOn(t, m, "[Forward (f)]", 1)
		if !m.composing || m.forward == "" {
			t.Fatalf("clicking [Forward (f)] composing=%v forward=%q", m.composing, m.forward)
		}
		press(t, m, "\x1b")

		// Avatar
		clickOn(t, m, "[Avatar (A)]", 1)
		if !m.avatarPick {
			t.Fatal("clicking [Avatar (A)] did not open avatar picker")
		}
		press(t, m, "\x1b")

		// Back
		clickOn(t, m, "[Back (u)]", 1)
		if m.mode != modeList {
			t.Fatalf("clicking [Back (u)] mode = %d, want modeList", m.mode)
		}
	})

	t.Run("archive button archives item", func(t *testing.T) {
		m, led, item := toolbarModel(t, &decide.Item{Question: "note to archive"}, 120, 24)
		clickOn(t, m, "[Archive (e)]", 1)
		if m.mode != modeList {
			t.Fatalf("after archive mode = %d, want modeList (status=%q)", m.mode, m.status)
		}
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateDone || got.Outcome != "archived" {
			t.Fatalf("item = state %s outcome %q, want done/archived", got.State, got.Outcome)
		}
	})

	t.Run("custom keymap updates toolbar hints", func(t *testing.T) {
		km, err := decide.ResolveKeyMap(map[string][]string{
			"detail.back":    {"B"},
			"detail.reply":   {"R"},
			"detail.archive": {"x"},
			"detail.outcome": {"D"},
			"detail.forward": {"F"},
			"detail.avatar":  {"a"},
		})
		if err != nil {
			t.Fatal(err)
		}
		m, _, _ := toolbarModel(t, &decide.Item{Question: "custom keys"}, 120, 24)
		m.SetKeys(km)
		title := pinnedTitle(m.View())
		wantToolbar := "[Back (B)] [Reply (R)] [Archive (x)] [Archive with note (D)] [Forward (F)] [Avatar (a)]"
		if !strings.HasPrefix(title, wantToolbar+" ") {
			t.Fatalf("custom title line = %q, want prefix %q", title, wantToolbar+" ")
		}
	})
}

func TestThreadToolbarWithReplyFocused(t *testing.T) {
	dimmedHint := regexp.MustCompile(`\x1b\[(?:\d+;)*2(?:;\d+)*m \(u\)`)
	m, _, _ := toolbarModel(t, &decide.Item{Question: "reply focus test"}, 120, 24)

	// Before focusing reply: toolbar hints are NOT dimmed.
	rawBefore := strings.Split(m.View(), "\n")[2]
	if dimmedHint.MatchString(rawBefore) {
		t.Fatalf("toolbar hint dimmed before reply focus: %q", rawBefore)
	}

	// Focus reply box.
	press(t, m, "0")
	if !m.replyFocus {
		t.Fatal("0 did not focus reply box")
	}
	rawFocused := strings.Split(m.View(), "\n")[2]
	if !dimmedHint.MatchString(rawFocused) {
		t.Fatalf("toolbar hint not dimmed while reply focused: %q", rawFocused)
	}

	// Send, Send and archive, Discard stay at the bottom, not in the toolbar.
	title := pinnedTitle(m.View())
	for _, bottomOnly := range []string{"Send", "Discard"} {
		if strings.Contains(title, bottomOnly) {
			t.Fatalf("toolbar contains bottom button %q: %q", bottomOnly, title)
		}
	}

	// Typing toolbar keys goes into the reply box.
	press(t, m, "uedfA")
	if m.reply != "uedfA" || m.mode != modeDetail || m.composing || m.avatarPick {
		t.Fatalf("typed keys did not stay in reply box: reply=%q mode=%d composing=%v avatar=%v", m.reply, m.mode, m.composing, m.avatarPick)
	}

	// Toolbar buttons still work when clicked while reply is focused.
	clickOn(t, m, "[Archive with note (E)]", 1)
	if m.mode != modeOutcome {
		t.Fatalf("clicking [Archive with note (E)] while reply focused: mode=%d, want modeOutcome", m.mode)
	}
	press(t, m, "\x1b")

	// Refocus reply, then Esc leaves reply and restores un-dimmed hints & keyboard shortcuts.
	press(t, m, "0")
	if !m.replyFocus {
		t.Fatal("failed to refocus reply")
	}
	press(t, m, "\x1b")
	if m.replyFocus {
		t.Fatal("Esc did not leave reply box")
	}
	rawAfterEsc := strings.Split(m.View(), "\n")[2]
	if dimmedHint.MatchString(rawAfterEsc) {
		t.Fatalf("toolbar hint still dimmed after Esc: %q", rawAfterEsc)
	}
	press(t, m, "u")
	if m.mode != modeList {
		t.Fatalf("u after Esc did not return to list: mode=%d", m.mode)
	}
}

func TestToolbarBackAndArchiveSaveUnsentReplyDraft(t *testing.T) {
	t.Run("back saves draft", func(t *testing.T) {
		m, _, item := toolbarModel(t, &decide.Item{Question: "draft on back"}, 120, 24)
		press(t, m, "0unsent reply before back")
		clickOn(t, m, "[Back (u)]", 1)
		if m.mode != modeList {
			t.Fatalf("mode = %d, want modeList", m.mode)
		}
		db := &LocalBackend{Dir: m.Dir}
		list, err := db.ListDrafts()
		if err != nil {
			t.Fatal(err)
		}
		key := decide.ReplyDraftKey(item.ID)
		var found string
		for _, d := range list {
			if d.Key == key {
				found = d.Text
			}
		}
		if found != "unsent reply before back" {
			t.Fatalf("saved draft = %q, want %q (all=%+v)", found, "unsent reply before back", list)
		}
	})

	t.Run("archive saves draft first", func(t *testing.T) {
		m, led, item := toolbarModel(t, &decide.Item{Question: "draft on archive"}, 120, 24)
		press(t, m, "0unsent reply before archive")
		clickOn(t, m, "[Archive (e)]", 1)
		if m.mode != modeList {
			t.Fatalf("mode = %d, want modeList (status=%q)", m.mode, m.status)
		}
		got, err := led.Get(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != decide.StateDone || got.Outcome != "archived" {
			t.Fatalf("item = state %s outcome %q, want done/archived", got.State, got.Outcome)
		}
		db := &LocalBackend{Dir: m.Dir}
		list, err := db.ListDrafts()
		if err != nil {
			t.Fatal(err)
		}
		key := decide.ReplyDraftKey(item.ID)
		var found string
		for _, d := range list {
			if d.Key == key {
				found = d.Text
			}
		}
		if found != "unsent reply before archive" {
			t.Fatalf("saved draft = %q, want %q (all=%+v)", found, "unsent reply before archive", list)
		}
	})
}

func TestComposeCancelAndEditInEditorButtons(t *testing.T) {
	t.Run("compose buttons render and save draft works", func(t *testing.T) {
		m, _, _ := composerModel(t, composerNote)
		m.width, m.height = 100, 24
		press(t, m, "keep me")
		view := visibleLine(m.View())
		for _, want := range []string{"[Send (^⏎)]", "[Discard (M-d)]", "[Save draft (Esc)]", "[Edit in editor (^G)]"} {
			if !strings.Contains(view, want) {
				t.Fatalf("compose view missing %q:\n%s", want, view)
			}
		}
		clickOn(t, m, "[Save draft (Esc)]", 1)
		if m.composing {
			t.Fatal("clicking [Save draft (Esc)] did not close compose")
		}
		if m.note != "keep me" {
			t.Fatalf("clicking [Save draft (Esc)] lost compose draft: %q", m.note)
		}
	})

	t.Run("compose edit in editor launches external editor", func(t *testing.T) {
		m, _, _ := composerModel(t, composerNote)
		m.width, m.height = 100, 24
		log := filepath.Join(t.TempDir(), "editor.log")
		script := writeEditorScript(t, t.TempDir(), log, "replace")
		t.Setenv("VISUAL", script)
		t.Setenv("EDITOR", "")
		press(t, m, "draft note")
		cmd := clickMouse(t, m, "[Edit in editor (^G)]")
		if cmd == nil {
			t.Fatal("clicking [Edit in editor (^G)] in compose did not return exec cmd")
		}
		finishEditor(t, m, cmd)
		if m.note != "replaced" {
			t.Fatalf("compose note = %q, want %q", m.note, "replaced")
		}
		if !m.composing {
			t.Fatal("compose closed after external editor")
		}
	})

	t.Run("reply edit in editor launches external editor", func(t *testing.T) {
		m, _, _ := composerModel(t, composerReply)
		m.width, m.height = 100, 24
		log := filepath.Join(t.TempDir(), "editor.log")
		script := writeEditorScript(t, t.TempDir(), log, "replace")
		t.Setenv("VISUAL", script)
		t.Setenv("EDITOR", "")
		press(t, m, "draft reply")
		view := visibleLine(m.View())
		if !strings.Contains(view, "[Edit in editor (^G)]") {
			t.Fatalf("reply view missing [Edit in editor (^G)]:\n%s", view)
		}
		cmd := clickMouse(t, m, "[Edit in editor (^G)]")
		if cmd == nil {
			t.Fatal("clicking [Edit in editor (^G)] in reply did not return exec cmd")
		}
		finishEditor(t, m, cmd)
		if m.reply != "replaced" {
			t.Fatalf("reply = %q, want %q", m.reply, "replaced")
		}
	})

	t.Run("custom edit.external shows shortest bound key", func(t *testing.T) {
		km, err := decide.ResolveKeyMap(map[string][]string{
			"edit.external": {"ctrl+x ctrl+e", "ctrl+t"},
		})
		if err != nil {
			t.Fatal(err)
		}
		m, _, _ := composerModel(t, composerNote)
		m.width, m.height = 100, 24
		m.SetKeys(km)
		if got := visibleLine(m.View()); !strings.Contains(got, "[Edit in editor (^T)]") {
			t.Fatalf("compose custom shortest key missing [Edit in editor (^T)]:\n%s", got)
		}

		kmChordOnly, err := decide.ResolveKeyMap(map[string][]string{
			"edit.external": {"ctrl+x ctrl+e"},
		})
		if err != nil {
			t.Fatal(err)
		}
		m.SetKeys(kmChordOnly)
		if got := visibleLine(m.View()); !strings.Contains(got, "[Edit in editor (^X ^E)]") {
			t.Fatalf("compose chord-only key missing [Edit in editor (^X ^E)]:\n%s", got)
		}
	})
}

func TestToolbarNarrowCollapsesToHintsAndTruncatesTitleFirst(t *testing.T) {
	longQ := "this is a very long thread title that should truncate before the toolbar collapses"
	m, _, item := toolbarModel(t, &decide.Item{Question: longQ}, 108, 20)

	// At width 108 (inner 104): full toolbar (87) + space (1) leaves 16 cols for title -> title truncates first while toolbar stays full.
	midTitle := pinnedTitle(m.View())
	wantFull := "[Back (u)] [Reply (0)] [Archive (e)] [Archive with note (E)] [Forward (f)] [Avatar (A)]"
	if !strings.HasPrefix(midTitle, wantFull+" "+item.ID) || !strings.Contains(midTitle, "…") {
		t.Fatalf("width 108 title = %q, want full toolbar + truncated title", midTitle)
	}

	// At narrow width 60 (inner 56): toolbar collapses to key hints.
	m.width = 60
	narrowTitle := pinnedTitle(m.View())
	wantCollapsed := "[u] [0] [e] [E] [f] [A]"
	if !strings.HasPrefix(narrowTitle, wantCollapsed+" "+item.ID) {
		t.Fatalf("width 60 title = %q, want prefix %q", narrowTitle, wantCollapsed+" "+item.ID)
	}

	// Collapsed buttons are still clickable.
	clickOn(t, m, "[0]", 1)
	if !m.replyFocus {
		t.Fatal("clicking collapsed [0] did not focus reply box")
	}
	clickOn(t, m, "[u]", 1)
	if m.mode != modeList {
		t.Fatalf("clicking collapsed [u] mode = %d, want modeList", m.mode)
	}
}
