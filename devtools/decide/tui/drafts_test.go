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

func draftTestSetup(t *testing.T) (string, *decide.Ledger, decide.Item) {
	t.Helper()
	t.Setenv("NO_COLOR", "1")
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	item := mustCreate(t, led, &decide.Item{
		Question: "ship persistent drafts?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		FreeText: true,
		Messages: []decide.Message{{Author: "ada", Text: "ship persistent drafts?"}},
	})
	return dir, led, item
}

func TestDraftDebounceAndRestartRestore(t *testing.T) {
	dir, _, item := draftTestSetup(t)

	now := time.Date(2026, 11, 21, 12, 0, 0, 0, time.UTC)
	m := New(dir)
	m.Now = func() time.Time { return now }
	m.width = 80
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Open item, focus reply, type text.
	press(t, m, "o0hello")
	if m.reply != "hello" {
		t.Fatalf("reply = %q, want %q", m.reply, "hello")
	}

	// Before 1.5s, debounce tick must NOT save.
	now = now.Add(1000 * time.Millisecond)
	sendKeyMsg(t, m, draftTickMsg{kind: composerReply, seq: m.replyDraft.seq})
	db := &LocalBackend{Dir: dir}
	list, err := db.ListDrafts()
	if err != nil {
		t.Fatal(err)
	}
	if len(list) != 0 {
		t.Fatalf("saved before debounce: %+v", list)
	}

	// After 1.5s from last edit, debounce tick saves.
	now = now.Add(600 * time.Millisecond)
	sendKeyMsg(t, m, draftTickMsg{kind: composerReply, seq: m.replyDraft.seq})
	list, err = db.ListDrafts()
	if err != nil {
		t.Fatal(err)
	}
	if len(list) != 1 || list[0].Key != decide.ReplyDraftKey(item.ID) || list[0].Text != "hello" || list[0].Rev != 1 {
		t.Fatalf("after debounce: %+v", list)
	}

	// Also test answer draft flush on backToList.
	m.focusAnswer()
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("a")})
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("n")})
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune("s")})
	if m.draft != "ans" {
		t.Fatalf("draft = %q, want %q", m.draft, "ans")
	}
	press(t, m, "\x1bu") // Esc answer, u backToList -> flushes answer draft.

	// Restart with a fresh Model on the same backend.
	m2 := New(dir)
	m2.width = 80
	m2.height = 24
	if err := m2.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m2, "o")
	if m2.reply != "hello" || m2.replyDraft.baseRev != 1 {
		t.Fatalf("restored reply = %q baseRev=%d", m2.reply, m2.replyDraft.baseRev)
	}
	if m2.draft != "ans" || m2.answerDraft.baseRev != 1 {
		t.Fatalf("restored answer = %q baseRev=%d", m2.draft, m2.answerDraft.baseRev)
	}
}

func TestDraftEmptyDeletesAndTooLargeWarns(t *testing.T) {
	dir, _, item := draftTestSetup(t)
	m := New(dir)
	m.width = 80
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Save a draft via flush on backToList.
	press(t, m, "o0hi\x1bu")
	db := &LocalBackend{Dir: dir}
	if list, _ := db.ListDrafts(); len(list) != 1 {
		t.Fatalf("expected 1 draft, got %+v", list)
	}

	// Re-open, clear text to whitespace-only, leave -> deletes draft.
	press(t, m, "o0")
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyBackspace})
	sendKeyMsg(t, m, tea.KeyMsg{Type: tea.KeyBackspace})
	press(t, m, " ")
	press(t, m, "\x1bu")
	if list, _ := db.ListDrafts(); len(list) != 0 {
		t.Fatalf("expected draft deleted when cleared, got %+v", list)
	}

	// Oversized draft (>64 KiB) warns and keeps in-memory text.
	press(t, m, "o0")
	big := strings.Repeat("x", decide.MaxDraftTextBytes+1)
	m.setComposerText(composerReply, big)
	m.markDraftDirty(composerReply)
	m.flushDraft(composerReply)
	if !strings.Contains(m.status, "64 KiB") {
		t.Fatalf("expected size warning, got status %q", m.status)
	}
	if m.reply != big {
		t.Fatalf("in-memory reply len = %d, want %d", len(m.reply), len(big))
	}
	if list, _ := db.ListDrafts(); len(list) != 0 {
		t.Fatalf("oversized draft should not be stored for %s: %+v", item.ID, list)
	}
}

func TestDraftListMarksAndSendDiscardCleanup(t *testing.T) {
	dir, _, item := draftTestSetup(t)
	db := &LocalBackend{Dir: dir}
	if _, _, err := db.PutDraft(decide.Draft{
		Key:  decide.ReplyDraftKey(item.ID),
		Text: "unsent reply",
	}, 0); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.width = 100
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view := m.View()
	if !strings.Contains(view, "draft") || strings.Contains(view, "draft!") {
		t.Fatalf("list view should show 'draft' mark:\n%s", view)
	}

	// Conflict changes mark to draft!
	if _, _, err := db.PutDraft(decide.Draft{
		Key:       decide.ReplyDraftKey(item.ID),
		Text:      "other edit",
		UpdatedBy: "tiger",
	}, 0); err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	view = m.View()
	if !strings.Contains(view, "draft!") {
		t.Fatalf("list view should show 'draft!' mark:\n%s", view)
	}

	// Open and discard clears the draft and the list mark.
	press(t, m, "o")
	m.discardReply()
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "u")
	view = m.View()
	if strings.Contains(view, "relay draft") {
		t.Fatalf("list view should not show draft mark after discard:\n%s", view)
	}

	// Sending reply also deletes draft.
	press(t, m, "o0sending now")
	m.markDraftDirty(composerReply)
	m.flushDraft(composerReply)
	if list, _ := db.ListDrafts(); len(list) != 1 {
		t.Fatalf("expected 1 draft before send, got %+v", list)
	}
	if err := m.submitReply(); err != nil {
		t.Fatal(err)
	}
	if list, _ := db.ListDrafts(); len(list) != 0 {
		t.Fatalf("expected 0 drafts after send, got %+v", list)
	}
}

func TestComposeRestoreDiscardAndForwardNoClobber(t *testing.T) {
	dir, _, item := draftTestSetup(t)
	db := &LocalBackend{Dir: dir}
	if _, _, err := db.PutDraft(decide.Draft{
		Key:     decide.DraftKeyCompose,
		Kind:    decide.DraftKindCompose,
		To:      "worker",
		Bus:     true,
		Forward: item.ID,
		Text:    "saved compose body",
	}, 0); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.Targets = []Target{{Name: "ada", Pane: "p1"}, {Name: "worker", Pane: "agentbus:worker", Bus: true}}
	m.width = 80
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Opening fresh compose shows restore/discard prompt.
	press(t, m, "c")
	view := m.View()
	if !strings.Contains(view, "Saved draft") || !strings.Contains(view, "[r] Restore") || !strings.Contains(view, "[d] Discard") {
		t.Fatalf("compose restore prompt missing:\n%s", view)
	}

	// Restore populates To/Bus, Forward, Text, and baseRev.
	press(t, m, "r")
	if m.note != "saved compose body" || m.forward != item.ID || m.composeDraft.baseRev != 1 {
		t.Fatalf("restored note=%q forward=%q baseRev=%d", m.note, m.forward, m.composeDraft.baseRev)
	}
	if tg, ok := m.selectedTarget(); !ok || tg.Name != "worker" {
		t.Fatalf("restored target = %+v, ok=%v", tg, ok)
	}

	// Forwarding an item (f) does not clobber forward prefill with saved compose draft.
	m.clearNote()
	press(t, m, "of")
	if !m.composing || m.composeRestorePrompt || !strings.Contains(m.note, "Forwarded from "+item.ID) {
		t.Fatalf("forward clobbered or showed restore: composing=%v prompt=%v note=%q", m.composing, m.composeRestorePrompt, m.note)
	}

	// Esc discards forward prefill, then fresh compose + 'd' discards the saved compose draft.
	press(t, m, "\x1bu")
	press(t, m, "c")
	if !m.composeRestorePrompt {
		t.Fatal("expected compose restore prompt after forward cancelled")
	}
	press(t, m, "d")
	if m.composeRestorePrompt || m.note != "" {
		t.Fatalf("after discard: prompt=%v note=%q", m.composeRestorePrompt, m.note)
	}
	if list, _ := db.ListDrafts(); len(list) != 0 {
		t.Fatalf("expected compose draft deleted, got %+v", list)
	}
}

func TestConflictResolutionKeepUseMerge(t *testing.T) {
	dir, _, item := draftTestSetup(t)
	db := &LocalBackend{Dir: dir}
	key := decide.ReplyDraftKey(item.ID)

	seedConflict := func() {
		_ = db.DeleteDraft(key, "")
		v1, _, err := db.PutDraft(decide.Draft{Key: key, Text: "local text", UpdatedBy: "dragon"}, 0)
		if err != nil {
			t.Fatal(err)
		}
		if _, conflict, err := db.PutDraft(decide.Draft{Key: key, Text: "remote text", UpdatedBy: "tiger"}, 0); err != nil || !conflict {
			t.Fatalf("seed conflict failed: conflict=%v err=%v", conflict, err)
		}
		_ = v1
	}

	// 1. Keep current.
	seedConflict()
	m := New(dir)
	m.width = 100
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "o")
	view := m.View()
	if !strings.Contains(view, "Conflict with tiger") {
		t.Fatalf("expected conflict banner:\n%s", view)
	}
	m.resolveConflict(composerReply, conflictKeepCurrent)
	if m.reply != "local text" || m.hasDraftConflict(composerReply) {
		t.Fatalf("keep current failed: reply=%q conflict=%v", m.reply, m.hasDraftConflict(composerReply))
	}

	// 2. Use other.
	seedConflict()
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "uo")
	m.resolveConflict(composerReply, conflictUseOther)
	if m.reply != "remote text" || m.hasDraftConflict(composerReply) {
		t.Fatalf("use other failed: reply=%q conflict=%v", m.reply, m.hasDraftConflict(composerReply))
	}
	if list, _ := db.ListDrafts(); len(list) != 1 || list[0].Text != "remote text" || len(list[0].Conflicts) != 0 {
		t.Fatalf("stored draft after use other: %+v", list)
	}

	// 3. Merge both.
	seedConflict()
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m, "uo")
	m.resolveConflict(composerReply, conflictMergeBoth)
	if !strings.Contains(m.reply, "<<<<<<< current\nlocal text\n=======\nremote text\n>>>>>>> tiger ") {
		t.Fatalf("merge both text unexpected:\n%s", m.reply)
	}
	if m.hasDraftConflict(composerReply) {
		t.Fatal("conflict should be cleared after merge")
	}
}

func TestExternalEditorSavesAndDetectsConflict(t *testing.T) {
	dir, _, item := draftTestSetup(t)
	db := &LocalBackend{Dir: dir}
	key := decide.ReplyDraftKey(item.ID)
	if _, _, err := db.PutDraft(decide.Draft{Key: key, Text: "v1"}, 0); err != nil {
		t.Fatal(err)
	}

	m := New(dir)
	m.width = 100
	m.height = 24
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	log := filepath.Join(t.TempDir(), "editor.log")
	script := writeEditorScript(t, t.TempDir(), log, "replace")
	t.Setenv("VISUAL", script)
	t.Setenv("EDITOR", "")
	m.suspend, m.resume = logHooks(log)

	press(t, m, "o0")
	if m.replyDraft.baseRev != 1 {
		t.Fatalf("baseRev = %d, want 1", m.replyDraft.baseRev)
	}

	// Start external edit, simulate another client updating the draft while $EDITOR is open.
	cmd := m.startExternalEdit()
	if cmd == nil || m.editor == nil {
		t.Fatal("startExternalEdit did not initialize editor")
	}
	if _, _, err := db.PutDraft(decide.Draft{Key: key, Text: "from remote", UpdatedBy: "tiger"}, 1); err != nil {
		t.Fatal(err)
	}
	if err := m.editor.Run(); err != nil {
		t.Fatal(err)
	}
	m.finishExternalEdit(nil)

	if m.reply != "replaced" {
		t.Fatalf("reply = %q, want %q", m.reply, "replaced")
	}
	if !m.hasDraftConflict(composerReply) {
		t.Fatal("expected conflict banner after concurrent edit during $EDITOR")
	}
	if !strings.Contains(m.View(), "Conflict with") {
		t.Fatalf("conflict banner not rendered:\n%s", m.View())
	}

	// Also verify emptying text in $EDITOR deletes the draft.
	_ = db.DeleteDraft(key, "")
	v, _, err := db.PutDraft(decide.Draft{Key: key, Text: "to clear"}, 0)
	if err != nil {
		t.Fatal(err)
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	emptyScript := filepath.Join(t.TempDir(), "empty-ed.sh")
	if err := os.WriteFile(emptyScript, []byte("#!/bin/sh\n: > \"$1\"\n"), 0o755); err != nil { //nolint:gosec // G306: test script
		t.Fatal(err)
	}
	t.Setenv("VISUAL", emptyScript)
	if m.replyDraft.baseRev != v.Rev {
		t.Fatalf("baseRev = %d, want %d", m.replyDraft.baseRev, v.Rev)
	}
	finishEditor(t, m, editorKeys(m, tea.KeyCtrlG))
	if list, _ := db.ListDrafts(); len(list) != 0 {
		t.Fatalf("expected draft deleted after emptying in $EDITOR, got %+v", list)
	}
}
