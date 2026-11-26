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

func findDraft(t *testing.T, db *LocalBackend, key string) (decide.Draft, bool) {
	t.Helper()
	list, err := db.ListDrafts()
	if err != nil {
		t.Fatal(err)
	}
	for _, d := range list {
		if d.Key == key {
			return d, true
		}
	}
	return decide.Draft{}, false
}

func TestSaveDraftEscComposeAndReplyReopen(t *testing.T) {
	dir, _, item := draftTestSetup(t)
	m := New(dir)
	m.Targets = []Target{{Name: "ada", Pane: "p1"}}
	m.width, m.height = 120, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Compose: Save draft (Esc) keeps the draft in memory and on disk, and comes back when reopened.
	press(t, m, "c\tcompose saved\x1b")
	if m.composing || m.note != "compose saved" {
		t.Fatalf("after compose Esc: composing=%v note=%q", m.composing, m.note)
	}
	press(t, m, "c")
	if !m.composing || m.note != "compose saved" {
		t.Fatalf("reopened compose did not keep draft: composing=%v note=%q", m.composing, m.note)
	}
	press(t, m, "\x1b")

	// Restart recovers the saved compose draft via [r] Restore.
	m2 := New(dir)
	m2.Targets = []Target{{Name: "ada", Pane: "p1"}}
	m2.width, m2.height = 120, 30
	if err := m2.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m2, "cr")
	if m2.note != "compose saved" {
		t.Fatalf("restarted compose restore = %q, want %q", m2.note, "compose saved")
	}
	press(t, m2, "\x1b")

	// Reply: [Save draft (Esc)] button is shown and clicking or pressing Esc keeps and flushes the reply draft.
	press(t, m, "o0reply saved")
	view := visibleLine(m.View())
	if !strings.Contains(view, "[Save draft (Esc)]") {
		t.Fatalf("reply box missing [Save draft (Esc)]:\n%s", view)
	}
	clickOn(t, m, "[Save draft (Esc)]", 1)
	if m.replyFocus || m.reply != "reply saved" {
		t.Fatalf("clicking [Save draft (Esc)] replyFocus=%v reply=%q", m.replyFocus, m.reply)
	}
	// Reload in a fresh model verifies it was persisted and comes back on reopen.
	m3 := New(dir)
	m3.width, m3.height = 120, 30
	if err := m3.Reload(); err != nil {
		t.Fatal(err)
	}
	press(t, m3, "o")
	if m3.reply != "reply saved" {
		t.Fatalf("reopened reply = %q, want %q", m3.reply, "reply saved")
	}
	db := &LocalBackend{Dir: dir}
	d, ok := findDraft(t, db, decide.ReplyDraftKey(item.ID))
	if !ok || d.Text != "reply saved" {
		t.Fatalf("stored reply draft = (%+v, %v)", d, ok)
	}
}

func TestForwardDraftSavedPerItemRestoredAndCleared(t *testing.T) {
	dir, led, item1 := draftTestSetup(t)
	item2 := mustCreate(t, led, &decide.Item{
		Question: "second thread?",
		Owner:    "beta",
		Kind:     decide.KindRelay,
		State:    decide.StateOpen,
		Messages: []decide.Message{{Author: "beta", Text: "second thread?"}},
	})
	m := New(dir)
	m.Targets = []Target{{Name: "ada", Pane: "p1"}, {Name: "beta", Pane: "p2"}}
	m.runSend = func([]string) error { return nil }
	m.width, m.height = 120, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Open forward on item1, edit text and target, then press Esc (Save draft).
	if err := m.openItemByID(item1.ID); err != nil {
		t.Fatal(err)
	}
	press(t, m, "f")
	m.ShiftTab()
	m.Update(tea.KeyMsg{Type: tea.KeyDown})
	press(t, m, "\t")
	press(t, m, "note for item1")
	press(t, m, "\x1b")
	if m.composing || m.note != "" || m.forward != "" {
		t.Fatalf("Esc in forward should close and clear in-memory compose state: composing=%v note=%q forward=%q", m.composing, m.note, m.forward)
	}

	// Plain compose stays clean and does not inherit the forward draft.
	press(t, m, "c")
	if m.note != "" || m.forward != "" {
		t.Fatalf("plain compose polluted by forward: note=%q forward=%q", m.note, m.forward)
	}
	press(t, m, "\x1b")

	// Forward on item2 gets its own separate draft key and prefill.
	press(t, m, "u")
	if err := m.openItemByID(item2.ID); err != nil {
		t.Fatal(err)
	}
	press(t, m, "fnote for item2\x1bu")

	db := &LocalBackend{Dir: dir}
	d1, ok1 := findDraft(t, db, decide.ForwardDraftKey(item1.ID))
	d2, ok2 := findDraft(t, db, decide.ForwardDraftKey(item2.ID))
	if !ok1 || !strings.HasPrefix(d1.Text, "note for item1") || d1.To != "beta" {
		t.Fatalf("item1 forward draft = (%+v, %v)", d1, ok1)
	}
	if !ok2 || !strings.HasPrefix(d2.Text, "note for item2") {
		t.Fatalf("item2 forward draft = (%+v, %v)", d2, ok2)
	}

	// Restarting and reopening forward on item1 restores note and target.
	m2 := New(dir)
	m2.Targets = []Target{{Name: "ada", Pane: "p1"}, {Name: "beta", Pane: "p2"}}
	m2.runSend = func([]string) error { return nil }
	m2.width, m2.height = 120, 30
	if err := m2.Reload(); err != nil {
		t.Fatal(err)
	}
	if err := m2.openItemByID(item1.ID); err != nil {
		t.Fatal(err)
	}
	press(t, m2, "f")
	if !strings.HasPrefix(m2.note, "note for item1") || m2.forward != item1.ID {
		t.Fatalf("reopened forward note=%q forward=%q", m2.note, m2.forward)
	}
	if tg, ok := m2.selectedTarget(); !ok || tg.Name != "beta" {
		t.Fatalf("reopened forward target = %+v, ok=%v", tg, ok)
	}

	// Sending clears the forward draft for item1 while keeping item2's draft.
	press(t, m2, "\t\r")
	if m2.composing {
		t.Fatal("send did not close forward")
	}
	if _, ok := findDraft(t, db, decide.ForwardDraftKey(item1.ID)); ok {
		t.Fatal("item1 forward draft was not cleared on send")
	}
	if _, ok := findDraft(t, db, decide.ForwardDraftKey(item2.ID)); !ok {
		t.Fatal("item2 forward draft should still exist")
	}
}

func TestComposeDiscardConfirmationAndClear(t *testing.T) {
	dir, _, item := draftTestSetup(t)
	m := New(dir)
	m.Targets = []Target{{Name: "ada", Pane: "p1"}}
	m.width, m.height = 120, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	// Empty compose discards immediately without confirmation.
	press(t, m, "c\t")
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'d'}, Alt: true})
	if m.confirmKind != confirmNone || m.composing || m.status != "discarded draft" {
		t.Fatalf("empty discard: confirm=%d composing=%v status=%q", m.confirmKind, m.composing, m.status)
	}

	// Non-empty compose asks for confirmation; n keeps the draft, y clears it and deletes stored draft.
	press(t, m, "c\thello compose\x1b")
	db := &LocalBackend{Dir: dir}
	if _, ok := findDraft(t, db, decide.DraftKeyCompose); !ok {
		t.Fatal("compose draft should be saved before discard")
	}
	press(t, m, "c\t")
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'d'}, Alt: true})
	if m.confirmKind != confirmDiscardCompose || !strings.Contains(m.status, "discard draft?") {
		t.Fatalf("non-empty compose discard did not ask: confirm=%d status=%q", m.confirmKind, m.status)
	}
	press(t, m, "n")
	if m.confirmKind != confirmNone || !m.composing || m.note != "hello compose" {
		t.Fatalf("cancel compose discard lost draft: confirm=%d composing=%v note=%q", m.confirmKind, m.composing, m.note)
	}
	m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'d'}, Alt: true})
	press(t, m, "y")
	if m.confirmKind != confirmNone || m.composing || m.note != "" || m.status != "discarded draft" {
		t.Fatalf("confirmed compose discard did not clear: confirm=%d composing=%v note=%q status=%q", m.confirmKind, m.composing, m.note, m.status)
	}
	if _, ok := findDraft(t, db, decide.DraftKeyCompose); ok {
		t.Fatal("stored compose draft was not deleted on discard")
	}

	// Forward discard also asks confirmation and clears stored forward draft.
	press(t, m, "ofedited fwd\x1b")
	if _, ok := findDraft(t, db, decide.ForwardDraftKey(item.ID)); !ok {
		t.Fatal("forward draft should be saved")
	}
	press(t, m, "f")
	clickOn(t, m, "[Discard (M-d)]", 1)
	if m.confirmKind != confirmDiscardCompose {
		t.Fatalf("forward discard confirm=%d, want confirmDiscardCompose", m.confirmKind)
	}
	press(t, m, "y")
	if m.composing || m.note != "" || m.forward != "" {
		t.Fatalf("forward discard left state: composing=%v note=%q forward=%q", m.composing, m.note, m.forward)
	}
	if _, ok := findDraft(t, db, decide.ForwardDraftKey(item.ID)); ok {
		t.Fatal("stored forward draft was not deleted on discard")
	}
}
